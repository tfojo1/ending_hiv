# Settings and Imports ####
library('data.table')
library('dplyr')


# Functions: Defunct
# TableauPrep.cols.output.names.get.i <- function(field) {
#   ifelse(
#     field %in% .cols.statistic.factorValues, 
#     paste(.pivotCols.names, field, sep='.'),
#     field)
# }


# Functions: General
mergeData.union <- function(df1, df2) {  # --> data.frame
  bind_rows(df1, df2)
}


# Functions: Module specific ####
TableauPrep.cols.standardize.renameCols <- function(
  srcDf,  # data.frame
  mapping  # list
) {  # --> data.frame
  newColNames = sapply(
    colnames(srcDf), 
    function(fieldName) {
      ifelse(
        fieldName %in% names(mapping),
        mapping[[fieldName]][['renameTo']],
        fieldName
      )})
  names(newColNames) = c()
  
  names(srcDf) = newColNames
  
  return(srcDf)  # data.frame
}


TableauPrep.cols.standardize.newCols.add <- function(
  df,  # data.frame
  whichDataset,  # char[]
  mapping  # List[Any]
) {  # --> data.frame
  
  # Determine required cols
  requiredCols = c()
  for (input.col in names(mapping)) {
    output.col = mapping[[input.col]][['renameTo']]
    if (mapping[[input.col]][['required']] == TRUE) {
      requiredCols = c(requiredCols, output.col) }}
  requiredCols = unique(requiredCols)
  
  # Determine missing cols
  missingCols = c()  # char[]
  for (reqCol in requiredCols)
    if (!(reqCol %in% colnames(df)))
      missingCols = c(missingCols, reqCol)
  missingCols  # char[]
  
  # Add default seed values for missing cols
  possibleDatasets = c('epi', 'sim')
  for (missingCol in missingCols) {
    col.specs = mapping[[missingCol]]
    val = NA
    for (dataset in possibleDatasets)
      if (whichDataset == dataset) {
        valFld = paste('defaultValues', dataset, sep='.')
        print(valFld)  # TODO
        if (valFld %in% names(col.specs))
          val = col.specs[[valFld]]
      }
    if (is.na(val))
      val = col.specs[['defaultValues.common']]
    df[[missingCol]] = val
  }
  
  return(df)  # data.frame
}


TableauPrep.cols.standardize.orderColumns <- function(
  df,  # data.frame
  mapping  # list[Any]
) {
  output.colnames = c()
  for (possibleCol in unique(unlist(mapping))) {
    if (possibleCol %in% names(df)) {
      output.colnames = c(output.colnames, possibleCol) }}
  df2 = setcolorder(
    df, 
    output.colnames)
  return(df2)
}


TableauPrep.cols.standardize <- function(
  df,  # data.frame
  whichDataset,  # char[]
  inputOutputMapping  # list[str: str]
) {  # Returns --> data.frame
  df2 = TableauPrep.cols.standardize.renameCols(
    df, 
    inputOutputMapping)
  df3 = TableauPrep.cols.standardize.newCols.add(
    df2, 
    whichDataset,
    inputOutputMapping)
  df4 = TableauPrep.cols.standardize.orderColumns(
    df3,
    inputOutputMapping)
  return(df4)
}


TableauPrep.joinTables.all <- function(
  df,  # data.frame
  fields.df_and_keys,  # list[list[str, df]]
  naTokenList  # list
) {  # --> data.frame
  
  # Join all tables that haven't been joined
  toJoin = c()
  for (field in names(fields.df_and_keys))
    if (fields.df_and_keys[[field]][['join']] == TRUE)
      toJoin = c(toJoin, field)
  
  for (field in toJoin) {
    pk = fields.df_and_keys[[field]][['primaryKey']]
    fk = fields.df_and_keys[[field]][['foreignKey']]
    table = fields.df_and_keys[[field]][['df']]
    keyMapping = c(pk)
    names(keyMapping) = fk
    
    table.fields.nonPK = c()
    for (col in names(table))
      if (!(col == pk))
        table.fields.nonPK = c(table.fields.nonPK, col)
    
    alreadyJoined.vec = sapply(table.fields.nonPK, function (col) {
      if (col %in% names(df)) {
        vals = unique(df[[col]])
        return(length(vals) > 1)
      }
      return(FALSE)
    })
    alreadyJoined = any(alreadyJoined.vec)
    
    # Join if not already joined
    if (alreadyJoined == F) {
      # Remove temporary blank cols if they exist
      toRemove = c()
      for (col in names(table))
        if (col != fields.df_and_keys[[field]][['primaryKey']])
          toRemove = c(toRemove, col)
      for (col in toRemove)
        df[[col]] = NULL
        
      # Join
      df = left_join(
        x=df,
        y=table,
        by=keyMapping)         
    }
  }
  
  # Remove redundant key fields
  df2 = df
  primaryKeys = sapply(
    names(fields.df_and_keys), 
    function(table) {
      table.specs = fields.df_and_keys[[table]]
      table.specs[['primaryKey']] })
  for (pk in primaryKeys)
    df2[[pk]] = NULL
  
  # Re-order columns
  cols.order = c()
  cols.namespaces.metaGroupings.names.order = c('pos', 'label', 'value')
  for (grouping in cols.namespaces.metaGroupings.names.order)
    for (col in names(df2))
      if (startsWith(col, paste(grouping, '.', sep='')))
        cols.order = c(cols.order, col)
  df3 = data.table::setcolorder(df2, cols.order)
  return(df3)
}


TableauPrep.tables.cols.standardize <- function(
  df,  # data.frame
  cols.prefixes.namespace,  # char[label | pos | value]
  cols.affixes.table.name,  # char[]
  remappings  # list
) {
  colnames.2.formatted = sapply(names(df), function(col) {
    col2 = gsub('_', '.', col)
    for (key in names(remappings)) {
      if(col2 == key)
        col2 = remappings[[key]]
      col3 = gsub(
        paste('.', key, sep=''), 
        paste('.', remappings[[key]], sep=''), 
        col2)
    }
    return(col3)
  })
  
  # to-do: Var naming; Actually both of above are namespaces. But they differ
  # both positionally and semantically.
  colnames.3.standardized = sapply(colnames.2.formatted, function(col) {
    if (startsWith(col, paste(
      cols.prefixes.namespace, cols.affixes.table.name, sep='.')))
      return(col)
    if (startsWith(col,cols.affixes.table.name))
      return(paste(cols.prefixes.namespace, col, sep='.'))
    return(paste(
      cols.prefixes.namespace, cols.affixes.table.name, col, sep='.'))
  })
  names(df) = colnames.3.standardized
  return(df)
}