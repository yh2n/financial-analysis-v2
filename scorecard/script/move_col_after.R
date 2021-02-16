move_col_after <- function(df, to_move, move_after) {
  temp_cols <- names(df)[names(df)!=to_move]
  move_after_idx <- which(temp_cols==move_after)
  if(move_after_idx == length(temp_cols)) {
    new_cols <- c(temp_cols[1:move_after_idx], to_move)
  } else {
    new_cols <- c(temp_cols[1:move_after_idx], to_move,
                  temp_cols[(move_after_idx+1):length(temp_cols)])
  }
  df <- df[, new_cols]
}