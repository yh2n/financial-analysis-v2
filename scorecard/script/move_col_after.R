move_col_after <- function(df, to_move, move_after) {
  #' Reorder df by moving column(s) to after targer column
  #' 
  #' @param df The data.frame to manipulate.
  #' @param to_move Columns(s) to move.
  #' @param move_after The target column that columns-to-move will follow.
  #' @return data.frame with new order
  temp_cols <- names(df)[!names(df) %in% to_move]
  move_after_idx <- which(temp_cols == move_after)
  if(move_after_idx == length(temp_cols)) {
    new_cols <- c(temp_cols[1:move_after_idx], to_move)
  } else {
    new_cols <- c(temp_cols[1:move_after_idx], to_move,
                  temp_cols[(move_after_idx+1):length(temp_cols)])
  }
  df <- df[, new_cols]
}