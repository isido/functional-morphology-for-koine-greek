process_comment [] = []
process_comment ('-':'-':_) = []
process_comment (x:xs)      = x:process_comment xs
