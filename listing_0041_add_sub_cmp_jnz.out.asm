bits 16

add bx, [bx + si]
add bx, [bp]
add si, word 2
add bp, word 2
add cx, word 8
add bx, [bp]
add cx, [bx + 2]
add bh, [bp + si + 4]
add di, [bp + di + 6]
add [bx + si], bx
add [bp], bx
add [bp], bx
add [bx + 2], cx
add [bp + si + 4], bh
add [bp + di + 6], di
add [bx], byte 34
add [bp + si + 1000], word 29
