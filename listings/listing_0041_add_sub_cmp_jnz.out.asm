bits 16

add bx, [bx + si]
add bx, [bp]
add si, 2
add bp, 2
add cx, 8
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
add ax, [bp]
add al, [bx + si]
add ax, bx
add al, ah
add ax, 1000
add al, 226
add al, 9
sub bx, [bx + si]
sub bx, [bp]
sub si, 2
sub bp, 2
sub cx, 8
sub bx, [bp]
sub cx, [bx + 2]
sub bh, [bp + si + 4]
sub di, [bp + di + 6]
sub [bx + si], bx
sub [bp], bx
sub [bp], bx
sub [bx + 2], cx
sub [bp + si + 4], bh
sub [bp + di + 6], di
sub [bx], byte 34
sub [bx + di], word 29
sub ax, [bp]
sub al, [bx + si]
sub ax, bx
sub al, ah
sub ax, 1000
sub al, 226
sub al, 9
cmp bx, [bx + si]
cmp bx, [bp]
cmp si, 2
cmp bp, 2
cmp cx, 8
cmp bx, [bp]
cmp cx, [bx + 2]
cmp bh, [bp + si + 4]
cmp di, [bp + di + 6]
cmp [bx + si], bx
cmp [bp], bx
cmp [bp], bx
cmp [bx + 2], cx
cmp [bp + si + 4], bh
cmp [bp + di + 6], di
cmp [bx], byte 34
cmp [4834], word 29
cmp ax, [bp]
cmp al, [bx + si]
cmp ax, bx
cmp al, ah
cmp ax, 1000
cmp al, 226
cmp al, 9
jne $+2+2
jne $+2-4
jne $+2-6
jne $+2-4
je $+2-2
jl $+2-4
jle $+2-6
jb $+2-8
jbe $+2-10
jp $+2-12
jo $+2-14
js $+2-16
jne $+2-18
jnl $+2-20
jnle $+2-22
jnb $+2-24
jnbe $+2-26
jnp $+2-28
jno $+2-30
jns $+2-32
loop $+2-34
loopz $+2-36
loopnz $+2-38
jcxz $+2-40
