.486
IDEAL

macro PRINT_CHAR   CHAR
	push ax
	push dx
	mov dl,CHAR
	mov ah,2
	int 21h
	mov dl,' '
	int 21h
	pop dx
	pop ax
endm

MODEL small
STACK 100h
DATASEG
; --------------------------
	;	procs data
	bpspot 		dw	?
	bpspot2		dw	?
	
	;	graphics
	ScrLine 	db 200 dup (0)  ; One Color line read buffer
	Intro_File_Name 	db "intro1.bmp", 0
	FileHandle	dw ?
	Header 	    db 54 dup(0)
	Palette 	db 400h dup (0)
	ScoreTXT 	db "Score: " ,'$'
	
; Diagonals 

	TempW dw ?
    pointX dw ? 
	pointY dw ?
    point1X dw ? 
	point1Y dw ?
    point2X dw ? 
	point2Y dw ?
	Color db 0
	
	
	Board_File 	db "board1.bmp", 0
	
	BmpFileErrorMsg    	db 'Error At Opening Bmp File ',"board.bmp", 0dh, 0ah,'$'
	ErrorTxt	db "sorry, you can't position your battleship here." , '$'
	ErrorFile           db 0
	
	HandleCoordinatesREAD dw 0
	HandleCoordinatesWRITE dw 0
	Coordinates_File1 db	"places1.txt", 0
	Coordinates_File2 db 	"places2.txt", 0 
	Coordinates_Reading	db 20 dup(?)
	Player2End db 0
	YposBS	db 0
	XposBS	db 0
	newline	db 0Dh, 0Ah
	SquarePos	db "  "
	EndSign	db 	"*"
	
	; other players pixels X axis
	XposPixelsOP dw 146, 158, 170, 182, 194, 206, 218, 230, 242, 254, 266
	XposBSOP	dw ?
	YposBSOP	dw ?
	
	; this player's pixels
	XposSquarePixels db 11, 23, 36, 48, 60, 72, 84, 96, 108, 120, 132
	YposSquarePixels db 69, 81, 93, 105, 117, 129, 141, 153, 165, 177, 188
	YposNums 	db '1', '2', '3', '4', '5', '6', '7', '8', '9', ':'
	XposLetters db 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
	finalPoses dw 10 dup(?)
	promoter 	db 0
	
	matrix 	dw 	?
	XforRect	db ?
	YforRect	db ?
	SquareColor	db ?
				
	GameStage 	db 0 ; מונה שבודק לי את מצב המשחק, אם מעל 10 המשחק נמצא בשלב השני
	GameOn 		db 0 
	Turns 	dw 0
	Score	dw 0
	CorrectSpot	db 0
	BmpLeft dw 0
	BmpTop dw 0
	BmpColSize dw 320		
	BmpRowSize dw 200
	EraseSign	db 0
; --------------------------

CODESEG
start:
	mov ax, @data
	mov ds, ax
; --------------------------

	call 	SetGraphic
	push 	offset Intro_File_Name
	call 	PrintGraphics
	mov 	al, 0
	mov 	ah, 1
	int 	21h
	
	mov 	cx, 0
	mov 	dx, 0
	mov 	si, 320
	mov 	di, 200
	mov 	al, 0
	call 	rect

	mov 	ax, 0 ; resetting the mouse
	int 	33h   
	
	call 	SetAsyncMouse
	
	push 	offset Board_File
	call 	PrintGraphics

;	while
CheckingOtherPlayer:
	cmp 	[GameStage], 10
	jb 		CheckingOtherPlayer
	call 	CoordinatesReading

	cmp 	[Player2End], "*"
	je		@@Game
	jmp 	CheckingOtherPlayer
	
@@Game:
	mov 	[GameOn], 1
@@MainLoop:
	cmp 	[GameOn], 0
	je 		Bye
	mov 	dl, 100
	mov 	dh, 1
	call 	SetCursorPosition
	call 	PrintScore
	mov 	ax, [Score]	
	call 	ShowAxDecimal
	jmp 	@@MainLoop

Bye:
	mov 	ah, 1
	int 	21h
	Call 	SetTextMode
; --------------------------

exit:
	mov ax, 4c00h
	int 21h
	
;; procedures

; --------------- Graphics

proc PrintScore
	mov		dx, offset ScoreTXT
	mov 	ah, 9
	int 	21h
ret
endp PrintScore

proc GetmyXY
	push 	bx
	
	mov 	bh, 0
	mov 	bl, [SquarePos]
	sub		bl, '0'
	dec		bl
	
	mov 	dl, [YposSquarePixels+bx]
	mov 	dh, 0
	
	mov 	bl, [SquarePos+1]
	sub 	bl, 'A'
	mov 	ch, 0
	mov 	cl, [XposSquarePixels+bx]
	
	pop		bx

ret
endp GetmyXY

proc GetXY ; for other player
	
	mov 	bl, [SquarePos]
	sub 	bl, '0'
	dec 	bl
	mov 	bh, 0
	
	mov 	dl, [YposSquarePixels+bx]
	mov 	dh, 0
	
	mov 	bl, [SquarePos+1]
	sub 	bl, 'A'
	shl 	bl, 1
	
	mov 	cx, [XposPixelsOP+bx]
	
ret
endp GetXY

proc PrintGraphics
	pop 	[bpspot]
	mov 	[BmpLeft],0
	mov 	[BmpTop],0
	mov 	[BmpColSize], 320
	mov 	[BmpRowSize] , 200
	
	pop dx
	call OpenShowBmp
	cmp [ErrorFile],1
	jne cont 
	jmp exitError
	
cont:
    jmp @@exit
	
exitError:
	mov ax,2
	int 10h
	
    mov dx, offset BmpFileErrorMsg
	mov ah,9
	int 21h
	
@@exit:
	push 	[bpspot]
	
	call 	ShowMouse
	
	; mov ah,7
	; int 21h
	
;	mov ax,2
;	int 10h
	

	
	
	ret
endp PrintGraphics

; cx = col dx= row al = color si = height di = width 
proc Rect
	push cx
	push di
;	call 	HideMouse
NextVerticalLine:	
	
	cmp di,0
	jz @@EndRect
	
	cmp si,0
	jz @@EndRect
	call DrawVerticalLine
	inc cx
	dec di
	jmp NextVerticalLine
	
	
@@EndRect:
	pop di
	pop cx
	
;	call 	ShowMouse
	ret
endp Rect


proc DrawVerticalLine	near
	push si
	push dx
 
DrawVertical:
	cmp si,0
	jz @@ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	
	 
	
	inc dx
	dec si
	jmp DrawVertical
	
	
@@ExitDrawLine:
	pop dx
    pop si
	ret
endp DrawVerticalLine

proc  SetGraphic
	mov ax,13h   ; 320 X 200 
				 ;Mode 13h is an IBM VGA BIOS mode. It is the specific standard 256-color mode 
	int 10h
	ret
endp 	SetGraphic

proc OpenShowBmp near

	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call 	ReadBmpHeader	
	call 	ReadBmpPalette	
	call 	CopyBmpPalette	
	call  	ShowBmp	
	Call	CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp


; input dx filename to open
proc OpenBmpFile	near						 
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile


proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile


; Read 54 bytes the Header
proc ReadBmpHeader	near					
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader

proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)			
	push cx
	push dx
	
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette


; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest
proc CopyBmpPalette		near					
										
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 						
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al 							
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al 							
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx
	
	ret
endp CopyBmpPalette

proc SetTextMode
	mov 	ax, 10h
	int 	10h
	ret
endp SetTextMode

 
proc ShowBMP 
; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; displaying the lines from bottom to top.
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
	mov cx,[BmpRowSize]
	
 
	mov ax,[BmpColSize] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	xor dx,dx
	mov si,4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok
	mov bp,4
	sub bp,dx

@@row_ok:	
	mov dx,[BmpLeft]
	
@@NextLine:
	push cx
	push dx
	
	mov di,cx  ; Current Row at the small bmp (each time -1)
	add di,[BmpTop] ; add the Y on entire screen
	
 
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	mov cx,di
	shl cx,6
	shl di,8
	add di,cx
	add di,dx
	 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpColSize]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	
	pop dx
	pop cx
	 
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP 

proc 	SetCursorPosition ; moving by val, via dx
	push 	ax
	push 	bx
	
	mov 	al, 0
	mov 	ah, 2
	mov 	bh, 0
	mov 	bl, 0
	int 	10h
	
	pop 	bx
	pop 	ax
ret
endp 	SetCursorPosition

;; --------------------- File Related
proc 	PutEndSign
	push 	ax
	push 	bx
	push 	cx
	push 	dx
	
	mov 	ah, 40h
	mov 	bx, [HandleCoordinatesWRITE]
	mov 	cx, 1
	mov 	dx, offset EndSign
	int 	21h
	
	push 	offset HandleCoordinatesWRITE
	call 	CloseFile
	
	pop 	dx
	pop 	cx
	pop 	bx
	pop		ax
ret
endp 	PutEndSign


;; writes the coordinates into a file
proc CoordinatesWriting

	cmp		[HandleCoordinatesWRITE], 0 
	ja 		@@writing
@@openingFile:
	;; opening file
	mov 	ah, 3dh
	mov 	dx, offset Coordinates_File1
	mov 	al, 1
	int 	21h
	mov 	[HandleCoordinatesWRITE], ax
	jc 		@@error
	
@@writing:
	mov 	bx, [HandleCoordinatesWRITE]
	mov 	ah, 40h
	mov 	cx, 20
	mov 	dx, offset finalPoses
	int 	21h
	
	jmp 	@@ending
	
@@error:
	mov 	dl, 'E'
	mov 	ah, 2
	int 	21h
	
@@ending:
	ret
endp CoordinatesWriting

proc CoordinatesReading
	push	ax
	push 	cx
	push 	dx
	
	;; opening file
	mov 	ah, 3dh
	mov 	dx, offset Coordinates_File2
	mov 	al, 0
	int 	21h
	mov 	[HandleCoordinatesREAD], ax ; saving handle for later
;	jc 		@@error

@@reading:	
	; reading into array
	mov 	ah,	3Fh
	mov 	bx, [HandleCoordinatesREAD]
	mov 	cx, 21
	mov 	dx, offset Coordinates_Reading
	int 	21h
	
	push 	offset HandleCoordinatesREAD
	call 	CloseFile
	
	pop 	dx
	pop		cx
	pop 	ax
	
	
ret
endp CoordinatesReading

proc checkPlaces
	
	call 	ThisPlayerBoard
	cmp 	bx, 1
	je		@@ending
	call 	OtherPlayerBoard ; if bx's val is 2 it's the other player's board
@@ending:
	ret 
	; returns in bx 1 if the mouse's position is in his board
	; returns in bx 2 if the mouse's position is in the other player's board
	; returns in bx 0 if the mouse's position is in neither boards
endp checkPlaces

proc 	OtherPlayerBoard
	mov 	bx, 0
	cmp 	cx, 145
	jb 		@@ending
	cmp 	cx, 268
	ja 		@@ending
	cmp 	dx, 67
	jb 		@@ending	; top side of the board
	cmp 	dx, 189
	ja 		@@ending	; bottom side of the board
	mov 	bx, 2
@@ending:
	ret 
endp 	OtherPlayerBoard

proc 	ThisPlayerBoard
	mov 	bx, 0
	cmp 	cx, 11
	jb 		@@ending ; left side of the board
	cmp		cx, 133
	ja		@@ending; right side of the board
	cmp 	dx, 67
	jb 		@@ending	; top side of the board
	cmp 	dx, 189
	ja 		@@ending	; bottom side of the board
	mov 	bx, 1
@@ending:
	ret
endp 	ThisPlayerBoard

proc SquarePosition
	pop 	[bpspot]
	mov 	[XposBS], cl
	mov 	[YposBS], dl
	mov 	cx, 9
	mov 	di, 1
	mov 	ah, 'A'
	mov 	si, 9
	
XposCheck:
	mov 	si, cx
	mov 	cl, [XposBS]
	cmp 	cl, [XposSquarePixels+di]

	jbe		PreYposCheck
	inc 	ah
	inc 	di
	mov 	cx, si
	loop 	XposCheck
PreYposCheck:
	mov 	cx, 9
	mov 	[SquarePos+1], ah
	mov 	di, 1
	mov 	ah, '1'
YposCheck:
	cmp 	dl, [YposSquarePixels+di]
 	jbe		@@ending
	inc 	ah
	inc 	di
	loop 	YposCheck
	
@@ending:
	mov 	[SquarePos], ah
	mov 	al, [SquarePos+1]
	mov 	di, [word ptr promoter]
	cmp 	[EraseSign], 1
	je 		return
	mov 	al, [SquarePos]
	mov 	ah, [SquarePos+1]
	mov 	[finalPoses+di], ax
	add 	[promoter],2
	
	jmp 	return
return:
	push 	[bpspot]
	ret
endp SquarePosition


proc 	Check_ifBS_Correct
	pop 	[bpspot2]
	push 	bx
	push 	cx
	push 	dx
	push 	di
	push 	si
	

	call	 GetSquarePosOPB
	mov 	ah, [SquarePos]
	mov 	al, [SquarePos+1]
	mov 	cx, 10
	mov 	di, 0
@@checking:
	cmp 	[CorrectSpot], 1
	je 		@@return
	call 	cmprPlacing
	add 	di, 2
	loop 	@@checking
	mov 	ax, 0
	jmp 	@@cont
@@return:
	mov 	ax, 1
@@cont:
	pop 	si
	pop 	di
	pop		dx
	pop		cx
	pop		bx
	push 	[bpspot2]
	ret
endp 	Check_ifBS_Correct

proc cmprPlacing ;livdok
@@checking:

	cmp 	ah, [Coordinates_Reading+di]
	jne 	@@return
	cmp 	al, [Coordinates_Reading+di+1]
	jne 	@@return
@@ending:
	mov 	[CorrectSpot], 1
@@return:
	ret
endp 	cmprPlacing


proc GetSquarePosOPB
	pop 	[bpspot]
	
	mov 	[XposBSOP], cx
	mov 	[YposBSOP], dx
	mov 	cx, 9
	mov 	di, 2
	mov 	ah, 'A'
	mov 	si, 9
	
@@XposCheck:
	mov 	si, cx
	mov 	cx, [XposBSOP]
	cmp 	cx, [XposPixelsOP+di]

	jbe		PreYposCheck
	inc 	ah
	add 	di, 2
	mov 	cx, si
	loop 	@@XposCheck
@@PreYposCheck:
	mov 	cx, 9
	mov 	[SquarePos+1], ah
	mov 	di, 1
	mov 	ah, '1'
@@YposCheck:
	cmp 	dl, [YposSquarePixels+di]
 	jbe		@@postYcheck
	inc 	ah
	inc 	di
	loop 	@@YposCheck
@@postYcheck:
	mov 	[SquarePos], ah
@@ending:	
	push 	[bpspot]
@@return:	
	ret
endp	GetSquarePosOPB


proc CloseFile
	pop 	[bpspot]
	pop 	bx ; passing handle by ref
	
	mov 	ax, [bx]
	mov 	bx, ax
	mov 	ah, 3Eh
	int 	21h
	
	push	[bpspot]
ret
endp CloseFile


proc ShowAxDecimal
       push ax
	   push bx
	   push cx
	   push dx
	   
	   ; check if negative
	 ;  test ax,08000h
	  ; jz PositiveAx
			
	   ;  put '-' on the screen
	   ; push ax
	   ; mov dl,'-'
	   ; mov ah,2
	   ; int 21h
	   ; pop ax

	   ;neg ax ; make it positive
PositiveAx:
       mov cx,0   ; will count how many time we did push 
       mov bx,10  ; the divider
   
put_mode_to_stack:
       xor dx,dx
       div bx
       add dl,30h
	   ; dl is the current LSB digit 
	   ; we cant push only dl so we push all dx
       push dx    
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_mode_to_stack

	   cmp ax,0
	   jz pop_next  ; jump if ax was totally 0
       add al,30h  
	   mov dl, al    
  	   mov ah, 2h
	   int 21h        ; show first digit MSB
	       
pop_next: 
       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   mov dl, al
       mov ah, 2h
	   int 21h        ; show all rest digits
       loop pop_next
   
	   pop dx
	   pop cx
	   pop bx
	   pop ax
	   
	   ret
endp ShowAxDecimal


proc 	ShowPalette
	mov 	cx, 400h
	mov 	di, 0

innerLoop:
	call 	printcolour
	inc 	di
	loop 	innerLoop
ret
endp ShowPalette

proc printcolour
	push 	cx
	mov cx, 10
loopy:
	mov 	bx, 0h
	mov 	al, [palette+di]
	mov 	ah, 0Ch
	int 	10h 
	loop 	loopy
	pop 	cx
ret
endp 	printcolour

proc mouse far
	push 	ax
	push 	bx
	push 	cx
	push 	dx
	
	shr 	cx, 1
	cmp 	ax, 2
	je 		@@checkPlaces
@@checkRightClick:
	cmp 	ax,8
	jne 	@@ending
	call 	checkPlaces
	cmp 	bx, 1
	jne 	@@ending
	call 	EraseBS
	jmp 	@@ending

@@checkPlaces:
	call 	checkPlaces
	cmp 	bx, 1
	je 		@@writing
	cmp 	bx, 2
	je 		@@reading
	jmp 	@@ending
@@writing:
	cmp 	[GameStage], 10
	ja 		@@ending
	
	call 	SquarePosition
	
	call	GetmyXY
	inc 	cx
	inc 	dx
	call 	HideMouse
	mov 	si, 11
	mov 	di, 11
	mov 	al, 4
	call 	Rect
	call 	ShowMouse
	
	inc 	[GameStage]
	cmp 	[GameStage], 10
	jne 	@@ending
	
	call 	CoordinatesWriting
	call 	PutEndSign
	inc 	[GameStage]
	jmp 	@@ending
@@reading:
	call 	CoordinatesReading
	cmp 	[GameOn], 1
	jne 	@@ending
	cmp 	[GameStage], 10
	jb 		@@ending
	cmp 	[Turns], 10
	ja		@@stopGame
	inc 	[Turns]
	call 	Check_ifBS_Correct
;	PRINT_CHAR 'A'
	cmp 	ax, 1
	jne 	@@printBlack
	call 	printGreenRect
	add		[Score], 100
	jmp 	@@next
@@printBlack:
	call 	printBlackRect

@@next:
	mov 	[CorrectSpot], 0
	jmp 	@@ending
@@stopGame:
	mov 	[GameOn], 0
@@ending:
	
	pop		dx
	pop		cx
	pop		bx
	pop		ax
retf
endp 	mouse

proc 	printGreenRect
	call 	GetSquarePosOPB
	call 	GetXY
	inc 	cx
	inc 	dx
	mov 	si, 11
	mov 	di, 11
	call 	HideMouse
	mov 	al, 36h
	call 	rect
	call 	ShowMouse
ret
endp 	printGreenRect

proc 	printBlackRect
	call 	GetSquarePosOPB
	call 	GetXY
	inc 	cx
	inc 	dx
	mov 	si, 11
	mov 	di, 11
	call 	HideMouse
	mov 	al, 0
	call 	rect
	call 	ShowMouse
ret
endp 	printBlackRect

proc 	ShowMouse

	push 	ax

	mov 	ax, 1
	int 	33h

	pop 	ax
	
	ret
endp 	ShowMouse

proc 	HideMouse
	push	ax
	
	mov 	ax, 2
	int 	33h
	
	pop		ax
	ret
endp 	HideMouse

proc EraseBS	
	push 	ax
	push 	cx
	cmp 	[GameStage], 0
	je 		@@ending
	cmp 	[GameOn], 0
	jne 	@@ending
	
	mov 	[EraseSign], 1
	call 	SquarePosition
	call 	EraseFromFile
	call	GetmyXY
	inc 	cx
	inc 	dx
	call 	HideMouse
	mov 	si, 11
	mov 	di, 11
	mov 	al, 255
	call 	Rect
	call 	ShowMouse
	dec 	[GameStage]
@@ending:
	mov 	[EraseSign], 0
	pop 	cx
	pop 	ax
ret
endp EraseBS

proc EraseFromFile
	mov 	di, 0
	mov 	al, [SquarePos+1]
	mov 	ah, [SquarePos]
	mov 	cx, 10
@@cmpr:
	cmp 	ax, [finalPoses+di]
	je 		Erase
	add		di, 2
	loop 	@@cmpr
	jmp 	@@return
Erase:
	cmp 	di, 18
	je 		@@return
	mov 	ax, [finalPoses+di+2]
	mov 	[finalPoses+di], ax
	add 	di, 2
	jmp 	Erase
@@return:
	sub 	[promoter], 2
	ret
endp EraseFromFile

proc 	SetAsyncMouse
	push ds
	pop  es	 
	mov ax, seg mouse
	mov es, ax
	mov dx, offset mouse   ; ES:DX ->Far routine
    mov ax,0Ch             ; interrupt number
    mov cx,0Ah              ; 2   Left Down
    int 33h   	
ret
endp 	SetAsyncMouse

END start	




