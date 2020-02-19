.486
    .model flat, stdcall
    option casemap :none   ; case sensitive
    include include.inc

    LookChar Proto character:Byte, AddrUString:Dword
    LookChar2 Proto character:Byte, AddrUString:Dword
    externdef LastSuccessfulTokenString:Dword
   
.data
StreamCursor dd 0
StreamEnd dd 0
StreamStart dd 0 ;address of program text on memory
CurrentLine dd 0
CurrentCol dd 0
SysLine dd 0
SysCol dd 0

CommentLevel dd 0

;DFA TOKENIZER
CurrentDFAState dd 0

;LALR PARSER:
CurrentLALRState dd 0 ; is a LALR-State

LastSuccessfulTokenString dd 0 ; to show better error info.

MiniBuf2 dw 0
Align 4
MiniBuf1 db 0


.code

;*************************************************************** 
RetrieveToken Proc uses esi edi
Local Length_:Dword
Local AcceptLength:Dword
Local AcceptState:Dword ;Symbol
Local ReadChar:Byte

Mov AcceptState, 0

;if Source has no more characters
;Create a new Token.
;Set the Type property of Token = End-Of-File.
;**Set the parentsymbol property of token to the EOF symbol, which has the table index of 0.

mov ecx, StreamCursor
.if ecx>=StreamEnd
    invoke GetSymbolFromIndex, 0 
    push eax
    invoke CreateToken, FORINPUT
    pop edx
    mov (Token Ptr [eax]).ParentSymbol, edx
    ret
.else

    invoke GetDFAFromIndex, InitDFA
    mov CurrentDFAState, eax
    mov Length_, 0


				    ;*************
				    GoForTheToken:
				    ;*************

	    ;Part 1:
	    ;If the current state accepts a terminal, keep track of the cursor position and the state index. 
	    ;This will be used later.

	    ;if the State accepts a termimal
	    ;Set Accept-State = State.
	    ;Set Accept-Length = Length.

	    mov eax, CurrentDFAState
	    mov dl, (DFAState Ptr [eax]).AcceptState

	    .IF dl==1
		mov ax, (DFAState Ptr [eax]).AcceptIndex
		invoke GetSymbolFromIndex, ax
		mov AcceptState, eax
		mov ecx, Length_
		mov AcceptLength, ecx
	    .ENDIF		  

	    ;Part 2:
	    ;Look through the edges of the current state and advance if an edge is found that 
	    ;contains the character at Lookahead(Position). 
	    ;When no edge is found, a token is created and the loop is exited.

	    ;If no Accept State was set then an error token is created. 
	    ;A single character is read from the Source. 
	    ;This allows error recovery by discarding the character that caused the error.

	    ;If an Edge in State exists that contains the character Lookahead(Length) in Source
	    mov eax, CurrentDFAState
	    mov edi, (DFAState Ptr [eax]).Edges
	    mov ecx, (DFAState Ptr [eax]).EdgeCount

	    xor esi, esi
	    .WHILE esi<ecx  ;Look in all the edges.
		    push ecx ;save it

		    mov ax, (Edge Ptr [edi]).CharSetIndex ;;because edi is an edge.

		    invoke GetCharSetFromIndex, ax
		    mov eax, (CharSet Ptr [eax]).UString
		    mov edx, StreamCursor
		    mov ecx, Length_

		    mov dl, [edx+ecx]			 
		    mov ReadChar, dl

		    invoke LookChar2, dl, eax ; It may be nicer to see it as a macro, one day.

		    .IF eax!=0
			  mov ax, (Edge Ptr [edi]).TargetIndex
			  invoke GetDFAFromIndex, ax
			  mov CurrentDFAState, eax
			  add Length_, 1  
			  pop ecx ; just balance stack. 
			  jmp GoForTheToken  
		    .ELSE
			  add edi, EDGESIZE
			  pop ecx
			  add esi, 1
		    .ENDIF
	    .ENDW

	    ;if the Accept-State was set
	    ;Create a new Token.
	    ;Set the Parent-Symbol property of Token = symbol in Accept-State.
	    ;Set the Data property of Token = read **AcceptLength** characters from Source.
	    
	    mov eax, AcceptState
	    .IF eax!=0
		    mov ecx, AcceptLength
		    add ecx, 1
		    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, ecx
		    mov LastSuccessfulTokenString, eax
		    push eax
		    invoke CreateToken, FORINPUT
		    pop esi

		    push eax ;; save it for return value.
		    mov (Token Ptr [eax]).TokenData, esi
		    mov ecx, AcceptState
		    mov (Token Ptr [eax]).ParentSymbol, ecx
		    mov eax, StreamCursor
		    mov ecx, AcceptLength
		    add StreamCursor, ecx
		    invoke MemCopy, eax, esi, ecx

		    ;implement line/col counters.
		    mov eax, SysLine
		    mov ecx, SysCol
		    mov CurrentLine, eax
		    mov CurrentCol, ecx
		    LCCounter:
			mov al, [esi]
			.IF al==13
			    add SysLine, 1
			    mov SysCol, 0
			    add esi, 1 ; for 10 as well
			.ELSEIF al==0
			    jmp ExitDFA
			.ELSE
			    add SysCol, 1 ; unfortunately, it always runs until the next
			    ;token which results in wrong number. Maybe one day... :)
			.ENDIF
		     add esi, 1   
		     jmp LCCounter					     
		    
	    .ELSE
			;note: we don't use that information. it's for future use.
		    ;Create a new Token.
		    ;Set the Data property of Token = read one character from Source.
		    ;**Set the Parent Symbol property of token to the Error symbol which has index=1.
		    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, 2
		    mov esi, eax ; esi has text

		    invoke GetSymbolFromIndex, 1
		    mov edi, eax ; edi has errror symbol
		    
		    invoke CreateToken, FORINPUT
		    mov (Token Ptr [eax]).ParentSymbol, edi
		    
		    mov ecx, StreamCursor
		    add ecx, 1
		    mov cl, [ecx]
		    mov [esi+0], cl		       
		    ret
	    .ENDIF
.ENDIF

ExitDFA:
pop eax ; token!

ret
RetrieveToken endP
;*************************************************************** 


;*************************************************************** 
Initialize Proc
;Set the **State property of Start to the Initial-LALR-State.
;Set the Parent-Symbol property of Start to the Start-Symbol.
;Push Start onto the Token-Stack.

mov eax, StreamStart
mov StreamCursor, eax

mov CurrentLine, 0
mov CurrentCol, 0
mov SysLine, 0
mov SysCol, 0

invoke CreateToken, FORSTACK
push eax
invoke GetSymbolFromIndex, pStartSymbol
pop edx ;token
mov (Token PTR [edx]).ParentSymbol, eax
push edx

invoke GetLALRFromIndex, InitLALR
mov CurrentLALRState, eax

pop edx
mov (Token PTR [edx]).State, eax

invoke PushToken, FORSTACK, edx

;Part 2: Set initial values.
;Other code related to setting line counters, etc... can also be added to this procedure.
;Set the Comment-Level to 0.

mov CommentLevel, 0
ret
Initialize endP
;*************************************************************** 

;;following functions may as well use shl for further optimization.
;*************************************************************** 
GetCharSetFromIndex Proc index:Word
movzx ecx, index
mov eax, CHARSETSIZE
mul ecx
add eax, CharSets
ret
GetCharSetFromIndex endP
;*************************************************************** 

;*************************************************************** 
GetLALRFromIndex Proc index:Word
movzx ecx, index
mov eax, LALRSIZE
mul ecx
add eax, LALRs
ret
GetLALRFromIndex endP
;*************************************************************** 

;*************************************************************** 
GetDFAFromIndex Proc index:Word
movzx ecx, index
mov eax, DFASIZE
mul ecx
add eax, DFAStates
ret
GetDFAFromIndex endP
;*************************************************************** 

;*************************************************************** 
GetSymbolFromIndex Proc index:Word
movzx ecx, index
mov eax, SYMBOLSIZE
mul ecx
add eax, Symbols
ret
GetSymbolFromIndex endP
;*************************************************************** 

;***************************************************************
GetRuleFromIndex Proc index:Word
movzx ecx, index
mov eax, RULESIZE
mul ecx
add eax, Rules
ret
GetRuleFromIndex endP
;***************************************************************

;*************************************************************** 
LookChar2 Proc Character:Byte, AddrUStr:Dword ;;if found return value is non zero, else zero.
xor eax, eax
mov al, Character
mov edx, AddrUStr

@@:
    mov cx, [edx]
    add edx, 2

    cmp ax, cx
    je EndWithSuccess

    or cx, cx ; is zero?
    jnz @B
    xor eax, eax ; indicate failure.

EndWithSuccess:
ret
LookChar2 endP
;*************************************************************** 

			COMMENT 7		       
;***************************************************************
LookChar Proc Character:Byte, AddrUStr:Dword ;;if found return value is non zero, else zero.
mov dl, Character
mov MiniBuf1, dl

invoke MultiByteToWideChar, CP_ACP, MB_PRECOMPOSED, offset(MiniBuf1) , 1, offset(MiniBuf2), 1
mov ax, MiniBuf2

mov edx, AddrUStr

Baslar2:
    mov cx, [edx]
    add edx, 2

    cmp ax, cx
    je EndWithSuccess2

    or cx, cx ; is zero?
    jnz Baslar2
    xor eax, eax

EndWithSuccess2:
ret
LookChar endP
;***************************************************************
			COMMENT 7
			
;***************************************************************
DiscardRestOfLine Proc

mov eax, StreamCursor
LookForCR:
    cmp eax, StreamEnd
    jge DoneLooking

    mov dl, [eax]
    add eax, 1	
    cmp dl, 13
    jne LookForCR

sub eax, 1 ;very important. We should leave the CR in the stream
;untouched so that it is treated as it is. 

DoneLooking:
    mov StreamCursor, eax
ret
DiscardRestOfLine endP
;***************************************************************


end