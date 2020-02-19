

;               GOLD Intel x86 Engine v1.2
;               A GOLD Parser Engine implementation
;               written by Ege Madra 
;               June 8th 2006.
;
;               egemadra@icqmail.com

 

.486
.model flat, stdcall
option casemap :none   ; case sensitive
include include.inc

ReadUString Proto addrStart:Dword

.data
stri db "                                                                       ",0

Align 4

ProcessHeap dd 0

TotalActions dd 0
TotalEdges dd 0
TotalRuleSymbolIndices dd 0

;;PARAMETERS SECTION:
pName dd 0
pVersion dd 0
pAuthor dd 0
pAbout dd 0
pCaseSensitive db 0
pStartSymbol dw 0

Align 4

;;TABLE SECTION:
SymbolTableCount dw 0 ; number of symbols
CharacterSetTableCount dw 0 ;The number of character sets used by the DFA state table. 
RuleTableCount dw 0; The number of rules in the language. 
DFATableCount dw 0; The number of Deterministic Finite Automata states. 
LALRTableCount dw 0; The number of LALR States. 

;;INITIAL SECTION:
InitDFA dw 0
InitLALR dw 0

StreamIsFile dd 0 ; required to free the memory or not.
TablesLoaded dd 0

;Add1CrLf dd 0 ; for line based grammars. if not 0, adds an additional 1310.


.data?
Align 4

CharSets dd ?
Symbols dd ?
Rules dd ?
RuleSymbolIndices dd ?
DFAStates dd ?
Edges dd ?
LALRs dd ?
Actions dd ?




.code
			comment =
			    Start:
			    call Main
			    invoke ExitProcess, 0
			comment  =



				COMMENT #
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
Main Proc
invoke GetProcessHeap
mov ProcessHeap, eax

push CTXT("Gramer1.cgt")
call LoadGrammarFile
.IF eax==0
    Showerror "Can't load grammar file."
    ret
.ENDIF

push CTXT("parsertest.mbas")
call LoadProgramFile	
.IF eax==0
    Showerror "Can't load program file."
    ret
.ENDIF

 ;          gpMsgTokenRead = 1
 ;          gpMsgReduction = 2
 ;          gpMsgAccept = 3
 ;          gpMsgNotLoadedError = 4
 ;          gpMsgLexicalError = 5
 ;          gpMsgSyntaxError = 6
 ;          gpMsgCommentError = 7
 ;          gpMsgInternalError 
Parsing:
call Parse
.if eax==6
    Showerror "Syntax Error."
    ret
.elseif eax==5
    Showerror "Lexical Error."
    ret
.elseif eax==8
    Showerror "Internal Error."
    ret
.elseif eax==3
    Showtext "Done."
    ret
.elseif eax==2
.endif
jmp Parsing
ret
Main endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
				;COMMENT #


;*************************** INTERFACE FUNCTIONS **************************

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
IsGrammarLoaded Proc
    mov eax, TablesLoaded
ret
IsGrammarLoaded endP	
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetErrExpectedCount Proc
    mov eax, ErrExpectedCount
ret
GetErrExpectedCount endP    
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetErrExpectedSize Proc index:Dword
    mov ecx, index
    mov eax, [ErrExpStrList+ecx*4]
    invoke SysStringLen, eax
ret
GetErrExpectedSize endP    
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetErrLastToken Proc addrStr:Dword
    call GetErrLastTokenSize
    add eax, 1
    push eax
    invoke MemCopy, LastSuccessfulTokenString, addrStr, eax
    pop eax
    mov byte ptr [addrStr+eax], 0
    ret
GetErrLastToken endp
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл



; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetErrLastTokenSize Proc
    invoke lstrlen, LastSuccessfulTokenString
    ;inc eax
ret
GetErrLastTokenSize endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetErrExpectedStr Proc index:Dword, addrStr:Dword
    invoke GetErrExpectedSize, index
    add eax, 1
    push eax
    mov ecx, index
    mov edx, [ErrExpStrList+ecx*4]
    invoke WideCharToMultiByte, CP_ACP, 0, edx, -1, addrStr, eax, 0, 0
    pop eax
    mov edx, addrStr
    mov [edx+eax], byte ptr 0
ret
GetErrExpectedStr endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
		    
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetCurrentLine Proc
    mov eax, CurrentLine
    inc eax
ret
GetCurrentLine endP    
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetCurrentColumn Proc
    mov eax, CurrentCol
    inc eax
ret
GetCurrentColumn endP	 
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetRTokenCount Proc
    mov eax, CurrentReduction.TokenCount
ret
GetRTokenCount endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetRTokenData Proc uses esi index:Dword, addrStr:Dword
    mov ecx, TOKENSIZE
    mov eax, index
    mul ecx
    mov edx, offset ReductionTokens
    add eax, edx
    mov esi, (Token Ptr [eax]).TokenData

    invoke szLen, esi
    add eax, 1
    invoke MemCopy, esi, addrStr, eax
ret
GetRTokenData endP    
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetRTokenDataSize Proc index:Dword
    mov ecx, TOKENSIZE
    mov eax, index
    mul ecx
    mov edx, offset ReductionTokens
    add eax, edx
    mov eax, (Token Ptr [eax]).TokenData

    or eax, eax
    jz ZeroLength_

    invoke szLen, eax
    ret
    
ZeroLength_:

ret    
GetRTokenDataSize endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
GetRSymbolIndex Proc
    mov eax, CurrentReduction.ParentRule
    mov ax, (Rule Ptr [eax]).index
    movzx eax, ax
ret
GetRSymbolIndex endP	
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
ResetParser Proc
mov CurrentReduction.ParentRule, 0
mov CurrentReduction.TokenCount, 0
;Other fields aren't used.

mov ErrExpectedCount, 0

invoke ResetStacks
invoke Initialize
.IF StreamIsFile==-1
    invoke GlobalFree, StreamStart
.ENDIF
ret
ResetParser endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
LoadProgramText Proc ProgramText:Dword
invoke ResetParser

mov eax, ProgramText

mov StreamStart, eax
mov StreamCursor, eax

invoke szLen, ProgramText
add eax, StreamStart

mov StreamEnd, eax
mov StreamIsFile, 0
mov eax, -1

ret
LoadProgramText endP 
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
LoadProgramFile proc ProgramFile:Dword 
invoke ResetParser

invoke read_disk_file, ProgramFile, addr StreamStart, addr StreamEnd
.IF eax==0
    ;Showerror "Failed to open program file."
    mov StreamStart, eax
    mov StreamEnd, eax
    ret
.ENDIF

mov eax, StreamStart
add StreamEnd, eax
mov StreamCursor, eax
mov StreamIsFile, -1
mov eax, -1

ret
LoadProgramFile endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
Parse Proc

.IF TablesLoaded==0
    mov eax, MessageNotLoaded
    ret
.ENDIF


ParseStarts:
    .IF TokenPtrQ==0
	invoke RetrieveToken ; token on eax
	invoke PushToken, FORINPUT, eax
	.IF CommentLevel==0
	    mov edx, (Token Ptr [eax]).ParentSymbol
	    mov dx, (Symbol Ptr [edx]).Kind
	    .IF dx==STERMINAL
		mov eax, MessageTokenRead
		ret
	    .ENDIF
	.ENDIF	  
	jmp ParseStarts
    .ELSEIF CommentLevel>0
	      invoke PopToken, FORINPUT
	      mov edx, (Token Ptr [eax]).ParentSymbol
	      mov dx, (Symbol Ptr [edx]).Kind  
	      .IF dx==SCOMMENTSTART
		  add CommentLevel, 1
	      .ELSEIF dx==SCOMMENTEND
		  sub CommentLevel, 1
	      .ELSEIF dx==SENDOFFILE
		  mov eax, MessageCommentError
		  ret
	      .ENDIF	 
	jmp ParseStarts       
    .ELSE 
	;;System in normal mode.
	
	invoke GetTopToken, FORINPUT
	mov edx, (Token Ptr [eax]).ParentSymbol
	mov dx, (Symbol Ptr [edx]).Kind 

	.IF dx==SWHITESPACE
	    invoke PopToken, FORINPUT		 
	    jmp ParseStarts
	.ELSEIF dx==SCOMMENTSTART
	    mov CommentLevel, 1
	    invoke PopToken, FORINPUT
	    jmp ParseStarts
	.ELSEIF dx==SCOMMENTLINE
	    invoke DiscardRestOfLine
	    invoke PopToken, FORINPUT
	    jmp ParseStarts
	.ELSEIF dx==SERROR
	    mov eax, MessageLexicalError
	    ret
	.ELSE ; we can finally parse the token :)
	
	   invoke ParseToken, Token Ptr [eax]
	   
	    .IF eax==PARSESHIFT ; 2
		invoke PopToken, FORINPUT
		jmp ParseStarts
		
	    .ELSEIF eax==PARSEREDUCENORMAL ; 3
		mov eax, MessageReduction
		ret
	       
	    .ELSEIF eax==PARSEACCEPT
		mov eax, MessageAccept
		ret

	    .ELSEIF eax==PARSESYNTAXERROR		 
		mov eax, MessageSyntaxError
		ret

	    .ELSEIF eax==PARSEINTERNALERROR
		mov eax, MessageInternalError
		ret 

	    .ELSE
		;What is this? Is it possible to end up here?
		jmp ParseStarts
		
	    .ENDIF
	.ENDIF	 
    .ENDIF    

;can't end up here but just in case...
mov eax, MessageInternalError
ret
Parse endP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
LoadGrammarFile proc uses esi edi ebx GrammarFile:Dword
Local FileMem:dword
Local FileLen:Dword
Local tempFileMem:DWord
Local MemEnd:Dword

Local CharSetStartPos:Dword
Local SymbolStartPos:Dword
Local RuleStartPos:Dword
Local RuleIndiceStartPos:Dword
Local DFAStartPos:Dword
Local EdgeStartPos:Dword
Local LALRStartPos:Dword
Local ActionStartPos:Dword

mov CharSetStartPos, 0
mov SymbolStartPos, 0
mov RuleStartPos, 0
mov RuleIndiceStartPos, 0
mov DFAStartPos, 0
mov EdgeStartPos, 0
mov LALRStartPos, 0
mov ActionStartPos, 0


.IF TablesLoaded!=0
    Showerror "Can't load another grammar file within the session, sorry."
    xor eax, eax
    ret
;One day I will stop being lazy and enable this future, but not now.
.ENDIF	  

;***********load the file*************
invoke read_disk_file, GrammarFile, addr FileMem, addr FileLen
.if eax==0
    ;Showerror "Failed to open grammar file."
    ret
.endif

;***********check header*******************
mov eax, CTXT("GOLD Parser Tables/v1.0")
invoke a2wc, eax
push eax
mov ecx, eax
invoke ucCmp, ecx, FileMem

.if eax==0
    ;Showerror "Wrong grammar file header."
    pop ecx    
    ret
.endif

pop eax
invoke SysFreeString, eax

;********************************************

mov eax, FileMem
add eax, FileLen
mov MemEnd, eax

mov eax, FileMem
add eax, 48 ; 48th reads 'M'
mov tempFileMem, eax

.WHILE 1
    mov eax, tempFileMem
    .IF eax>=MemEnd
	.IF eax>MemEnd
	    Showerror "An unknown error occured while loading the grammar file. Sorry."
	    xor eax, eax
	.ELSE
	    mov eax, -1 ;return true.
	.ENDIF
	jmp Finito    
    .ENDIF

    add eax, 1 ; to skip 'M'
    add tempFileMem, 3;  to skip totalEntryCount and 'M'
    
    add tempFileMem, 1 ; to skip 'b' that precedes the record type byte.
    mov eax, tempFileMem
    mov dl, [eax]
    
    .IF dl=='P' ;Parameter information.

	add eax, 1; skip 'P'
	mov ebx, eax
	
	add ebx, 1; skip 'S'
	invoke ReadUString, ebx
	mov pName, eax
    
	add ebx, 1
	invoke ReadUString, ebx
	mov pVersion, eax
    
	add ebx, 1
	invoke ReadUString, ebx
	mov pAuthor, eax
    
	add ebx, 1
	invoke ReadUString, ebx
	mov pAbout, eax
    
	add ebx, 1
	mov al, [ebx]
	mov pCaseSensitive, al
	add ebx, 1
    
	add ebx, 1
	mov ax, [ebx]
	mov pStartSymbol, ax
	add ebx, 2
    
	mov tempFileMem, ebx
    .ELSEIF dl=='T'

	add eax, 1 ; skip 'T'
	add eax, 1; redundant, it is always 'I'
	mov cx, [eax]
	mov SymbolTableCount, cx
	add eax, 2

	;;Create the necessary space for Symbols.
	    push eax
	    movzx ecx, cx
	    shl ecx, 3 ; SymbolSize is 8
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, ecx
	    mov Symbols, eax
	    pop eax

	add eax, 1; redundant, it is always 'I'
	mov cx, [eax]
	mov CharacterSetTableCount, cx
	add eax, 2

	;;Create the necessary space for DFA states & Edges:
	    push eax
	    movzx ecx, cx
	    shl ecx, 3 ; Charset size is 8
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, ecx
	    mov CharSets, eax
	    pop eax

	add eax, 1; redundant, it is always 'I'
	mov cx, [eax]
	mov RuleTableCount, cx
	add eax, 2

	    ;;Create the necessary space for Rules and RuleSymbolIndices:
	    push eax
	    movzx eax, cx
	    mov cx, RULESIZE
	    mul cx 
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, eax
	    mov Rules, eax
    
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, RULESYMBOLINDEXSPACE
	    mov RuleSymbolIndices, eax
	    pop eax
	
	add eax, 1; redundant, it is always 'I'
	mov cx, [eax]
	mov DFATableCount, cx
	add eax, 2
	
	    ;;Create the necessary space for DFA states & Edges:
	    push eax
	    movzx ecx, cx
	    shl ecx, 4 ; dfasize is 16
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, ecx
	    mov DFAStates, eax
    
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, EDGESPACE
	    mov Edges, eax
	    pop eax

	add eax, 1; redundant, it is always 'I'
	mov cx, [eax]
	mov LALRTableCount, cx
	add eax, 2

	    ;;Create the necessary space for LALRs and & Actions:
	    push eax
	    movzx eax, cx
	    mov cx, LALRSIZE
	    mul cx 
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, eax
	    mov LALRs, eax
    
	    invoke HeapAlloc, ProcessHeap, HEAP_ZERO_MEMORY, ACTIONSPACE
	    mov Actions, eax
	    pop eax

	mov tempFileMem, eax

    .ELSEIF dl=='I' ; initial States
	add eax, 1 ; skip 'I' (for 'I'nit)

	add eax, 1 ; skip 'I' (for 'I'nteger)
	mov cx, [eax]
	mov InitDFA, cx
	add eax, 2

	add eax, 1 ; skip 'I' (for 'I'nteger)
	mov cx, [eax]
	mov InitLALR, cx
	add eax, 2

	mov tempFileMem, eax
    .ELSEIF dl=='C' ; Character Sets
	add eax, 1; skip 'C'

	add eax, 1; skip 'I'
	mov cx, [eax]
	add eax, 2
	push ecx ;save it against readUstring function

	add eax, 1; skip 'S'
	invoke ReadUString, eax

	pop ecx
	
	mov edx, CharSets
	add edx, CharSetStartPos
	Assume edx: Ptr CharSet
	mov [edx].index, cx
	mov [edx].UString, eax
	Assume edx: Nothing

	add CharSetStartPos, 8
       
	mov tempFileMem, ebx

     .ELSEIF dl=='S' ; Symbols
	add eax, 1 ; skip 'S' (for 'S'ymbol)

	mov esi, Symbols
	add esi, SymbolStartPos
	
	Assume esi: Ptr Symbol

	add eax, 1 ; skip 'I'
	mov cx, [eax]
	mov [esi].index, cx
	add eax, 2
	
	add eax, 1 ; skip 'S' (name of symbol)
	invoke ReadUString, eax
	mov [esi].sName, eax
	mov eax, ebx

	add eax, 1; skip 'I' (kind)
	mov cx, [eax]
	mov [esi].Kind, cx
	add eax, 2

	Assume esi : Nothing
	
	add SymbolStartPos, 8

	mov tempFileMem, eax
	
    .ELSEIF dl=='R' ; Rules     ;                       ************* RULES *************
	add eax, 1 ; Skip 'R'

	mov esi, Rules
	add esi, RuleStartPos
	Assume esi: Ptr Rule	

	add eax, 1 ; Skip 'I'
	mov cx, [eax]
	mov [esi].index, cx
	add eax, 2

	add eax, 1 ; Skip 'I'
	mov cx, [eax]
	mov [esi].Nonterminal, cx
	add eax, 2

	add eax, 1 ; Skip 'E'
	xor edi, edi

	mov ebx, RuleSymbolIndices
	
	;;Read one byte, if it is 'M' then exit, otherwise ('I') add symbol to the rule's symbols array
	.WHILE 1
	    mov dl, [eax]
	    .if dl=='I'
		add TotalRuleSymbolIndices, 1 
		.if TotalRuleSymbolIndices>MAXRULESYMBOLINDEXCOUNT
		    Showerror "Total number of Rule symbols exceeded the limit. Sorry."
		    xor eax, eax ; return false                    
		    jmp Finito
		.endif

		add eax, 1 ; skip 'I'
		mov cx, [eax]
		
		add ebx, RuleIndiceStartPos
		mov [ebx], cx		     

		add edi, 1 ; symbolCount
		add eax, 2
		add RuleIndiceStartPos, 2
	    .else
		jmp KeepOn1
	    .endif
	.ENDW

KeepOn1:
	
	mov [esi].SymbolIndexCount, edi
	.if edi==0
	    xor ebx, ebx
	    mov [esi].SymbolIndices, ebx
	.else
	    mov [esi].SymbolIndices, ebx
	.endif

	add RuleStartPos, RULESIZE
	mov tempFileMem, eax
	Assume esi: Nothing

    .ELSEIF dl=='D' ;                       ************* DFA STATE *************

	add eax, 1 ; skip 'D'

	mov esi, DFAStates
	add esi, DFAStartPos
	Assume esi: Ptr DFAState

	add eax, 1 ; Skip 'I'
	mov cx, [eax]
	mov [esi].index, cx
	add eax, 2

	add eax, 1 ; Skip 'b'
	mov cl, [eax]
	mov [esi].AcceptState, cl
	add eax, 1

	add eax, 1 ; Skip 'I'
	mov cx, [eax]
	mov [esi].AcceptIndex, cx
	add eax, 2

	add eax, 1 ; Skip 'E'

	mov edi, Edges
	add edi, EdgeStartPos
	push edi

	xor ecx, ecx
	;;Read one byte, if it is 'M' then exit, otherwise ('I') add symbol to the rule's symbols array 
	.WHILE 1
	    mov dl, [eax]
	    .if dl=='I'
		add TotalEdges, 1 
		.if TotalEdges>MAXEDGECOUNT
		    Showerror "Total number of DFA edges exceeded the limit. Sorry."
		    xor eax, eax ; return false
		    jmp Finito
		.endif

		mov edi, Edges
		add edi, EdgeStartPos
		Assume edi: Ptr Edge

		add eax, 1 ; skip 'I'
		mov dx, [eax]
		mov [edi].CharSetIndex, dx
		add eax, 2

		add eax, 1 ; skip 'I'
		mov dx, [eax]
		mov [edi].TargetIndex, dx
		add eax, 2

		add eax, 1 ; skip 'E'

		add ecx, 1
		add EdgeStartPos, EDGESIZE
		
		Assume edi: Nothing	      
	    .else
		jmp KeepOn2
	    .endif
	.ENDW

KeepOn2:
	pop edi
	mov [esi].EdgeCount, ecx
	.if ecx==0
	    xor ebx, ebx
	    mov [esi].Edges, ebx
	.else
	    mov [esi].Edges, edi
	.endif

	add DFAStartPos, DFASIZE
	mov tempFileMem, eax
	Assume esi: Nothing

    .ELSEIF dl=='L' ;                       ************* LALR STATE *************
	add eax, 1 ; skip 'L'

	mov esi, LALRs
	add esi, LALRStartPos
	Assume esi: Ptr LALR

	add eax, 1 ; Skip 'I'
	mov cx, [eax]
	mov [esi].index, cx
	add eax, 2

	add eax, 1 ; Skip 'E'

	mov edi, Actions
	add edi, ActionStartPos
	push edi

	xor ecx, ecx
	;;Read one byte, if it is 'M' then exit, otherwise ('I') add symbol to the rule's symbols array 

	.WHILE 1
	    mov dl, [eax]
	    .if dl=='I'

		add TotalActions, 1 
		.if TotalActions>MAXACTIONCOUNT
		    Showerror "Total number of LALR actions exceeded the limit. Sorry."
		    xor eax, eax ; return false
		    jmp Finito
		.endif

		mov edi, Actions
		add edi, ActionStartPos
		Assume edi: Ptr Action

		add eax, 1 ; skip 'I'
		mov dx, [eax]
		mov [edi].SymbolIndex, dx
		add eax, 2

		add eax, 1 ; skip 'I'
		mov dx, [eax]
		mov [edi].ActionType, dx
		add eax, 2

		add eax, 1 ; skip 'I'
		mov dx, [eax]
		mov [edi].Target, dx
		add eax, 2

		add eax, 1 ; skip 'E'

		add ecx, 1
		add ActionStartPos, ACTIONSIZE
		
		Assume edi: Nothing   
	    .else
		jmp KeepOn3
	    .endif
	.ENDW

KeepOn3:
	pop edi
	mov [esi].ActionCount, ecx
	.if ecx==0
	    xor ebx, ebx
	    mov [esi].Actions, ebx
	.else
	    mov [esi].Actions, edi
	.endif

	add LALRStartPos, LALRSIZE
	mov tempFileMem, eax
	Assume esi: Nothing

    .ELSE
	Showerror "Unidentified multitype byte is encountered. Probably the grammar file is corrupt."
	xor eax, eax ; return false
	jmp Finito
    .ENDIF
.ENDW

Finito:
push eax

invoke GlobalFree, FileMem
pop eax ;return value

mov TablesLoaded, eax
ret
LoadGrammarFile endp
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
;************************* END OF INTERFACE FUNCTIONS *********************




; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
ReadUString Proc addrStart:Dword ; returns an address & updates the pointer at EBX
invoke SysAllocString, addrStart
push eax

invoke SysStringByteLen, eax
add eax, 2 ; terminating 0
mov ebx, addrStart
add ebx, eax

pop eax
ret
ReadUString EndP
; ллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

;***********************************************************************
DllMain proc hInstDLL:DWORD, reason:DWORD, unused:DWORD
    .if reason == DLL_PROCESS_ATTACH
	invoke GetProcessHeap
	mov ProcessHeap, eax
	mov eax, TRUE
    .elseif reason == DLL_PROCESS_DETACH
    .elseif reason == DLL_THREAD_ATTACH
    .elseif reason == DLL_THREAD_DETACH
    .endif   
ret
DllMain Endp 
;***********************************************************************


end DllMain
;end Start