.486
    .model flat, stdcall
    option casemap :none   ; case sensitive
    include include.inc

ZeroMemoryByDword Proto address:Dword , bCount:Dword

TOKENSTACKELEMENTCOUNT equ 128
TOKENSIZE equ 12
TOKENSTACKSIZE equ TOKENSTACKELEMENTCOUNT * TOKENSIZE 

.data?
ReductionTokens db TOKENSTACKSIZE dup (?)
Tokens db TOKENSTACKSIZE  dup (?)
TokensQ db TOKENSTACKSIZE  dup (?)
TokenPtr dd ? ; Stack Tokens.
TokenPtrQ dd ? ; Input Tokens.

;.data?
DefaultToken Token <> ;used to create new token.

.code
;********************************************************************************
CreateToken Proc ForWhat:dword
mov eax, offset DefaultToken
xor ecx, ecx
mov [eax+0], ecx
mov [eax+4], ecx
mov [eax+8], ecx
ret
CreateToken endP
;********************************************************************************

;********************************************************************************
CreateToken2 Proc ForWhat:Dword

.IF ForWhat==FORINPUT
    mov eax, offset TokensQ
    mov ecx, TokenPtrQ
.ELSE
    mov eax, offset Tokens
    mov ecx, TokenPtr
.ENDIF	     

add eax, ecx
add eax, TOKENSIZE

.IF ecx>=TOKENSTACKSIZE
    Showerror "Can't create new token, stack full."
    invoke ExitProcess, -1
.ENDIF

xor ecx, ecx
mov [eax+0], ecx
mov [eax+4], ecx
mov [eax+8], ecx
ret
CreateToken2 endP
;********************************************************************************

;********************************************************************************
PushToken Proc ForWhat:Dword, Tkn:Dword

.IF ForWhat==FORINPUT
    mov eax, offset TokensQ
    mov ecx, TokenPtrQ
    add TokenPtrQ, TOKENSIZE
.ELSE
    mov eax, offset Tokens
    mov ecx, TokenPtr
    add TokenPtr, TOKENSIZE
.ENDIF	 

add eax, ecx

.IF ecx>=TOKENSTACKSIZE
    Showerror "Can't push token, stack full."
    invoke ExitProcess, -1
.ENDIF

mov ecx, Tkn
Assume ecx:Ptr Token
mov edx, [ecx].ParentSymbol
mov [eax+0], edx
mov edx, [ecx].TokenData
mov [eax+4], edx
mov edx, [ecx].State
mov [eax+8], edx
Assume ecx:Nothing

ret
PushToken endP
;********************************************************************************

;********************************************************************************
PopToken Proc ForWhat:Dword

.IF ForWhat==FORINPUT
    mov eax, offset TokensQ
    sub TokenPtrQ, TOKENSIZE
    mov ecx, TokenPtrQ
.ELSE
    mov eax, offset Tokens
    sub TokenPtr, TOKENSIZE
    mov ecx, TokenPtr
.ENDIF	 

		    COMMENT /
.IF ecx<0
    Showerror "Can't pop token, stack empty."
    invoke ExitProcess, -1
.ENDIF
		    COMMENT /

add eax, ecx

;important: it seems that when we pop data, we never use the result.
;Therefore, we can release the token's data when we pop.
mov edx, (Token Ptr [eax+TOKENSIZE]).TokenData
.IF edx!=0	  
    push eax
    invoke HeapFree, ProcessHeap, 0, edx
    pop eax
    mov (Token Ptr [eax+TOKENSIZE]).TokenData, 0 ; indicate we erased it.
.ENDIF
   
ret
PopToken endP
;********************************************************************************

;********************************************************************************
GetTopToken Proc ForWhat:Dword

.IF ForWhat==FORINPUT
    mov eax, offset TokensQ
    mov ecx, TokenPtrQ
.ELSE
    mov eax, offset Tokens
    mov ecx, TokenPtr
.ENDIF	 

			    COMMENT ²
.IF ecx<=0
    Showerror "Can't get token, stack empty."
    invoke ExitProcess, -1
.ENDIF
			    COMMENT ²

add eax, ecx
sub eax, TOKENSIZE
ret
GetTopToken endP
;********************************************************************************

;***************************************************************
CopyTokensAndPop Proc uses esi edi TknCount:Dword

cmp TknCount, 0
je NoNeed


;first of all, release the old reductions' tokens' data:
mov esi, offset ReductionTokens
xor edi , edi
@@:
mov edx, (Token Ptr [esi]).TokenData
.IF edx!=0
    invoke HeapFree, ProcessHeap, 0, edx
    ;mov (Token Ptr [esi]).TokenData, 0 ; to indicate we already realesed it.
    ;no need for the above line as we'll overwrite it anyways.
.ENDIF
add esi, TOKENSIZE
add edi, 1
cmp edi, TknCount
jl @B


;Ok, now copy and move the stack pointer.
mov eax, TknCount
mov edi, TOKENSIZE
mul edi
mov edi, eax

mov esi, offset Tokens
add esi, TokenPtr
sub esi, eax

mov eax, offset ReductionTokens

sub TokenPtr, edi

invoke MemCopy, esi, eax, edi

;Finally, zero out the normal stack to indicate that it is already cleared
;Not doing it resulted in read page fault. (because reset stack tries to release memory
;that already has been realesed.)

invoke ZeroMemoryByDword, esi, edi

NoNeed:
ret
CopyTokensAndPop endP
;***************************************************************

;***************************************************************
ResetStack Proc uses esi edi StackAddr:Dword

    mov esi, StackAddr
    xor edi, edi
@@:    
    mov eax, (Token Ptr [esi+edi]).TokenData
    .IF eax!=0
	invoke HeapFree, ProcessHeap, 0, eax
    .ENDIF
    add edi, TOKENSIZE
    cmp edi, TOKENSTACKSIZE
    jl @B

    invoke ZeroMemoryByDword, esi, TOKENSTACKSIZE
ret   
ResetStack endP
;***************************************************************

;***************************************************************
ResetStacks Proc
    invoke ResetStack, offset Tokens
    invoke ResetStack, offset TokensQ
    invoke ResetStack, offset ReductionTokens

    mov TokenPtr, 0
    mov TokenPtrQ, 0 
ret
ResetStacks endP
;***************************************************************

;******************************************************************************************
ZeroMemoryByDword Proc dest:Dword, ByteCount:Dword
; a fast zeromemory function if you know that the size is divisible by 4.

mov ecx, ByteCount
or ecx, ecx
jz ZeroingFinished

xor edx, edx
xor ecx, ecx
mov eax, dest

KeepOnZeroing:
mov [eax+ecx], edx

add ecx, 4
cmp ecx, ByteCount
jl KeepOnZeroing

ZeroingFinished:
ret
ZeroMemoryByDword EndP 
;******************************************************************************************
end
