.486
    .model flat, stdcall
    option casemap :none   ; case sensitive
    include include.inc

.data?
ReduceRule dd ?
CurrentReduction Reduction <>

.data 
ErrExpectedCount dd 0		 
ErrExpStrList dd 128 dup (0)

.code

;********************************************************************************
ParseToken Proc uses esi edi ebx Tkn:Token
;if an Action for Token exists in the Current-LALR-State

mov edx, CurrentLALRState
mov ebx, (LALR Ptr [edx]).ActionCount

.IF ecx!=0
    mov edi, (LALR Ptr [edx]).Actions ; edi is an action
    xor esi, esi
    .WHILE esi<ebx

	    mov ax, (Action Ptr [edi]).SymbolIndex
	    invoke GetSymbolFromIndex, ax

	    .IF Tkn.ParentSymbol==eax	 
		jmp ActionFound  
	    .ELSE
		add esi, 1
		add edi, ACTIONSIZE
	    .ENDIF
    .ENDW	     
.ENDIF

;********************************
;;COMING HERE IS A SYNTAX ERROR.
mov eax, CurrentLALRState
mov edx, (LALR Ptr [eax]).ActionCount
mov ebx, (LALR Ptr [eax]).Actions
xor edi, edi
xor esi, esi ; ErrExpCount
.WHILE edi<edx
    push edx ;save action count
    mov ax, (Action Ptr [ebx]).ActionType
    ;.IF ax==ACTIONSHIFT
	mov cx, (Action Ptr [ebx]).SymbolIndex		  
	invoke GetSymbolFromIndex, cx
	mov cx, (Symbol Ptr [eax]).Kind
	.IF cx==STERMINAL
	    mov eax, (Symbol Ptr [eax]).sName
	    mov [ErrExpStrList+esi*4], eax
	    add esi, 1
	.ENDIF
    ;.ENDIF
    add edi, 1
    add ebx, ACTIONSIZE
    pop edx ; restore action count
.ENDW	 

mov [ErrExpectedCount], esi
mov eax, PARSESYNTAXERROR
ret
;********************************

ActionFound:

mov cx, (Action Ptr [edi]).ActionType

.IF cx==ACTIONSHIFT ;1
;**order!
;Set Current-LALR-State = target state of Action.
;Set the State property of Token to the Current-LALR-State.
;Push Token onto the **Token-Stack. (Queue!)                          
;Set Result = Shift.
	mov ax, (Action Ptr [edi]).Target
	invoke GetLALRFromIndex, ax
	mov CurrentLALRState, eax

	mov eax, CurrentLALRState
	mov Tkn.State, eax

	invoke PushToken, FORSTACK, addr Tkn
	mov eax, PARSESHIFT
	ret
.ELSEIF cx==ACTIONREDUCE ;2
	mov ax, (Action Ptr [edi]).Target

	invoke GetRuleFromIndex, ax
	mov ReduceRule, eax

	;PART 1: REDUCE THE RULE
		mov CurrentReduction.ParentRule, eax
		mov ecx, (Rule Ptr [eax]).SymbolIndexCount
		mov CurrentReduction.TokenCount, ecx
		invoke CopyTokensAndPop, ecx		

		mov edx, ReduceRule
		mov di, (Rule Ptr [edx]).Nonterminal ;;dx holds NonTerminal Symbol for the rule.
		      
		invoke GetSymbolFromIndex, di
		push eax ; Nonterminal Symbol on the stack. (real cpu stack :)
		invoke GetTopToken, FORSTACK
		mov eax, (Token Ptr [eax]).State  ;; eax holds new LALR state
		mov ecx, (LALR Ptr [eax]).ActionCount
		mov eax, (LALR Ptr [eax]).Actions ;; eax holds an action
		
		xor esi, esi
		.WHILE esi<ecx
			mov dx, (Action Ptr [eax+esi*ACTIONSIZE]).SymbolIndex
			.IF di==dx
				invoke GetLALRFromIndex, (Action Ptr [eax+esi*ACTIONSIZE]).Target
				mov esi, eax
				mov CurrentLALRState, eax
				invoke CreateToken, FORSTACK
				mov (Token Ptr [eax]).State, esi
				pop edx ;; Nonterminal Symbol previously pushed on the stack.
				mov (Token Ptr [eax]).ParentSymbol, edx
				invoke PushToken, FORSTACK, eax
				mov eax, PARSEREDUCENORMAL
				ret
			.ENDIF
			add esi, 1
		.ENDW 

		;;Coming here indicates a serious error.                 
		mov eax, PARSEINTERNALERROR; 8
		ret		
.ELSEIF cx==ACTIONACCEPT ; 4
	mov eax, PARSEACCEPT
	ret
.ENDIF

;can't possibly end up in here but just in case:
mov eax, PARSEINTERNALERROR
ret	   
ParseToken endP
;********************************************************************************


End