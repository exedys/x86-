    include \masm32\include\windows.inc
    include \masm32\include\user32.inc
    include \masm32\include\kernel32.inc
    include \masm32\include\masm32.inc
    include \masm32\include\oleaut32.inc
    include \masm32\macros\macros.asm
    
    includelib \masm32\lib\user32.lib
    includelib \masm32\lib\kernel32.lib
    includelib \masm32\lib\masm32.lib
    includelib \masm32\lib\oleaut32.lib

    main PROTO

	;Some macros for debug.
    Showsmall Macro value
    pushad
    movzx eax, value
    invoke dwtoa, eax, addr stri
    invoke MessageBox, 0, addr stri, addr stri, 0
    popad
    EndM

    Showint Macro value
    pushad
    invoke dwtoa, value, addr stri
    invoke MessageBox, 0, addr stri, addr stri, 0
    popad
    EndM

    Showhex Macro value
    pushad
    invoke dw2hex, eax, addr stri
    invoke MessageBox, 0, addr stri, addr stri, 0
    popad
    EndM

    Geldi Macro
    pushad
    invoke MessageBox, 0, SADD("Geldi"), SADD("Geldi"), 0
    popad
    endM

    Showtext Macro str
    pushad
    invoke MessageBox, 0, SADD(str), SADD ("Geldi"), 0
    popad
    endM

    Showtext2 Macro str
    pushad
    invoke MessageBox, 0, str, str, 0
    popad
    endM


    MsgBox Macro valu
    pushad
    invoke MessageBoxA, 0, valu, valu, 0
    popad
    endm

    MsgBoxW Macro valu
    pushad
    invoke MessageBoxW, 0, valu, valu, 0
    popad
    endm

	;This one is used for real.
    Showerror Macro str
    pushad
    invoke MessageBox, 0, SADD(str), SADD("Error."), MB_ICONERROR
    popad
    endM


;system responses
MessageTokenRead equ 1
MessageReduction equ 2
MessageAccept equ 3
MessageNotLoaded equ 4
MessageLexicalError equ 5
MessageSyntaxError equ 6
MessageCommentError equ 7
MessageInternalError equ 8





CHARSETSIZE equ 8
SYMBOLSIZE equ 8
RULESIZE equ 12
DFASIZE equ 16
EDGESIZE equ 4
LALRSIZE equ 12
ACTIONSIZE equ 8
TOKENSIZE equ 12

;; Some Limitations for data allocation:

MAXSYMBOLCOUNTPERRULE equ 32
MAXRULESYMBOLINDEXCOUNT equ MAXSYMBOLCOUNTPERRULE * 512 ;; assuming a grammar can't contain more than 512 rules.
RULESYMBOLINDEXSPACE equ MAXRULESYMBOLINDEXCOUNT * 2 

MAXEDGECOUNT equ 2048
EDGESPACE equ MAXEDGECOUNT * EDGESIZE

MAXACTIONCOUNT equ 16384 ; Simply because my grammer produced about 5200 of them.
ACTIONSPACE equ MAXACTIONCOUNT * ACTIONSIZE



SNONTERMINAL equ 0  ;SymbolTypeNonterminal Normal Nonterminal 
STERMINAL equ 1; SymbolTypeTerminal Normal Terminal 
SWHITESPACE equ 2; SymbolTypeWhitespace Whitespace Terminal 
SENDOFFILE equ 3; SymbolTypeEnd End Character - End of File. This symbol is used to represent the end of the file or the end of the source input. 
SCOMMENTSTART equ 4; SymbolTypeCommentStart Start of a block quote 
SCOMMENTEND equ 5; SymbolTypeCommentEnd  End of a block quote 
SCOMMENTLINE equ 6; SymbolTypeCommentLine Line Comment Terminal 
SERROR equ 7; SymbolTypeError Error Terminal. If the parser encounters an error reading a token, this kind of symbol can used to differentiate it from other terminal types. 

;;STRUCTURES:::::::::
CharSet Struct ; 6 bytes
    index Word ?
    UString Dword ?
CharSet EndS

Symbol Struct ; 8 bytes
    index Word ?
    Kind Word ? ;Symbol kinds
    sName Dword ?
Symbol Ends

Rule Struct ;12 bytes
    index Word ?; This parameter holds the index of the rule in the Rule Table. The resulting rule should be stored at this Index. 
    Nonterminal Word ?; Each rule derives a single nonterminal symbol. This field contains the index of the symbol in the Symbol Table. 
    SymbolIndices Dword ?; Address of an array which holds the rules indices.
    SymbolIndexCount Dword ?; total number of indices hold by the above array. NOT IN SPECIFICATION OF GOLD.
Rule EndS

DFAState Struct ;13 bytes
    index Word ? ; This parameter holds the index of the DFA state in the DFA State Table.
    AcceptIndex Word ? ;Integer If the state accepts a terminal symbol, this field will contain the symbol's index in the Symbol Table. Otherwise, the value in this field should be ignored. 
    Edges Dword ? ; Address of an array which holds the edges.
    EdgeCount Dword ? ; total number of edges hold by the above array. NOT IN SPECIFICATION OF GOLD. 
    AcceptState Byte ? ; Boolean Each DFA state can accept one of the grammar's terminal symbols. If the state accepts a terminal symbol, the value will be set to True and the Accept Index parameter will contain the symbol's index. 
DFAState Ends

Edge Struct
    CharSetIndex Word ?
    TargetIndex Word ?
Edge EndS

LALR Struct ; 10 bytes. 
    index Word ? ; This parameter holds the index of the state in the LALR State Table. 
    Actions Dword ? ; Memory location of the actions array.
    ActionCount Dword ? ; total number of actions hold by the above array. NOT IN SPECIFICATION OF GOLD.
LALR EndS

Action Struct
    SymbolIndex Word ? ;This field contains the index in the Symbol Table. 
    ActionType Word ? ;This field contains a value that represents the action that LALR parsing engine is to take based on the symbol. These values are numerated below. 
    Target Word ? ; Depending on the value of the Action field, the target will hold different types of information. 
Action EndS

Token Struct
    ParentSymbol dd ?      ;   is a Symbol
    TokenData    dd ?      ;   is any object
    State dd ? ; is a LALR state.
Token endS

Reduction Struct ;;14 bytes.
     ParentRule dd ? ; is a Rule address
     TokenCount dd ? ; integer
     Tokens dd ? ; Pointer to the beinning of reductions' tokens in TokenStack.
     ParentRuleIndex dw ? ; is a Rule index
Reduction endS

;;ACTION CONSTS
ACTIONSHIFT equ 1 ;This action indicates the symbol is to be shifted. The Target field will contain the index of the state in the LALR State table that the parsing engine will advance to. 
ACTIONREDUCE equ 2 ; ActionReduce This action denotes that the parser can reduce a rule. The Target field will contain the index of the rule in the Rule Table. 
ACTIONGOTO equ 3 ; ActionGoto This action is used when a rule is reduced and the parser jumps to the state that represents the shifted nonterminal. 
                 ;The Target field will contain the index of the state in the LALR State table that the parsing engine will jump to after a reduction if completed. 
ACTIONACCEPT equ 4 ; When the parser encounters the Accept action for a given symbol, the source text is accepted as correct and complete. In this case, the Target field is not needed and should be ignored. 

;;parsetoken consts:
PARSEACCEPT equ 1
PARSESHIFT equ 2
PARSEREDUCENORMAL equ 3
PARSEREDUCETRIMMED equ 4
PARSESYNTAXERROR equ 5
PARSEINTERNALERROR equ 1939

externdef stri:byte
externdef ProcessHeap:dword

externdef pName: dword 
externdef pVersion: dword 
externdef pAuthor: dword 
externdef pAbout: dword 
externdef pCaseSensitive: byte
externdef pStartSymbol: word 

externdef CharSets:Dword
externdef Symbols :Dword
externdef Rules :Dword
externdef RuleSymbolIndices :Dword
externdef DFAStates :Dword
externdef Edges :Dword
externdef LALRs :Dword
externdef Actions :Dword

externdef InitDFA : Word
externdef InitLALR : Word

externdef CurrentLALRState : Dword
externdef CurrentReduction : Reduction

externdef StreamStart : Dword
externdef StreamCursor : Dword
externdef StreamEnd : Dword 

externdef CurrentLine : Dword
externdef CurrentCol : Dword

externdef ErrExpectedCount : Dword
externdef ErrExpStrList : Dword

externdef LastSuccessfulTokenString : Dword

externdef TokenPtr : Dword
externdef TokenPtrQ : Dword
externdef ReductionTokens : Byte

externdef CommentLevel : Dword

Initialize PROTO
ResetStacks PROTO
 
RetrieveToken PROTO
ParseToken PROTO Tkn:Token

    GetCharSetFromIndex PROTO index:Word
    GetSymbolFromIndex PROTO index:Word
    GetLALRFromIndex PROTO index:Word
    GetRuleFromIndex PROTO index:Word
    GetDFAFromIndex PROTO index:Word
    

    CreateToken PROTO ForWhat:Dword
    PopToken PROTO ForWhat :Dword
    PushToken PROTO ForWhat:Dword , Tkn:Dword
    GetTopToken PROTO ForWhat :Dword

    CopyTokensAndPop PROTO TknCount:Dword

    DiscardRestOfLine PROTO
	
FORINPUT equ 0
FORSTACK equ 1