	device zxspectrum128


BUFF    EQU #C000
ADR     EQU #C000
INIT_NUMBER_PACK EQU #01
TOTAL_PACKS      EQU #0B
SCREEN_ATTR_WS   EQU #180
SCREEN_ADDR_TOP  EQU #5B00
SCREEN_PIX_WS    EQU #0C00
BANK_WITH_BUFF   EQU #11
BANK_WITH_MUSIC  EQU #10
INIT_MUSIC       EQU #C00B
PLAY_MUSIC       EQU #C00E
PAUSE_MUSIC      EQU #C011
IM2_I_REG        EQU #5B
IM2_B_DATA       EQU #FF

	ORG #6000
begin

        EI
        LD (ALASM_STACK),SP
        LD SP,(PROGRAM_STACK)        

INIT:   LD BC,#0001
        LD (PACK_NUMBER),BC

 
        DI
        LD A,BANK_WITH_MUSIC
        CALL SET_AND_SAVE_BANK  ;SAVE CURRENT BANK AND SET PAGE
        CALL INIT_MUSIC
        EI
        
        ; очищаем область экрана
	LD BC,%0100
        LD D,%00000000
        LD E,%00000000
        LD A,0
        OUT (#FE),A
        DI
        CALL CLEAR_SCR_STACK
	EI
              
        
        	; ------------------------------------------------------


		; формируем таблицу адресов расположения каждого упакованного экрана
        LD HL, PACK1
        LD C,INIT_NUMBER_PACK
        LD B,TOTAL_PACKS ;COUNT OF PACKS-1
        LD (CURENT_ADR),HL

        CALL FILL_TABLE

		; переклчаем банк памяти на страницу с буфером экрана, куда будет идти распаковка
        DI
        LD A,BANK_WITH_BUFF
        CALL SET_BANK
        EI
        ; в RESERV1 содержится адрес таблицы с адресами упакованных экранов
	LD DE,RESERV1 ;DE ADR TABLE OF PACKS

        LD A,(DE)
        LD L,A			; получаем адрес первого упакованного экрана по таблице адресов
        INC DE			; в HL
        LD A,(DE)
        LD H,A
        INC DE			; переходим к следующей записи в таблице

        PUSH BC
        PUSH DE
        CALL DECOMPR	; распаковавыем экран в буфер
        POP DE
        POP BC
            

        PUSH DE
        PUSH BC
        LD BC,%0100		
        LD D,%00000000
        LD E,%00000000
        LD A,0
        OUT (#FE),A
        
       ; DI
       ; CALL CLEAR_SCR_STACK ; очищаем основной экран
       ; EI
        push af
        xor a
        OUT (#FE),A
        ld hl,$5AFF,de,$5AFe,bc,$1B00-1,(hl),0:lddr
        pop af 

        POP BC
        POP DE
        CALL BUFF_TO_SCREEN	; копируем содержимое буфера в экранную область памяти
        

;AGAIN:
;MAIN_LOOP:       
LK1:    ; опрос клавиатуры на нажатие клавиши "пробел"
        PUSH BC
        LD BC,#7FFE
        IN A,(C)
        RRA 
        POP BC

        JR C, LK1 ; повторяем, пока не нажата клавиша
	PUSH DE				
        LD DE, IM2			; устанавливаем вектор прерывания на обработчик, инициализируем прерывания IM 2
        CALL IM2_SETUP
        POP DE
AGAIN:
        ; опрос клавиатуры на нажатие клавиши "пробел"
        PUSH BC
        LD BC,#7FFE
        IN A,(C)
        RRA 
        POP BC

        JR C, AGAIN ; повторяем, пока не нажата клавиша

		
	;переключаем банк памяти на страницу с буфером экрана  
	DI      
        LD A,BANK_WITH_BUFF
        CALL SET_BANK
        EI
        
        LD A,(CURRENT_NUMBER_PACK) ; берем из переменной номер текущего упакованного экрана
        LD B, A
        CALL GET_IMAGE	; распаковывем в буфер и копируем изображение на экран

        LD A,(CURRENT_NUMBER_PACK)	; берем из переменной номер текущего упакованного экрана
        LD B,A						; увеличиваем счетчик
        INC B
        LD A,B
        LD (CURRENT_NUMBER_PACK),A	; записываем новый номер пака в ячейку
        CP #0C						; проверяем на последний номер пака	
        JR NZ, AGAIN			; если не последний, то переходим на опрос нажатия клавиши

	    ; картинки закончились, начинаем сначала
	DI 			
        LD A, INIT_NUMBER_PACK		
        LD (CURRENT_NUMBER_PACK),A	
        EI 

        JR AGAIN

VIEW1:  LD A,(DE)
        LD L,A
        INC DE
        LD A,(DE)
        LD H,A
        INC DE
        PUSH BC
        PUSH DE
        CALL DECOMPR
        POP DE
        POP BC
                
        PUSH DE
        PUSH BC
        LD BC,%0100
        LD D,%00000000
        LD E,%00000000
        LD A,0       
        POP BC
        POP DE
        CALL BUFF_TO_SCREEN      
        RET      

GET_IMAGE:

        LD DE,RESERV1 ;DE ADR TABLE OF PACKS

VIEW2:  LD A,(DE)
        LD L,A
        INC DE
        LD A,(DE)
        LD H,A
        INC DE
        DJNZ VIEW2

        PUSH BC
        PUSH DE
        CALL DECOMPR
      
        push af
        xor a
        OUT (#FE),A
        ld hl,$5AFF,de,$5AFe,bc,$1B00-1,(hl),0:lddr
        pop af
  
        CALL BUFF_TO_SCREEN
        POP DE
        POP BC

        RET

SET_AND_SAVE_BANK:
        LD (SAVED_BANK),A

SET_BANK
        PUSH BC
        LD BC, #7FFD
        OUT (C),A
        POP BC
        RET 

FILL_TABLE:

        LD A,C
        CP #01
        LD DE,RESERV1
        JR Z, GET_RES1
        INC DE
        INC DE
      
FILL1:  PUSH DE
        CALL GET_LENGHT
        ADD HL,DE
        INC HL
        INC HL
        POP DE
        LD (CURENT_ADR),HL
        LD A,L
        LD (DE),A
        INC DE
        LD A,H
        LD (DE),A
        INC DE
        DJNZ FILL1

        RET 

GET_RES1:
        LD (CURENT_ADR),HL
        LD A,L
        LD (DE),A
        INC DE
        LD A,H
        LD (DE),A
        INC DE
        INC C
        LD A,C
        CP #0F
        JR NZ, FILL1

        RET 


GET_LENGHT:

        PUSH BC
        LD HL,(CURENT_ADR)
        LD DE,5
        ADD HL,DE
        LD A,(HL)
        LD C,A
        INC HL
        LD A,(HL)
        LD B,A
        PUSH BC
        POP DE
        POP BC
        RET 

BUFF_TO_SCREEN:


SCREEN          EQU #4000
LENGHT_SCREEN   EQU #1B00

        PUSH HL
        PUSH DE
        PUSH BC
        PUSH AF

        LD HL,BUFF
        LD DE,SCREEN
        LD BC,LENGHT_SCREEN
        LDIR 

        POP AF
        POP BC
        POP DE
        POP HL

        RET 

CLEAR_SCR_STACK:

        PUSH AF
        PUSH BC
        PUSH HL
        PUSH IX
        LD BC,SCREEN_ATTR_WS
        LD IX,#0000
        ADD IX,SP
        LD SP,SCREEN_ADDR_TOP
        LD H,D
        LD L,D
CLRF_1: PUSH HL
        DEC BC
        LD A,B
        OR C
        JR NZ,CLRF_1
        LD BC,SCREEN_PIX_WS
        LD H,E
        LD L,E
CLRF_2: PUSH HL
        DEC BC
        LD A,B
        OR C
        JR NZ,CLRF_2
        LD SP,IX
        POP IX
        POP HL
        POP BC
        POP AF
        RET 

IM2_SETUP:
        DI 
        PUSH AF
        PUSH HL
        LD H,IM2_I_REG
        LD L,IM2_B_DATA
        LD A,H
        LD I,A

        LD (HL),E
        INC HL
        LD (HL),D
        POP HL
        POP AF
        IM 2
        EI 
        RET 

IM2:    ;halt
        DI 
        PUSH AF
        PUSH HL
        PUSH BC
        PUSH DE
        PUSH IX
        PUSH IY


        LD A,(SAVED_BANK)
        CALL SET_BANK
        
        CALL PLAY_MUSIC

        LD A,BANK_WITH_BUFF
        CALL SET_BANK

        POP IY
        POP IX
        POP DE
        POP BC
        POP HL
        POP AF
        EI 
        RETI 


// Декодируем и декомпрессируем изображение
DECODER:

DECOMPR:
        LD DE,7;SKIP "LCMP5" & LENGTH
        ADD HL,DE

        LD A,(HL)
        INC HL
        LD E,A
        ADD HL,DE

        LD A,(HL)
        LD E,A ;разрыв

        AND 3
        RLCA 
        RLCA 
        RLCA 
        OR ADR/256

        EXX 
        LD D,A; начало
        LD E,0
        EXX 

        LD A,(HL)
        INC HL
        XOR ADR/256+#18
        AND #FC
        LD HX,A ;конец

DLC1:
        LD A,(HL)
        INC HL
        LD LX,#FF

DLC2:
        EXX 
        JR NZ,DLC10
        LD B,1

DLC3:   EXA 
        SLA D
        JR NZ,$+6
        LD D,(HL)
        INC HL
        SLI D

        DJNZ DLC7

        JR C,DLC1

        INC B

        ;----------

DLC4:
        LD C,%01010110
        LD A,#FE

DLC5:
        SLA D
        JR NZ,$+6
        LD D,(HL)
        INC HL
        RL D
        RLA 
        SLA C
        JR Z,DLC6
        JR C,DLC5
        RRCA 
        JR NC,DLC5
        SUB 8
DLC6:
        ADD A,9
        ;------------

        DJNZ DLC3

        CP 0-8+1
        JR NZ,$+4
        LD A,(HL)
        INC HL

        ADC A,#FF
        LD LX,A
        JR C,DLC4
        LD HL,#2758
        EXX 
        RET 

        ;-------------

DLC7:
        LD A,(HL)
        INC HL

        EXX 
        LD L,A
        EXA 
        LD H,A
        ADD HL,DE

        CP #FF-2
        JR NC,DLC8
        DEC LX

DLC8:
        LD A,H
        CP HX
        JR NC,DLC13
        XOR L
        AND #F8
        XOR L
        LD B,A
        XOR L
        XOR H
        RLCA 
        RLCA 
        LD C,A

DLC9:
        EXA 
        LD A,(BC)

DLC10:
        EXA 
        LD A,D
        CP HX
        JR NC,DLC14
        XOR E
        AND #F8
        XOR E
        LD B,A
        XOR E
        XOR D
        RLCA 
        RLCA 

        LD C,A

DLC11:
        EXA 
        LD (BC),A

        INC DE
        JR NC,$+4
        DEC HL
        DEC HL
        INC HL
        EXA 
        INC LX
        JR NZ,DLC8
        JR DLC2

DLC13:
        SCF 

DLC14:
        PUSH AF
        EXX 
        ADD A,E
        EXX 
        LD B,A
        POP AF
        LD C,E
        JR NC,DLC11
        LD C,L
        JR DLC9

LENGDEC EQU $-DECOMPR


        RET 


;Global variables and  data

RESERV1: DUP 30
         DB #00
         EDUP 


SAVED_BANK     DB #00
ALASM_STACK    DEFW #0000
PROGRAM_STACK  DEFW #5FFF
PACK_BLOCK     DEFW #0000
CURENT_ADR     DEFW #0000
PACK_NUMBER    DEFW #0001
CURRENT_NUMBER_PACK   DB #02

// ПОДКЛЮЧАЕМ ИЗ РЕСУРСОВ НУЖНЫЕ ИЗОБРАЖЕНИЯ (УПАКОВАННЫЕ БЛОКИ)
ADR_PACK_BLOCK:
PACK1:  INCBIN "res/1_1.P"
PACK2:  INCBIN "res/2.P"
PACK3:  INCBIN "res/3.P"
PACK4:  INCBIN "res/4_2.P"
PACK5:  INCBIN "res/5_1.P"

PACK6   INCBIN "res/6.P"
PACK7:  INCBIN "res/7.P"
PACK8:  INCBIN "res/8.P"
PACK9   INCBIN "res/9_2.P"
;PACK10  INCBIN "res/10.P"
;PACK11: INCBIN "res/11.P"
PACK12: INCBIN "res/12_1.P"
;PACK13  INCBIN "res/13.P"
PACK14  INCBIN "res/14_1.P"
;PACK15: INCBIN "res/15_3.P"
;PACK16  INCBIN "16.P"

	
	ORG #C000

	INCBIN "res/PolonezM.M"

END_OF_PROGRAME  DB #00
        DS 16,#C9
	
	
end

// Формируем Бейсик загрузчик
 
BASIC_ORG	equ #5d3b

loader		disp BASIC_ORG
basic		dw #0100                 ; line number
		dw endloader-begin       ; line size
begin_b		db #ec                   ; "GO TO" token
		db #c0                   ; "USR" token
		db "0"                   ; "0" fake number
		db #0e, #00, #00
		dw start_b                 ; real call
		db #00, #0d, #80         ; end of line

		; your code goes here
start_b		;ld a, 4: out (#fe), a
		
                ld hl,name
                ld c,#13
                call #3d13
                
                LD C,#0A ; По перенесенному имени и типу файла ищем его в каталоге
                CALL #3D13 ; номер файла получаем в регистре С
                
                LD A,C ; Копируем полученный номер файла в каталоге в А
                LD C,#08 ; и по его номеру считываем из каталога в область системных переменных
                CALL #3D13 ; полные данные о файле
                
                LD HL,begin ; в HL - адрес куда будем считывать файл
                LD DE,(#5CEB) ; в DE - из сист.переменных берем начальную дорожку и сектор файла
                LD A,(#5CEA) ; в B через А заносим длину файла в секторах
                LD B,A ;
                LD C,5 ; читаем!!!
                CALL #3D13
                call begin
                di
                halt
name
	db "polonez C"
        db #0d   ; enter (end of basic line)
                
                
                
                di: halt







endloader     



// Подготавливаем и сохраняем образы памяти и диска

;-------------------------------------------------------------

		display "------------------------------"
		display "start:  ",/A,begin," bytes"
		display "total:  ",/A,end-begin," bytes"
		display "------------------------------"		

;-----------------------------------------------------------

        ;display /d,end-begin
        savesna "C:\ZX\_Unreal\qsave1.sna", begin_b
	labelslist "c:\ZX\_Unreal\user.l"

         display "Boot: ", basic, "-", endloader, " (", /d, endloader-basic, "b)"
		ent

		define TRD_NAME "polonez.trd"

		emptytrd TRD_NAME       ;create empty TRD image
                ;PAGE 7 ;set 7 page to current slot

		savetrd TRD_NAME, "boot.B", loader, endloader-basic, 1     
    	        SAVETRD "polonez.trd","polonez.C",begin,end-begin ;
    
;-----------------------------------------------------------