; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page TOC-1
; 							Table of Contents					

; 1		KLX.MIC[4,24]	15:48 8-Mar-86
; 1	KL10 Microcode with KL Paging
; 12		EDHIS.MIC[4,24]	12:02 29-May-86
; 29	REVISION HISTORY
; 1228		DEFINE.MIC[4,24]	16:58 23-May-86
; 1229	CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS
; 1316	HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS
; 1519	MICROCODE LISTING TEMPLATE
; 1570	KL10 INSTRUCTION OPCODE MAP
; 1626	CONTROL RAM DEFINITIONS -- J, AD
; 1682	CONTROL RAM DEFINITIONS -- DATA PATH MIXERS
; 1750	CONTROL RAM DEFINITIONS -- 10-BIT LOGIC
; 1783	CONTROL RAM DEFINITIONS -- SHIFT, ARMM, VMA, TIME
; 1815	CONTROL RAM DEFINITIONS -- MEM SPECIAL FUNCTIONS
; 1838	CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS
; 1918	CONTROL RAM DEFINITIONS -- DISP/SPEC SPECIAL FUNCTIONS
; 1963	CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD
; 2255	DISPATCH RAM DEFINITIONS
; 2308		MACRO.MIC[4,24]	11:59 29-May-86
; 2309	CRAM Macros--Miscellaneous and A
; 2348	CRAM Macros--AR
; 2572	CRAM Macros--AR Miscellaneous, ARL, and ARR
; 2651	CRAM Macros--ARX
; 2760	CRAM Macros--B, C, D
; 2839	CRAM Macros--E, F
; 2935	CRAM Macros--G, H, I, J, L
; 3043	CRAM Macros--M, N, O, P
; 3155	CRAM Macros--R
; 3211	CRAM Macros--S
; 3415	CRAM Macros--T, U, V, W, X
; 3506	DRAM Macros
; 3577		BASIC.MIC[4,24]	11:56 29-May-86
; 3578	THE INSTRUCTION LOOP
; 3679	NEXT INSTRUCTION DISPATCH
; 3810	EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH
; 3847	WAIT FOR (E)
; 3891	TERMINATION
; 3944	MOVE GROUP, EXCH, XMOVEI, XHLLI
; 3998	Physical MOVE Instructions--PMOVE, PMOVEM
; 4027	DMOVE, DMOVN, DMOVEM, DMOVNM
; 4083	HALFWORD GROUP
; 4227	BOOLEAN GROUP
; 4374		SKPJMP.MIC[4,24]	11:57 29-May-86
; 4375	TEST GROUP
; 4483	COMPARE -- CAI, CAM
; 4509	ARITHMETIC SKIPS -- AOS, SOS, SKIP
; 4559	CONDITIONAL JUMPS -- JUMP, AOJ, SOJ, AOBJ
; 4615	AC DECODE JUMPS -- JRST
; 4722	HALT LOOP
; 4746	AC DECODE JUMPS -- JFCL
; 4764	MAP
; 4789	STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ
; 4864	SUBROUTINE CALL/RETURN -- JSR, JSP, JSA, JRA
; 4897	UUO'S
; 5112	JSYS, ADJSP, XCT, PXCT
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page TOC-2
; 							Table of Contents					

; 5151		SHIFT.MIC[4,24]	16:52 3-Apr-86
; 5152	Rotate and Logical Shift -- LSH, ROT
; 5199	Rotate and Logical Shift Combined (ROTC, LSHC)
; 5245	Arithmetic Shifts (ASH, ASHC) and JFFO
; 5339		ARITH.MIC[4,24]	16:07 19-Mar-86
; 5340	ADD, SUB
; 5365	MUL, IMUL
; 5421	MULTIPLY SUBROUTINE
; 5469	DIV, IDIV
; 5522	INTEGER DIVIDE SUBROUTINE
; 5585	BASIC DIVIDE LOOP
; 5634	DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV
; 5752		FP.MIC[4,24]	15:33 8-Feb-86
; 5753	SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR
; 5833	SINGLE FLOATING MULTIPLY -- FMP, FMPR
; 5877	SINGLE FLOATING DIVIDE -- FDV, FDVR
; 6004	UFA, DFN, FSC
; 6055	FIX, FIXR, FLTR
; 6101	SINGLE PRECISION FLOATING NORMALIZATION
; 6242	DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV
; 6371	DOUBLE PRECISION NORMALIZATION
; 6417		EXTEXP.MIC[4,24]	15:33 8-Feb-86
; 6418	GFLT DOUBLE PRECISION ARITHMETIC
; 6536	GFLT MULTIPLY
; 6583	GFLT DIVIDE
; 6626	GFLT NORMALIZATION
; 6769	GFLT TO INTEGER CONVERSION
; 6895	GFLT DATA CONVERSION INSTRUCTIONS
; 7104		BLT.MIC[4,24]	16:34 23-May-86
; 7105	BLT - Neatly Optimized
; 7241	XBLT--Also Neatly Modernized
; 7333		BYTE.MIC[4,24]	17:29 14-Mar-86
; 7334	Single Byte Instructions:  ILDB, LDB
; 7451	Single Byte Instructions:  DPB, IDPB
; 7558	Single Byte Instructions:  IBP, ADJBP
; 7765	Subroutines for Single Byte Instructions
; 7933		BYTSUB.MIC[4,24]	14:19 22-May-86
; 7934	BYTE GROUP -- Some Old Style Subroutines
; 7943	INCREMENT BYTE POINTER SUBROUTINE
; 7957	BYTE EFFECTIVE ADDRESS EVALUATOR - XADDR
; 7984	Load and Deposit Byte Subroutines
; 8030		EIS.MIC[4,24]	10:37 27-May-86
; 8031	EXTENDED INSTRUCTION SET DECODING
; 8171	ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND
; 8234	EIS -- STRING MOVE
; 8353	EIS -- STRING COMPARE
; 8419	EIS -- DECIMAL TO BINARY CONVERSION
; 8488	EIS -- BINARY TO DECIMAL CONVERSION
; 8645	EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE
; 8818	EIS -- EDIT FUNCTION
; 9016		IO.MIC[4,24]	15:27 17-Mar-86
; 9017	I/O INSTRUCTIONS
; 9116	EXTERNAL DEVICE I/O INSTRUCTIONS
; 9208	INTERNAL DEVICE FUNCTIONS -- APR, CCA
; 9262	INTERNAL DEVICE FUNCTIONS -- PI
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page TOC-3
; 							Table of Contents					

; 9316	INTERNAL DEVICE FUNCTIONS -- PAG
; 9423	INTERNAL DEVICE FUNCTIONS -- TIM & MTR
; 9531	PRIORITY INTERRUPT PROCESSING
; 9682	KL-MODE PAGE REFILL LOGIC
; 10046	Page Fail Cleanup and Special Instruction Dispatch
; 10137	PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS
;	Cross Reference Index
;	DCODE Location / Line Number Index
;	UCODE Location / Line Number Index
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; KLX.MIC[4,24]	15:48 8-Mar-86					KLX.MIC[4,24]	15:48 8-Mar-86			

						; 1	.TOC	"KL10 Microcode with KL Paging"
						; 2	
						; 3	.SET/SNORM.OPT=1
						; 4	.SET/FPLONG=0
						; 5	.SET/EXTEXP=1
						; 6	.SET/MULTI=1		;DOES NOT CACHE PAGE TABLE DATA
						; 7	.SET/NOCST=1		;DOES NOT DO AGE UPDATES, ETC. WITH CST = 0
						; 8	.SET/OWGBP=1		;ONE WORD GLOBAL BYTE POINTERS
						; 9	.SET/IPA20=1		;IPA20-L
						; 10	.SET/GFTCNV=0		;DO NOT DO GFLOAT CONVERSION INSTRUCTIONS [273]
						; 11				;SAVES 75 WORDS. MONITOR WILL TAKE CARE OF THEM.
						; 12	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; EDHIS.MIC[4,24]	12:02 29-May-86				KLX.MIC[4,24]	15:48 8-Mar-86			

; 13	.NOBIN
; 14	
; 15	;	THE INFORMATION IN THIS DOCUMENT IS SUBJECT TO CHANGE WITHOUT
; 16	; NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL
; 17	; EQUIPMENT CORPORATION.  DIGITAL EQUIPMENT CORPORATION ASSUMES NO
; 18	; RESPONSIBITY FOR ANY ERRORS THAT MAY APPEAR IN THIS DOCUMENT.
; 19	;	THE SOFTWARE DESCRIBED IN THIS DOCUMENT IS FURNISHED TO THE
; 20	; PURCHASER UNDER A LICENSE FOR USE ON A SINGLE COMPUTER SYSTEM AND
; 21	; CAN BE COPIED (WITH INCLUSION OF DIGITAL'S COPYRIGHT NOTICE) ONLY
; 22	; FOR USE IN SUCH SYSTEM, EXCEPT AS MAY OTHERWISE BE PROVIDED IN WRITING
; 23	; BY DIGITAL.
; 24	;	DIGITAL EQUIPMENT CORPORATION ASSUMES NO RESPONSIBILITY FOR THE
; 25	; USE OR RELIABILITY OF ITS SOFTWARE ON EQUIPMENT THAT IS NOT SUPPLIED
; 26	; BY DIGITAL.
; 27	; COPYRIGHT (C) 1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986 DIGITAL EQUIPMENT CORPORATION
; 28	
; 29	.TOC	"REVISION HISTORY"
; 30	
; 31	;	The following collection of people have contributed to the
; 32	;	production and maintenance of this code.  In reverse chronological
; 33	;	order:
; 34	;
; 35	;	QQSV (Dick Wagman) -- beginning with edit 301
; 36	;	Sean Keenan
; 37	;	Don Dossa
; 38	;	Mike Newman
; 39	;	Jud Leonard
; 40	;
; 41	.TITLE	"KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986"
; 42	.VERSION/MAJOR=2/MINOR=1/EDIT=442/WHO=0
; 43	;REV	WHY
; 44	;
; 45	;442	29 May 86--Clean up the listing a bit in a few miscellaneous places.
; 46	;	Rewrite the SETCA group to use AR_AC0 COMP (looks more obvious than
; 47	;	the previous stuff).  Avoid the write test for HRRZM, using the last
; 48	;	remaining microword to optimize it.  This looks like the last edit
; 49	;	from QQSV.  So long, everybody.  It's been real!
; 50	;441	27 May 86--An afterthought edit.  Move DB2WD in line with CVTDBx
; 51	;	code, and use the space to make JUMPA fetch faster.  This required
; 52	;	moving the TDN dispatch block as well due to DRAM constraints.
; 53	;440	23 May 86--A final omnibus speedup edit.  Add an instruction to allow
; 54	;	TLO/TSO/TLZ/TSZ/TLC/TSC to prefetch.  Special case AOJ, AOJA, SOJ,
; 55	;	SOJA, AOS, and SOS to speed the next instruction fetch in all cases.
; 56	;	Allow JFCL 0, to take a fast path.  Make MOVES, SKIP, HLLS, and HRRS
; 57	;	all use new code that starts the instruction fetch a fair amount
; 58	;	quicker in a few cases.  Juggle the DRAM for all adjacent instructions,
; 59	;	and reconfigure the explicit CRAM addresses where necessary.
; 60	;437	22 May 86--Make MOVSO and MOVST start the I FETCH a few micro-
; 61	;	instructions later when they abort.  A page fault on the I FETCH was
; 62	;	leaving the SR set, and resulted in random garbage ending up in AC0.
; 63	;	Merge GTST and CNV2WD into TST2WD, saving a few words and a little bit
; 64	;	of time.
; 65	;436	17 Apr 86--Back off optimization of JRSTF.  Going to user mode doesn't
; 66	;	set USER in time for the FETCH to occur on the same microinstruction.
; 67	;435	14 Apr 86--Install bit 4 of APRID as PMOVE present option bit.
; 68	;434	7 Apr 86--Edit PMOVE and PMOVEM onto proper op codes.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 69	;433	4 Apr 86--Edit new JFFO onto proper op code.  Install prototype PMOVE
; 70	;	and PMOVEM, using op codes 100 and 101 for now.
; 71	;432	1 Apr 86--Edit new ADJSP onto proper op code.  Rewrite JFFO to isolate
; 72	;	the bit number within a group more quickly, and install it on op codes
; 73	;	100 and 101 for debugging.
; 74	;431	31 Mar 86--Rewrite ADJSP to start the I FETCH several cycles earlier
; 75	;	by not using PUSH code to set TRAP2.
; 76	;430	20 Mar 86--Start the I FETCH for IMULB, IDIVB, and friends at ST2AC+1
; 77	;	by making it use the DSHIFT code.  Eliminate the EXIT DBL macro as
; 78	;	obsolete.  Move the op code 247 dispatch in with other UUOs in
; 79	;	SKPJMP.  Knock a cycle out of SETMM and SETMB by making them go
; 80	;	directly to IFNOP and IFSTAC.  (This is pretty useless, but
; 81	;	harmless and correct.)  Make a few cosmetic edits.
; 82	;427	18 Mar 86--Install rewritten XBLT in production form, and eliminate
; 83	;	old code.  This makes the BLT file entirely new code.  Make room
; 84	;	for this by rewriting effective address decodes for EXTEND sub
; 85	;	op and for byte pointer in string instructions, making use of
; 86	;	indirection decoder used for single byte instructions.  (Note that
; 87	;	this could impact the PXCTability of EXTEND op codes, but since
; 88	;	only XBLT is supposed to be PXCTed, this should never matter.)  In
; 89	;	the process, fix bug where indirection in byte pointer calculations
; 90	;	was taking interrupt without going to CLEAN first.  Also fix bug in
; 91	;	LDB of an illegal OWG, where an LDB of an OWG with a P&S field of
; 92	;	(for example) 45 was not properly clearing the right half.  This
; 93	;	was introduced when we rewrote SETZ.
; 94	;426	13 Mar 86--Rewrite XBLT.  Implement it on opcodes 100 and 101 for
; 95	;	debugging convenience.
; 96	;425	8 Mar 86--Install optimized BLT.  Eliminate obsolete conditionals
; 97	;	BACK.BLT, BLT.PXCT, and RPW (not related) in the process.
; 98	;424	17 Feb 86--Remove IMULM and IMULB from IMULx optimization, allowing
; 99	;	IMUL and IMULI to begin their I FETCH one microinstruction earlier.
; 100	;	Fix ADJBP of a TWG with byte size zero to correctly load both
; 101	;	halves of the byte pointer (at TWGCPY+1).  Reedit BYTSUB.MIC to
; 102	;	remove a few extraneous words that are no longer needed now that
; 103	;	the single byte instructions have been rewritten.
; 104	;423	13 Feb 86--Install upgraded ASH code. Reedit ASHC to remove code
; 105	;	used only by ASH previously.
; 106	;422	29 Jan 86--Rework speeded IDIVx code onto the proper opcodes, and
; 107	;	delete old IDIVx code.  Reedit a few instructions in that vicinity
; 108	;	to clean up the listing, and remove references to NODIVD label by
; 109	;	making those instructions do SET NO DIVIDE on their own.
; 110	;421	23 Jan 86--Force the SETZ group to use HLLZ code, saving a word.
; 111	;	Make sure DMOVNM loads both halves of AC1 when it starts.
; 112	;	Implement speeded up IDIVx on opcodes 100 and 101 for easy debugging.
; 113	;420	21 Jan 86--Add logic to the integer divide instructions to enable
; 114	;	generating the maximum negative number as a quotient.  This is
; 115	;	in preparation for the IDIVx optimization.
; 116	;417	10 Jan 86--Prevent ROT from (sometimes) clobbering the next
; 117	;	instruction in ARX on a short right rotation (the timing was
; 118	;	close, so it worked most of the time--very bad).  Costs a
; 119	;	microword (sigh).
; 120	;416	7 Dec 85--Rewrite IMULx to do what the old IMULI.OPT should have
; 121	;	done, namely, optimize IMULx of a positive by a positive when
; 122	;	we can be sure that no overflow will occur.  Costs five words,
; 123	;	only two more than the original (broken) IMULI.OPT.  Eliminate
; 124	;	that conditional as obsolete.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-2
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 125	;415	7 Dec 85--Rewrite LSHC and ROTC (at a cost of one word), saving
; 126	;	some time, particularly on right rotates and shifts.
; 127	;414	5 Dec 85--Rewrite the JRST group, not affecting speed very much
; 128	;	but saving many microwords in the process.  Shuffle MOVMI dispatch
; 129	;	to make it identical to HRRZI, saving a bit of time.  Work on UUO
; 130	;	code to save some space.  Crack a couple of words out of JRA,
; 131	;	speeding it up (almost in spite of ourselves).  Force old EIS
; 132	;	effective address dispatch to exit to 3177 instead of 3077, releasing
; 133	;	3077 for other uses.  Knock another word out of POPJ.  Make PF24
; 134	;	go directly to PFPAR, saving a cycle for illegal indirect page
; 135	;	faults (and buying back three words in the process).  Edits 234,
; 136	;	242, and 271 appear ill conceived.  Fix ARL IND and TIME fields
; 137	;	for second counting loop in JFFO, buying a little free speed.
; 138	;	Rework dispatches for FAD/FSB/FMP/FDV, and fix FPLONG conditional,
; 139	;	saving a couple of words and making FADL/FSBL/FMPL/FDVL work when
; 140	;	the conditional is turned on.  Also twiddle definitions of EXP_SIGN
; 141	;	macros to fix a couple of conflicts.  Make FMPRI and FDVRI go to
; 142	;	same spot, as well as FDVx and FMPx, saving a couple of words.
; 143	;413	24 Sept 85--Continue the cleanup/speedup begun in 412.  Rewrite LSH
; 144	;	and ROT, spending some CRAM for speed.  Rework the DMOVxx group to
; 145	;	minimize the memory dead time for DMOVE and DMOVEM, particularly.
; 146	;	Prevent SETA from referencing memory (useless, but harmless and
; 147	;	correct).  Squeeze words out of POP and POPJ.  There are some old
; 148	;	tailings from the DMOVxx code left for use by floating point and
; 149	;	IO code; it would be nice to clean them up.
; 150	;412	12 Sept 85--Freeze work on strings and start doing cleanup and
; 151	;	speedup on other (simpler) instructions.  Move around DRAM and
; 152	;	first words of several instructions (e.g., BLT, EXTEND, XCT,
; 153	;	and several EXTEND sub ops) so that instructions don't jump around
; 154	;	from place to place so much.  Decommit obsolete conditionals WRTST,
; 155	;	XADDR, EPT540, LONG.PC, MODEL.B, KLPAGE, SMP, SHIFT.MUUO, SXCT,
; 156	;	PUSHM, EXTEND, DIAG.INST, and DBL.INT--we haven't supported one
; 157	;	side or the other of these for a long time, in some cases.
; 158	;	Rework all noops and pure skips to use the same code as TRN and
; 159	;	TRNA.  Make TRO, TDO, TRC, TDC, TRZ, and TDZ use the equivalent
; 160	;	boolean code.  Make SETAM and SETAB equivalent to MOVEM.  Save a
; 161	;	cycle in MOVN by having it exit directly without going through MOVE.
; 162	;	All of this involves moving some code around in a few cases.  Clean
; 163	;	up the listings a bit.
; 164	;411	24 July 85--Another try at the SMP fix.  PI cycle 7 must go to
; 165	;	memory for interlock to work, so delete use of the cache on the
; 166	;	PHYS REF.  This may have performance drawbacks for TOPS-20 and
; 167	;	TOPS-10 uniprocessor, so there may have to be two versions of
; 168	;	microcode (again!) to resolve this.
; 169	;410	11 July 85--Force PI functions 3 and 7 to use RPW cycles, so
; 170	;	SMP will work properly.  Save a couple words in the process.
; 171	;407	18 June 85--Change macro ARX_2 to ARX_2+MQ0 and fix related bug
; 172	;	in ADJBP by clearing MQ on entry to instruction.  This prevents
; 173	;	ADJBP from computing the wrong byte capacities for OWGs with
; 174	;	byte sizes of 6 and 18.  Also reverse AC1 and AC2 in DB2WD.
; 175	;	That was causing CVTDBx to reverse the byte pointer halves if
; 176	;	an OWG was used, ruining things entirely.
; 177	;406	11 Mar 85--Define R17 as HARDPFW, and save the hard page fail word
; 178	;	there for TOPS-10, thus protecting it from getting clobbered by a
; 179	;	later soft page fail.
; 180	;405	15 Jan 84--Finish initial installation of rewritten MOVSLJ and; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-3
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 181	;	MOVSO code.  Also, remove obsolete conditional code for SXCT and
; 182	;	SHIFT.MUUO, and for the .IFNOT cases of LONG.PC, MODEL.B, XADDR,
; 183	;	DBL.INT, EXTEND, and EPT540.  Remove some code from IBPS (not
; 184	;	needed by DTE support), and an obsolete constraint above PGRST1.
; 185	;	Also put all statistics code into separate assembly.  As currently
; 186	;	implemented, the statistics code won't fit into CRAM without removing
; 187	;	something else.
; 188	;404	12 Oct 84--Fix THAW so that source VMA doesn't get complemented
; 189	;	for OWGs.
; 190	;403	9 Oct 84--Special case the null destination length so that the
; 191	;	byte pointers don't get clobbered when nothing gets done.
; 192	;402	5 Oct 84--Fix byte counting in OWG decode subroutine so that
; 193	;	references to the zeroth byte don't force the pointer forward
; 194	;	one word (shifting P can't easily sign extend here).  Do this by
; 195	;	forcing PLOOP to loop one extra time.
; 196	;401	5 Oct 84--Add code to support OWGs for version 2 MOVSLJ.
; 197	;400	9 Aug 84--Initial first edit number for releasable version 2.0.
; 198	;377	9 Aug 84--All of these are reserved (somewhat paranoically, I think)
; 199	; .	for version 1 as well.  The likelihood of them actually being used
; 200	;360	is vanishingly small!
; 201	;357	9 Aug 84--Add the 136 location constraint (forgotten in 356).
; 202	;356	8 Aug 84--Make the # field of location 136 contain the major and
; 203	;	minor version numbers.  Grab a random instruction with no # field
; 204	;	in use to do this.
; 205	;355	29 May 84--Make BPEA hang on to the original Y field when doing
; 206	;	the EA calculation for an OWL which does not overflow to the next
; 207	;	word.  Untangle copy length calculation, which was confusing
; 208	;	source and destination lengths.  Fix generation of fill count in
; 209	;	loop (a decimal number used where octal was needed).  Make sure
; 210	;	that the fill character gets stored the first time through the fill
; 211	;	loop.  Build the final source count from the proper numbers (not
; 212	;	from a byte pointer!).  Fix the test for storing the final buffer
; 213	;	if no filling was required.  Force the final destination byte pointer
; 214	;	Y field to the proper value by making IDSTMA be the actual first
; 215	;	destination VMA, saving two words in the process.
; 216	;354	25 May 84--Implement recoded version of MOVSLJ.  Temporarily
; 217	;	decommit the G floating instructions in order to make room for it.
; 218	;353	21 May 84--LDB and DPB in version 1 were leaving state register bit
; 219	;	3 set when the byte word was loaded, resulting in the page fault
; 220	;	handler treating it as if it were a string instruction and trying
; 221	;	to back up a byte pointer in AC1 when the reference page faulted.
; 222	;	Cure it by reseting the state register in GBYTE.  (Sure hope this
; 223	;	is the last bug in version 1!)
; 224	;352	4 Apr 84--It turns out that the string instructions had the same
; 225	;	problem as the byte instructions in 351!  Copy AR to ARX one
; 226	;	cycle earlier in both GRSC2 and IDST to fix it.  Also make sure
; 227	;	that all byte pointers default to PC section by initializing VMA
; 228	;	to PC on all calls to both of these routines.  This cleans up edit
; 229	;	300.
; 230	;351	12 Mar 84--When ILDB or IDPB incremented a one word local pointer
; 231	;	in such a way that the low half word changed sign, the section
; 232	;	computation for the byte address would get screwed up if the
; 233	;	index AC had a global address.  Fix this by copying the updated
; 234	;	pointer into ARX, thus forcing EA MOD DISP to look at the proper
; 235	;	bit in ARX18.
; 236	;350	15 Feb 84--Fix indexed indirection byte pointer effective address; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-4
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 237	;	calculations to load the indirect word into both AR and ARX.
; 238	;347	20 Jan 84--Rewrite the MVST and CMPS dispatches to test for illegal
; 239	;	bits in the lengths before BRX gets smashed.  UUO was reporting a
; 240	;	bogus op code in these situations.
; 241	;	Turn on BIG.PT by default, since it should work with both old and
; 242	;	new software and hardware.
; 243	;346	18 Jan 84--Fix the .IFNOT variation of BIG.PT to clear the Keep
; 244	;	bit if anybody sets it.  This was introduced in 343.
; 245	;	Add the DDT.BUG conditional.  Under it, rewrite APRID to move
; 246	;	bit 23 to bit 5 if it is set in the serial number.  This is a
; 247	;	piece of garbage which I hope can disappear soon (it seems EDDT
; 248	;	used the serial number to test for a KS-10!).
; 249	;	Fix the time field on the page map word type dispatch (the assembler
; 250	;	default was too high).  Also make the PAGCNT conditional hang on
; 251	;	to the original AR value after it counts the PFH entry (this would
; 252	;	only matter for an AR parity error).  Rename AR and ARX defaults to
; 253	;	MEM for the AR_MEM and ARX_MEM macros, respectively.
; 254	;345	6 Dec 83--Clean up all the pieces and integrate the new byte
; 255	;	instruction implementation into the rest of the microcode.  Also
; 256	;	add the FIN LOAD macro (more mnemonic than FIN XFER, its equivalent)
; 257	;	and include AR/AR on AR_MEM and ARX/ARX on ARX_MEM, as those macros
; 258	;	really don't work unless those fields take their default.  This
; 259	;	version marks the first time that major chunks of code have been
; 260	;	bodily replaced; accordingly, with this edit the major version has
; 261	;	been bumped to 2.
; 262	;344	1 Dec 83--Save CVTBDx fill character address, which was getting lost
; 263	;	if OWGBPs were in use, in a manner similar to that used in CMPSx
; 264	;	(see edit 310).  Also, fix some conditionals for EXPMSK constant
; 265	;	generation, so that OWGBPs will assemble with EXTEXP off.
; 266	;343	18 Nov 83--Install new code for IBP and ADJBP, saving time and
; 267	;	microwords.
; 268	;342	8 Nov 83--Change definition of CLR PT LINE to be consistent with
; 269	;	new paging board (see also 333).  Also, redefine bit 3 of effective
; 270	;	word to reverse keep sense (so unkept only pages are cleared when
; 271	;	bit 3 is set).
; 272	;341	28 Sep 83--Force the ARX to contain the first byte pointer word on
; 273	;	exit from INCRBP so that subsequent EA MOD DISP wil work.  Force
; 274	;	OWGs to explicitly wait for store to complete after increment
; 275	;	(unfortunately there is no implicit MB WAIT in MEM/EA CALC.  Sorry!).
; 276	;340	The OWG/OWL test for the byte instructions had the sense of the
; 277	;	test backwards in several places.  Rework LDBDSP, INCRBP, and
; 278	;	DPBDSP.  This makes it impossible for DPEA to test for a byte
; 279	;	off the top of a word on its return, so it has been desubroutinized.
; 280	;337	15 Sep 83--Start work on a complete rewrite of all byte and character
; 281	;	instructions.  Begin by installing initial versions of LDB, ILDB,
; 282	;	DPB, and IDPB.  All of these are designed to make one word global
; 283	;	byte pointers run faster by loading their shift counts from CRAM
; 284	;	dispatch tables.  Also, reduce time for DISP/SH0-3 to three ticks.
; 285	;	Move CDBLST into FP to allow EXTEXP conditional to be turned off.
; 286	;	Also, shuffle conditional placement to prevent EXTEXP shutoff from
; 287	;	turning off XBLT as well.
; 288	;336	9 Aug 83--Back off 330 for a bit, since TOPS-10 7.02 must be tested
; 289	;	and OWGs in section 0 fail for string instructions (they get converted
; 290	;	to TWGs, which are illegal in section 0).  For now, we will maintain
; 291	;	both sources.
; 292	;335	Force memory to be released for SMP case of DPB if P > 36 causes no; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-5
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 293	;	actual data to be stored.  Make an OWG reference to an address >
; 294	;	37,,777777 cause a page fail (GBYTE was stripping the excess bits).
; 295	;334	Fix conflict generated in CLRPT by 333 by creating new subroutine
; 296	;	ARSWAP which is just AR_AR SWAP.  Make several other routines call it,
; 297	;	thus saving a few words.
; 298	;333	Add new conditional BIG.PT.  Under it, add code to implement the "Keep
; 299	;	me" bit for paging as bit 5 of the page table, and to move it to page
; 300	;	map bit 23 during page refill.  Also make DATAO PAG not clear Kept
; 301	;	pages if bit 3 of the word is off.
; 302	;332	Redefine all bank 7 ACs as R0,...,R17, and all bank 6 ACs as P0,...,
; 303	;	P17.  Change all other alias definitions to refer to these.  This
; 304	;	gives us a uniform cross reference for all scratch register references.
; 305	;	Put all macro definitions into alphabetical order, making it easier
; 306	;	to look up a macro definition.  Split the edit history into its own
; 307	;	file.  There are no functional changes from 331.
; 308	;331	Allow XSFM anywhere.  Clean up the code a bit in the process.  There
; 309	;	still remain a number of references to XSFM or XPCW distinctions,
; 310	;	and these could almost certainly be cleaned up further.
; 311	;330	Allow one word global byte pointers in section zero.  This includes
; 312	;	changes in BYTE, EIS, and FP.  Change GBYTE and CNV2WD to return 2;
; 313	;	eliminate GTST as obsolete.  Also shuffle the calls to these routines
; 314	;	to conform to the new calling conventions, and put the OWG test at
; 315	;	the beginning of IBP, ILDB, IDBP, LDB, DPB, and ADJBP.
; 316	;327	Add PAGCNT conditional.  Under it, include control to count entry
; 317	;	into PFH code and DATAO PAG with bit 2 set.
; 318	;326	Change VMA restoration in INC2WD and CNV2WD (see edits 320 and 307)
; 319	;	to use RSTR VMA_MQ in order to keep the global/local sense of the
; 320	;	reference.  This was causing ILDBs of OWGs in shadow memory to
; 321	;	save the incremented byte pointer in the ACs instead of memory.
; 322	;325	Add VMA/LOAD to local indexed EA computation for EXTEND E1 to make
; 323	;	it read the section number from VMA instead of PCS (!) if the index
; 324	;	is section local.
; 325	;324	Force the XADDR conditional to use RPW type references for DPB and
; 326	;	IDPB if the SMP conditional is on, even if one word globals are not
; 327	;	active.
; 328	;323	Add missing constraint near NOT.WR, accidentally broken by 322.
; 329	;322	Generate the A(cessible) bit in a page fail word caused by a read
; 330	;	violation if the page is otherwise accessible and if no CST is present.
; 331	;	This could be fixed for the CST present case as well, but has been
; 332	;	deferred since we are tight on space and no one seems to need it
; 333	;	anyway.
; 334	;321	Prevent statistics microcode from losing traps by forcing NICOND
; 335	;	dispatch 11 to ignore the statistics and take the trap.
; 336	;320	Restore the VMA again in INC2WD (broken by 307), since the state
; 337	;	register bits may have changed in the interim.  This was causing
; 338	;	PXCT to do surprising things (mostly bad).
; 339	;317	Originally, this was an attempt to uncount multiply counted op
; 340	;	codes which resulted from interrupts during long instructions.
; 341	;	That project has been shelved for now.  Instead, the second
; 342	;	NICOND dispatch during op code counting has had its final constraint
; 343	;	fixed.
; 344	;316	Make counting only version compatible with time and counting by making
; 345	;	counting only version use TRX2 and TRX3, removing physical contiguity
; 346	;	requirement.
; 347	;315	Op code counting lives again!  The setup code activated by DATAO PI
; 348	;	was attempting to write the TRX registers with data fresh from memory,; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-6
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 349	;	resulting in parity checks when it was used (see edit 73, for example).
; 350	;	Juggle code to overlap next address calculation with parity wait.
; 351	;314	Add CST.WRITE conditional to facilitate assembly of microcode
; 352	;	without the CST writable bit (see edit 303).
; 353	;313	Put TIME/3T on XFERW, as the assembler was getting the wrong
; 354	;	value with both AR_MEM and ARX_MEM macros present.
; 355	;312	Fix definition of BYTE RPW to include a write test.  This was
; 356	;	causing the SMP version of DPB to hang when memory was readable
; 357	;	but not writable.
; 358	;311	Make all IOP function 7 style of references look in the cache.
; 359	;310	Improve the fix in 307 to save the computed E0+1 in FILL during
; 360	;	OWGBP conversion and to restore the VMA from there when done.
; 361	;	Also, make sure that the VMA is initialized to PC for all cases
; 362	;	when doing effective address calculations for two word globals
; 363	;	in string instructions.  307 was not enough to clean up the
; 364	;	CMPSx fill problem, since VMA HELD was never loaded.
; 365	;	Force EXT2WD to prereference AC4 and AC5 so that glitch discovered
; 366	;	for second edit 210 will not be activated.
; 367	;307	Restore VMA from MQ at end of CNV2WD (and remove it from INC2WD,
; 368	;	saving a word in the process).  This was causing CMPSx to load
; 369	;	a random fill word and MOVSLJ to store to a random place when the
; 370	;	source length was zero if one word globals were in use.
; 371	;	Force page fail code to look for ARX as well as AR parity errors
; 372	;	(now possible with BYTE RPW implemented).
; 373	;	Make sign extension of E1 go to right place in EXTEND decoding of
; 374	;	offset instructions (broken in 301).
; 375	;306	Add University of Essex code to statistics (TRACKS) code to make
; 376	;	it work with address break enabled.
; 377	;305	Fix CST write bit logic to not test bit 18 when reading.
; 378	;304	Switch byte read interlock from LDB to DPB (broken in 303).
; 379	;303	Implement bit 18 of a CST entry as a write enable bit in addition
; 380	;	to all the other write enable functions.
; 381	;	Knock one cycle out of byte deposit where the byte is being
; 382	;	deposited into the high order byte of a word.
; 383	;	Implement the SMP conditional for extended addressing by
; 384	;	replicating all the byte effective address calculation code for
; 385	;	DPB.  This is unfortunate, but necessary due to the huge dispatch
; 386	;	table that ends this subroutine.
; 387	;302	Move XFERW out of EIS (which no longer absolutely requires it
; 388	;	in line) into SKPJMP (more in the heart of things).  Also
; 389	;	juggle comment lines and code layout to reduce the listing
; 390	;	size a bit and to force some of the .TOC lines into the table
; 391	;	of contents (even though the code nearby may be suppressed).
; 392	;301	Fix ADJBP so that instructions which occur at the last word on
; 393	;	a page do not cause a page failure of some random type (one cycle
; 394	;	too many between I FETCH and NICOND).
; 395	;	Fix effective address calculation for EXTEND so that only offset
; 396	;	instructions (and not GSNGL, for example) will have E1 sign
; 397	;	smeared.
; 398	;	Implement XJRST.  Also force JSP and JSR to do full 30 bit
; 399	;	effective address calculations.
; 400	;300	ADD LOAD OF VMA FROM PC IN PUTDST TO GET THE SECTION ADDRESS
; 401	;	CORRECT ON THE STRING INSTRUCTIONS.
; 402	;277	Add EA CALC table for SMP configurations of extended addressing
; 403	;	for TOPS-10.  (TOPS-20 paging)
; 404	;276	Force global EA CALC for EXTEND instructions in PUTDST.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-7
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 405	;275	FIX THE ERROR CODE IN STRING COMPARE FOR ILLEGAL BITS IN THE
; 406	;	LENGTH FIELD. WAS CAUSING AR PARITY ERRORS.
; 407	;274	SAVE THE API FUNCTION WORD ON AN IO PAGE FAIL INSTEAD OF THE
; 408	;	PAGE FAIL WORD. THIS TAKES PLACE IN BOTH THE AC BLK 7 AC 2
; 409	;	AND THE MONITOR.
; 410	;273	PUT CONDITIONALS AROUND 4 GFLOAT CONVERSION INSTRUCTIONS.
; 411	;	THEY WILL ACT AS MUUO'S AND MONITOR WILL TAKE CARE OF THEM.
; 412	;272	CONO APR 200000 AT TIMES WAS NOT GENERATING EBUS RESET OF A
; 413	;	SUFFICIENT LENGTH TO CLEAR DTE REGISTERS. ADDED ANOTHER
; 414	;	MICROWORD SO THAT CONO APR IS NOW UP FOR TWO FULL WORDS WHICH
; 415	;	GETS AROUND THE HARDWARE PROBLEM.
; 416	;271	ILLEGAL INDIRECT PAGE FAIL (24) WAS NOT ALLOWING USER TO BE SET.
; 417	;270	WHEN IN SECTIONS > 1, AN UPDATED OWGBP WOULD BE WRITTEN INTO
; 418	;	MEMORY INSTEAD OF THE AC'S.
; 419	;267	CHANGED TESTS FOR OWGBP TO TEST FOR PC SEC0 FIRST. SAVES 33 NS.
; 420	;266	CONDITIONALS ON FOR TOPS-20 DEVELOPMENT.
; 421	;265	REMOVED EDIT 244. SOFTWARE ENGINEERING WILL SUPPLY MONITOR
; 422	;	CODE TO TAKE CARE OF PROBLEM. CODE COSTS TOO MUCH TIME IN
; 423	;	THE INSTRUCTION EXECUTION.
; 424	;264	ADDED CONDITIONALS TO CODE FOR IPA20, OWGBP AND NO CST UPDATE IF
; 425	;	CBR IS ZERO. THIS IS FOR RELEASE 5 OF TOPS-20.
; 426	;263	IBP DID NOT CLEAR FPD ON EXIT.
; 427	;262	ALLOW XBLT TO BE VALID IN SECTION 0.
; 428	;261	FIX CODE AT END OF ADJBP CODE TO CLEAR STATE REG. IF ILDB
; 429	;	WITH 2 WD GLOBAL POINTER POINTING TO ADDRESS NOT IN CORE
; 430	;	CLEAN DISPATCHES TO WRONG CODE BECAUSE SR LEFT OVER FROM
; 431	;	ADJBP.
; 432	;260	FIX FM PARITY ERRORS AT MVF1: ADDED NULL CALL TO RET2:
; 433	;	AT MVST: TO TAKE CARE OF EXTRA TICK FOR PARITY.
; 434	;257	MAKE SURE THAT THE UPDATED ONE WORD GLOBAL BYTE POINTER IS
; 435	;	WRITTEN BACK INTO THE CORRECT CONTEXT.
; 436	;256	MAKE ANOTHER ATTEMPT TO FIX PXCT OF ONE WORD GLOBAL BYTE POINTERS.
; 437	;	THE GIBP CODE GETS THE SAME CHANGES AS EDIT 255.
; 438	;255	MAKE ONE WORD GLOBAL BYTE POINTERS WORK WITH PXCT. THE STATE
; 439	;	REGISTER BITS ON MCL4 (NOT TO BE CONFUSED WITH CON3), WERE NOT
; 440	;	BEING SET PROPERLY TO ALLOW PREVIOUS ENA AND USER ENA TO BE SET.
; 441	;	GUARANTEE THAT THESE SR BITS ARE SET PRIOR TO THE LOAD OF THE VMA.
; 442	;254	FIX PROBLEM WITH OWGBP WHERE FPD DOES NOT EFFECT
; 443	;	INC OF POINTER AFTER PAGE FAIL
; 444	;253	FIXED ADDRESSING FOR SH DISP AT GADJL0:
; 445	;252	MOVE STRING INSTRUCTIONS DO NOT GET THE CORRECT DATA ON
; 446	;	LOCAL POINTERS IN NON 0 SECTIONS
; 447	;251	ADD CODE FOR ONE WORD GLOBAL BYTE POINTERS.
; 448	;	TOOK OUT EDITS 243 AND 250 TO GET ENOUGH SPACE IN CRAM
; 449	;	FOR THIS EDIT. OWGBP WITH EXTEND INSTRUCTIONS WILL NOT
; 450	;	RETURN A OWGBP. THEY WILL RETURN A TWO WORD GLOBAL BP.
; 451	;250	ALLOW SMP SWITCH TO EFFECT TOPS-20 MODEL B TO DO RPW IN
; 452	;	BYTE INSTRUCTIONS.
; 453	;247	DO NOT DO A CST UPDATE OR AGE UPDATE IF THE CBR IS ZERO.
; 454	;246	EXTEND OP CODE DECODE FOR MODEL A WAS ACCEPTING MODEL B
; 455	;	OP CODES (20-31). ADDED CONDITIONALS TO CODE TO FIX.
; 456	;245	FIX 2 WORD GLOBAL BYTE POINTER BUG WITH IBP INSTRUCTION
; 457	;	WITH EXTENDED ADDRESSING OUT OF SECTION 0
; 458	;244	FIX MOVST EXTEND INST. SO THAT ILLEGAL (> 36) S FIELD
; 459	;	DOES NOT CAUSE STOP CODE TO CRASH SYSTEM FOR TOPS-10 MODEL B.
; 460	;243	WRTIME TRIED TO DO MEM WRITE EVEN THOUGH THE INSTRUCTION; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-8
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 461	;	DOES NOT DO ANYTHING TO MEMORY. CAUSED PROBLEMS IF THE MEMORY
; 462	;	LOCATION WAS NOT WRITABLE.
; 463	;242	FIX CODE FROM EDIT 234 TO GET PF CODE OF 24.
; 464	;241	FIX DFAD AND DFMP FOR ROUNDING OCCURS PROPERLY. ADDED STICKY
; 465	;	BIT FOR LEAST SIGNIFICANT BITS OF THE RESULT.
; 466	;240	FIX GFLT INSTRUCTIONS GFIX AND DGFIX SO THEY WILL TRUNCATE NEGATIVE
; 467	;	NUMBERS IN THE CORRECT DIRECTION. THE MQ MUST BE ZERO BEFORE
; 468	;	THE ARX_2 MACRO IS INVOKED OR THE ARX MIGHT GET A 3 FROM MQ00.
; 469	;237	ADD OPTION BIT FOR PV CPU IN THE APRID WORD AS IT IS DOCUMENTED
; 470	;	IN ALL OF THE HARDWARE DOCUMENTATION. SET THE BIT ACCORDING
; 471	;	TO THE MODEL.B OPTION SWITCH. IT WILL BE MAGIC NUMBER BIT 3.
; 472	;236	ALLOW THE INTEGER DIVIDE OF THE LARGEST NEGATIVE NUMBER BY
; 473	;	PLUS ONE TO SUCCEED. THIS USED TO BE A DOCUMENTED RESTRICTION
; 474	;	THAT THIS OPERATION WOULD CAUSE AN OVERFLOW AND NO DIVIDE.
; 475	;235	FIX JRA SO IT DOESN'T FALL INTO SECTION ZERO FROM A NON-ZERO
; 476	;	SECTION EVERY TIME BY WRITING THE PC SECTION INTO THE VMAX.
; 477	;234	BUILD A PAGE FAIL CODE OF 24 WHEN AN ILLEGAL INDIRECT WORD
; 478	;	IS FOUND DURING THE EFFECTIVE ADDRESS CALCULATION IN 
; 479	;	A NON-ZERO SECTION. THE PAGE FAIL CODE WAS PREVIOUSLY NOT
; 480	;	BEING REPORTED.
; 481	;233	SAVE THE IOP FUNCTION WORD THAT APPEARS ON THE EBUS WHEN AN
; 482	;	EXTERNAL DEVICE INTERRUPTS THE CPU. SAVE THIS INFORMATION
; 483	;	ON EVERY INTERRUPT IN AC BLOCK 7, AC 3. THE CONTENTS
; 484	;	OF THIS AC WILL BE PRESERVED UNTIL THE NEXT INTERRUPT.
; 485	;	OPERATING SYSTEMS SHOULD SAVE THIS INFORMATION AS SOON AS POSSIBLE
; 486	;	IF ITS CONTENTS ARE TO BE RELIABLE AND MEANINGFUL.
; 487	;232	ADDS 13 NEW INSRUCTIONS FOR SUPPORTING FORTRAN78 ON MODEL
; 488	;	B MACHINES. THESE INSTRUCTIONS ARE:
; 489	;	       OPCODE     SYMBOL
; 490	;	       ======     ======
; 491	;		102	GFAD AC,E
; 492	;		103	GFSB AC,E
; 493	;		106	GFMP AC,E
; 494	;		107	GFDV AC,E
; 495	;		EXTEND INSTRUCTIONS    EXTEND OPCODE
; 496	;		====== ============    ====== ======
; 497	;		EXTEND AC,[GSNGL  0,E]	    21
; 498	;		EXTEND AC,[GDBLE  0,E]	    22
; 499	;		EXTEND AC,[DGFIX  0,E]	    23
; 500	;		EXTEND AC,[GFIX   0,E]	    24
; 501	;		EXTEND AC,[DGFIXR 0,E]	    25
; 502	;		EXTEND AC,[GFIXR  0,E]	    26
; 503	;		EXTEND AC,[DGFLTR 0,E]	    27
; 504	;		EXTEND AC,[GFLTR  0,E]	    30
; 505	;		EXTEND AC,[GFSC   0,E]	    31
; 506	;231	FIX IN PROBLEM IN EDIT 215 TO XDPB THAT PREVENTED THE KL
; 507	;	FROM HANDLING INTERRUPTS WHILE EVALUTAING AN INDEXED INDIRECT CHAIN.
; 508	;	AN "=0" WAS MISSING BY BYTEIP.
; 509	;230	TO PRESERVE COMPATABILITY WITH THE KS10 AND BECAUSE OF SPACE
; 510	;	LIMITATIONS IN TOPS20 MODEL A, THE SPECIFICATION FOR THE
; 511	;	CVTDBX INSTRUCTIONS HAVE BEEN CHANGED TO ELIMINATE THE NEED
; 512	;	FOR AN OVERFLOW TEST DURING THE CONVERSION. THIS CHANGE
; 513	;	EFFECTIVELY REMOVES EDIT 221.
; 514	;227	DELETE EDIT 222 AND RETURN THE CVTBDX INSTRUCTIONS TO THEIR
; 515	;	OLD, BROKEN FUNCTIONALITY SINCE ANY ATTEMPT TO PREVENT THE
; 516	;	FLAGS FROM BEING CHANGED PREMATURELY HAS TO CONTEND WITH; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-9
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 517	;	INTERRUPTABILITY PROBLEMS. THE HARDWARE REFERENCE MANUAL
; 518	;	HAS A FOOTNOTE ABOUT THE FLAG PROBLEM SO THE CURRENT FUNCTIONALITY
; 519	;	IS DOCUMENTED FOR USERS.
; 520	;226	PREVENT AR PARITY ERRORS WHEN INCREMENTING BYTE POINTERS IN THE ACS.
; 521	;225	THE CODE TO SUPPORT THE MX20 VIA THE SBUS DIAG LOOP MECHANISM
; 522	;	DOES NOT TIME OUT CORRECTLY BECAUSE THE LOOP COUNTER IS BEING
; 523	;	REINITIALIZED EVERY TIME THROUGH THE LOOP. FIX THIS PROBLEM
; 524	;	EVEN THOUGH THE CODE IS NOT ASSEMBLED IN CURRENT RELEASES.
; 525	;224	FIX BUG IN EDIT 223 THAT CAUSED THE WRONG PAGE FAIL
; 526	;	WORD TO BE WRITTEN WHEN AN I/O PAGE FAIL OCCURS.
; 527	;223	WHEN A MEMORY PARITY ERROR OCCURRS AT PI LEVEL, AS EVIDENCED
; 528	;	BY AN AR DATA PARITY ERROR, THE DTE MAY BE WAITING FOR A
; 529	;	RESPONSE. IF IT IS, A DEX FAILURE WILL OCCUR UNLESS WE CAUSE
; 530	;	DEMAND TO WIGGLE.  WE CAN DO THIS BY FORCING THE DATA  IN THE
; 531	;	AR OVER THE EBUS.
; 532	;222	CVTBDX IS NOT SUPPOSED TO CHANGE THE CONTENTS OF THE ACS
; 533	;	OR MEMORY IF THE CONVERTED NUMBER WILL NOT FIT INTO THE
; 534	;	DESTINATION FIELD. IT WAS, HOWEVER, CHANGING THE FLAGS
; 535	;	BEFORE IT KNEW IF THE NUMBER WOULD FIT.
; 536	;221	THE CVTDBX WERE FAILING TO SET OV AND TRAP1 WHEN THE
; 537	;	CONVERTED DECIMAL NUMBER WOULD NOT FIT INTO A
; 538	;	DOUBLE WORD.
; 539	;220	THE TRANSLATE INSTRUCTIONS WERE USING A 15 BIT WIDE
; 540	;	FIELD FOR THE REPLACEMENT BYTE IN THE TRANSLATE TABLE
; 541	;	WHILE THE SPECIFICATION STATED THAT THE TRANSLATE
; 542	;	INSTRUCTIONS WOULD USE ONLY 12 BITS.
; 543	;217	PREVENT CRAM PARITY ERRORS CAUSED BY DISPATCHING TO LOCATION
; 544	;	3042 WHEN INDEXING IS SPECIFIED IN THE EFFECTIVE ADDDRESS
; 545	;	CALCULATION OF E1 WHEN THE EXTEDED OPCODE IS ZERO (ILLEGAL).
; 546	;	THE FIX IS TO PUT A JUMP TO UUO AT 3042.
; 547	;216	CHANGE THE DEFAULT VALUE FOR THE SMP SWITCH TO BE ONE. THIS
; 548	;	CAUSES THE MICROCODE TO INCLUDE SMP SUPPORT BY DEFAULT.
; 549	;215	CHANGES DPB INSTRUCTION TO R-P-W CYCLE ON DATA FETCH PORTION OF
; 550	;	INSTRUCTION TO SOLVE AN INTERACTION PROBLEM IN AN SMP OPERATING
; 551	;	SYSTEM.  THIS CHANGE ONLY APPLIES TO MICROCODES FOR TOPS-10 
; 552	;	AND TOPS-20, MODEL A.
; 553	;214	ADDED CHANGES FOR XADR, RELEASE 4 AS FOLLOWS.
; 554	;	STORE PREVIOUS CONTEXT SECTION (PCS) IN FLAGS WORD (BITS 31-35)
; 555	;	IF EXEC MODE AND XSFM OR XPCW INSTRUCTION,MUUO OR PAGE FAIL.
; 556	;	RESTORE PCS FROM FLAGS WORDS (BITS 31-35) WHEN XJRSTF OR XJEN
; 557	;	IS EXECUTED IN EXEC MODE AND THE NEW PC IS ALSO IN EXEC MODE.
; 558	;213	SET/FPLONG=0 PARAMETER ADDED TO TOPS-10 MICROCODE FOR KL MODEL 
; 559	;	A AND MODEL B.
; 560	;212	CHANGE THE CODE AT LDIND: TO TEST FOR USER MODE IF USER MODE
; 561	;	TURN OFF SPECIAL CYCLE THAT MAY STILL BE ON. THE MICROCODE WILL DEPEND
; 562	;	ON KERNAL PROGRAMS TO NOT GET IN PAGE POINTER
; 563	;	LOOPS. INSTRUCTIONS EXECUTED FROM THE CONSOLE WILL NOT WORK.
; 564	;	PI INSTRUCTIONS GET A RESTRICTION TO NOT GET INDIRECT PAGE POINTERS
; 565	;	IN THEIR PAGING CHAIN AS DO EXAMINES AND DEPOSITS AND BYTE TRANSFERS.
; 566	;211	CHANGE THE TEST FOR INDIRECT POINTERS TO NOT HAPPEN ON SECTION
; 567	;	POINTERS AND JUST ON INDIRECT PAGE POINTERS. AT LDIND:+1 AND LDIMM:+2
; 568	;210	MAKE ALL AC+# MICROINSTRUCTIONS HAVE THE # FIELD THE SAME IN THE
; 569	;	PREVIOUS MICROINSTRUCTION TO SOLVE A TIMONG GLITCH IN THE HARDWARE.
; 570	;	MAKE EXCHANG MARK AND DESTINATION POINTERS UUO IF THEY DO NOT
; 571	;	HAVE BYTE POINTERS OF EQUAL LENGTH. CHANGES PERVASIVE IN EIS ALSO IN PF
; 572	;	RECOVERY IN IO.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-10
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 573	;	MAKE THE LOAD OF AN INDIRECT POINTER CLEAR PI CYCLE IF SET.
; 574	;	THIS MEANS THAT THE MONITOR CANNOT USE KERNAL CYCLE, INSTR ABORT
; 575	;	INH PC+1 OR HALT IN A PI CYCLE IF AN INDIRECT POINTER CAN
; 576	;	BE A PART OF THE REFILL. ALSO NOTE THE POSSIBILITY OF GETTING AN
; 577	;	INTERUPT BEFOR THE PI INSTRUCTION COMPLETES. (NEVER CONTINUES PI
; 578	;	INSTRUCTION) CHANGES AT LDIND.
; 579	;207	CHANGE SBUS DIAG CODE FOR MOS PUT IT IN MOS CONDITIONAL /MOS=1
; 580	;	IF ON SBUS DIAG TRIES AT LEAST 8 TIMES TO GET A RESPONSE
; 581	;	OTHER THAN -1 IF IT GOT -1 ALL THOSE TIMES THE MICROCODE
; 582	;	GIVES UP AND RETURNS 0
; 583	;206	FINAL FIXES TO PUSHM AND POPM
; 584	;205	FIX BUG IN INDEX CALCULATION OF E1 FOR EXTENDED ADDRESSING.
; 585	;	INDEXING REQUIRED THAT AN AREAD BE PERFORMED IN ORDER TO LOAD
; 586	;	THE AR WITH A CORRECT FINAL RESULT. THE EFFECTIVE ADDRESS CALCULATION
; 587	;	AROUND EXTLA: GOT A NEW MACRO ADDED FOR INDEXING THAT DOES THE AREAD.
; 588	;	ABSOLUTE LOCATIONS IN THE RANGE 3040 GET USED AS TARGETS FOR THIS
; 589	;	AREAD THEN THE CODE REJOINS THE OLD CODE AT EXT2: 
; 590	;	THE AREAD WAS NECESSARY FOR THE HARDWARE MAGIC TO LOAD PARTS OF THE
; 591	;	AR DEPENDING ON THE INDEX REGISTER AND OTHER EXTENDED ADRESSING
; 592	;	PARAMETERS.
; 593	;204	ADD AUTOMATIC VERSION NUMBER
; 594	;	ADD CODE TO DO SBUS DIAG TESTING REQUIRED BY MOS
; 595	;203	PUT THE BLKO PAG, CHANGE IN 201 IN A KLPAGING CONDITIONAL
; 596	;	KIPAGING GETS TANGLED IN AR PARITY ERRORS AND IN GENERAL DOES
; 597	;	THE WRONG THINGS
; 598	;202	TURN OFF IMULI OPTIMIZATION IT GETS THE SIGN BIT AND THE OVERFLOW
; 599	;	FOULED UP (TURNED OFF FOR MODEL B ONLY WAS OFF IN MODEL A)
; 600	;201	CHANGE BLKO PAG, TO INVALIDATE ONLY ONE ENTRY BY CLEARING IT
; 601	;	CHANGES AT PAGBO PAGBO+1 AND CLRPT+3 CLRPT+3 GETS SETUP THAT USED
; 602	;	TO BE AT PAGBO+1, PAGBO+1 NOW CLEARS ENTRY AND QUITS
; 603	;	KLPAGE ERROR CHECK FOR TOPS 10 MODEL A TO CAUSE ERROR
; 604	;	IF SWITCH SETTINGS ARE IN CONFLICT DIDDLED
; 605	;200	CHANGE ALL EXEC REF TRACKS FEATURES BACK TO PHYS REF
; 606	;	ON SUSPICION THAT PAGE FAULTS ARE NOT HANDLED PROPERLY
; 607	;	MAKE NON TRACKS INSTR STAT FEATURES GET FOUR PHYSICAL
; 608	;	PAGE NUMBERS FROM FIRST FOUR LOCATIONS IN THE PAGE PRESENTED
; 609	;	IN THE DATAO PI, THE CODE ALSO USES THAT PAGE FIRST
; 610	;	LOCATION TO PUT THE INITIAL JUNK INTO ON STARTUP
; 611	;177	FIX SOME BUGS IN OPCODE TIMING CODE AT OPTM0: AND BEYOND
; 612	;176	ADD TO THE TIME COUNTING CODE CODE THAT COUNTS FREQUENCY
; 613	;	OF EACH OPCODE IN PAGE+2 AND PAGE+3
; 614	;175	FIX TIME COUNTING CODE TO ACOUNT FOR EACH OPCODE IN THE
; 615	;	USER OR EXEC MODE IT WAS SEEN IN, EDGE COUNTS WERE DONE IN
; 616	;	WRONG MODE CHANGES UNDER OP.TIME CONDITONALS (PERVASIVE)
; 617	;174	CHANGE TRACKS AND TIME COUNTING TO USE EXEC VIRTUAL SPACE
; 618	;	INSTEAD OF PHYSICAL SPACE
; 619	;173	SEPERATE OUT THE DISMISS AT 626: BECAUSE OF SUSPECTED BUG
; 620	;172	THE FACT THAT XJEN DISMISSES BEFORE READING NEW PC WORDS CAUSES
; 621	;	A PROBLEM FOR TOPS 20. REHASH THE CODE AT 600: TO 637: TO MAKE
; 622	;	XJEN READ THE TWO WORDS FIRST AND THEN DISMISS.
; 623	;171	CAUSE IO PAGE FAIL FIX IN 170 TO SHIFT AT END GETTING CORRECT
; 624	;	PAGE FAIL WORD CHANGE AT IOPGF:
; 625	;170	MAKE CLRFPD: GO DIRECT TO FINI: INSTEAD OF THROUGH NOP: THIS WAS
; 626	;	COSTING 2 TICS IN BYTE INSTRUCTIONS
; 627	;	CHANGE IO PAGE FAIL TO SAVE A VIRTUAL ADDRESS IN THE AC BLOCK 7
; 628	;	LOCATION 2 INSTEAD OF THE DATA THAT WAS ON THE EBUS CHANGES AT; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-11
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 629	;	PGF4:+1 AND IOPGF:
; 630	;167	CHANGE DEFAULT ON ADB MIXER SELECTS. NO DEFAULT NOW SUBFIELD U23
; 631	;	IS DEFAULTED TO 1 TO AVOID SELECTING FM AND NEEDING TO WAIT FOR PARITY.
; 632	;	THIS LEAVES THE OTHER BIT OF THE FIELD AVAILABLE FOR PARITY
; 633	;	EPT MOVED TO 540 USING SWITCH IN KLX,KLL (KLA,KLB NOW DEFUNCT)
; 634	;166	CHANGE FIELD DEFINITION FORMAT CHANGE THE WAY THE OPTIONS FIELD
; 635	;	GETS ITS VALUES ASSIGNED. EACH BIT GETS A FIELD DEFINITION.
; 636	;165	BUG IN 161 TO 164 WAS MISSING AC0 AT POP2: PARITY BIT WAS PUT THERE
; 637	;	IN THE NEWER MICROCODES
; 638	;	INSTALL MANY THINGS TO MAKE WORD STRING MOVES WORK START AT
; 639	;	MOVWD1 AND UNTILL BMVWD1 ALSO ASSORTED MACROS ARE ADDED
; 640	;	THESE ARE INSTALLED IN A SEPERATED EIS FILE (WDEIS) FOR THE MOST PART
; 641	;	THERE ARE SOME NEW MACROS AND THE CLEAN+17 LOCATION IS USED FOR
; 642	;	THIS CASE UNDER MODEL B CONDITIONAL INTERRUPTS DO NOT WORK YET
; 643	;	IN THIS CODE BUT ALL DATA TRANSFERS ARE CORRECT. INTERRUPTS ARE
; 644	;	TAKEN SO SUSPECT THE PROBLEM IS IN THE CLEANUP CODE.
; 645	;164	LEAVE IN ONLY MAP FIX
; 646	;163	TAKE OUT MAP FIX LEAVING XHLLI IN AND JRSTF IN
; 647	;162	PUT XHLLI BACK IN TAKE OUT JRSTF ONLY IN SEC 0 CODE
; 648	;161	XHLLI OUT TO DEBUG ADD RSTF0: TO MAKE TEST FOR JRSTF IN NON
; 649	;	0 SECTIONS TEST IN ALL CASES
; 650	;157	INSTALL XHLLI MAKE JRSTF UUO ON NON ZERO SECTIONS
; 651	;	ALSO MAKE MAP DOING A REFILL PAGE FAIL RIGHT THIS MEANS THAT AFTER
; 652	;	CLEAN IT CANNOT DO ANYTHING INTERESTING IF AN INTERRUPT IS PENDING
; 653	;	CHANGES AT MAP2:
; 654	;156	REINSERT A SKP INTRPT IN THE PAGE FAULT HANDLER TO HAVE INDIRECT
; 655	;	POINTER CHAINS INTERRUPTABLE. AT PGRF6:+6
; 656	;155	ABORTIVE MAP FIX FIX REMOVED PROBLEM MUST BE FIXED IN HARDWARE.
; 657	;154	ADD TESTS FOR AC'S IN PHYSICAL REFERENCES FOR EXAMINES AND DEPOSITS
; 658	;	PHYS REFS GO TO MEMORY, NOT AC'S AFTER PROBLEM SHEET 1675
; 659	;	CHANGES AT PILD+3 PIFET+2 PSTOR PHYS1 PHYS2 PHYS3
; 660	;	ADD CHANGES IN TRACKS TO MAKE MODEL A WORK AT TRK2+2 AND +3
; 661	;153	ADD SPECIAL CODE FOR PXCT OF BLT THIS HOPEFULLY CAN GO AWAY
; 662	;	WHEN THE EXTENDED ADDRESSING MONITOR DOES NOT USE PXCT ANYMORE
; 663	;	IT IS UNDER .IF/BLT.PXCT CONDITIONAL AND COSTS 12 WORDS
; 664	;152	CHANGE WHAT BLT DOES TO MATCH THE SPEC SR_BLT(XXX) IS CHANGED TO
; 665	;	NOT FORCE GLOBAL ADDRESSING THE LOAD VMA(EA)_ARX+BR AND
; 666	;	STORE VMA(EA)_ARX MACROS ARE ADDED TO FORCE THE GLOBAL/LOCAL PARAMETERS
; 667	;	TO BE THE SAME AS THOSE OF THE EFFECTIVE ADDRESS
; 668	;151	PUT THE EPT AND UPT AT 540 UNDER SWITCH CONTROL .IF/EPT540
; 669	;150	VERSION NUMBER BACKED UP TO PRESERVE SPACE IN VERSION NUMBER FIELD
; 670	;304	EXTEND 0 WOULD GET A JUMP TO AN UNUSED MICROLOCATION IN MODEL.B
; 671	;	ONLY THIS WAS BECAUSE LOCATION 2002: IN MODEL.A SHOULD BE AT 3002:
; 672	;	IN MODEL.B 3002: AND 3003: PUT IN WHERE 2002: AND 2003: ARE UNDER
; 673	;	CONDITIONALS.
; 674	;303	CHANGE THE NUMBER FIELD OF THE SR_BLT(XXX) MACROS TO GIVE THE
; 675	;	BIT 0 OFF ALL THE TIME. THIS GIVES BLT MORE THE FORM OF THE OTHER
; 676	;	EXTENDED ADDRESSING STUFF IN HOW IT REFERS TO THE SHADOW AC'S.
; 677	;	IT IS STILL BELIEVED TO BE BROKEN BUT IS BETTER THAN IT WAS.
; 678	;302	ADD LONGER ADDRESS CONSTRAINTS FOR THE NEW MICROASSEMBLER. EVERY
; 679	;	LOCATION THAT THE DISPATCH RAM CAN JUMP TO IS EFFECTED. THE
; 680	;	CONSTRAINTS THATUSED TO LOOK LIKE =00**** MUST NOW LOOK LIKE
; 681	;	=0****00**** THIS IS BECAUSE THE MODEL B MACHINE CAN AND DID
; 682	;	REALLY SET THAT BIT. THE CHANGE MAKES THE MICROCODE INCOMPATIBLE
; 683	;	WITH THE OLD ASSEMBLER.
; 684	;301	HALT IS CLEARING THE RUN FLOP WITH HARDWARE MUST CHECK FOR; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-12
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 685	;	KERNAL MODE BEFOR THE HALT MACRO SO USER IOT MODE WILL
; 686	;	NOT BE ABLE TO HALT. THIS TAKES ONE MICROWORD AT 1017:
; 687	;	THE SENSE OF THE SKIP IS REVERSED AGAIN SO 1016: IS BACK TO
; 688	;	BEING THE UUO AND CHALT: IS NOW A SEPERATE WORD AFTER 1017:.
; 689	;300	REPLACE HALT CODE AGAIN BUT THIS TIME GET THE SENSE OF THE
; 690	;	SKIP RIGHT BY SWAPPING THE CONTENTS OF LOCATIONS 1016: AND 1017:
; 691	;	PUT THE 1: ADDRESS CONSTRAINT ON CONT:.
; 692	;277	PUT HALT BACK THE WAY IT WAS SKP USER HAS THE INVERSE SKIP SENSE
; 693	;	AND HENCE DOES THE WRONG THING. HALT TO BE FIXED LATER.
; 694	;276	YET ANOTHER TRY AT THE BLKO PROBLEM BLK1: SHOULD HAVE HAD A
; 695	;	J/BLK2.
; 696	;275	THE LONG PC CHANGES HAD XSFM1: BEFOR THE ADDRESS CONSTRAINT THUS
; 697	;	GIVEING THE WRONG ADDRESS. THE =0 IS PUT BEFOR THE LABEL.
; 698	;274	FIX THE DIAG.INST CONDITIONALS TO BEHAVE PROPERLY WITH THE
; 699	;	CONSTRAINTS OF DRAM LOCATIONS MAP DIED BECAUSE IT NEVER WAS
; 700	;	REACHED OUT OF A DISPATCH.
; 701	;273	INSERT THE DIAG.INST FEATURE FOR THE DIAGNOSTICS PEOPLE.
; 702	;	CHANGES AT DCODE 104:, 106: AND AT XCT: SHOULD NOT EFFECT OTHER
; 703	;	ASSEMBLIES.
; 704	;272	THE FIX TO THE GARBAGE IN THE LEFT HALF OF VMA IN 265 FORGOT TO
; 705	;	LOAD THE VMA IN BLK3:+1 PUT THAT IN. ALSO ON JUD'S RECOMENDATION
; 706	;	PUT A COPY OF THE NOP MICROINSTRUCTION AFTER CLRFPD: TO MAKE
; 707	;	ENOUGH TIME IN THE SKIP CASE. IT SEEMED TO WORK WITHOUT THIS
; 708	;	AND IF SPACE GETS TIGHT IT SOULD BE REMOVED.
; 709	;271	FIX IN 267 PGF4:+4 DOES NOT WORK, CANNOT PUT VMA_# THERE. POSSIBLY BECAUSE
; 710	;	VMA_# CONFLICTS IN SOME ESOTERIC WAY WITH STORE? THAT CHANGE
; 711	;	IS TAKEN OUT AND AT PGF1 THE VMA IS GIVEN 500 OR 501. THIS IS SLIGHTLY
; 712	;	LESS DESIREABLE AND FURTHER EFFORT COULD BE SPENT IN THE UCODE TO
; 713	;	MAKE PAGE FAILS LESS UNWEILDY FOR THE SOFTWARE ROUTINE THAT CONVERTS
; 714	;	THEM TO MODEL B FORM.
; 715	;270	CHANGE HALT TO CHECK FOR USER MODE INSTEAD OF IO LEGAL. A JOB
; 716	;	IN USER IOT SHOULD NOT BE ABLE TO HALT THE MACHINE.
; 717	;267	ADD NEW CONDITIONAL SHIFT.MUUO TO PROVIDE THE SHIFTED DOWN MUUO
; 718	;	DATA BLOCKS MORE SIMILAR TO THE XADDR TYPES. CONDITIONAL IS USED
; 719	;	AT 1003: AND PGF4:+4 TO PROVIDE A DIFFERENT STARTING ADDRESS.
; 720	;266	FIX PILD+3 TO LOAD THE VMA AT THE SAME TIME THUS ENABLING
; 721	;	THE MODEL HACK FIX TO LOAD THE LONG VMA.
; 722	;265	HAIR UP THE ALREADY HAIRY BLKXX CODE TO CLOBBER THE LEFT HALF OF AR
; 723	;	BEFOR USING IT AS AN ADDRESS. CLOBBERED ARL AT BLK2 AND LOADED
; 724	;	VMA AT BLK3.
; 725	;264	ADD J/CLRFPD AT BFIN TO MAKE IT THE SAME AS IT WAS. BFIN GOT
; 726	;	MOVED TO A DIFFERENT PLACE IN THE LAST EDIT AND THIS J FIELD
; 727	;	WAS NOT FIXED.
; 728	;263	ADD THE MIT FIXES. IOTEND AND THE BLK1 TO BLK4 GROUP ARE CHANGED
; 729	;	EXTENSIVELY. CLRFPD IS PUT JUST BEFORE FINI CONSTRAINT ON IOFET
; 730	;	IS CHANGED.
; 731	;	ADD THE LONG PC FORMAT UNDER A NEW CONDITIONAL LONG.PC THE
; 732	;	CONDITIONAL IS TURNED ON BY XADDR. CONDITIONALS ARE ADDED TO THE
; 733	;	LONG PC CODE TO MAKE IT SMALLER WHEN ONLY SECTION 0 IS POSSIBLE.
; 734	;	ADD COMMENTS TO THE MICROCODE OPTIONS FIELD.
; 735	;	RESTORE SXCT CODE FROM VERSION 131. TO BE USED ONLY IN MODEL A
; 736	;	NON KLPAGING CODE.
; 737	;262	PUT WORD AT INDR1+1 UNDER SXCT CONDITIONAL SO WHEN SXCT IS OFF WE
; 738	;	GET AN ADDITIONAL SAVINGS OF ONE WORD.
; 739	;261	ADD PHYS REFS AT PGRF6+4 AND PIDISP+4 TO MAKE MODEL.A LOAD A LONG
; 740	;	VMA. PART OF THIS CODE IS NOT UNDER CONDITIONAL BECAUSE IT SHOULD NOT MATTER; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-13
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 741	;	TO A MODEL.B MACHINE. PIDISP+4 ALSO GETS THE LOAD OF THE SAME DATA
; 742	;	REPEATED SO THE PHYS REF HAS SOMETHING TO WORK ON.
; 743	;	FLUSH THE NOW USELESS CODE AT CHALT TO GENERATE THE LD AR.PHYS
; 744	;	CONSTANTS.
; 745	;	CURRENTLY THERE IS SORT OF A BUG IN THAT THE SBR AND THE CBR
; 746	;	CAN NOT BE ABOVE 256K IN A MODEL.A MACHINE. THIS DOES NOT BOTHER
; 747	;	THE CURRENT MONITORS AT ALL IN THAT THESE TABLES ARE IN VERY LOW CORE.
; 748	;	IF THAT CHANGES THE LOCATIONS SECIMM+3 SECIMM+7, LDIND, PGRF5, LDSHR
; 749	;	AND LDPT1+1 MUST ALL GET FIXED UP. THE GENERAL FIX IS TO GET A PHYS REF
; 750	;	IN THE MICROINSTRUCTION THAT LOADS THE VMA. THIS CAN BE DONE BY
; 751	;	POSTPONING THE LOAD OF THE VMA ONE MICROINSTRUCTION IN ALL OF THESE
; 752	;	PLACES, BUT, SINCE THAT CAUSES A PERFORMANCE DEGRADATION IT WAS NOT
; 753	;	DONE.
; 754	;260	DIVERGANT CHANGES TO MAKE KLPAGING PHYS REFS THE OLD WAY
; 755	;	CAUSE ALL CASES OF VMA_XXX+LD AR.PHYS TO GO BACK TO THE 
; 756	;	OLD PHYS REF WAY
; 757	;257	IN MODEL B MACHINES AT LDPT+1 THE VMA IS GETTING GARBAGE IN THE
; 758	;	LEFT HALF BECAUSE IT ADDED IN JUNK THAT WAS IN AR LEFT. FIX IS TO
; 759	;	CLEAR ARL AFTER LDPT AND TO DO THE SHUFFLE PERFORMED THERE ONE
; 760	;	MICROINSTRUCTION LATER.
; 761	;******	A HACK FIX IS USED HERE THAT TAKES TWO WORDS. THIS WAS DONE BECAUSE
; 762	;	OF EXTREEM TIME PRESSURE TO DEBUG >256K MODEL B. THERE OUGHT TO BE
; 763	;	A WAY TO REDUCE THIS FIX TO ONLY ONE WORD IN SPACE AND TIME, OR
; 764	;	EVEN LESS.
; 765	;256	EDIT JUMPED TO RANDOMNESS WITH AN EXTRA RETURN. THIS HAPPENED
; 766	;	BECAUSE THERE WAS NO CALL AT EDSFLT IN THE MODEL B NON XADDR CODE
; 767	;	ADDED CALL TO EDSFLT.
; 768	;255	SAVE EDIT FROM GETTING AN EXTRA STORE CYCLE AT EDSSIG BY SENDING
; 769	;	IT ALWAYS TO THE EDFLT1 LOCATION INSTEAD OF EDFLT THIS ONLY
; 770	;	CHANGES WHAT HAPPENS IN MODEL B NON XADDR BECAUSE IN MODEL A
; 771	;	EDFLT AND EDFLT1 ARE THE SAME LOCATION ANYWAY
; 772	;254	CAUSE THE A INDRCT CHANGE IN 253 TO BE ONLY FOR NON EXTENDED
; 773	;	ADDRESSING MACHINES. THIS THROWS DOUBT ON THE WORD SAVINGS
; 774	;	THAT MIGHT HAVE BEEN POSSIBLE
; 775	;253	CHANGE A INDRCT TO LOAD BOTH THE AR AND ARX, IN THE EXTENDED
; 776	;	INSTRUCTION SET THIS HAPPENED TO BE DEPENDED ON AT EXT2+2 AND
; 777	;	EXT2+3. THE DEFINITION OF A IND IN EA CALC/ WAS FIXED TO
; 778	;	LOAD THE AR AND THE ARX
; 779	;	I THINK THIS PERMITS THE SAVINGS OF AN EXTRA WORD AND SOME
; 780	;	TIME ON ALL INDIRECTS. CHECK OUT FLUSHING INDR1 AND MAKING INDRCT
; 781	;	DO THE DISPATCH AND GO TO COMPEA
; 782	;	  FORCE ADB TO GENERATE AR*4 AS DEFAULT THIS DISABLES PARITY
; 783	;	CHECKING ON THE FM WHEN IT IS NOT BEING READ FIXED IN
; 784	;	DEFINITION OF ADB THIS WILL ALSO SPEED UP THE MACHINE BY SOME
; 785	;	BECAUSE THE ADB FIELD CAN NO LONGER FORCE 3 TICS WITHOUT REALLY
; 786	;	NEEDING THAT LONG
; 787	;252	SAVE A WORD AT IOPGF+1 BY MAKING IT PILD+3 THIS ADDS THE SET
; 788	;	ACCOUNT ENABLE TO AN UNDEFINED CASE.
; 789	;251	TURNING ON PAGING CAUSED A HANG THIS WAS BECAUSE OF A MISIMPLIMENTED
; 790	;	FIX IN 250. THE ATTEMPT TO PUT THAT FIX IN NO SPACE FAILED AND IT TOOK
; 791	;	ONE WORD. AT LDPT+1 ADD BR/AR AT GTCST1 RECOVER THE AR FROM THE BR
; 792	;	THIS SEEMS LIKE IT SHOULD BE ABLE TO BE BUMMED BUT I CANNOT
; 793	;	FIGURE OUT HOW
; 794	;	ALSO FIX A PLACE WHERE A PHYS REF WAS LEFT IN THE MODEL A CODE
; 795	;	AT PGRF6+4 MODEL B CONDITIONAL IS AS IT WAS MODEL A IS NEW TO USE
; 796	;	LD AR.PHYS MECHANISM; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-14
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 797	;250	LOADING HIGH ORDER GARBAGE TO THE VMA WITH THE FIX FOR
; 798	;	>256K CAUSES FUNNY THINGS TO HAPPEN. BITS GET CLOBBERED
; 799	;	WITH AR0-8_SCAD 14 LINES AFTER SECIMM. ACTUALLY IS MORE
; 800	;	HAIR BECAUSE OF CONFLICTING FIELDS. CODE ABOVE AND BELOW
; 801	;	THAT GOT REARRANGED TO SIMPLER MODEL A AND MODEL B CONDITIONALS
; 802	;	SINCE NOW ALL LINES ARE DIFFERENT. SHUFFLING OF FE IS DONE
; 803	;	TO PROVIDE ROOM FOR A CONSTANT ON THE CORRECT SIDE OF THE SCAD
; 804	;	AT LDPT A SIMILAR
; 805	;	RECODING IS NEEDED. 4 LINES OF CODE ARE REDONE IN MODEL
; 806	;	A CONDITIONAL AND CONDITIONALS ARE RESHUFFLED TO HAVE
; 807	;	SIMPLER FORMAT
; 808	;	NEW MACROS ARE ADDED GEN AR0-8, GEN FE AND AR0-8
; 809	;	VMA_AR+LD AR.PHYS AND ITS FRIENDS ARE TAKEN OUT OF KLPAGING
; 810	;	CONDITIONAL THEY ARE USED TO DO EXAMINES AND DEPOSITS NOW
; 811	;247	FIX ST AR.PHYS TO GIVE BIT 4 INSTEAD OF BIT 5 AT CHALT
; 812	;	AT PSTORE CHECK FOR AC REF AND IF SO WRITE FM MUST DO THIS
; 813	;	BECAUSE LOAD AD FUNC DOES NOT SET MCL STORE AR
; 814	;246	FIX MUUO, IN EXTENDED ADDRESSING, TO GET NEW PC BEFORE CLOBBERING
; 815	;	THE USER AND PUBLIC FLAGS THAT TELL WHERE TO GET IT.  FIX CONDITIONAL
; 816	;	ASSEMBLY AT INDRCT TO DO EA TYPE DISP IN MODEL A, NOT MODEL B.
; 817	;245	ADDITIONAL FIXES FOR THE 256K PROBLEM, TO MAKE EXAMINE AND
; 818	;	DEPOSIT WORK.  CHANGES AT CHALT TO CREATE CONSTANT "ST AR.PHYS",
; 819	;	AND EXTENSIVELY NEAR PICYC1, PIDATI, AND PIDATO.  CHANGES ARE ALL
; 820	;	UNDER MODEL B CONDITIONAL, BECAUSE MODEL B HARDWARE WORKS OK, AND
; 821	;	THE FIX IS REGARDED AS CROCKISH.
; 822	;244	WAIT FOR COMPLETION OF INDIRECT REFERENCE AT BYTEI+1 AND EXTI+1
; 823	;	EVEN THOUGH INTERRUPT REQUEST HAS BEEN SEEN, SO AS NOT TO CONFUSE MBOX.
; 824	;243	VARIOUS FIXES TO MAKE THESE SOURCES WITH MODEL.B SWITCH OFF
; 825	;	EQUIVALENT TO MODEL A SOURCES, SO WE CAN DISCARD MODEL A SOURCES
; 826	;	THE FIXES ARE:
; 827	;		1) SWITCH SNORM.OPT, TO SAVE SPACE IN SINGLE PRECISION
; 828	;		FLOATING NORMALIZATION.
; 829	;		2) CREATION OF LD AR.PHYS MAGIC CONSTANT, TO SOLVE HARDWARE
; 830	;		PROBLEMS GENERATING ADDRESSES ABOVE 256K.
; 831	;242	FIX AT SECPTR+1 TO PRESERVE AR LEFT UNTIL WE CAN CHECK
; 832	;	FOR BITS 12-17 NON ZERO CORRECT ADDRESS CONSTRAINTS AT
; 833	;	SECIMM+1 & +2 TO GET BRANCHING RIGHT FOR SHARED AND INDIRECT
; 834	;	SECTION POINTERS.  FIX AT LDIMM+1 TO CLEAR LH OF AR BEFORE
; 835	;	LOADING VMA WITH SPT ADDRESS, TO PREVENT PAGE FAULT ON SPT
; 836	;	REFERENCE.
; 837	;241	MORE FIXES AT START: AND NEWPC:, FOR SAME PROBLEM AS 240.
; 838	;	MUST LOAD FLAGS AND CLEAR VMAX, THEN LOAD VMA INTO PC TO CLEAR
; 839	;	PCX, THEN RELOAD VMA TO GET EFFECT OF NEW FLAGS AND CLEARED
; 840	;	PCX.  (MODEL A ONLY).
; 841	;240	FIXES AT START: AND NEWPC: TO LOAD 23-BIT ADDRESS RATHER
; 842	;	THAN 30-BIT, SINCE OTHER BITS ARE PC FLAGS.  AT SAME TIME AND
; 843	;	PLACE, FIX MODEL A CODE TO CLEAR PC SECTION NUMBER.
; 844	;237	CHANGE CONDITIONALS AROUND PUSH AND POP CODE FROM XADDR TO
; 845	;	MODEL.B. COULD SIMPLIFY IFNOT XADDR.
; 846	;236	FIX ADDRESS CONSTRAINTS ON USES OF EA MOD DISP IN MODEL
; 847	;	B MACHINE WITH EXTENDED ADDRESSING OFF.  PROBLEMS AT COMPEA,
; 848	;	BFETCH, AND EXT2.
; 849	;235	SLIGHTLY CLEANER FIXES FOR PROBLEMS IN 234 TO AVOID WASTING TIME
; 850	;	AND SPACE.  BYTE READ MACRO NEEDS TO SET VMA/LOAD, AND VMA_VMA
; 851	;	HELD MACRO DOESN'T USE MEM FIELD UNLESS MODEL B AND KL PAGING.
; 852	;	ALSO FIX CONDITIONAL ASSEMBLY STUFF TO AVOID SPURIOUS ERRORS.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-15
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 853	;234	INSTALL FIXES FOR SOME PLACES WHERE MODEL B CODE CAUSES CONFLICT
; 854	;	WITH THE OLD NON KLPAGING NON EXTENDED ADDRESSING CODE
; 855	;	THESE ARE AT BFETCH, PGF3-1, PGF6, EXT1+2
; 856	;233	FIX THE FOLLOWING PROBLEMS:
; 857	;		KL PAGING SHOULD PRODUCE A PAGE FAILURE WHEN BITS
; 858	;		 12-17 OF A PRIVATE SECTION POINTER ARE NON 0
; 859	;		 FIXED AT SECPTR ETC.
; 860	;		EDIT DOES NOT ALLOW INTERUPTS
; 861	;		 FIXED AT EDNXT1 AND AFTER THAT
; 862	;		MAP SHOULD NOT BE LEGAL IN USER MODE
; 863	;		 FIXED AT MAP2 AND CLEAN+15
; 864	;		MOVMI IS SHORTENED BY MAKING IT THE SAME AS MOVEI
; 865	;		 AT DON LEWINES SUGGESTION THIS IS IN DCODE 215
; 866	;232	MERGE THE SECOND ORDER STATISTICS GATHERING CODE WITH THIS
; 867	;	CODE INTENT IS TO KEEP IT HERE
; 868	;231	CHANGE THE LOAD CCA DEFINITION TO REFLECT THE NEW HARDWARE
; 869	;	THIS IS ENABLED WHEN THE MODEL.B ASSEMBLY SWITCH IS ON
; 870	;230	THIS IS THE POINT WHERE MICHAEL NEWMAN TAKES OVER THE MICROCODE
; 871	;	MAINTENCE SEVERAL BUG FIXES GET EDITED INTO 126 AT THIS POINT
; 872	;	TWO SETS OF PARALLEL CODE WILL BE MAINTAINED FOR A WHILE.
; 873	;	FIX THE CMPS PARODY ERROR PROBLEM WHEN ILLEGAL BITS ARE FOUND IN
; 874	;	THE LENGTHS.
; 875	;227	FIX PIBYTE TO GET DTE# CORRECT ON TO-10 TRANSFERS.  FIX MTRREQ
; 876	;	CYCLES TO WAIT FOR STORE TO FINISH BEFORE RE-ENABLING ACCOUNT.
; 877	;	FIX ADJSP OF LONG STACK POINTERS TO FETCH NEXT INSTR.
; 878	;226	FIX EXMD TO LOAD AR, RATHER THAN ARX, WITH MARK POINTER, AS
; 879	;	EXPECTED BY THE HANDLER.  FIX EDIT, SEVERAL PLACES, TO IGNORE
; 880	;	LEFT HALF OF MARK & PATTERN ADDRESSES WHEN PC SECTION IS ZERO.
; 881	;	FIX EDIT TO MAKE EXTENDED REFERENCE FOR PATTERN BYTES.
; 882	;	FIX ADJSP TO BE MEANINGFUL WITH LONG STACK POINTERS
; 883	;225	FIX BYTEA NOT TO CLOBBER FE ON INDIRECTS, FIX EXMD TO BACK
; 884	;	UP VMA AFTER STORING DSTP2 AND BEFORE STORING DSTP.  FIX EDIT TO
; 885	;	COUNT THE WHOLE PATTERN ADDRESS IF PC SECTION NOT ZERO.
; 886	;224	FIX EXTEND ADDRESS CALCULATION TO RECOVER E0 FROM MQ, AND
; 887	;	FIX EXTEND OPCODE TEST TO DISALLOW OPS >20.
; 888	;	FIXES TO HANDLE NEW ENCODING OF AC-OP ON APR BOARD.
; 889	;223	COMPLETE 222.  P HAS TO GO TO SC AS WELL AS AR0-5.  CREATE
; 890	;	SUBROUTINE RESETP TO DO IT.  GET CODE IN SYNC WITH HARDWARE AND
; 891	;	MOST RECENT SPEC FOR MEANING OF PXCT AC BITS IN EXTEND.  THUS
; 892	;	UNDO COMMENT IN 221:  WE SHOULD LOOK AT PXCT B11.  ALSO FIX
; 893	;	EXTEND TO USE CORRECT ENCODING OF BITS 9, 11, AND 12 FOR PXCT
; 894	;	OF STRING OPERATIONS.  FIX DATAI PAG SO IT DOESN'T LOSE THE
; 895	;	PREVIOUS CONTEXT AC BLOCK WHEN LOADING PREVIOUS SECTION #.
; 896	;	INSERT CHANGE CLAIMED FOR EDIT 55, TO INHIBIT INTERRUPT DURING
; 897	;	PI CYCLES.
; 898	;222	FIX BYTE POINTER UPDATE ROUTINES GSRC & IDST IN EIS CODE
; 899	;	TO UPDATE P WHEN INCREMENTING SECOND WORD.  JUST FORGOT TO. TRY
; 900	;	AGAIN TO CONTROL EIS REFERENCES OFF E0, FOR EXTENDED OR NOT.
; 901	;221	COMPLETE FIX OF 220, TO KEEP SR CORRECT THROUGH RELOAD OF IR
; 902	;	IN EXTEND DECODING, AND TO CONTROL SR CORRECTLY FOR XBLT DST
; 903	;	REFERENCES.  (WE WERE LOOKING AT PXCT B11, SHOULD BE B12).
; 904	;220	FIXES SEVERAL PLACES TO USE "EA" IN DRAM A FIELD INSTEAD OF "I",
; 905	;	NOTABLY BLT, WHICH WAS USING WRONG SECTION.  FIX EXTEND TO
; 906	;	CONTROL VMA EXTENDED BEFORE FETCHING EXTEND-OP, SO AS NOT TO
; 907	;	LOOK "UNDER" THE AC'S.  FIX XBLT FOREWARD TO STOP WHEN AC GOES
; 908	;	TO ZERO, NOT -1.  ALSO CONTROL SR BEFORE INITIAL STORE TO GET; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-16
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 909	;	CORRECT CONTEXT.
; 910	;217	CODE CHANGES TO MAKE SECOND WORD OF BYTE POINTER WORK RIGHT
; 911	;	WHETHER EFIW OR IFIW, BY CONTROLLING CRY18 OR CRY6.
; 912	;216	RECODE EXTENDED INSTRUCTION SET DECODING & EFFECTIVE ADDRESS
; 913	;	CALCULATION.  FIX UUO CODE TO INCREMENT VMA AFTER STORING PC.
; 914	;	FIX ADJBP TO GET 36 BIT ADDRESS ADJUSTMENT IF B12 SET.
; 915	;215	REARRANGE CONDITIONAL ASSEMBLY DEFAULTS TO BE MORE LOGICAL
; 916	;	INSERT FORM FEEDS AND COMMENTS TO HELP BEAUTIFY THE LISTING.
; 917	;	REWORK THE NEW JRST'S, TO MAKE THEM SMALLER, FASTER, AND TEST
; 918	;	IO LEGAL BEFORE DISMISSING.  PUT IN XBLT.
; 919	;214	MODIFY ADJBP AND UUO'S FOR EXTENDED ADDRESSING. REWORK PARITY
; 920	;	ERROR HANDLING, IN A FRUITLESS ATTEMPT TO MAKE IT SMALLER,
; 921	;	BUT SUCCESSFULLY MAKING IT CLEARER.  FIX ASSEMBLY ERRORS IN EIS
; 922	;	DUE TO AC4 CHANGES, AND ADD CODE TO HANDLE LONG BYTE POINTERS
; 923	;	IN AC'S.  PUT IN CODE TO GIVE PAGE FAIL 24 ON ILLEGAL FORMAT
; 924	;	INDIRECT WORD.
; 925	;213	FIX LDB & DPB TO TEST POINTER BIT 12 ON CALL TO BYTEA.
; 926	;212	MODIFY JSP, JSR TO STORE FULL PC WITHOUT FLAGS IN NON-ZERO SEC
; 927	;	SEPARATE CONDITIONALS FOR "MODEL B" MACHINE FROM THOSE FOR
; 928	;	EXTENDED ADDRESSING MICROCODE.
; 929	;211	REMOVE UNNECESSARY DIDDLING OF VMA USER BIT DURING PAGE REFILL,
; 930	;	AND ELIMINATE SPECIAL CASE FOR MAP INSTRUCTION, WHEN EXTENDED
; 931	;	ADDRESSING HARDWARE EXISTS TO SOLVE THESE PROBLEMS.
; 932	;	FIX SEVERAL CASES OF SIGNS DISP WITH INADEQUATE CONSTRAINT.
; 933	;210	FIX DEFINITION OF "SKP LOCAL AC REF", WHICH CONFUSED "AC
; 934	;	REF" WITH "LOCAL AC REF".
; 935	;207	FIX JRSTF (AND ITS DERIVATIVES) TO LOAD FLAGS INTO AR AFTER
; 936	;	DOING EA MOD DISP, WHICH WOULD OTHERWISE CLOBBER THEM.  FIX
; 937	;	COMPEA CODE TO LET AREAD HARDWARE LOAD AR.  OTHERWISE GET SEC #.
; 938	;206	FIX PCTXT ROUTINE TO GET PREVIOUS CONTEXT SECTION.
; 939	;205	FIX POPJ TO LOAD HALFWORD OR FULLWORD PC ACCORDING TO PC SECT
; 940	;204	FIX CONDITIONALS AROUND LOC 47, WRONG IN 202.  FIX DEFINITION
; 941	;	OF A INDRCT, DOESN'T NEED #07.  FIX STACK INSTRUCTIONS FOR
; 942	;	EXTENDED ADDRESSING.  MUST NOT LOAD VMA FROM FULL AD.
; 943	;203	INCLUDE CODE AT NEXT+2 TO GENERATE ADDRESS MASK (LOW 23 BITS)
; 944	;	AT HALT TIME, AND CODE IN PICYCLE TO USE IT TO GET 23 BIT ADDR
; 945	;	OUT OF IOP FUNCTION WORD.
; 946	;202	MOVE "40+A" LOCATIONS TO "A" UNDER EXTENDED ADDRESSING.  CHANGE
; 947	;	ALL CALL MACROS TO GENERATE CALL BIT INSTEAD OF SPECIAL FUNC'S.
; 948	;201	BEGIN EXTENDED ADDRESSING CHANGES IN EARNEST.  INTEGRATE NEW
; 949	;	EFFECTIVE ADDRESS COMPUTATION CODE, AND REVISE INSTRUCTION
; 950	;	ROUTINES AS NECESSARY.
; 951	;126	FIX STRAC3-2, WHERE COMMA GOT LEFT OFF WHEN IFETCH MOVED
; 952	;125	REMOVE NXT INSTR FROM STAC1, STRAC3, & STAC4, MAKING THEM JUMP
; 953	;	TO FINI INSTEAD.  PROBLEM INVOLVES A RACE IF PAGE FAIL OCCURS
; 954	;	WHILE WRITING FM.  IF FM ADDRESS CHANGES BEFORE COND/FM WRITE
; 955	;	GOES FALSE, APR BOARD MAY GRONK PARITY BIT OF SOME FM LOC'N.
; 956	;	THIS RESULTS IN SOME SOME PATHS FROM FETCH TO NICOND BECOMING
; 957	;	LONGER THAN 6 TICKS, SO THE FETCHES GOT SHUFFLED IN SOME PLACES.
; 958	;	MICROCODE PATCH ELIMINATES MOST PROBABLE CAUSE, WHICH IS PAGE
; 959	;	FAIL AT NICOND TIME WHILE WRITING AC OTHER THAN 0.  IT DOES NOT
; 960	;	TAKE CARE OF THE POSSIBILITY THAT COND/FM WRITE WILL GLITCH AT
; 961	;	INSTR 1777 TIME.
; 962	;124	FIXES IN SEVERAL PLACES TO SET AND CLEAR ACCOUNT ENABLE SO AS
; 963	;	TO GET REPEATABLE ACCOUNTING MEASURES OF USEFUL WORK DONE. THE
; 964	;	ENABLE IS NOW CLEARED FOR METER UPDATE CYCLES AND KL PAGE REFILL; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-17
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 965	;	CYCLES.  THE HARDWARE ALREADY TAKES CARE OF PI CYCLES.
; 966	;123	CORRECT 122 TO CONSTRAIN LOC "UNHALT", AND TO LOAD ARX FROM AR,
; 967	;	SO AS TO LET "SKP AR EQ" WORK.  PROBLEM AROSE BECAUSE MACRO ALSO
; 968	;	TESTS ARX00-01.  FIX EDIT, WHEN STORING DEST POINTER ON SELECT
; 969	;	SIGNIFICANCE START, TO ELIMINATE AMBIGUITY IN DEST P FIELD.
; 970	;122	SPEC CHANGE TO EXIT FROM HALT LOOP, SO THAT AR0-8=0 WITH AR9-35
; 971	;	NON-ZERO LOADS AR INTO PC TO START PROCESSOR.  THIS IS DIFFERENT
; 972	;	FROM EXECUTING JRST BECAUSE PC FLAGS ARE CLEARED.
; 973	;121	FIX TO 120 TO ALLOW A CYCLE BETWEEN FILLER FROM MEMORY AND
; 974	;	WRITING IT INTO FM (THUS PARITY CAN BE COMPUTED).  ALSO CLEAR
; 975	;	STATE REGISTER IN EDIT BEFORE GETTING NEXT PATTERN BYTE.
; 976	;120	FIX EIS TO TOLERATE PAGE FAIL ON READ OF FILL BYTE IN MOVSRJ
; 977	;	OR B2D CONVERSION.  REQUIRES GETTING FILLER BEFORE STORING DLEN
; 978	;	ALSO INTEGRATE OPCODE COUNTING/TIMING CODE UNDER CONDITIONALS
; 979	;117	FIX PARITY ERROR CODE TO WRITEBACK AR ON RPW ERROR.
; 980	;116	REWRITE OF DDIV, SO THAT THE NO-DIVIDE TEST IS ON THE MOST
; 981	;	SIGNIFICANT HALF OF THE MAGNITUDE OF THE DIVIDEND, RATHER THAN
; 982	;	THE MAGNITUDE OF THE MOST SIGNIFICANT HALF.  IN THE PROCESS,
; 983	;	SAVE TIME AND SPACE.  ALSO PUT IN CONDITIONAL ASSEMBLY VARIABLE
; 984	;	"WRTST" TO INHIBIT WRITE TEST CYCLE FOR INSTRUCTIONS WHICH
; 985	;	APPEAR NOT TO NEED IT, AND THUS TO SPEED THEM UP.
; 986	;115	FIX SBDIAG TO SET MCL REG FUNC TO INHIBIT EBOX MAY BE PAGED.
; 987	;114	RECODE STRING COMPARE TO SAVE SPACE AND TIME.  CHANGE DEFAULTS
; 988	;	FOR KLPAGING TO INCLUDE EIS, EXCLUDE TRACKS FEATURE.  CHANGE
; 989	;	KLPAGING (NEW SPEC) TO KEEP "LOGICALLY WRITABLE" IN SOFTWARE BIT
; 990	;113	RECODE KL PAGING TO ELIMINATE PROBLEM OF WRITING HARDWARE
; 991	;	PAGE TABLE BEFORE CHECKING FOR AGE TRAP, AND THEREFORE LEAVING
; 992	;	THE PAGE ACCESSIBLE AFTER THE TRAP.  THE RECODING ALSO IMPROVES
; 993	;	THE ALGORITHM IN THAT THE HARDWARE ENTRY INCLUDES THE W BIT SET
; 994	;	IF THE CORE TABLES ALLOWED WRITE AND THE CST INDICATES WRITTEN,
; 995	;	EVEN IF THE CURRENT REFERENCE WAS NOT A WRITE.
; 996	;	ALSO FIX CODE WHICH WRITES PT DIR, TO GET WRITE REF BIT FROM
; 997	;	VMA HELD INTO BIT 5 OF SAVED PAGE FAIL WORD.
; 998	;112	FIX PAGE FAIL CODE FOR USE WITH PROB SHEET 1396, WHICH LOADS
; 999	;	PC IF PAGE FAIL OCCURS ON NICOND.  THUS CODE NEEDN'T CHECK FOR
; 1000	;	FETCH AT CLEAN, WHICH CAUSED OTHER PROBLEMS ON PARITY ERRORS.
; 1001	;	CLEAR FE AND SC IN NXT INSTR MACRO (JUST CLEANLINESS).
; 1002	;111	PATCH SEVERAL ROUTINES WITH THE FOLLOWING MACRO --
; 1003	;	FETCH WAIT	"MEM/MB WAIT"
; 1004	;	TO PREVENT SEQUENCES IN WHICH PAGE FAIL INFO CAN GET LOST
; 1005	;	BECAUSE OF LONG TIME FROM REQUEST TO MB WAIT.  THESE PATCHES
; 1006	;	SHOULD BE REMOVED AFTER AN ECO HAS BEEN INSTALLED TO FIX.
; 1007	;	IN ADDITION, EBUSX SUBROUTINE HAS BEEN MODIFIED TO PREVENT RACE
; 1008	;	CONDITION WHEN SETTING UP IO FUNCTION WITH COND/EBUS CTL AND
; 1009	;	MAGIC # BIT 4.  MUST NOT CHANGE #5 THROUGH #8 ON NEXT CYCLE.
; 1010	;	FIX KLPAGING CODE TO GO BACK TO AREAD ON MAP REF, BECAUSE
; 1011	;	MEM/AD FUNC DOESN'T CORRECTLY RESTORE APR REG FUNC.  ALSO MAKE
; 1012	;	THE CODE SMARTER ON NO MATCH CONDITION, SO REQUEST DOESN'T HAVE
; 1013	;	TO BE RESTARTED AND IMMEDIATELY FAIL AGAIN.
; 1014	;110	GIVE UP ON THE OLD STRING COMPARE CODE, INSTALLING MIKE NEWMAN'S
; 1015	;	VERSION.  SOMEWHAT SLOWER, BUT GIVES THE RIGHT ANSWERS.
; 1016	;	FIX LDB CODE TO WAIT FOR MEM WORD EVEN IF INTERRUPT REQUEST
; 1017	;	SEEN, SO AS NOT TO GET CONFUSED WHEN IT ARRIVES OR PAGE FAILS.
; 1018	;	ALSO IMPROVE CLRPT ROUTINE USED BY CONO AND DATAO PAG TO START
; 1019	;	LOOP WITH VMA CLEARED AND PT WR SELECTION SETUP CORRECTLY.
; 1020	;107	FIX STRING COMPARES TO CHECK FOR INTERRUPT.  THIS INVOLVED; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-18
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 1021	;	CHECKING DURING GSRC ROUTINE, WHICH ELIMINATES NEED FOR CHECK
; 1022	;	IN SRCMOD (WHICH CALLS GSRC).  IT ALSO REQUIRED CLEARING SFLGS
; 1023	;	AT STARTUP, AND ADJUSTING DLEN UPDATE CODE IN DEST FILL TO GET
; 1024	;	VALID LENGTH STORED ON INTERRUPT.
; 1025	;106	ELIMINATE RACE IN DECODING OF # FIELD ON MTR BOARD BY HOLDING
; 1026	;	LOW 3 BITS THROUGH NEXT MICROINSTRUCTION.
; 1027	;	FIX LUUO AND MUUO TO ALLOW INTERRUPTS.
; 1028	;	FIX B2D OFFSET TO SIGN-EXTEND E1 AFTER INTERRUPT.  FINISH 105,
; 1029	;	TO GET ENTIRE AR LOADED WHILE CLEARING MQ (ARL WAS HOLDING).
; 1030	;	FIX KL PAGING TO USE VMA/1 INSTEAD OF VMA/AD WHEN RESTORING VMA
; 1031	;	FROM VMA HELD OR COPIES THEREOF.
; 1032	;	FIX UFA NOT TO ALWAYS GET UNDERFLOW ON NEGATIVE RESULTS.
; 1033	;	SAME FIX AS EDIT 103 OF BREADBOARD.  WHERE DID IT GET LOST?
; 1034	;105	FIX KL PAGING AS REVISED BY EDIT 103 TO CORRECTLY RESTORE
; 1035	;	BR ON NO-MATCH CONDITION
; 1036	;	ANOTHER FIX TO B2D, TO CLEAR MQ ON ENTRY.  BUG INVOLVED GARBAGE
; 1037	;	FROM MQ SHIFTING INTO ARX DURING DEVELOPMENT OF POWER OF TEN.
; 1038	;104	FIX BINARY TO DECIMAL CONVERSION, WHICH WAS NOT GOING TO CLEAN
; 1039	;	ON FINDING AN INTERRUPT, AND ON RESTART WITH FPD SET, WAS NOT
; 1040	;	SETTING UP SLEN.  TSK, TSK.  CORRECT CLEANUP FOR DEST FILL IN
; 1041	;	MOVSRJ, WHICH WAS INCREMENTING BOTH SLEN AND DLEN, SHOULD
; 1042	;	HAVE BEEN NEITHER.  FIX JSR, BROKEN BY EDIT 103.  JUMP MUST BE
; 1043	;	TO E+1, NOT E.
; 1044	;103	CREATE CONDITIONAL ASSEMBLY FOR EXTENDED ADDRESSING. UNDER IT,
; 1045	;	CREATE MEM FIELD DEFINITIONS, SUPPRESS SXCT.
; 1046	;	SAVE A WORD IN JSR BY USING JSTAC IN COMMON WITH PUSHJ.
; 1047	;	FORCE TIME FIELD IN CASES WHERE ASSEMBLER DEFAULT SCREWS UP.
; 1048	;	ADD INTERRUPT TESTS IN KL PAGING CODE TO PREVENT HANGS, AND
; 1049	;	REVISE PAGE FAIL WORD TO ELIMINATE THE NEW FAIL CODES.
; 1050	;102	ATTEMPT ANOTHER FIX OF MOVSRJ, CVTBDX FILL.  EDIT 71 LOSES
; 1051	;	DUE TO INCONSISTENCY -- DLEN UPDATE MUST NOT PRECEED CLEANUP.
; 1052	;	CREATE CONDITIONAL ASSEMBLY SWITCHES TO CONTROL EXTENDED
; 1053	;	INSTRUCTION SET, DOUBLE INTEGER ARITHMETIC, AND ADJBP.  CHANGE
; 1054	;	DEFAULT OF IMULI.OPT, WHICH CAN GET SIGN WRONG ON OVERFLOW.
; 1055	;101	FIX METER REQUEST CODE TO "ABORT INSTR" EVEN IF NOT SETTING
; 1056	;	PI CYCLE.  THIS SHOULD FIX OCCASIONAL LOSS OF TRAPS PROBLEM.
; 1057	;100	FIXES TO KL PAGING CODE TO PREVENT LOADING VMA FROM AD WHILE
; 1058	;	REQUESTING PHYSICAL REF.  FIX JSR TO PREVENT FM PARITY STOP
; 1059	;	ON STORE TO AC.  FIX 1777 TO FORCE RECIRCULATION OF AR/ARX,
; 1060	;	EVEN IF MBOX RESP STILL TRUE.
; 1061	;77	FIX DDIV TO GET MQ SHIFTED LEFT ONE PLACE, WITHOUT INTRODUCING
; 1062	;	AN EXTRA BIT, AT DDVX1.  THIS INVOLVES INHIBITING ADA TO PREVENT
; 1063	;	AD CRY0 FROM COMMING INTO MQ35.
; 1064	;76	FIX UFA TO ALLOW AN EBOX CYCLE BETWEEN FETCH AND NICOND WHEN
; 1065	;	FRACTION SUM IS ZERO, AT UFA3.
; 1066	;75	PUT BACK INSTRUCTION "MBREL" REMOVED BY EDIT 64.  NECESSARY TO
; 1067	;	ENSURE THAT EBOX REQUEST FOR FETCH DOESN'T COME UP WHILE
; 1068	;	REGISTER FUNCTION IS IN PROGRESS, WHICH WOULD CONFUSE MBOX ON
; 1069	;	STARTING THE FETCH.
; 1070	;74	CHANGES TO EIS FOR NEW-SPEC AC USAGE.  CHANGES TO KL PAGING FOR
; 1071	;	INDIRECT, IMMEDIATE SECTION POINTERS
; 1072	;73	FIX JRA TO PREVENT WRITING AC WITH DATA FRESH FROM MEMORY (ALLOW
; 1073	;	A CYCLE FOR PARITY CHECK).  FIX DPB CODE TAKE ONLY 3 TICKS ON
; 1074	;	RETURN FROM BYTEA, SO THAT CACHE DATA DOESN'T ARRIVE INTO AR
; 1075	;	AND ARX UNTIL DPB1, WHEN THE BYTE HAS GOTTEN OUT TO MQ.
; 1076	;72	FIX DEFINITION OF SP MEM/UNPAGED TO INHIBIT VMA USER.  FIX; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-19
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 1077	;	PAGE FAIL CODE TO CHECK FOR VMA FETCH BEFORE LOOKING AT
; 1078	;	INTERRUPT REQUEST.  PROBLEM WAS INTERRUPT CONCURRENT WITH
; 1079	;	PAGE FAIL ON JRSTF TO USER.  PC FLAGS GOT RESTORED, BUT VMA
; 1080	;	NEVER COPIED TO PC BECAUSE PAGE FAIL INHIBITED NICOND, AND
; 1081	;	INTERRUPT ABORTED PAGE FAIL HANDLING TO LOAD PC.
; 1082	;71	DEFINE FMADR/AC4=6.  FIX MOVFIL ROUTINE TO PUT AWAY UPDATED
; 1083	;	LENGTH DIFFERENCE WHEN INTERRUPTED, THUS AVOIDING RANDOMNESS
; 1084	;	IN MOVSRJ, CVTBDX. FIX CVTBD CALL TO MOVFIL TO PRESERVE SR.
; 1085	;	CHANGE STMAC AND PIDONE FROM "FIN XFER" TO "FIN STORE", BECAUSE
; 1086	;	STORE WAS IN PROGRESS, WHICH CAUSED FM WRITE IF AC REF, AND
; 1087	;	GOT A PARITY ERROR DUE TO ADB/FM.
; 1088	;70	FIX PXCT 4,[POP ...], WHICH DIDN'T GET DEST CONTEXT SET FOR
; 1089	;	STORE.  MUST USE SR_100 TO SET IT.
; 1090	;67	FIX PROBLEM IN ADJBP BY WHICH BYTES/WORD WAS GETTING LOST
; 1091	;	WHEN DIVIDE ROUTINE LOADED REMAINDER INTO BR.  SOLVED BY
; 1092	;	SAVING BYTES/WORD IN T1.
; 1093	;66	FIX KL PAGING TO RESTORE VMA ON TRAP, SAVE ADDRESS OF POINTER
; 1094	;	CAUSING TRAP, AND NOT RESTORE ARX EXCEPT FOR BLT PAGE FAIL.
; 1095	;	ALSO SET TIME PARAMETER ON ADB/FM TO ALLOW TIME FOR PARITY
; 1096	;	CHECKING OF FM.
; 1097	;65	FIX KL PAGING CODE TO DO MBWAIT AFTER DETERMINING THAT PARITY
; 1098	;	ERROR HAS NOT OCCURRED, SO AS TO GET CORRECT VMA TO SAVE.
; 1099	;	CREATE SYMBOLS FOR KL PAGE FAIL CODES.  PUT CONDITIONAL
; 1100	;	ASSEMBLY AROUND IMULI OPTIMIZATION CODE, AND SXCT.  CREATE
; 1101	;	SYMBOL "OPTIONS" IN # FIELD FOR MICROCODE OPTIONS.
; 1102	;64	MICROCODE FOR KL10 PAGING (PAGE REFILL, MAP INSTR)...
; 1103	;	REMOVE UNNECESSARY INSTRUCTION MBREL: FROM SWEEP AND APRBO
; 1104	;	COSMETIC CHANGES TO KEEP COMMENTS & MACRO DEFINITIONS FROM
; 1105	;	OVERFLOWING LINE OF LISTING, AND INSERTION OF CONDITIONAL
; 1106	;	ASSEMBLY CONTROL OF LONG FLOATING POINT INSTRUCTIONS.
; 1107	;63	IN MTR REQUEST ROUTINE, DON'T DISMISS WHEN PI CYCLE HASN'T
; 1108	;	BEEN SET.
; 1109	;62	FIX RDMTR CODE TO PUT 35 IN SC BEFORE GOING TO DMOVEM CODE.
; 1110	;61	FIX PIIBP ROUTINE TO USE CALL.M INSTEAD OF SPEC/CALL,
; 1111	;	WHICH GETS OVERRIDDEN BY P_P-S... IN MTR REQUEST SERVICE
; 1112	;	ROUTINE, DON'T SET PI CYCLE UNLESS REQUEST IS FOR VECTOR.
; 1113	;60	FIX DATAO PAG TO DO MB WAIT AFTER STORING EBOX ACCT AND
; 1114	;	BEFORE CHANGING VMA.
; 1115	;57	RE-CODE USES OF A@, B@ TO USE VMA/1, RATHER THAN VMA/AD,
; 1116	;	IN ORDER TO GET CORRECT CONTEXT ON INDIRECT WORD. SEE MCL4
; 1117	;56	FIX SECOND PART OF PICYCLE (TAG NEXT:) TO ENSURE THAT
; 1118	;	PC+1 INH, KERNEL CYCLE, ETC REMAIN UP DURING 2ND PART.
; 1119	;	ALSO CHANGE SPEC/FLAG CTL FOR ECO 1261, WHICH REQUIRES
; 1120	;	#07 TO BE OPPOSITE OF #04 TO GENERATE SCD LEAVE USER.
; 1121	;55	FIX SPEC INSTR/SET PI CYCLE TO INHIBIT INTERRUPTS
; 1122	;	(IN PARTICULAR, METER UPDATE REQUESTS).  MAKE SURE VALID
; 1123	;	DATA SAVED ON IO PAGE FAIL AND PARITY ERRORS. REMOVE
; 1124	;	BACKWARDS BLT... IT BROKE TOO MANY PROGRAMS.
; 1125	;54	FIX OVERFLOW CHECK IN IMULI OPTIMIZATION TO INH CRY 18
; 1126	;	UPDATE TO USE CONDITIONAL ASSEMBLY IN MICRO VERS 20.
; 1127	;53	FIX T1,T2 PARAMETERS ON BYTE DISP, SIGNS DISP
; 1128	;52	CORRECT SHIFT AMOUNT FOR IMULI OPTIMIZATION, AND FIX MACRO
; 1129	;	DEFINITIONS FOR SET SR?, WHICH WERE ALWAYS SETTING SR0.
; 1130	;51	OPTIMIZE IMULI OF TWO POSITIVE OPERANDS (TO SPEED UP SUBSCRIPT
; 1131	;	CALCULATIONS) BY TAKING ONLY 9 MULTIPLY STEPS AND STARTING
; 1132	;	NEXT INSTRUCTION FETCH EARLIER.  OPTIMIZATION CAN BE REMOVED; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-20
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 1133	;	BY COMMENTING OUT TWO INSTRUCTIONS AT IMULI, AND ONE FOLLOWING
; 1134	;	IMUL.  ALSO FIX APRBI/UVERS TO KEEP SERIAL # OUT OF LH.
; 1135	;50	INTRODUCE SKIP/FETCH AND CODE IN PAGE FAIL RECOVERY TO LOAD
; 1136	;	PC FROM VMA IF PAGE FAIL OCCURED ON FETCH, BECAUSE NICOND
; 1137	;	CYCLE, WHICH SHOULD HAVE LOADED PC, GETS INHIBITED BY INSTR 1777
; 1138	;	ALSO INCLUDE EXTENDED INSTRUCTION SET.
; 1139	;47	UNDO XCT CHANGES OF EDIT 46, WHICH BROKE XCT DUE TO INSUFFICIENT
; 1140	;	TIME FOR DRAM HOLD BEFORE USING "A READ". ALSO FIX VECTOR
; 1141	;	INTERRUPT CODE TO LOOK AT CORRECT BITS FOR CONTROLLER NUMBER.
; 1142	;46	FOLLOW-ON TO EDIT 45, SAVING 2 WORDS AND A CYCLE
; 1143	;	ALSO MOVE JRST TO 600, JFCL TO 700, UUO'S TO 100X AS PREPARATION
; 1144	;	FOR EXTENDED INSTRUCTION SET
; 1145	;45	FIX SXCT TO LOOK AT AC FIELD OF SXCT, NOT SUBJECT INSTRUCTION,
; 1146	;	WHEN DECIDING WHETHER TO USE BASE-TYPE ADDRESS CALCULATION.
; 1147	;44	FIX PAGE FAIL LOGIC TO WORK FOR EITHER PAGE FAIL OR PARITY
; 1148	;	ERROR.  EDITS 42 AND 43 BOTH WRONG.  ALSO CORRECT RACE IN
; 1149	;	WRITING PERFORMANCE ANALYSIS ENABLES TO PREVENT SPURIOUS COUNTS.
; 1150	;43	CORRECT USE OF PF DISP BY EDIT 42.  LOW BITS ARE INVERTED
; 1151	;42	FIX BUGS INTRODUCED BY EDIT 40, WHICH MADE FLTR OF 1B0 HANG
; 1152	;	TRYING TO NEGATE IT, AND FIX UP EXPONENT CORRECTION ON LONG
; 1153	;	SHIFT LEFT.  ALSO PUT IN CODE TO HANDLE PARITY ERROR PAGE
; 1154	;	FAILURES, AND SET TIME CONTROLS ON 43-47.
; 1155	;41	REWRITE OF VECTOR INTERRUPT PROCESSING TO MAKE DTE VECTORS
; 1156	;	GO TO 142+8N, WHERE N IS DTE#.  RH20 GO TO PROGRAMMED ADDRESS
; 1157	;	IN EPT, EXTERNAL DEVICES USE EXEC VIRTUAL ADDRESSES.
; 1158	;40	IMPROVEMENTS TO FLOATING NORMALIZATION TO MAKE LONG SHIFTS
; 1159	;	FASTER, PRIMARILY TO HELP FLTR
; 1160	;37	FIX FLOATING DIVIDE SO THAT THE TRUNCATED FORM OF A NEGATIVE
; 1161	;	QUOTIENT IS EQUAL TO THE HIGH-ORDER PART OF THE INFINITE-
; 1162	;	PRECISION QUOTIENT.  SEE COMMENTS IN THE CODE.  ALSO BUM
; 1163	;	A CYCLE OUT OF FLOATING DIVIDE BY STARTING THE NORMALIZE
; 1164	;	WHILE MOVING THE QUOTIENT INTO AR.
; 1165	;	SEVERAL CHANGES TO MAKE TRACKS FEATURE WORK
; 1166	;36	FIX CONO MTR TO PUT DATA ON BOTH HALVES, SO PI CAN SEE PIA
; 1167	;35	FIX CONI PI TO READ BACK WRITE EVEN PARITY ENABLES
; 1168	;34	FIX BLT USE OF SR, SO NO CORRECTION OF ARX NECESSARY
; 1169	;33	FIX PAGE TABLE REFERENCES TO FORCE UNPAGED REF.  FIX TRAP
; 1170	;	TO SET PC+1 INHIBIT.
; 1171	;32	CORRECT SETTING OF SC FOR SHIFTING METER COUNTERS, TO GET
; 1172	;	12 BITS UNUSED AT RIGHT WHEN IT GETS TO CORE.
; 1173	;31	RECODE ASH AND ASHC TO SAVE SPACE
; 1174	;30	FIX JFFO TO SHIFT AR CORRECTLY AT JFFO2.  BUM ADJSP TO USE
; 1175	;	STMAC FOR UPDATING PDL POINTER.
; 1176	;27	FIX CONI PAG TO READ EBUS.  CORRECT DEFINITIONS OF MBOX
; 1177	;	REGISTER FUNCTIONS, WHICH HAD BITS 0 AND 3 INVERTED.
; 1178	;26	FIX DEFINITIONS OF DIAG FUNC CONO MTR AND CONO TIM, WHICH
; 1179	;	WERE REVERSED
; 1180	;25	FIX DECODING OF PHYSICAL DEVICE NUMBER IN PI FUNCTION CODE
; 1181	;	AND RE-CODE JFCL FOR FEWER MICROWORDS
; 1182	;24	FIX JFFO TO SHIFT ON FIRST 6-BIT TEST STEP, AND JRSTF TO
; 1183	;	KEEP E AND XR DISTINCT.  ALSO SET LOAD-ENABLE BITS IN
; 1184	;	DATAI PAG, WORD.
; 1185	;23	FIX CONO PI, TO HOLD AR ONTO EBUS THRU REL EBUS, BECAUSE
; 1186	;	PI BOARD DELAYS CONO PI TO GET CONO SET EQUIVALENT.
; 1187	;22	MORE JFCL FIXES.  MUST USE FLAG CTL/JFCL WHILE CLEARING BITS,
; 1188	;	AS WELL AS WHILE TESTING THEM.  BUM A WORD OUT OF JFFO BY; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-21
; EDHIS.MIC[4,24]	12:02 29-May-86			REVISION HISTORY					

; 1189	;	MAKING THE SIXBIT COUNT NEGATIVE.  CHANGES SO SHIFT SUBR
; 1190	;	RETURNS 2, BYTEA 1.  FIX SETMB TO STORE BACK AND FETCH.
; 1191	;21	RE-WRITE JFCL TO KEEP LOW OPCODE BITS OUT OF AR0-1, BECAUSE
; 1192	;	PC00 GETS PROPAGATED LEFT TO ADA -1 AND -2.
; 1193	;20	FIX BLT TO LOAD BR WITH SRC-DST ADDR
; 1194	;	ALSO SET TIME PARAMETERS ON CONDITIONAL FETCH FUNCTIONS
; 1195	;17	CHANGE SWEEP ONE PAGE TO PUT PAGE # IN E, RATHER THAN ADDR.
; 1196	;	ALSO CHANGE COND/FM WRITE TO MATCH ECO #1068.
; 1197	;16	FIX JUMP FETCH MACRO TO LOAD VMA FROM PC+1 (TEST SATISFIED
; 1198	;	OVERRIDES THIS TO HOLD VMA).  ALSO BUM ONE MICROWORD FROM MUUO.
; 1199	;15	INCLUDE PAGE FAIL DISP IN DISP/ FIELD
; 1200	;	ALSO MAKE MUUO STORE PROCESS CONTEXT WORD AT 426, AND SETUP
; 1201	;	PCS FROM PC EXTENSION, CWSX FROM SXCT
; 1202	;14	FIX DEFINITIONS OF SKIP/IO LEGAL, AC#0, SC0, EVEN PAR
; 1203	;	ALSO FIX DATAO PAG, TO SEND LH DATA ON BOTH HALVES OF EBUS
; 1204	;13	ALIGN SETEBR SO CALL TO SHIFT RETURNS CORRECTLY
; 1205	;12	MAKE SURE AD COPIES AR DURING DATAO, CONO, AND CLEAR AR AT
; 1206	;	SET DATAI TIME.
; 1207	;11	FIXES TO CONTINUE CODE SO CONSOLE WORKS, AND CORRECTIONS TO
; 1208	;	PROTECTED DEP/EXAM SO PROTECTION PROTECTS.
; 1209	;10	FIX A READ MACRO TO VMA/PC+1.  AD OVERRIDES UNLESS DRAM A=1
; 1210	;07	RE-WRITE OF PI CYCLE CODE TO RECOGNIZE NEW EBUS SPEC.
; 1211	;06	FIX DEFINITIONS OF SKIPS 40-57 BY COMPLEMENTING 3 LOW ORDER BITS
; 1212	;	FIX MULSUB TO CORRESPOND TO NEW CRA LOGIC
; 1213	;05	FIX EBUS CTL DEFINITIONS TO GET F01 CORRECT.  CORRECT FLAG CTL
; 1214	;	DEFINITIONS TO PREVENT LEAVE USER WHEN NOT WANTED, AND FIX
; 1215	;	JRST/JFCL TO HAVE FLAGS IN AR WHEN NEEDED.
; 1216	;04	FIX RETURNS FROM MULSUB, PUT BETTER COMMENTS ON SNORM CODE,
; 1217	;	IMPROVE SNORM ALGORITHM TO MINIMIZE WORST-CASE TIME.
; 1218	;03	FIX DISPATCH ADDRESS PROBLEMS, MOSTLY JRST/JFCL AND UUO'S.
; 1219	;02	CHANGES PER INSTRUCTION SET REVIEW -- DELETE USE OF BIT12 OF
; 1220	;	BYTE POINTERS, CHANGE BLT TO PUT FINAL SRC,DST ADDRESSES IN AC,
; 1221	;	MAKE TRUNCATE FORM FLOATING POINT REALLY TRUNCATE, ELIMINATE
; 1222	;	LOCAL JSYS SUPPORT, DELETE PXCT OPCODE (XCT W/ NON-ZERO AC IN
; 1223	;	EXEC MODE), LUUO'S GO TO 40/41 OF CURRENT SPACE.
; 1224	;01	UPDATES FOR .TITLE AND .TOC PSEUDO OPS,
; 1225	;	AND VARIOUS CHANGES FOR PROTO HARDWARE
; 1226	;00	CREATION, BASED ON BREADBOARD AS OF EDIT 66
						; 1227	.BIN
						; 1228	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; DEFINE.MIC[4,24]	16:58 23-May-86			CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS		

						; 1229	.TOC	"CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS"
; 1230	.NOBIN
; 1231	
; 1232	; [COST ESTIMATES IN BRACKETS INDICATE NUMBER OF ADDITIONAL
; 1233	; MICROINSTRUCTIONS REQUIRED BY TURNING ON THE FEATURE SWITCH]
; 1234	
; 1235	.DEFAULT/TRACKS=0	;1 ENABLES STORING PC AFTER EVERY INSTRUCTION,
; 1236				; & CREATES DATAI/O PI TO READ/SETUP PC BUFFER
; 1237				;ADDRESS. [COST = 21 WDS]
; 1238	
; 1239	.DEFAULT/OP.CNT=0	;1 ENABLES CODE TO BUILD A HISTOGRAM IN CORE
; 1240				; COUNTING USES OF EACH OPCODE IN USER & EXEC
; 1241	
; 1242	.DEFAULT/OP.TIME=0	;1 ENABLES CODE TO ACCUMULATE TIME SPENT BY
; 1243				; EACH OPCODE
; 1244	
; 1245	.DEFAULT/SO.CNT=0	;SECOND ORDER COUNTING IN 128K STARTING AT LOC
; 1246				; 400000 NOT DEBUGED [COST = 28 WDS]
; 1247	
; 1248	.DEFAULT/SO2.CNT=0	;SECOND ORDER COUNTING IN 128K STARTING AT LOC
; 1249				; PRESENTED AT START DOES ONE MORE ADD THAN
; 1250				; SO.CNT AND HENCE AN INSTRUCTION TAKES
; 1251				; 120 NS LONGER THAN SO.CNT [COST = 28 WDS]
; 1252	
; 1253	.DEFAULT/PAGCNT=0	;Enable code to count entries into the PFH and
; 1254				; number of DATAO PAGs with bit 2 set.  [Cost =
; 1255				; 6 words] [327]
; 1256	
; 1257	.DEFAULT/FPLONG=1	;1 ENABLES KA-STYLE DOUBLE PRECISION FLOATING
; 1258				;POINT INSTRUCTIONS: FADL, FSBL, FMPL, FDVL,
; 1259				; UFA, DFN. [COST = 49 WDS]
; 1260	
; 1261	.DEFAULT/MULTI=0	;1 IF MULTIPROCESSOR SYSTEM, TO SUPPRESS CACHE
; 1262				;ON UNPAGED REF'S.  PAGED REF'S ARE UP TO EXEC.
; 1263	
; 1264	.DEFAULT/MOS.MULTI=0	;1 if we have multiported MOS memory.  This hardware
; 1265				;project was abandoned before it was completed.
; 1266				;[Cost = 5 wds.]
; 1267	
; 1268	.DEFAULT/SNORM.OPT=0	;1 ENABLES FASTER NORMALIZATION OF SINGLE-
; 1269				; PRECISION RESULTS WHICH HAVE SEVERE LOSS OF
; 1270				; SIGNIFICANCE [COST = 4 WDS]
; 1271	
;;1272	.IF/TRACKS		;SETUP CONTROL FOR COMMON CODE
;;1273		.SET/INSTR.STAT=1
; 1274	.ENDIF/TRACKS
; 1275	
;;1276	.IF/OP.CNT
;;1277		.SET/INSTR.STAT=1	;ENABLE COMMON CODE, ERROR IF TRACKS TOO
; 1278	.ENDIF/OP.CNT
; 1279	
;;1280	.IF/OP.TIME
;;1281		.SET/INSTR.STAT=1	;ERROR IF TRACKS OR OP.CNT ALSO SET
; 1282	.ENDIF/OP.TIME
; 1283	
;;1284	.IF/SO.CNT; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; DEFINE.MIC[4,24]	16:58 23-May-86			CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS		

;;1285		.SET/INSTR.STAT=1
; 1286	.ENDIF/SO.CNT
; 1287	
;;1288	.IF/SO2.CNT
;;1289		.SET/INSTR.STAT=1
; 1290	.ENDIF/SO2.CNT
; 1291	
; 1292	.DEFAULT/INSTR.STAT=0		;IF NO STATISTICS, TURN OFF COMMON CODE
; 1293	
;;1294	.IF/INSTR.STAT
;;1295		.SET/NONSTD=1		;STATISTICS CODE IS NONSTANDARD
;;1296		.SET/TRXDEF=1		;Make sure TRX registers get defined [327]
; 1297	.ENDIF/INSTR.STAT
; 1298	
;;1299	.IF/PAGCNT
;;1300		.SET/NONSTD=1		;All statistics are nonstandard
;;1301		.SET/TRXDEF=1		;We need the TRX registers
; 1302	.ENDIF/PAGCNT
; 1303	
; 1304	.DEFAULT/TRXDEF=0		;Normally no TRX registers needed
; 1305	
; 1306	.DEFAULT/NONSTD=0		;NONSTANDARD MICROCODE IS NORMALLY OFF
; 1307	.DEFAULT/OWGBP=0		;[264]
; 1308	.DEFAULT/IPA20=0		;[264]
; 1309	.DEFAULT/NOCST=0		;[264]
; 1310	.DEFAULT/CST.WRITE=1		;[314] Enable CST writable bit
; 1311	.DEFAULT/BIG.PT=1		;[333][347] Special code for big page table and Keep bit
; 1312	.DEFAULT/DDT.BUG=0		;[346] If on, enable APRID hack to move bit 23
; 1313	.DEFAULT/GFTCNV=1		;[273] GFLOAT CONVERSION INST.
; 1314	.DEFAULT/EDIT=1			;Edit is usually here  ****HACK****
; 1315	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; DEFINE.MIC[4,24]	16:58 23-May-86			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1316	.TOC	"HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS"
; 1317	
; 1318	;(1)	FIELD DEFINITIONS
; 1319	;	THESE OCCUR AT THE BEGINNING OF THE LISTING, IN THE SOURCE FILE
; 1320	; DEFINE.MIC (CONTROL AND DISPATCH RAM DEFINITIONS).
; 1321	; THEY HAVE THE FORM:
; 1322	;	SYMBOL/=<L:R>M,J
; 1323	;ANOTHER FORM ACCEPTED BY THE ASSEMBLER (FOR HISTORIC REASONS) IS:
; 1324	;	SYMBOL/=J,K,R,M		;THIS FORM HAS BEEN REMOVED FROM THIS CODE
; 1325	;	THE PARAMETER (J) IS MEANINGFUL ONLY WHEN "D" IS SPECIFIED
; 1326	; AS THE DEFAULT MECHANISM, AND IN THAT CASE, GIVES THE DEFAULT VALUE OF
; 1327	; THE FIELD IN OCTAL.
; 1328	;	THE PARAMETER (K) GIVES THE FIELD SIZE IN (DECIMAL) NUMBER
; 1329	; OF BITS. THIS IS USED ONLY IN THE OUTDATED FORMAT.
; 1330	;	THE PARAMETER (L) GIVES THE BIT POSITION OF THE LEFTMOST BIT
; 1331	;IN THE FIELD. THE SAME METHOD IS USED AS FOR (R) BELOW.
; 1332	;	THE PARAMETER (R) GIVES THE FIELD POSITION IN DECIMAL
; 1333	; AS THE BIT NUMBER OF THE RIGHTMOST BIT OF THE FIELD.  BITS ARE NUMBERED
; 1334	; FROM 0 ON THE LEFT.  NOTE THAT THE POSITION OF BITS IN THE MICROWORD
; 1335	; SHOWN IN THE LISTING BEARS NO RELATION TO THE ORDERING OF BITS IN THE
; 1336	; HARDWARE MICROWORD, WHERE FIELDS ARE OFTEN BROKEN UP AND SCATTERED.
; 1337	;	THE PARAMETER (M) IS OPTIONAL, AND SELECTS A DEFAULT
; 1338	; MECHANISM FOR THE FIELD.  THE LEGAL VALUES OF THIS PARAMETER ARE THE
; 1339	; CHARACTERS "D", "T", "P", OR "+".
; 1340	;	  "D" MEANS (J) IS THE DEFAULT VALUE OF THE FIELD IF NO EXPLICIT
; 1341	;	VALUE IS SPECIFIED.
; 1342	;	  "T" IS USED ON THE TIME FIELD TO SPECIFY THAT THE VALUE OF THE
; 1343	;	FIELD DEPENDS ON THE TIME PARAMETERS SELECTED FOR OTHER FIELDS.
; 1344	;	THE VALUE OF A FIELD WITH THIS SPECIFICATION DEFAULTS TO THE
; 1345	;	MAX OF <SUM OF THE T1 PARAMETERS DEFINED FOR FIELD/VALUES
; 1346	;	SPECIFIED IN THIS MICROINSTRUCTION>, <SUM OF THE T2 PARAMETERS
; 1347	;	FOR THIS MICROINSTRUCTION>, <J PARAMETER OF THIS FIELD>.
; 1348	;	WITHIN THE KL10 MICROCODE, T1 PARAMETERS ARE USED TO SPECIFY
; 1349	;	FUNCTIONS WHICH DEPEND ON THE ADDER SETUP TIME, AND T2 PARAMETERS
; 1350	;	ARE USED FOR FUNCTIONS WHICH REQUIRE ADDITIONAL TIME FOR CORRECT
; 1351	;	SELECTION OF THE NEXT MICROINSTRUCTION ADDRESS.
; 1352	;	  "P" IS USED ON THE PARITY FIELD TO SPECIFY THAT THE VALUE OF THE
; 1353	;	FIELD SHOULD DEFAULT SUCH THAT PARITY OF THE ENTIRE WORD
; 1354	;	IS ODD.  IF THIS OPTION IS SELECTED ON A FIELD WHOSE SIZE (K) IS
; 1355	;	ZERO, THE MICRO ASSEMBLER WILL ATTEMPT TO FIND A BIT SOMEWHERE
; 1356	;	IN THE WORD FOR WHICH NO VALUE IS SPECIFIED OR DEFAULTED.
; 1357	;	  "+" IS USED ON THE JUMP ADDRESS FIELD TO SPECIFY THAT THE DEFAULT
; 1358	;	JUMP ADDRESS IS THE ADDRESS OF THE NEXT INSTRUCTION ASSEMBLED (NOT,
; 1359	;	IN GENERAL, THE CURRENT LOCATION +1).
; 1360	;	IN GENERAL, A FIELD CORRESPONDS TO THE SET OF BITS WHICH PROVIDE
; 1361	; SELECT INPUTS FOR MIXERS OR DECODERS, OR CONTROLS FOR ALU'S.
; 1362	; EXAMPLES:
; 1363	;	AR/=<24:26>D,0	OR	AR/=0,3,26,D
; 1364	;	THE MICROCODE FIELD WHICH CONTROLS THE AR MIXER (AND THEREFORE
; 1365	; THE DATA TO BE LOADED INTO AR ON EACH EBOX CLOCK) IS THREE BITS WIDE
; 1366	; AND THE RIGHTMOST BIT IS SHOWN IN THE LISTING AS BIT 26 OF THE
; 1367	; MICROINSTRUCTION.  IF NO VALUE IS SPECIFICALLY REQUESTED FOR THE FIELD,
; 1368	; THE MICROASSEMBLER WILL ENSURE THAT THE FIELD IS 0.
; 1369	;	AD/=<12:17>	OR	AD/=0,6,17
; 1370	;	THE FIELD WHICH CONTROLS THE AD IS 6 BITS WIDE, ENDING ON
; 1371	; BIT 17.  THE FOURTH PARAMETER OF THE FIELD IS OMITTED, SO THE FIELD; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-1
; DEFINE.MIC[4,24]	16:58 23-May-86			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1372	; IS AVAILABLE TO THE MICROASSEMBLER (IF NO VALUE IS EXPLICITLY
; 1373	; CALLED OUT FOR THE FIELD) FOR MODIFICATION TO ENSURE ODD PARITY IN THE
; 1374	; ENTIRE WORD.
; 1375	;
; 1376	;(2)	VALUE DEFINITIONS
; 1377	;	FOLLOWING A FIELD DEFINITION, SYMBOLS MAY BE CREATED IN THAT
; 1378	; FIELD TO CORRESPOND TO VALUES OF THE FIELD.  THE FORM IS:
; 1379	;	SYMBOL=N,T1,T2
; 1380	;	"N" IS, IN OCTAL, THE VALUE OF SYMBOL WHEN USED IN THE FIELD.
; 1381	; T1 AND T2 ARE OPTIONAL, AND SPECIFY PARAMETERS IN THE TIME FIELD
; 1382	; CALCULATION FOR MICROINSTRUCTIONS IN WHICH THIS FIELD/SYMBOL IS USED.
; 1383	; THE MICROASSEMBLER COMPUTES THE SUMS OF ALL THE T1'S AND ALL THE T2'S
; 1384	; SPECIFIED FOR FIELD/SYMBOL SPECIFICATIONS IN A WORD, AND USES THE MAX
; 1385	; OF THE TWO SUMS AS THE DEFAULT VALUE FOR THE FIELD WHOSE DEFAULT
; 1386	; MECHANISM IS "T".  EXAMPLES:
; 1387	;	AD/=<12:17>	;FIELD DEFINITION IN WHICH FOLLOWING SYMBOLS EXIST
; 1388	;	XOR=31
; 1389	;	A+B=6,1
; 1390	;	HERE THE SYMBOLS "XOR" AND "A+B" ARE DEFINED FOR THE "AD" FIELD.
; 1391	; TO THE ASSEMBLER, THEREFORE, WRITING "AD/XOR" MEANS PUT THE VALUE 31
; 1392	; INTO THE 6-BIT FIELD ENDING ON BIT 17 OF THE MICROWORD.  THE SYMBOLS
; 1393	; ARE CHOSEN FOR MNEMONIC SIGNIFICANCE, OF COURSE, SO ONE READING
; 1394	; THE MICROCODE WOULD INTERPRET "AD/XOR" AS "THE OUTPUT OF AD SHALL BE THE
; 1395	; EXCLUSIVE OR OF ITS A AND B INPUTS".  SIMILIARLY, "AD/A+B" IS READ AS
; 1396	; "AD PRODUCES THE SUM OF A AND B".  THE SECOND PARAMETER IN THE DEFINITION
; 1397	; OF "A+B" IS A CONTROL TO THE MICRO ASSEMBLER'S TIME-FIELD CALCULATION,
; 1398	; WHICH TELLS THE ASSEMBLER THAT THIS OPERATION TAKES LONGER THAN THE
; 1399	; BASIC CYCLE, AND THEREFORE THAT THE TIME FIELD SHOULD BE INCREASED.
; 1400	;	AR/=<24:26>D,0	;FIELD DEFINITION FOR FOLLOWING SYMBOLS
; 1401	;	AR=0
; 1402	;	AD=2
; 1403	;	HERE THE SYMBOLS "AR" AND "AD" ARE DEFINED FOR THE FIELD NAMED
; 1404	; "AR", WHICH CONTROLS THE AR MIXER.  WE COULD WRITE AR/AR TO MEAN THAT
; 1405	; THE AR MIXER SELECT INPUTS WOULD BE 0, WHICH IN THE
; 1406	; HARDWARE SELECTS THE AR OUTPUT FOR RECIRCULATION TO THE REGISTER.  IN
; 1407	; PRACTICE, HOWEVER, WE WANT THIS TO BE THE DEFAULT CASE, SO THAT AR
; 1408	; DOES NOT CHANGE UNLESS WE SPECIFICALLY REQUEST IT, SO THE FIELD
; 1409	; DEFINITION SPECIFIES 0 AS THE DEFAULT VALUE OF THE FIELD.  IF WE
; 1410	; WANT AR LOADED FROM THE AD OUTPUT, WE WRITE "AR/AD" TO SET THE
; 1411	; MIXER SELECTS TO PASS THE AD OUTPUT INTO THE AR.
; 1412	;
; 1413	;(3)	LABEL DEFINITIONS
; 1414	;	A MICRO INSTRUCTION MAY BE LABELLED BY A SYMBOL FOLLOWED BY COLON
; 1415	; PRECEDING THE MICROINSTRUCTION DEFINITION.  THE ADDRESS OF THE
; 1416	; MICROINSTRUCTION BECOMES THE VALUE OF THE SYMBOL IN THE FIELD NAMED "J".
; 1417	; EXAMPLE:
; 1418	;	FOO:	J/FOO
; 1419	;	THIS IS A MICROINSTRUCTION WHOSE "J" FIELD (JUMP ADDRESS) CONTAINS
; 1420	; THE VALUE "FOO".  IT ALSO DEFINES THE SYMBOL "FOO" TO BE THE ADDRESS
; 1421	; OF ITSELF.  THEREFORE, IF EXECUTED BY THE MICROPROCESSOR, IT WOULD
; 1422	; LOOP ON ITSELF.
; 1423	;
; 1424	;(4)	COMMENTS
; 1425	;	A SEMICOLON ANYWHERE ON A LINE CAUSES THE REST OF THE LINE
; 1426	; TO BE IGNORED BY THE ASSEMBLER.  THIS TEXT IS AN EXAMPLE OF COMMENTS.
; 1427	;; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-2
; DEFINE.MIC[4,24]	16:58 23-May-86			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1428	;(5)	MICROINSTRUCTION DEFINITION
; 1429	;	A WORD OF MICROCODE IS DEFINED BY SPECIFYING A FIELD NAME,
; 1430	; FOLLOWED BY SLASH (/), FOLLOWED BY A VALUE.  THE VALUE MAY BE A
; 1431	; SYMBOL DEFINED FOR THAT FIELD, AN OCTAL DIGIT STRING, OR A DECIMAL
; 1432	; DIGIT STRING (DISTINGUISHED BY THE FACT THAT IT CONTAINS "8" AND/OR
; 1433	; "9" AND/OR IS TERMINATED BY A PERIOD). SEVERAL FIELDS MAY BE SPECIFIED
; 1434	; IN ONE MICROINSTRUCTION BY SEPARATING FIELD/VALUE SPECIFICATIONS WITH
; 1435	; COMMAS.  EXAMPLE:
; 1436	;	ADB/BR,ADA/AR,AD/A+B,AR/AD
; 1437	;	THE FIELD NAMED "ADB" IS GIVEN THE VALUE NAMED "BR" (TO
; 1438	; CAUSE THE MIXER ON THE B SIDE OF AD TO SELECT BR), FIELD "ADA" HAS VALUE
; 1439	; "AR", FIELD "AD" HAS VALUE "A+B", AND FIELD "AR" HAS VALUE "AD".
; 1440	;
; 1441	;(6)	CONTINUATION
; 1442	;	THE DEFINITION OF A MICROINSTRUCTION MAY CONTINUED ONTO TWO OR
; 1443	; MORE LINES BY BREAKING IT AFTER ANY COMMA.  IN OTHER WORDS, IF THE
; 1444	; LAST NON-BLANK, NON-COMMENT CHARACTER ON A LINE IS A COMMA, THE
; 1445	; INSTRUCTION SPECIFICATION IS CONTINUED ON THE FOLLOWING LINE.
; 1446	; EXAMPLE:
; 1447	;	ADB/BR,ADA/AR,		;SELECT AR & BR AS AD INPUTS
; 1448	;		AD/A+B,AR/AD	;TAKE THE SUM INTO AR
; 1449	; BY CONVENTION, CONTINUATION LINES ARE INDENTED AN EXTRA TAB.
; 1450	;
; 1451	;(7)	MACROS
; 1452	;	A MACRO IS A SYMBOL WHOSE VALUE IS ONE OR MORE FIELD/VALUE
; 1453	; SPECIFICATIONS AND/OR MACROS.  A MACRO DEFINITION IS A LINE CONTAINING
; 1454	; THE MACRO NAME FOLLOWED BY A QUOTED STRING WHICH IS THE VALUE OF THE
; 1455	; MACRO.  EXAMPLE:
; 1456	;	AR_AR+BR	"ADB/BR,ADA/AR,AD/A+B,AR/AD"
; 1457	; THE APPEARANCE OF A MACRO IN A MICROINSTRUCTION DEFINITION IS EQUIVALENT
; 1458	; TO THE APPEARANCE OF ITS VALUE.  MACROS FOR VARIOUS FUNCTIONS
; 1459	; ARE DEFINED IN "MACRO.MIC".
; 1460	;
; 1461	;(8)	PSEUDO OPS
; 1462	;	THE MICRO ASSEMBLER HAS 10 PSEUDO-OPERATORS:
; 1463	;.DCODE AND .UCODE SELECT THE RAM INTO WHICH SUBSEQUENT MICROCODE WILL
; 1464	;BE LOADED, AND THEREFORE THE FIELD DEFINITIONS AND MACROS WHICH ARE
; 1465	;MEANINGFUL IN SUBSEQUENT MICROCODE
; 1466	;.TITLE DEFINES A STRING OF TEXT TO APPEAR IN THE PAGE HEADER, AND
; 1467	;.TOC DEFINES AN ENTRY FOR THE TABLE OF CONTENTS AT THE BEGINNING.
; 1468	;.SET DEFINES THE VALUE OF A CONDITIONAL ASSEMBLY PARAMETER,
; 1469	;.CHANGE REDEFINES A CONDITIONAL ASSEMBLY PARAMETER,
; 1470	;.DEFAULT ASSIGNS A VALUE TO AN UNDEFINED PARAMETER.
; 1471	;.IF ENABLES ASSEMBLY IF THE VALUE OF THE PARAMETER IS NOT ZERO,
; 1472	;.IFNOT ENABLES ASSEMBLY IF THE PARAMETER VALUE IS ZERO, AND
; 1473	;.ENDIF RE-ENABLES ASSEMBLY IF SUPPRESSED BY THE PARAMETER NAMED.
; 1474	;
; 1475	;(9)	LOCATION CONTROL
; 1476	;	A MICROINSTRUCTION "LABELLED" WITH A NUMBER IS ASSIGNED TO THAT
; 1477	; ADDRESS.
; 1478	;	THE CHARACTER "=" AT THE BEGINNING OF A LINE, FOLLOWED BY
; 1479	; A STRING OF 0'S, 1'S, AND/OR *'S, SPECIFIES A CONSTRAINT ON THE
; 1480	; ADDRESS OF FOLLOWING MICROINSTRUCTIONS.  THE NUMBER OF CHARACTERS
; 1481	; IN THE CONSTRAINT STRING (EXCLUDING THE "=") IS THE NUMBER OF LOW-ORDER
; 1482	; BITS CONSTRAINED IN THE ADDRESS.  THE MICROASSEMBLER ATTEMPTS TO FIND
; 1483	; AN UNUSED LOCATION WHOSE ADDRESS HAS 0 BITS IN THE POSITIONS; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-3
; DEFINE.MIC[4,24]	16:58 23-May-86			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1484	; CORRESPONDING TO 0'S IN THE CONSTRAINT STRING AND 1 BITS WHERE THE
; 1485	; CONSTRAINT HAS 1'S.  ASTERISKS DENOTE "DON'T CARE" BIT POSITIONS.
; 1486	;	IF THERE ARE ANY 0'S IN THE CONSTRAINT STRING, THE CONSTRAINT
; 1487	; IMPLIES A BLOCK OF <2**N> MICROWORDS, WHERE N IS THE NUMBER OF 0'S
; 1488	; IN THE STRING.  ALL LOCATIONS IN THE BLOCK WILL HAVE 1'S IN THE ADDRESS
; 1489	; BITS CORRESPONDING TO 1'S IN THE STRING, AND BIT POSITIONS DENOTED BY *'S
; 1490	; WILL BE THE SAME IN ALL LOCATIONS OF THE BLOCK.
; 1491	;	IN SUCH A CONSTRAINT BLOCK, THE DEFAULT ADDRESS PROGRESSION IS
; 1492	; COUNTING IN THE "0" POSITIONS OF THE CONSTRAINT STRING, BUT A NEW
; 1493	; CONSTRAINT STRING OCCURING WITHIN A BLOCK MAY FORCE SKIPPING OVER
; 1494	; SOME LOCATIONS OF THE BLOCK.  WITHIN A BLOCK, A NEW CONSTRAINT
; 1495	; STRING DOES NOT CHANGE THE PATTERN OF DEFAULT ADDRESS PROGRESSION, IT
; 1496	; MERELY ADVANCES THE LOCATION COUNTER OVER THOSE LOCATIONS.  THE
; 1497	; MICROASSEMBLER WILL LATER FILL THEM IN.
; 1498	;	A NULL CONSTRAINT STRING ("=" FOLLOWED BY ANYTHING BUT "0",
; 1499	; "1", OR "*") SERVES TO TERMINATE A CONSTRAINT BLOCK.
; 1500	; EXAMPLES:
; 1501	;	=0
; 1502	;	THIS SPECIFIES THAT THE LOW-ORDER ADDRESS BIT MUST BE ZERO--
; 1503	; THE MICROASSEMBLER FINDS AN EVEN-ODD PAIR OF LOCATIONS, AND PUTS
; 1504	; THE NEXT TWO MICROINSTRUCTIONS INTO THEM.
; 1505	;	=11
; 1506	;	THIS SPECIFIES THAT THE TWO LOW-ORDER BITS OF THE ADDRESS MUST
; 1507	; BOTH BE ONES.  SINCE THERE ARE NO 0'S IN THIS CONSTRAINT, THE
; 1508	; ASSEMBLER FINDS ONLY ONE LOCATION MEETING THE CONSTRAINT.
; 1509	;	=0*****
; 1510	;	THIS SPECIFIES AN ADDRESS IN WHICH THE "40" BIT IS ZERO.  DUE
; 1511	; TO THE IMPLEMENTATION OF THIS FEATURE IN THE ASSEMBLER,  THE DEFAULT
; 1512	; ADDRESS PROGRESSION APPLIES ONLY TO THE LOW-ORDER 5 BITS, SO THIS
; 1513	; CONSTRAINT FINDS ONE WORD IN WHICH THE "40" BIT IS ZERO, AND DOES
; 1514	; NOT ATTEMPT TO FIND ONE IN WHICH THAT BIT IS A ONE.
; 1515	;THIS LIMITATION HAS BEEN CHANGED WITH NEWER ASSEMBLER VERSIONS.
; 1516	;HOWEVER NONE OF THE LOCATIONS IN THE MICROCODE REQUIRE ANYTHING BUT THE
; 1517	;CONSTRAINT MENTIONED ABOVE.
; 1518	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; DEFINE.MIC[4,24]	16:58 23-May-86			MICROCODE LISTING TEMPLATE				

; 1519	.TOC	"MICROCODE LISTING TEMPLATE"
; 1520	;HERE IS A TEMPLATE WHICH CAN BE USED WITH THE MICROCODE
; 1521	; LISTING TO IDENTIFY FIELDS IN THE OUTPUT --
; 1522	
; 1523	
; 1524	; ----  ---- ---- ---- ---- ---- ---- ----
; 1525	; [--]  [--] []!! !!!! !!!! !![] [][] ![-]
; 1526	;   !     !   !!! !!!! !!!! !! !  ! ! ! + # = MAGIC NUMBERS
; 1527	;   !     !   !!! !!!! !!!! !! !  ! ! + MARK = SCOPE SYNC
; 1528	;   !     !   !!! !!!! !!!! !! !  ! !
; 1529	;   !     !   !!! !!!! !!!! !! !  ! + CALL, DISP/SPEC = SPEC FUNCTIONS
; 1530	;   !     !   !!! !!!! !!!! !! !  + SKIP/COND = SPECIAL FUNCTIONS
; 1531	;   !     !   !!! !!!! !!!! !! !
; 1532	;   !     !   !!! !!!! !!!! !! + TIME, MEM = UINST TIME & MEM FUNCTION
; 1533	;   !     !   !!! !!!! !!!! !+ VMA = VMA INPUT SELECT
; 1534	;   !     !   !!! !!!! !!!! + SH/ARMM = SH FUNCTION / ARMM SELECT
; 1535	;   !     !   !!! !!!! !!!!
; 1536	;   !     !   !!! !!!! !!!+ SC, FE = SC INPUT SELECT & FE LOAD
; 1537	;   !     !   !!! !!!! !!+ SCADB = SELECT FOR SCAD "B" INPUT
; 1538	;   !     !   !!! !!!! !+ SCADA = ENABLE AND SELECT FOR SCAD "A" INPUT
; 1539	;   !     !   !!! !!!! + SCAD = SC/FE ADDER FUNCTION
; 1540	;   !     !   !!! !!!!
; 1541	;   !     !   !!! !!!+ FM ADR = FAST MEMORY ADDRESS SELECT
; 1542	;   !     !   !!! !!+ BR, BRX, MQ = LOAD BR & BRX, SEL FOR MQ
; 1543	;   !     !   !!! !+ ARX = SELECT FOR ARX INPUT
; 1544	;   !     !   !!! + AR = SELECT FOR AR INPUT
; 1545	;   !     !   !!!
; 1546	;   !     !   !!+ ADB = SELECT FOR ADDER "B" INPUT
; 1547	;   !     !   !+ ADA = SELECT AND ENABLE FOR ADDER "A" INPUT
; 1548	;   !     !   + AD = OPERATION IN ADDER AND ADDER EXTENSION
; 1549	;   !     !
; 1550	;   !     + J = BASE ADDRESS TO WHICH THIS MICROINSTRUCTION JUMPS
; 1551	;   !
; 1552	;   + LOCATION IN CRAM INTO WHICH THIS WORD IS LOADED
; 1553	;
; 1554	; U/V = MICRO INSTRUCTION FOR CRAM
; 1555	
; 1556	;*******************************************************************
; 1557	
; 1558	; D = WORD FOR DRAM
; 1559	;
; 1560	;   + LOCATION IN DRAM INTO WHICH THIS WORD IS LOADED
; 1561	;   !
; 1562	;   !   + A = OPERAND ACCESS CONTROL
; 1563	;   !   !+ B = INSTRUCTION "MODE"
; 1564	;   !   !! + P = PARITY FOR THIS WORD
; 1565	;   !   !! !
; 1566	;   !   !! !   + J = ADDRESS OF HANDLER FOR THIS INSTRUCTION
; 1567	; [--]  !! ! [--]
; 1568	; ----  ---- ----
; 1569	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; DEFINE.MIC[4,24]	16:58 23-May-86			KL10 INSTRUCTION OPCODE MAP				

; 1570	.TOC	"KL10 INSTRUCTION OPCODE MAP"
; 1571	
; 1572	;	0	1	2	3	4	5	6	7
; 1573	;100	(UUO)	(UUO)	GFAD	GFSB	JSYS	ADJSP	GFMP	GFDV
; 1574	;110	DFAD	DFSB	DFMP	DFDV	DADD	DSUB	DMUL	DDIV
; 1575	;120	DMOVE	DMOVN	FIX	EXTEND	DMOVEM	DMOVNM	FIXR	FLTR
; 1576	;130	(UFA)	(DFN)	FSC	IBP	ILDB	LDB	IDPB	DPB
; 1577	;140	FAD	(FADL)	FADM	FADB	FADR	FADRI	FADRM	FADRB
; 1578	;150	FSB	(FSBL)	FSBM	FSBB	FSBR	FSBRI	FSBRM	FSBRB
; 1579	;160	FMP	(FMPL)	FMPM	FMPB	FMPR	FMPRI	FMPRM	FMPRB
; 1580	;170	FDV	(FDVL)	FDVM	FDVB	FDVR	FDVRI	FDVRM	FDVRB
; 1581	;	0	1	2	3	4	5	6	7
; 1582	;200	MOVE	MOVEI	MOVEM	MOVES	MOVS	MOVSI	MOVSM	MOVSS
; 1583	;210	MOVN	MOVNI	MOVNM	MOVNS	MOVM	MOVMI	MOVMM	MOVMS
; 1584	;220	IMUL	IMULI	IMULM	IMULB	MUL	MULI	MULM	MULB
; 1585	;230	IDIV	IDIVI	IDIVM	IDIVB	DIV	DIVI	DIVM	DIVB
; 1586	;240	ASH	ROT	LSH	JFFO	ASHC	ROTC	LSHC	(UUO)
; 1587	;250	EXCH	BLT	AOBJP	AOBJN	JRST	JFCL	XCT	MAP
; 1588	;260	PUSHJ	PUSH	POP	POPJ	JSR	JSP	JSA	JRA
; 1589	;270	ADD	ADDI	ADDM	ADDB	SUB	SUBI	SUBM	SUBB
; 1590	;	0	1	2	3	4	5	6	7
; 1591	;300	CAI	CAIL	CAIE	CAILE	CAIA	CAIGE	CAIN	CAIG
; 1592	;310	CAM	CAML	CAME	CAMLE	CAMA	CAMGE	CAMN	CAMG
; 1593	;320	JUMP	JUMPL	JUMPE	JUMPLE	JUMPA	JUMPGE	JUMPN	JUMPG
; 1594	;330	SKIP	SKIPL	SKIPE	SKIPLE	SKIPA	SKIPGE	SKIPN	SKIPG
; 1595	;340	AOJ	AOJL	AOJE	AOJLE	AOJA	AOJGE	AOJN	AOJG
; 1596	;350	AOS	AOSL	AOSE	AOSLE	AOSA	AOSGE	AOSN	AOSG
; 1597	;360	SOJ	SOJL	SOJE	SOJLE	SOJA	SOJGE	SOJN	SOJG
; 1598	;370	SOS	SOSL	SOSE	SOSLE	SOSA	SOSGE	SOSN	SOSG
; 1599	;	0	1	2	3	4	5	6	7
; 1600	;400	SETZ	SETZI	SETZM	SETZB	AND	ANDI	ANDM	ANDB
; 1601	;410	ANDCA	ANDCAI	ANDCAM	ANDCAB	SETM	SETMI	SETMM	SETMB
; 1602	;420	ANDCM	ANDCMI	ANDCMM	ANDCMB	SETA	SETAI	SETAM	SETAB
; 1603	;430	XOR	XORI	XORM	XORB	IOR	IORI	IORM	IORB
; 1604	;440	ANDCB	ANDCBI	ANDCBM	ANDCBB	EQV	EQVI	EQVM	EQVB
; 1605	;450	SETCA	SETCAI	SETCAM	SETCAB	ORCA	ORCAI	ORCAM	ORCAB
; 1606	;460	SETCM	SETCMI	SETCMM	SETCMB	ORCM	ORCMI	ORCMM	ORCMB
; 1607	;470	ORCB	ORCBI	ORCBM	ORCBB	SETO	SETOI	SETOM	SETOB
; 1608	;	0	1	2	3	4	5	6	7
; 1609	;500	HLL	HLLI	HLLM	HLLS	HRL	HRLI	HRLM	HRLS
; 1610	;510	HLLZ	HLLZI	HLLZM	HLLZS	HRLZ	HRLZI	HRLZM	HRLZS
; 1611	;520	HLLO	HLLOI	HLLOM	HLLOS	HRLO	HRLOI	HRLOM	HRLOS
; 1612	;530	HLLE	HLLEI	HLLEM	HLLES	HRLE	HRLEI	HRLEM	HRLES
; 1613	;540	HRR	HRRI	HRRM	HRRS	HLR	HLRI	HLRM	HLRS
; 1614	;550	HRRZ	HRRZI	HRRZM	HRRZS	HLRZ	HLRZI	HLRZM	HLRZS
; 1615	;560	HRRO	HRROI	HRROM	HRROS	HLRO	HLROI	HLROM	HLROS
; 1616	;570	HRRE	HRREI	HRREM	HRRES	HLRE	HLREI	HLREM	HLRES
; 1617	;	0	1	2	3	4	5	6	7
; 1618	;600	TRN	TLN	TRNE	TLNE	TRNA	TLNA	TRNN	TLNN
; 1619	;610	TDN	TSN	TDNE	TSNE	TDNA	TSNA	TDNN	TSNN
; 1620	;620	TRZ	TLZ	TRZE	TLZE	TRZA	TLZA	TRZN	TLZN
; 1621	;630	TDZ	TSZ	TDZE	TSZE	TDZA	TSZA	TDZN	TSZN
; 1622	;640	TRC	TLC	TRCE	TLCE	TRCA	TLCA	TRCN	TLCN
; 1623	;650	TDC	TSC	TDCE	TSCE	TDCA	TSCA	TDCN	TSCN
; 1624	;660	TRO	TLO	TROE	TLOE	TROA	TLOA	TRON	TLON
; 1625	;670	TDO	TSO	TDOE	TSOE	TDOA	TSOA	TDON	TSON; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- J, AD			

; 1626	.TOC	"CONTROL RAM DEFINITIONS -- J, AD"
; 1627	;FIELDS ARRANGED FOR READABILITY, NOT COMPACTNESS
; 1628	; IN THE PROCESSOR, BITS ARE SCATTERED IN ANOTHER ORDER
; 1629	
; 1630	U0/=<0:0>D,0	;BIT 0 UNUSED
; 1631	J/=<1:11>+	;SYMBOLS WILL BE DEFINED BY TAGS (CRA1&CRA2)
; 1632	
; 1633	;MAIN ADDER CONTROLS.  Bit 0 = carry in, bit 1 = boolean operation
; 1634	; Bits 2-5 are S8-S1 of the 10181 ALU chip.  For normal arithmetic,
; 1635	; the AD and ADX are separated unless SPEC/AD LONG or equivalent is given.
; 1636	
; 1637	
; 1638	AD/=<12:17>	; (DP03, EXCEPT CARRY IN, ON CTL1)
; 1639		A+1=40,1
; 1640		A+XCRY=00,1
; 1641	;	A+ANDCB=01,1
; 1642	;	A+AND=02,1
; 1643		A*2=03,1
; 1644		A*2+1=43,1
; 1645	;	OR+1=44,1
; 1646	;	OR+ANDCB=05,1
; 1647		A+B=06,1
; 1648		A+B+1=46,1
; 1649	;	A+OR=07,1
; 1650		ORCB+1=50,1
; 1651		A-B-1=11,1
; 1652		A-B=51,1
; 1653	;	AND+ORCB=52,1
; 1654	;	A+ORCB=53,1
; 1655		XCRY-1=54,1
; 1656	;	ANDCB-1=15,1
; 1657	;	AND-1=16,1
; 1658		A-1=17,1
; 1659			;ADDER LOGICAL FUNCTIONS
; 1660		SETCA=20
; 1661		ORC=21		;NAND
; 1662		ORCA=22
; 1663		1S=23
; 1664		ANDC=24		;NOR
; 1665		NOR=24
; 1666		SETCB=25
; 1667		EQV=26
; 1668		ORCB=27
; 1669		ANDCA=30
; 1670		XOR=31
; 1671		B=32
; 1672		OR=33
; 1673		0S=34
; 1674		ANDCB=35
; 1675		AND=36
; 1676		A=37
; 1677			;BOOLEAN FUNCTIONS FOR WHICH CRY0 IS INTERESTING
; 1678		CRY A EQ -1=60,1	;GENERATE CRY0 IF A=1S, AD=SETCA
; 1679		CRY A.B#0=36,1		;CRY 0 IF A&B NON-ZERO, AD=AND
; 1680		CRY A#0=37,1		;GENERATE CRY0 IF A .NE. 0, AD=A
; 1681		CRY A GE B=71,1		;CRY0 IF A .GE. B, UNSIGNED; AD=XOR; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- DATA PATH MIXERS		

; 1682	.TOC	"CONTROL RAM DEFINITIONS -- DATA PATH MIXERS"
; 1683	
; 1684	ADA/=<18:20>		; (DP03)
; 1685		AR=0
; 1686		ARX=1
; 1687		MQ=2
; 1688		PC=3
; 1689	ADA EN/=<18:18>		;ADA ENABLE ALSO ENABLES ADXA (DP03)
; 1690		EN=0
; 1691		0S=1
; 1692	U21/=<21:21>D,0		;BIT 21 UNUSED
; 1693	ADB/=<22:23>		;CONTROLS ADB AND ADXB (DP03)
; 1694		FM=0,,1		;MUST HAVE TIME FOR PARITY CHECK
; 1695		BR*2=1		;ADB35 is BRX0; ADXB35 is 0
; 1696		BR=2
; 1697		AR*4=3		;ADB34,35 are ARX0,1; ADXB34,35 are 0
; 1698	U23/=<23:23>D,1		;PREVENT DEFAULT SELECTION OF FM
; 1699				;FORCE IT TO TAKE ONE OF THE SHORTER
; 1700				;PATHS IF FM NOT NEEDED ALSO DISABLES
; 1701				;PARITY CHECKING LOGIC
; 1702	
; 1703	;REGISTER INPUTS
; 1704	
; 1705	AR/=<24:26>D,0		; (DP01)
; 1706		AR=0
; 1707		ARMM=0		;REQUIRES SPECIAL FUNCTION
; 1708		MEM=0		;[346] MB WAIT will poke to 1 (CACHE) or 2 (AD)
; 1709		CACHE=1		;ORDINARILY SELECTED BY HWARE
; 1710		AD=2
; 1711		EBUS=3
; 1712		SH=4
; 1713		AD*2=5		;Low bit from ADX0
; 1714		ADX=6
; 1715		AD*.25=7
; 1716	ARX/=<27:29>D,0		; (DP02)
; 1717		ARX=0		;[345] BY DEFAULT
; 1718		MEM=0		;[346] Gets poked by MB WAIT to 1 or 2
; 1719		CACHE=1		;ORDINARILY BY MBOX RESP
; 1720		AD=2
; 1721		MQ=3
; 1722		SH=4
; 1723		ADX*2=5		;Low bit from MQ0
; 1724		ADX=6
; 1725		ADX*.25=7	;High bits from AD34,35
; 1726	BR/=<30:30>D,0		;DEFAULT TO RECIRCULATE (DP04)
; 1727		AR=1
; 1728	BRX/=<31:31>D,0		;DEFAULT TO RECIRCULATE (DP04)
; 1729		ARX=1
; 1730	MQ/=<32:32>D,0		;DEFAULT TO RECIRCULATE (DP02)
; 1731		SH=1		;LOAD FROM SHIFT MATRIX
; 1732		MQ*2=0		;With SPEC/MQ SHIFT--Low bit from AD CRY -2
; 1733		MQ*.25=1	;With SPEC/MQ SHIFT--High bits from ADX34, ADX35
; 1734		MQ SEL=0	;WITH COND/REG CTL
; 1735		MQM SEL=1	;WITH COND/REG CTL
; 1736	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- DATA PATH MIXERS		

; 1737	;FMADR SELECTS THE SOURCE OF THE FAST MEMORY ADDRESS,
; 1738	; RATHER THAN PROVIDING THE ADDRESS ITSELF
; 1739	
; 1740	FMADR/=<33:35>		; (APR4&APR5)
; 1741		AC0=0		;IR 9-12
; 1742		AC1=1		;<IR 9-12>+1 MOD 16
; 1743		XR=2		;ARX 14-17
; 1744		VMA=3		;VMA 32-35
; 1745		AC2=4		;<IR 9-12>+2 MOD 16
; 1746		AC3=5		;<IR 9-12>+3 MOD 16
; 1747		AC+#=6		;CURRENT BLOCK, AC+ MAGIC #
; 1748		#B#=7		;BLOCK AND AC SELECTED BY # FIELD
; 1749	
; 1750	.TOC	"CONTROL RAM DEFINITIONS -- 10-BIT LOGIC"
; 1751	
; 1752	SCAD/=<36:38>		; (SCD1)
; 1753		A=0
; 1754		A-B-1=1
; 1755		A+B=2
; 1756		A-1=3
; 1757		A+1=4
; 1758		A-B=5
; 1759		OR=6
; 1760		AND=7
; 1761	SCADA/=<39:41>		; (SCD1)
; 1762		FE=0
; 1763		AR0-5=1		;BYTE POINTER P FIELD
; 1764		AR EXP=2	;<AR 01-08> XOR <AR 00>
; 1765		#=3		;SIGN EXTENDED WITH #00
; 1766	SCADA EN/=<39:39>	; (SCD1)
; 1767		0S=1
; 1768	U42/=<42:42>D,0	;BIT 42 UNUSED
; 1769	SCADB/=<43:44>		; (SCD1)
; 1770		SC=0
; 1771		AR6-11=1	;BYTE POINTER S FIELD
; 1772		AR0-8=2
; 1773		#=3		;NO SIGN EXTENSION
; 1774	U45/=<45:45>D,0		;BIT 45 UNUSED
; 1775	SC/=<46:46>D,0		;RECIRCULATE BY DEFAULT (SCD2)
; 1776		FE=0		;WITH SCM ALT
; 1777		SCAD=1
; 1778		AR SHIFT=1	;WITH SCM ALT ;AR 18, 28-35
; 1779	FE/=<47:47>D,0		;RECIRCULATE BY DEFAULT (SCD2)
; 1780		SCAD=1
; 1781	U48/=<48:48>D,0		;BIT 48 UNUSED
; 1782	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- SHIFT, ARMM, VMA, TIME	

; 1783	.TOC	"CONTROL RAM DEFINITIONS -- SHIFT, ARMM, VMA, TIME"
; 1784	
; 1785	SH/=<49:50>		; (SH1)
; 1786		SHIFT AR!ARX=0	;LEFT BY (SC)
; 1787		AR=1
; 1788		ARX=2
; 1789		AR SWAP=3	;HALVES SWAPPED
; 1790	ARMM/=<49:50>		;SAME BITS AS SH CONTROL (SCD3)
; 1791		#=0		;MAGIC # 0-8 TO AR 0-8
; 1792		EXP_SIGN=1	;AR1-8 _ AR0
; 1793		SCAD EXP=2	;AR0-8_SCAD
; 1794		SCAD POS=3	;AR0-5_SCAD
; 1795	VMAX/=<49:50>		;SAME BITS AS SH CONTROL (VMA4)
; 1796		VMAX=0		;VMA SECTION #
; 1797		PC SEC=1	;PC SECTION #
; 1798		PREV SEC=2	;PREVIOUS CONTEXT SECT
; 1799		AD12-17=3
; 1800	U51/=<51:51>D,0		;BIT 51 UNUSED
; 1801	VMA/=<52:53>D,0		;ALSO CONTROLLED BY SPECIAL FUNCTIONS
; 1802		VMA=0		;BY DEFAULT
; 1803		PC=1		;MAY BE OVERRIDDEN BY MCL LOGIC	TO LOAD FROM AD
; 1804		LOAD=1		; IF WE KNOW IT WILL BE OVERRIDDEN, USE THIS
; 1805		PC+1=2
; 1806		AD=3		;ENTIRE VMA, INCLUDING SECTION
; 1807	TIME/=<54:55>T		;CONTROLS MINIMUM MICROINSTRUCTION EXECUTION
; 1808				; TIME, COUNTING MBOX CLOCK TICKS (CLK)
; 1809				;ASSEMBLER GENERALLY TAKES CARE OF THIS
; 1810		2T=0		;2 TICKS
; 1811		3T=1		;3 TICKS
; 1812		4T=2		;4 TICKS
; 1813		5T=3		;5 TICKS (COND/DIAG FUNC & #00, --> .5 USEC)
; 1814	
; 1815	.TOC	"CONTROL RAM DEFINITIONS -- MEM SPECIAL FUNCTIONS"
; 1816	
; 1817	MEM/=<56:59>D,0		;(MCL1, except MB WAIT on CON5 and CLK4)
; 1818	;
; 1819	;	Note:  MB WAIT is implicit whenever bit 58 is set.
; 1820	;
; 1821	;	NOP=0		;DEFAULT
; 1822		ARL IND=1	;CONTROL AR LEFT MUX FROM # FIELD
; 1823		MB WAIT=2	;WAIT FOR MBOX RESP IF PENDING
; 1824		RESTORE VMA=3	;AD FUNC WITHOUT GENERATING A REQUEST
; 1825		A RD=4		;OPERAND READ and load PXCT bits
; 1826		B WRITE=5	;CONDITIONAL WRITE ON DRAM B 01
; 1827		FETCH=6		;LOAD NEXT INSTR TO ARX (CONTROL BY #)
; 1828		REG FUNC=7	;MBOX REGISTER FUNCTIONS
; 1829		AD FUNC=10	;FUNCTION LOADED FROM AD LEFT
; 1830		EA CALC=11	;FUNCTION DECODED FROM # FIELD
; 1831		LOAD AR=12
; 1832		LOAD ARX=13
; 1833		RW=14		;READ, TEST WRITABILITY
; 1834		RPW=15		;READ-PAUSE-WRITE
; 1835		WRITE=16	;FROM AR TO MEMORY
; 1836		IFET=17		;UNCONDITIONAL instruction FETCH
; 1837	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS	

; 1838	.TOC	"CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS"
; 1839	
; 1840	SKIP/=<60:65>D,0	;MICRO-PROGRAM SKIPS
; 1841				; 40-57 DECODED ON (CRA2)
; 1842	;	SPARE=40
; 1843		EVEN PAR=41,,1	;AR PARITY IS EVEN
; 1844		BR0=42		;BR BIT 00
; 1845		ARX0=43		;ARX BIT 00
; 1846		AR18=44		;AR BIT 18
; 1847		AR0=45		;AR BIT 00
; 1848		AC#0=46		;IR9-12 .EQ. 0
; 1849		SC0=47		;SC BIT 00
; 1850		PC SEC0=50
; 1851		SCAD0=51,,1	;Sign of SCAD output--WRONG ON OVERFLOW FROM BIT 1!!!
; 1852		SCAD#0=52,,1	;SCAD OUTPUT IS NON-ZERO
; 1853		ADX0=53,1	;ADDER EXTENSION BIT 00
; 1854		AD CRY0=54,1	;CARRY OUT OF AD BIT -2 (BOOLE IGNORED)
; 1855		AD0=55,1	;ADDER BIT 00
; 1856		AD#0=56,1	;AD BITS 00-35 CONTAIN SOME ONES
; 1857		-LOCAL AC ADDR=57	;VMA18-31 =0 ON LOCAL REF IN SEC >1
; 1858	
; 1859				; 60-77 DECODED ON (CON2)
; 1860		FETCH=60	;VMA FETCH (LAST CYCLE WAS A FETCH)
; 1861		KERNEL=61	;PC IS IN KERNEL MODE
; 1862		USER=62		;PC IS IN USER MODE
; 1863		PUBLIC=63	;PC IS PUBLIC (INCLUDING SUPER)
; 1864		RPW REF=64	;MIDDLE OF READ-PAUSE-WRITE CYCLE
; 1865		PI CYCLE=65	;PI CYCLE IN PROGRESS
; 1866		-EBUS GRANT=66	;PI HASN'T RELEASED BUS FOR CPU USE
; 1867		-EBUS XFER=67	;NO TRANSFER RECIEVED FROM DEVICE
; 1868		INTRPT=70	;AN INTERRUPT REQUEST WAITING FOR SERVICE
; 1869		-START=71	;NO CONTINUE BUTTON
; 1870		RUN=72		;PROCESSOR NOT HALTED
; 1871		IO LEGAL=73	;KERNEL, PI CYCLE, USER IOT, OR DEVICE .GE. 740
; 1872		P!S XCT=74	;PXCT OR SXCT
; 1873		-VMA SEC0=75	;VMA SECTION NUMBER (13-17) IS NOT ZERO
; 1874		AC REF=76,,1	;VMA .LT.20 ON READ OR WRITE
; 1875		-MTR REQ=77	;INTERRUPT REQUEST NOT DUE TO METER
; 1876	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS	

; 1877	;SKIP/COND FIELD CONTINUED
; 1878	
; 1879	COND/=<60:65>D,0	;NON-SKIP SPECIAL FUNCTIONS
; 1880				;0-7 DECODED ON (CTL2)
; 1881	;	NOP=0		;BY DEFAULT
; 1882		LD AR0-8=1
; 1883		LD AR9-17=2	;Gates VMAX into ARMM (see VMA4)
; 1884		LD AR18-35=3
; 1885		AR CLR=4
; 1886		ARX CLR=5
; 1887		ARL IND=6	;CONTROL AR LEFT, CALL, AND CLEAR BITS FROM #
; 1888		REG CTL=7	;CONTROL AR LOAD, EXP TST, AND MQ FROM #
; 1889				; 10-37 DECODED ON (CON1)
; 1890		FM WRITE=10	;WRITE AR INTO CURRENTLY ADDRESSED FM LOC
; 1891		PCF_#=11	;SET PC FLAGS FROM # FIELD
; 1892		FE SHRT=12	;SHIFT FE RIGHT 1
; 1893		AD FLAGS=13	;SET PC CRY0, CRY1, OVRFLO, TRAP1 AS APPROPRIATE
; 1894		LOAD IR=14	;LATCH AD OR CACHE DATA INTO IR, load PXCT bits
; 1895		SPEC INSTR=15	;SET/CLR SXCT, PXCT, PICYC, TRAP INSTR FLAGS
; 1896		SR_#=16		;CONTROL FOR STATE REGISTER and PXCT bits (CON3, MCL4)
; 1897		SEL VMA=17	;READ VMA THROUGH ADA/PC
; 1898		DIAG FUNC=20	;SELECT DIAGNOSTIC INFO ONTO EBUS
; 1899		EBOX STATE=21	;SET STATE FLOPS
; 1900		EBUS CTL=22	;I/O FUNCTIONS
; 1901		MBOX CTL=23
; 1902	;	SPARE=24
; 1903		LONG EN=25	;THIS WORD CAN BE INTERPRETED AS LONG INDIRECT
; 1904	;	SPARE=26
; 1905	;	SPARE=27
; 1906		VMA_#=30
; 1907		VMA_#+TRAP=31
; 1908		VMA_#+MODE=32
; 1909		VMA_#+AR32-35=33
; 1910		VMA_#+PI*2=34
; 1911		VMA DEC=35	;VMA_VMA-1
; 1912		VMA INC=36	;VMA_VMA+1
; 1913		LD VMA HELD=37	;HOLD VMA ON SIDE
; 1914	
; 1915	CALL/=<66:66>D,0	;CALL function--May not coexist with DISP/RETURN
; 1916		CALL=1		;GOOD TO 15 LEVELS IN MODEL B
; 1917	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- DISP/SPEC SPECIAL FUNCTIONS	

; 1918	.TOC	"CONTROL RAM DEFINITIONS -- DISP/SPEC SPECIAL FUNCTIONS"
; 1919	
; 1920	DISP/=<67:71>D,10	;0-7 AND 30-37 ARE DISPATCHES (CRA1&CRA2)
; 1921		DIAG=0
; 1922		DRAM J=1
; 1923		DRAM A RD=2	;IMPLIES INH CRY18
; 1924		RETURN=3	;POPJ return--may not coexist with CALL
; 1925		PG FAIL=4	;PAGE FAIL TYPE DISP
; 1926		SR=5		;16 WAYS ON STATE REGISTER
; 1927		NICOND=6	;NEXT INSTRUCTION CONDITION (see NEXT for detail)
; 1928		SH0-3=7,,1	;[337] 16 WAYS ON HIGH-ORDER BITS OF SHIFTER
; 1929		MUL=30		;FE0*4 + MQ34*2 + MQ35; implies MQ SHIFT, AD LONG
; 1930		DIV=31,,1	;FE0*4 + BR0*2 + AD CRY0; implies MQ SHIFT, AD LONG
; 1931		SIGNS=32,1	;ARX0*8 + AR0*4 + BR0*2 + AD0
; 1932		DRAM B=33	;8 WAYS ON DRAM B FIELD
; 1933		BYTE=34,,1	;FPD*4 + AR12*2 + SCAD0--WRONG ON OVERFLOW FROM BIT 1!!
; 1934		NORM=35,2	;See normalization for details. Implies AD LONG
; 1935		EA MOD=36	;(ARX0 or -LONG EN)*8 + -(LONG EN and ARX1)*4 +
; 1936				;ARX13*2 + (ARX2-5) or (ARX14-17) non zero; enable
; 1937				;is (ARX0 or -LONG EN) for second case.  If ARX18
; 1938				;is 0, clear AR left; otherwise, poke ARL select
; 1939				;to set bit 2 (usually gates AD left into ARL)
; 1940	
; 1941	SPEC/=<67:71>D,10	;NON-DISPATCH SPECIAL FUNCTIONS (CTL1)
; 1942	;	NOP=10		;DEFAULT
; 1943		INH CRY18=11
; 1944		MQ SHIFT=12	;ENABLE MQ*2, MQ SHRT2
; 1945		SCM ALT=13	;ENABLE FE, ARSHIFT
; 1946		CLR FPD=14
; 1947		LOAD PC=15
; 1948		XCRY AR0=16	;CARRY INTO AD IS XOR'D WITH AR00
; 1949		GEN CRY18=17
; 1950		STACK UPDATE=20	;CONTROL CRY18 IF LOCAL STACK
; 1951	;	SUBR CALL=21	;Obsolete--model A only
; 1952		ARL IND=22	;# SPECIFIES ARL MIX, ENABLES, & CALL
; 1953		MTR CTL=23	;# CONTROLS METERS
; 1954		FLAG CTL=24	;FUNCTION ENCODED IN # FIELD
; 1955		SAVE FLAGS=25	;TELLS PI CYCLE TO HOLD INTRPT
; 1956		SP MEM CYCLE=26	;MEM REQUEST IS MODIFIED BY #
; 1957		AD LONG=27	;AD BECOMES 72 BIT ALU
; 1958	
; 1959	U73/=<72:73>D,0		;BITS 72-73 UNUSED
; 1960	
; 1961	MARK/=<74:74>D,0	;FIELD SERVICE "MARK" BIT
; 1962	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 1963	.TOC	"CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD"
; 1964	
; 1965	#/=<75:83>D,0		;THE INFAMOUS "MAGIC NUMBERS"
; 1966	
; 1967	MAJVER/=<75:80>		;[356] Major version number
; 1968	MINVER/=<81:83>		;[356] Minor version number
; 1969	
; 1970	;
; 1971	;	Options bits, designating different assemblies from the same sources
; 1972	;
; 1973	
; 1974	KLPAGE/=<75:75>			;KLPAGING
; 1975		OPTIONS=1
; 1976	
; 1977	LONGPC/=<76:76>			;LONG PC FORMAT AS IN EXTENDED ADDRESSING
; 1978		OPTIONS=1		;(The model A used a different format due
; 1979					; to space limitations)
; 1980	NONSTD/=<77:77>			;NONSTANDARD (EG DIAGNOSTIC) MICROCODE
;;1981	.IF/NONSTD
;;1982		OPTIONS=1
; 1983	.IFNOT/NONSTD
; 1984		OPTIONS=0
; 1985	.ENDIF/NONSTD
; 1986	
; 1987	PV/=<78:78>			;MODEL B - PV CPU
; 1988		OPTIONS=1
; 1989	
; 1990	PMOVE/=<79:79>			;[435] Physical memory move instructions
; 1991		OPTIONS=1
; 1992	
; 1993	ISTAT/=<83:83>			;STATISTICS GATHERING CODE (IE TRACKS)
;;1994	.IF/INSTR.STAT
;;1995		OPTIONS=1
; 1996	.IFNOT/INSTR.STAT
; 1997		OPTIONS=0
; 1998	.ENDIF/INSTR.STAT
; 1999	
; 2000	PXCT/=<75:77>		;(MCL4) Loaded by CON/SR_#, CON/LOAD IR, and MEM/A RD
; 2001				;Bit 0 enables the VMAX to not come from the AD when
; 2002				; VMA/AD (allowing local AC refs, for example).  Bits
; 2003				; 1 and 2 select which PXCT bits a memory reference
; 2004				; will select for possible previous context.
; 2005	
; 2006	ACB/=<77:79>		;AC block number. Used with FMADR/#B#
; 2007		PAGB=6		;AC block used for KL paging registers
; 2008		MICROB=7	;AC block for general microcode scratch
; 2009	
; 2010	AC#/=<80:83>		;AC number used with ACB or AC-OP (below)
; 2011	
; 2012	;
; 2013	;	Warning:  when AC-OP is used with COND/FM WRITE, the previous micro-
; 2014	;	instruction must have the same # field as the current one.  Otherwise
; 2015	;	the address lines won't make it in time for the write pulse.  [210]
; 2016	;
; 2017	AC-OP/=<75:79>		;CONTROLS AC #.  AD functions < 40 all work
; 2018		AC+#=6; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12-1
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2019		#=32		;JUST AC#
; 2020		OR=33		;AC <OR> AC#
; 2021	
; 2022	;VARIOUS SPECIAL FUNCTIONS ENABLE SPECIAL DECODING OF THE
; 2023	; "MAGIC #" FIELD, AS FOLLOWS:
; 2024	
; 2025	;SPECIAL DATA PATH CONTROLS
; 2026	
; 2027	;CALL/=<75:75>		;ENABLED BY ARL IND (CTL2)--Model A only
; 2028	;	CALL=1
; 2029	AR0-8/=<76:76>		;ENABLED BY ARL IND (CTL2)
; 2030		LOAD=1
; 2031	CLR/=<77:80>		;ENABLED BY ARL IND (CTL2)
; 2032		MQ=10
; 2033		ARX=4
; 2034		ARL=2
; 2035		ARR=1
; 2036		AR=3
; 2037		AR+ARX=7
; 2038		AR+MQ=13
; 2039		ARX+MQ=14
; 2040		AR+ARX+MQ=17
; 2041		ARL+ARX=6
; 2042		ARL+ARX+MQ=16
; 2043		ARR+MQ=11
; 2044	ARL/=<81:83>		;ENABLED BY ARL IND (CTL2)
; 2045		ARL=0
; 2046		ARMM=0		;REQUIRES SPECIAL FUNCTION
; 2047		CACHE=1		;ORDINARILY SELECTED BY HWARE
; 2048		AD=2
; 2049		EBUS=3
; 2050		SH=4
; 2051		AD*2=5
; 2052		ADX=6
; 2053		AD*.25=7
; 2054	AR CTL/=<75:77>		;ENABLED BY COND/REG CTL (CTL2)
; 2055		AR0-8 LOAD=4
; 2056		AR9-17 LOAD=2	;Gates VMAX into ARMM (see VMA4)
; 2057		ARR LOAD=1
; 2058		ARL LOAD=6
; 2059	EXP TST/=<80:80>	;ENABLED BY COND/REG CTL (CTL1)
; 2060		AR_EXP=1
; 2061	MQ CTL/=<82:83>		;ENABLED BY COND/REG CTL (CTL2)
; 2062	;	MQ=0		;WITH MQ/MQ SEL
; 2063		MQ*2=1		;WITH MQ/MQ SEL--Low bit is ADX0
; 2064	;	MQ*.5=2		; " (DROPS BITS 0,6,12,18,24,30)
; 2065		0S=3		; "
; 2066		SH=0		;WITH MQ/MQM SEL
; 2067		MQ*.25=1	;WITH MQ/MQM SEL--High bits are ADX34, ADX35
; 2068		1S=2		; "
; 2069		AD=3		; "
; 2070	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2071	;SPECIAL CONTROL OF EBOX FLAGS & FUNCTIONS
; 2072	
; 2073	PC FLAGS/=<75:83>	;ENABLED BY COND/PCF_# (SCD4)
; 2074	;	OVERF=400	;Any arithmetic overflow
; 2075	;	FLOVERF=200	;Floating overflow
; 2076		FPD=100		;SET FIRST PART DONE
; 2077		TRAP2=40	;SET TRAP2 (PDL OVFLO)
; 2078		TRAP1=20	;SET TRAP1 (ARITH OVFLO)
; 2079	;	EXPUND=10	;Exponent underflow
; 2080	;	NO DIV=4	;No divide
; 2081		AROV=420	;SET ARITH OVFLO & TRAP1
; 2082		FLOV=620	;SAME, PLUS FLOATING OVFLO
; 2083		FXU=630		;FLOV + EXP UNDERFLOW
; 2084		DIV CHK=424	;NO DIVIDE + AROV
; 2085		FDV CHK=624	;FLOATING NO DIVIDE
; 2086	FLAG CTL/=<75:83>	;ENABLED BY SPEC/FLAG CTL (SCD5)
; 2087		RSTR FLAGS=420	;AS IN JRSTF
; 2088		JFCL=602	;FORCE PC 00 = AROV
; 2089		JFCL+LD=622	;SECOND PART OF JFCL -- CLEAR TESTED FLAGS
; 2090		DISMISS=502	;CLEAR PI CYCLE IF SET (CON5)
; 2091				; ELSE DISMISS HIGHEST PI HOLD
; 2092		DISMISS+LD=522	;LOAD FLAGS AND DISMISS
; 2093		HALT=442	;STOP PROCESSOR IF LEGAL (CON2)
; 2094		SET FLAGS=20	;AS IN MUUO
; 2095		PORTAL=412	;CLEAR PUBLIC IF PRIVATE INSTR
; 2096	SPEC INSTR/=<75:83>	;ENABLED BY COND/SPEC INSTR
; 2097		SET PI CYCLE=714; (CON5)
; 2098		KERNEL CYCLE=200;MAKE IO LEGAL, EXEC ADDR SPACE (CON4)
; 2099		INH PC+1=100	;TO MAKE JSR WORK IN TRAP, INTRPT (CON4)
; 2100		SXCT=40		;START SECTION XCT (MCL4)
; 2101		PXCT=20		;START PREV CONTXT XCT (MCL4)
; 2102		INTRPT INH=10	;INHIBIT INTERRUPTS (CON4)
; 2103		INSTR ABORT=4	; (CON2)
; 2104		HALTED=302	;TELL CONSOLE WE'RE HALTED (CON4)
; 2105		CONS XCT=310	;FLAGS FOR INSTR XCT'D FROM CONSOLE
; 2106		CONT=0		;RESTORE NORMAL STATE FOR CONTINUE
; 2107	FETCH/=<75:83>		;ENABLED BY MEM/FETCH
; 2108		UNCOND=400
; 2109				;LOW 2 BITS DECODED ON (IR3)
; 2110		COMP=201,2	;DEPENDING ON AD AND DRAM B
; 2111		SKIP=202,2
; 2112		TEST=203,1
; 2113		JUMP=502,2	;AS IN JUMPX, ON AD AND DRAM B
; 2114		JFCL=503,1	;JUMP ON TEST CONDITION
; 2115	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2116	;SPECIAL MEMORY REQUEST FUNCTIONS
; 2117	
; 2118	EA CALC/=<75:83>	;SPECIFIC CONTROLS FOR MEM/EA CALC
; 2119	;	LOAD AR=400
; 2120	;	LOAD ARX=200
; 2121	;	PAUSE=100	;Freeze memory--always use with 040
; 2122	;	WRITE=040	;SET VMA WRITE
; 2123	;	PREV EN=20	;PREV CONTXT SELECTED BY SR AND PXCT
; 2124	;	INDIRECT=10	;PREV CONTXT FOR EA CALC
; 2125	;	EA=2		;RESTORATION OF ORIGINAL EA CONDITIONS
; 2126	;	STACK=1		;PREV CONTXT SELECTED BY PXCT B12
; 2127		A IND=230	;INDIRECT AT FIRST EA CALC TIME
; 2128		BYTE LD=420	;Read byte data to AR only [337]
; 2129		BYTE RD=620	;READ BYTE DATA TO AR & ARX
; 2130		BYTE RD PC=621	;READ BYTE DATA TO AR & ARX WITH PC SECTION
; 2131		BYTE RPW=760	;Read byte data to AR, ARX, write test, pause [312]
; 2132		BYTE IND=610	;INDIRECT AT BYTE EA CALC TIME
; 2133		PUSH=041	;STORE TO STACK
; 2134		POP AR=421	;READ FROM STACK TO AR
; 2135		POP ARX=221	;READ FROM STACK TO ARX
; 2136		POP AR-ARX=621	;POP TO BOTH
; 2137		WRITE(E)=042
; 2138		LD AR(EA)=402	;LOAD AR GLOBAL/LOCAL AS IN EA
; 2139		LD AR+WR=440	;LOAD AR, TEST WRITABILITY
; 2140		LD ARX+WR=240	;LOAD ARX, TEST WRITABILITY
; 2141	
; 2142	SP MEM/=<75:83>		;ENABLED BY SPEC/SP MEM CYCLE
; 2143		FETCH=400	;LOAD IR WHEN DATA ARRIVES (MCL5)
; 2144		USER=200	;FORCE USER OR UPT (MCL2)
; 2145		EXEC=100	;FORCE EXEC OR EPT (MCL3)
; 2146		SEC 0=40	;CLEAR VMAX (MCL4)
; 2147		UPT EN=20	;UPT IF USER EN (MCL3)
; 2148		EPT EN=10	;EPT IF NOT USER EN (MCL3)
; 2149		CACHE INH=2	; (MCL6)
; 2150		UNCSH+UNPAGE=103;UNCACHED AND UNPAGED
; 2151		UNPAGED+CACHED=101	;physical reference with cache enabled.
;;2152	.IFNOT/MULTI
;;2153		UNPAGED=101	; (MCL6)
;;2154		EPT=111
;;2155		EPT CACHE=111	;[260]
;;2156		EPT FETCH=511
;;2157		UPT=221
;;2158		UPT FETCH=621
;;2159		PT=31
;;2160		PT FETCH=431
; 2161	.IF/MULTI
; 2162		UNPAGED=103	; (MCL6)
; 2163		EPT=113
; 2164		EPT CACHE=111	;[260]
; 2165		EPT FETCH=513
; 2166		UPT=223
; 2167		UPT FETCH=623
; 2168		PT=33
; 2169		PT FETCH=433
; 2170	.ENDIF/MULTI
; 2171	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2172	;MBOX CONTROLS
; 2173	
; 2174	MREG FNC/=<75:83>	;ENABLED BY MEM/REG FUNC (APR6)
; 2175		SBUS DIAG=407	;PERFORM SBUS DIAGNOSTIC CYCLE
; 2176		READ UBR=502	;ASK MBOX TO LOAD UBR INTO EBUS REG
; 2177		READ EBR=503	;PUT EBR INTO EBUS REG
; 2178		READ ERA=504
; 2179		WR REFILL RAM=505	;DISGUISED AS A "READ REG" FUNCTION
; 2180		LOAD CCA=606	;START A SWEEP
; 2181		LOAD UBR=602	;SETUP UBR FROM VMA
; 2182		LOAD EBR=603	;SETUP EBR FROM VMA
; 2183		MAP=140		;GET PHYS ADDR CORRESPONDING TO VMA (MCL6)
; 2184	MBOX CTL/=<75:83>	;ENABLED BY COND/MBOX CTL (APR5)
; 2185		SET PAGE FAIL=200
; 2186		SET IO PF ERR=100
; 2187		CLR PT LINE(NK)=61,,1;[333] Clear valid if no Keep bit set
; 2188		PT DIR CLR(NK)=41;Enable clear of PT DIR for non keep entries
; 2189		CLR PT LINE=31,,1;CLEAR VALID FOR 4 ENTRIES (new pager board) [342]
; 2190		PT DIR WR=20,1	;WRITE PAGE TABLE DIRECTORY
; 2191		PT WR=10,1	;WRITE PAGE TABLE ENTRY SELECTED BY VMA
; 2192		PT DIR CLR=1	;SELECT FOR CLEARING PT DIR (PAG3)
; 2193		NORMAL=0	;RESET PT WR SELECTION
; 2194	MTR CTL/=<81:83>	;FUNCTION DECODING FOR METERS (MTR3)
; 2195		CLR TIME=0		; USUALLY USED WITH DIAG FUNC
; 2196		CLR PERF=1
; 2197		CLR E CNT=2
; 2198		CLR M CNT=3
; 2199		LD PA LH=4
; 2200		LD PA RH=5
; 2201		CONO MTR=6
; 2202		CONO TIM=7
; 2203	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 16
; DEFINE.MIC[4,24]	16:58 23-May-86			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2204	;I/O FUNCTIONS
; 2205	
; 2206	EBUS CTL/=<75:83>	;ENABLED BY COND/EBUS CTL (APR3)
; 2207		GRAB EEBUS=400	;"EBUS RETURN" TAKES ECL EBUS FOR EBOX
; 2208		REQ EBUS=200
; 2209		REL EBUS=100	; (CON3)
; 2210		EBUS DEMAND=60	;ASSERT DEMAND, KEEP CS, FUNC
; 2211		EBUS NODEMAND=20;DROP DEMAND, KEEP CS, FUNC
; 2212	;	CTL_IR=10	;SELECT F01 & F02 FROM IR
; 2213	;	DISABLE CS=4	;TURN OFF CONTROLLER SELECT
; 2214	;	DATAIO=2	;0 FOR CONI/O
; 2215	;	INPUT=1		;0 FOR OUTPUT
; 2216		IO INIT=30	;ENABLE IR3-9 TO EBUS CONTROLLER SELECT,
; 2217				; IR10-12 (DECODED) TO FUNCTION
; 2218				; AND AR ONTO EBUS IF FUNCTION IS OUTPUT
; 2219		DATAO=26	;0'S TO CS, DATAO TO FCN, AND AR TO EBUS
; 2220		DATAI=27	;0'S TO CS, DATAI TO FCN
; 2221		REL EEBUS=0	;LEGGO
; 2222	DIAG FUNC/=<75:83>	;ENABLED BY COND/DIAG FUNC (CTL3)
; 2223		.5 USEC=400,3		;STRETCH CLOCK TO LET EBUS SETTLE (CON?)
; 2224		LD PA LEFT=404,3	;LH PERF ANAL CONTROLS FROM RH (MTR)
; 2225		LD PA RIGHT=405,3	;RH PA CONTROLS FROM RH (MTR)
; 2226		CONO MTR=406,3		;ACCOUNTING CONTROLS (MTR)
; 2227		CONO TIM=407,3		;INTERVAL TIMER CONTROLS (MTR)
; 2228		CONO APR=414,3		; (CON3)
; 2229		CONO PI=415,3		; (CON3)
; 2230		CONO PAG=416,3		;CACHE & PAGING CTL (CON3)
; 2231		DATAO APR=417,3		;ADDRESS BREAK (CON3)
; 2232		DATAO PAG=620,3		;AC BLOCKS & PREV CONTXT (CON3)
; 2233		LD AC BLKS=425,3	;FORCE LOADING AC BLOCKS
; 2234		LD PCS+CWSX=426,3	;FORCE LOADING PREV CONTXT SEC, CWSX
; 2235		CONI PI(R)=500,3	;PI HOLD & ACTIVE TO LH (PI)
; 2236		CONI PI(L)=501,3	;PI GEN TO LH (PI)
; 2237		CONI APR(R)=510,3	;APR INTERRUPT & PIA TO LH (APR6)
; 2238		RD TIME=510,3		;TIME BASE TO RH (MTR5)
; 2239		DATAI PAG(L)=511,3	;AC BLOCKS, PREV CONTXT TO LH (APR6)
; 2240		RD PERF CNT=511,3	;PERFORMANCE COUNT TO RH (MTR5)
; 2241		CONI APR(L)=512,3	;APR INTERRUPT ENABLES TO LH (APR6)
; 2242		RD EBOX CNT=512,3	;EBOX COUNT TO RH (MTR5)
; 2243		DATAI APR=513,3		;ADDR BREAK CONDITIONS TO LH (APR6)
; 2244		RD CACHE CNT=513,3	;CACHE COUNT TO RH (MTR5)
; 2245		RD INTRVL=514,3		;INTERVAL TIMER TO RH (MTR5)
; 2246		RD PERIOD=515,3		;PERIOD REGISTER TO RH (MTR5)
; 2247		CONI MTR=516,3		;CONTROLS & PIA TO RH (MTR5)
; 2248		RD MTR REQ=517,3	;ENCODED UPDATE REQUEST TO 20-22 (MTR5)
; 2249		CONI PI(PAR)=530,3	;WRITE EVEN PARITY ENABLES TO RH (CON1)
; 2250		CONI PAG=531,3		;CACHE & TRAP CTL TO RH (CON1)
; 2251		RD EBUS REG=567,3	;EBUS REGISTER IN MBOX (MBZ1 & MBC1)
; 2252	
; 2253	PARITY/=0,0,0,P		;USE ANY AVAILABLE FIELD FOR PARITY
; 2254	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17
; DEFINE.MIC[4,24]	16:58 23-May-86			DISPATCH RAM DEFINITIONS				

; 2255	.TOC	"DISPATCH RAM DEFINITIONS"
; 2256	;FIELDS ARE ARRANGED FOR EASY READING, NOT COMPACTNESS
; 2257	
; 2258		.DCODE
; 2259	A/=<0:2>		;OPERAND FETCH MODE
; 2260		IMMED=0		;IMMEDIATE
; 2261		IMMED-PF=1	;IMMEDIATE, START PREFETCH
; 2262		ADDR=2		;FULL EFFECTIVE ADDRESS
; 2263		WR-TST=3	;TEST WRITABILITY
; 2264		READ=4		;READ ONLY
; 2265		READ-PF=5	;READ, THEN PREFETCH
; 2266		RD-WR=6		;READ WRITE (SEPARATE CYCLES)
; 2267		RD-P-WR=7	;READ PAUSE WRITE
; 2268	
; 2269	B/=<3:5>		;STORE RESULTS AT--
; 2270		DBL AC=1	;DOUBLE RESULT TO AC & AC+1
; 2271		DBL BOTH=2	;MULB, DIVB, ETC
; 2272		SELF=3		;SELF MODE INSTRUCTIONS
; 2273		AC=5		;SINGLE RESULT TO AC, PREFETCH IN PROG
; 2274		MEM=6		;RESULT TO MEMORY
; 2275		BOTH=7		;SINGLE RESULT TO MEMORY AND AC
; 2276	
; 2277		SJC-=3		;SKIP JUMP COMPARE CONTROLS
; 2278		SJCL=2
; 2279		SJCE=1
; 2280		SJCLE=0
; 2281		SJCA=7
; 2282		SJCGE=6
; 2283		SJCN=5
; 2284		SJCG=4
; 2285	B0/=<3:3>		;INVERTS VARIOUS TEST, SKIP, AND JUMP CONTROLS
; 2286		CRY0(0)=0	;TEST TST CAUSES PC SKIP IF CRY0=0
; 2287		CRY0(1)=1	; SAME IF CRY0=1
; 2288	B1-2/=<4:5>		;FLOATING RESULT STORE MODE
; 2289		AC=1	;RESULT TO AC
; 2290		MEM=2	;RESULT JUST TO MEM
; 2291		BOTH=3	;RESULT TO BOTH
; 2292	
; 2293	PARITY/=<11:11>P
; 2294	;
; 2295	;	The J field is the starting location of the microroutine to
; 2296	;	execute the instruction.  Note that the 40 and 20 bits must
; 2297	;	always be zero.  Also, even-odd pairs of DRAM J fields may
; 2298	;	differ only in the low order three bits.  (Someone thought he
; 2299	;	was being very clever when he designed the machine this way.
; 2300	;	It probably reduced our transfer cost by at least five dollars,
; 2301	;	after all, and the microcode pain that occurred later didn't cost
; 2302	;	anything, in theory.)
; 2303	;
; 2304	J/=<14:23>		;EXECUTOR
; 2305		.UCODE
; 2306	
						; 2307	.BIN
						; 2308	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--Miscellaneous and A			

						; 2309	.TOC	"CRAM Macros--Miscellaneous and A"
; 2310	.NOBIN
; 2311	;
; 2312	;	All the CRAM macros have been alphabetized for easy reference.  We have
; 2313	;	defined "_" to be alphabetically lower than the alphabet (although its
; 2314	;	ASCII representation makes it higher) so that macros such as AR_AR+1
; 2315	;	will precede ARX_AR+1, for example (this seems more intuitive).
; 2316	;
; 2317	[]_[]*[]	"@1/AD, ADA/@2, ADB/@3"
; 2318	[]_[]*FM[]	"@3, ADA/@2, ADB/FM, @1/AD"
; 2319	[]_[]-FM[]	"@3, ADA/@2, ADB/FM, @1/AD, AD/A-B"
; 2320	[]_#[]		"@1_#,#/@2"
; 2321	[]_ADA[]	"@1/AD, ADA/@2, AD/A"
; 2322	[]_ADB[]	"@1/AD, ADA EN/0S, ADB/@2, AD/B"
; 2323	[]_FM[]		"@1/AD, ADA EN/0S, ADB/FM, @2, AD/B"
; 2324	
; 2325	(AR+ARX+MQ)*.25	"ADA/AR,AD/A,AR/AD*.25,ARX/ADX*.25,(MQ)*.25"
; 2326	(AR+ARX+MQ)*2	"ADA/AR,AD/A,AR/AD*2,ARX/ADX*2,(MQ)*2"
; 2327	(MQ)*.25	"COND/REG CTL,MQ/MQM SEL,MQ CTL/MQ*.25"
; 2328	(MQ)*2		"COND/REG CTL,MQ/MQ SEL,MQ CTL/MQ*2"
; 2329	
; 2330	A INDRCT	"MEM/EA CALC,EA CALC/A IND,VMA/LOAD"
; 2331	A READ		"VMA/PC+1,DISP/DRAM A RD,MEM/A RD,#/300,J/0"
; 2332	ABORT INSTR	"COND/SPEC INSTR,SPEC INSTR/INSTR ABORT"
; 2333	AC0		"FMADR/AC0"
; 2334	AC0_AR		"FMADR/AC0,COND/FM WRITE"
; 2335	AC1		"FMADR/AC1"
; 2336	AC1_AR		"FMADR/AC1,COND/FM WRITE"
; 2337	AC2		"FMADR/AC2"
; 2338	AC2_AR		"FMADR/AC2,COND/FM WRITE"
; 2339	AC3_AR		"FMADR/AC3,COND/FM WRITE"
; 2340	AC4		"FMADR/AC+#,AC-OP/AC+#,AC#/4"
; 2341	AC4_AR		"AC4,COND/FM WRITE"
; 2342	AC5		"FMADR/AC+#,AC-OP/AC+#,AC#/5"
; 2343	AC5_AR		"AC5,COND/FM WRITE"
; 2344	AD FLAGS	"COND/AD FLAGS"
; 2345	AD LONG		"SPEC/AD LONG"
; 2346	ADMSK		"R15"		;23 ONES
; 2347	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--AR						

; 2348	.TOC	"CRAM Macros--AR"
; 2349	
; 2350	AR_[] AND FM[]	"ADA/@1,ADB/FM,@2,AD/AND,AR/AD"
; 2351	AR_(AR+2BR)*.25	"ADA/AR,ADB/BR*2,AD/A+B,AR/AD*.25"
; 2352	AR_(AR+BR)*.25	"ADA/AR,ADB/BR,AD/A+B,AR/AD*.25"
; 2353	AR_(AR-2BR)*.25	"ADA/AR,ADB/BR*2,AD/A-B,AR/AD*.25"
; 2354	AR_(AR-BR)*.25	"ADA/AR,ADB/BR,AD/A-B,AR/AD*.25"
; 2355	AR_(ARX OR AR*4)*.25	"ADA/ARX,ADB/AR*4,AD/OR,AR/AD*.25"
; 2356	AR_-AC0		"FMADR/AC0,ADB/FM,ADA EN/0S,AD/A-B,AR/AD"
; 2357	AR_-AR		"ADA EN/0S,ADB/AR*4,AD/A-B,AR/AD*.25"
; 2358	AR_-AR LONG	"GEN -AR LONG,AR_AD*.25 LONG"
; 2359	AR_-BR		"ADB/BR,ADA EN/0S,AD/A-B,AR/AD"
; 2360	AR_-BR LONG	"ADA EN/0S,ADB/BR,AD/A-B,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2361	AR_-BR*2 LONG	"ADA EN/0S,ADB/BR*2,AD/A-B,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2362	AR_-BRX		"ADB/BR,ADA EN/0S,AD/A-B,AR/ADX"
; 2363	AR_-DLEN	"DLEN,ADB/FM,ADA EN/0S,AD/A-B,AR/AD"
; 2364	AR_-FM[]	"ADA EN/0S,ADB/FM,@1,AD/A-B,AR/AD"
; 2365	AR_-SLEN	"SLEN,ADB/FM,ADA EN/0S,AD/A-B,AR/AD"
; 2366	AR_0.C		"COND/ARL IND,CLR/AR"
; 2367	AR_0.M		"MEM/ARL IND,CLR/AR"
; 2368	AR_0.S		"SPEC/ARL IND,CLR/AR"
; 2369	AR_0S		"AD/0S,AR/AD"
; 2370	AR_1		"ADA EN/0S,AD/A+1,AR/AD"
; 2371	AR_1 LONG	"ADA EN/0S,AD/A+1,AR/AD*.25,ARX/ADX"
; 2372	AR_1S		"AD/1S,AR/AD"
; 2373	AR_2		"ADA EN/0S,AD/A+1,AR/AD*2"
; 2374	AR_2(AR*BR)	"ADA/AR,ADB/BR,AR/AD*2"
; 2375	AR_2(AR+1)	"ADA/AR,AD/A+1,AR/AD*2"
; 2376	AR_2(AR+BR)	"AR_2(AR*BR),AD/A+B"
; 2377	AR_2(AR+BR) LONG "AR_2(AR*BR),AD/A+B,ARX/ADX*2,SPEC/AD LONG"
; 2378	AR_2(AR-BR)	"AR_2(AR*BR),AD/A-B"
; 2379	
; 2380	AR_AC0		"FMADR/AC0,ADB/FM,AD/B,AR/AD"
; 2381	AR_AC0 COMP	"FMADR/AC0,ADB/FM,AD/SETCB,AR/AD"
; 2382	AR_AC0+1	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A+B+1,AR/AD"
; 2383	AR_AC1		"FMADR/AC1,ADB/FM,AD/B,AR/AD"
; 2384	AR_AC1 COMP	"FMADR/AC1,ADB/FM,AD/SETCB,AR/AD"
; 2385	AR_AC1*2	"FMADR/AC1,ADB/FM,AD/B,AR/AD*2"
; 2386	AR_AC2		"FMADR/AC2,ADB/FM,AD/B,AR/AD"
; 2387	AR_AC3		"FMADR/AC3,ADB/FM,AD/B,AR/AD"
; 2388	AR_AC3*2	"FMADR/AC3,ADB/FM,AD/B,AR/AD*2"
; 2389	AR_AC4		"AC4,ADB/FM,AD/B,AR/AD"
; 2390	AR_AD*.25 LONG	"AR/AD*.25,ARX/ADX*.25,SPEC/AD LONG"
; 2391	AR_ADMSK AND VMA HELD	"COND/SEL VMA,ADA/PC,ADB/FM,ADMSK,AD/AND,AR/AD"
; 2392	
; 2393	AR_AR AND ADMSK	 "ADMSK,ADB/FM,ADA/AR,AD/AND,AR/AD"
; 2394	AR_AR AND CSMSK	"CSMSK,ADB/FM,ADA/AR,AD/AND,AR/AD"
; 2395	AR_AR OR PUR	"PUR,ADB/FM,ADA/AR,AD/OR,AR/AD"
; 2396	AR_AR SWAP	"SH/AR SWAP,AR/SH"
; 2397	AR_AR*.25	"ADA/AR,AD/A,AR/AD*.25"
; 2398	AR_AR*.25 LONG	"ADA/AR,AD/A,AR/AD*.25,ARX/ADX*.25"
; 2399	AR_AR*.5	"ADA/AR,AD/A*2,AR/AD*.25"
; 2400	AR_AR*.5 LONG	"ADA/AR,AD/A*2,SPEC/AD LONG,AR/AD*.25,ARX/ADX*.25"
; 2401	AR_AR*1.25 LONG	"ADA/AR,ADB/AR*4,AD/A+B,AR_AD*.25 LONG"
; 2402	AR_AR*10	"ADA/AR,ADB/AR*4,AD/A+B,AR/AD*2"
; 2403	AR_AR*10 LONG	"ADA/AR,ADB/AR*4,AD/A+B,AR/AD*2,ARX/ADX*2,SPEC/AD LONG"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--AR						

; 2404	AR_AR*2		"ADA/AR,AD/A,AR/AD*2"
; 2405	AR_AR*2 LONG	"ADA/AR,AD/A,AR/AD*2,ARX/ADX*2"
; 2406	AR_AR*4		"ADB/AR*4,AD/B,AR/AD"
; 2407	AR_AR*4 LONG	"ADB/AR*4,AD/B,AR/AD,ARX/ADX"
; 2408	AR_AR*5 LONG	"ADA/AR,ADB/AR*4,AD/A+B,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2409	AR_AR*8		"ADB/AR*4,AD/B,AR/AD*2"
; 2410	AR_AR*8 LONG	"ADB/AR*4,AD/B,AR/AD*2,ARX/ADX*2"
; 2411	AR_AR*AC0	"FMADR/AC0,ADB/FM,ADA/AR,AR/AD"	;GENERAL BINARY OPERATION
; 2412	AR_AR*AC1	"FMADR/AC1,ADB/FM,ADA/AR,AR/AD"
; 2413	AR_AR*BR	"ADA/AR,ADB/BR,AR/AD"
; 2414	AR_AR*EXPMSK	"EXPMSK,ADB/FM,ADA/AR,AR/AD"	;[224]
; 2415	AR_AR*MSK	"MSK,ADB/FM,ADA/AR,AR/AD"
; 2416	AR_AR*SFLGS	"SFLGS,ADB/FM,ADA/AR,AR/AD"
; 2417	AR_AR*SLEN	"SLEN,ADB/FM,ADA/AR,AR/AD"
; 2418	AR_AR*T0	"T0,ADB/FM,ADA/AR,AR/AD"
; 2419	
; 2420	AR_AR+1		"ADA/AR,AD/A+1,AR/AD"
; 2421	AR_AR+1 LONG	"AR_AR+1,ARX/ADX,SPEC/AD LONG"
; 2422	AR_AR+1-AR0	"ADA/AR,AD/A+1,AR/AD,SPEC/XCRY AR0"
; 2423	AR_AR+BR	"ADA/AR,ADB/BR,AD/A+B,AR/AD"
; 2424	AR_AR+BR LONG	"AR_AR+BR,ARX/ADX,SPEC/AD LONG"
; 2425	AR_AR+E1	"E1,ADB/FM,ADA/AR,AD/A+B,AR/AD"
; 2426	AR_AR+FM[]	"ADA/AR,ADB/FM,@1,AD/A+B,AR/AD";[343]
; 2427	AR_AR+SBR	"SBR,ADB/FM,ADA/AR,AD/A+B,AR/AD"
; 2428	AR_AR+T0	"T0,ADB/FM,ADA/AR,AD/A+B,AR/AD"
; 2429	AR_AR+T1	"T1,ADB/FM,ADA/AR,AD/A+B,AR/AD"
;;2430	.IF/TRXDEF
;;2431	AR_AR+TRB	"TRB,ADB/FM,ADA/AR,AD/A+B,AR/AD"
;;2432	AR_AR+TRX	"TRX,ADB/FM,ADA/AR,AD/A+B,AR/AD"
; 2433	.ENDIF/TRXDEF
; 2434	AR_AR+XR	"GEN AR+XR,AR/AD"
; 2435	AR_AR-1		"ADA/AR,AD/A-1,AR/AD"
; 2436	AR_AR-BR	"ADA/AR,ADB/BR,AD/A-B,AR/AD"
; 2437	AR_AR-BR LONG	"AR_AR-BR,ARX/ADX,SPEC/AD LONG"
; 2438	AR_AR-BR-1	"GEN AR*BR,AD/A-B-1,AR/AD"
; 2439	AR_AR-FM[]	"ADA/AR,ADB/FM,@1,AD/A-B,AR/AD"
; 2440	AR_AR-T0	"T0,ADB/FM,ADA/AR,AD/A-B,AR/AD"
; 2441	
; 2442	AR_ARX		"SH/ARX,AR/SH"
; 2443	AR_ARX (AD)	"ADA/ARX,AD/A,AR/AD"
; 2444	AR_ARX (ADX)	"ADA EN/EN,AD/A,AR/ADX"
; 2445	AR_ARX AND ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/AND,AR/AD"
; 2446	AR_ARX ANDC ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/ANDCB,AR/AD"
; 2447	AR_ARX COMP	"ADA EN/EN,AD/SETCA,AR/ADX"
; 2448	AR_ARX COMP AND BRX "ADA EN/EN,ADB/BR,AD/ANDCA,AR/ADX"
; 2449	AR_ARX OR PUR	"PUR,ADB/FM,ADA/ARX,AD/OR,AR/AD"
; 2450	AR_ARX*.25	"ADA/ARX,AD/A,AR/AD*.25"
; 2451	AR_ARX*.25-AR-1	"ADB/AR*4,ADA/ARX,AD/A-B-1,AR/AD*.25"
; 2452	AR_ARX*2	"ADA/ARX,AD/A,AR/AD*2"
; 2453	AR_ARX*4	"ADB/AR*4,AD/B,AR/ADX"
; 2454	AR_ARX*4 COMP	"ADB/AR*4,AD/SETCB,AR/ADX"
; 2455	AR_ARX*AC1	"FMADR/AC1,ADB/FM,ADA/ARX,AR/AD"
; 2456	AR_ARX*BR	"ADA/ARX,ADB/BR,AR/AD"
; 2457	AR_ARX*BRX	"ADA/AR,ADB/BR,AR/ADX"
; 2458	AR_ARX*E1	"E1,ADB/FM,ADA/ARX,AR/AD"
; 2459	AR_ARX+1	"ADA EN/EN,AD/A+1,AR/ADX"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-2
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--AR						

; 2460	AR_ARX+1 (AD)	"ADA/ARX,AD/A+1,AR/AD"
; 2461	AR_ARX+AC0	"ADA/ARX,ADB/FM,FMADR/AC0,AD/A+B,AR/AD"
; 2462	AR_ARX+AR*4	"ADA/ARX,ADB/AR*4,AD/A+B,AR/AD"
; 2463	AR_ARX+BR	"ADA/ARX,ADB/BR,AD/A+B,AR/AD"
; 2464	AR_ARX+BRX	"ADA EN/EN,ADB/BR,AD/A+B,AR/ADX"
; 2465	AR_ARX+BRX+1	"ADA EN/EN,ADB/BR,AD/A+B+1,AR/ADX"	;[343]
; 2466	AR_ARX+FM[]	"ADA/ARX,ADB/FM,@1,AD/A+B,AR/AD"
; 2467	AR_ARX+XR	"GEN ARX+XR,AR/AD"
; 2468	AR_ARX-1	"ADA EN/EN,AD/A-1,AR/ADX"
; 2469	AR_ARX-AC3	"ADA/ARX,ADB/FM,FMADR/AC3,AD/A-B,AR/AD"
; 2470	AR_ARX-BR	"ADA/ARX,ADB/BR,AD/A-B,AR/AD"
; 2471	
; 2472	AR_BR		"ADB/BR,AD/B,AR/AD"
; 2473	AR_BR COMP	"ADB/BR,AD/SETCB,AR/AD"
; 2474	AR_BR COMP LONG	"ADB/BR,AD/SETCB,AR/AD,ARX/ADX"
; 2475	AR_BR LONG	"ADB/BR,AD/B,AR/AD,ARX/ADX"
; 2476	AR_BR OR ARX	"ADA/ARX,ADB/BR,AD/OR,AR/AD"
; 2477	AR_BR*.5	"ADB/BR*2,AD/B,AR/AD*.25"
; 2478	AR_BR*.5 LONG	"ADB/BR*2,AD/B,AR/AD*.25,ARX/ADX*.25"
; 2479	AR_BR*2		"ADB/BR*2,AD/B,AR/AD"
; 2480	AR_BR*2 LONG	"ADB/BR*2,AD/B,AR/AD,ARX/ADX"
; 2481	AR_BR*4		"ADB/BR*2,AD/B,AR/AD*2"			;[230]
; 2482	AR_BR*4 LONG	"ADB/BR*2,AD/B,AR/AD*2,ARX/ADX*2"
; 2483	AR_BR+1		"ADB/BR,ADA EN/0S,AD/A+B+1,AR/AD"
; 2484	AR_BR+1 LONG	"ADA EN/0S,ADB/BR,AD/A+B+1,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2485	AR_BRX		"ADB/BR,AD/B,AR/ADX"
; 2486	AR_BRX+1	"ADA EN/0S,ADB/BR,AD/A+B+1,AR/ADX"
; 2487	
; 2488	AR_CACHE CNT	"DIAG IN,DIAG FUNC/RD CACHE CNT"
; 2489	AR_DLEN		"DLEN,AR_FM"
; 2490	AR_DLEN COMP	"DLEN,ADB/FM,AD/SETCB,AR/AD"
; 2491	AR_DLEN+1	"DLEN,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2492	AR_DSTP		"DSTP,AR_FM"
; 2493	AR_DSTP+1	"DSTP,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2494	AR_DSTP2	"DSTP2,AR_FM"
; 2495	AR_DSTP2+1	"DSTP2,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2496	AR_DSTW		"DSTW,AR_FM"
; 2497	AR_E0		"E0,AR_FM"
; 2498	AR_E1		"E1,AR_FM"
; 2499	AR_EBOX CNT	"DIAG IN,DIAG FUNC/RD EBOX CNT"
; 2500	AR_EBUS		"AR/EBUS,TIME/5T"
; 2501	AR_EBUS REG	"DIAG IN,DIAG FUNC/RD EBUS REG"
; 2502	AR_FILL		"FILL,AR_FM"
; 2503	AR_FM		"ADB/FM,AD/B,AR/AD"
; 2504	AR_FM[]		"AR/AD, AD/B, ADB/FM, @1"	;[274]
; 2505	AR_FM[] COMP	"ADB/FM,@1,AD/SETCB,AR/AD"
; 2506	AR_FM[]+1	"ADA EN/0S,ADB/FM,@1,AD/A+B+1,AR/AD"
; 2507	AR_FM(#)	"FMADR/AC+#,AC-OP/AC+#,ADB/FM,AD/B,AR/AD"
; 2508	AR_FM(VMA)	"FMADR/VMA,ADB/FM,AD/B,AR/AD"
; 2509	AR_INTERVAL	"DIAG IN,DIAG FUNC/RD INTRVL"
; 2510	
; 2511	AR_MEM		"MEM/MB WAIT,FMADR/VMA,ADB/FM,AD/B,AR/MEM"
; 2512	AR_MQ		"ADA/MQ,AD/A,AR/AD"
; 2513	AR_MQ COMP	"ADA/MQ,AD/SETCA,AR/AD"
; 2514	AR_MQ*.25	"ADA/MQ,AD/A,AR/AD*.25"
; 2515	AR_MQ*2		"ADA/MQ,AD/A,AR/AD*2"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-3
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--AR						

; 2516	AR_MQ*4		"ADA/MQ,AD/A*2,AR/AD*2"
; 2517	AR_MQ*AC1	"FMADR/AC1,ADB/FM,ADA/MQ,AR/AD"
; 2518	AR_MQ*AC2	"FMADR/AC2,ADB/FM,ADA/MQ,AR/AD"
; 2519	AR_MQ*AC3	"FMADR/AC3,ADB/FM,ADA/MQ,AR/AD"
; 2520	AR_MQ+1		"ADA/MQ,AD/A+1,AR/AD"
; 2521	AR_MQ+AC0	"FMADR/AC0,ADB/FM,ADA/MQ,AD/A+B,AR/AD"
; 2522	AR_MQ+BR	"ADA/MQ,ADB/BR,AD/A+B,AR/AD"		;[343]
; 2523	AR_MQ+FM[]	"ADA/MQ,ADB/FM,@1,AD/A+B,AR/AD"
; 2524	AR_MQ-1		"ADA/MQ,AD/A-1,AR/AD"
; 2525	AR_MQ-AC3	"ADA/MQ,ADB/FM,FMADR/AC3,AD/A-B,AR/AD"
; 2526	AR_MQ-BR	"ADA/MQ,ADB/BR,AD/A-B,AR/AD"
; 2527	AR_MQ-BR-1	"ADA/MQ,ADB/BR,AD/A-B-1,AR/AD"
; 2528	AR_MTR REQ	"DIAG IN,DIAG FUNC/RD MTR REQ"
; 2529	AR_PC		"ADA/PC,AD/A,AR/AD"
; 2530	AR_PC FLAGS	"ADMSK,ADB/FM,ADA/PC,AD/ANDCB,AR/AD"
; 2531	AR_PC+1		"ADA/PC,AD/A+1,AR/AD,SPEC/SAVE FLAGS"
; 2532	AR_PERF CNT	"DIAG IN,DIAG FUNC/RD PERF CNT"
; 2533	AR_PERIOD	"DIAG IN,DIAG FUNC/RD PERIOD"
; 2534	AR_PUR+AR0	"PUR,ADB/FM,ADA EN/0S,AD/A+B,SPEC/XCRY AR0,AR/AD"
; 2535	
; 2536	AR_SERIAL	"AR/ARMM,COND/REG CTL,AR CTL/ARR LOAD"
; 2537	AR_SFLGS	"SFLGS,AR_FM"
; 2538	AR_SHIFT	"SH/SHIFT AR!ARX,AR/SH"
; 2539	AR_SIGN		"AD/XCRY-1,SPEC/XCRY AR0,AR/AD"
; 2540	AR_SLEN		"SLEN,AR_FM"
; 2541	AR_SLEN COMP	"SLEN,ADB/FM,AD/SETCB,AR/AD"
; 2542	AR_SLEN+1	"SLEN,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2543	AR_SRCP		"SRCP,AR_FM"
; 2544	AR_SRCP+1	"SRCP,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2545	AR_SRCP2	"SRCP2,AR_FM"
; 2546	AR_SRCP2+1	"SRCP2,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2547	AR_SV.AR	"SV.AR,AR_FM"
; 2548	AR_SV.ARX	"SV.ARX,AR_FM"
; 2549	AR_SV.BR	"SV.BR,AR_FM"
; 2550	AR_SV.PFW	"SV.PFW,AR_FM"
; 2551	AR_SV.SC	"SV.SC,AR_FM"
; 2552	AR_SV.VMA	"SV.VMA,AR_FM"
; 2553	AR_SWD		"SWD,AR_FM"
; 2554	
; 2555	AR_T0		"T0,AR_FM"
; 2556	AR_T1		"T1,AR_FM"
; 2557	AR_T2		"T2,AR_FM"
; 2558	AR_TIME BASE	"DIAG IN,DIAG FUNC/RD TIME"
;;2559	.IF/TRXDEF
;;2560	AR_TRB		"TRB,AR_FM"
;;2561	AR_TRX		"TRX,AR_FM"
;;2562	AR_TRX+1	"TRX,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
;;2563	AR_TRX1		"TRX1,AR_FM"
;;2564	AR_TRX2		"TRX2,AR_FM"
;;2565	AR_TRX2+1	"TRX2,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
;;2566	AR_TRX3		"TRX3,AR_FM"
;;2567	AR_TRX3+1	"TRX3,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2568	.ENDIF/TRXDEF
; 2569	AR_VMA HELD	"COND/SEL VMA,AR_PC"
; 2570	AR_XR		"FMADR/XR,ADB/FM,AD/B,AR/AD"
; 2571	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--AR Miscellaneous, ARL, and ARR		

; 2572	.TOC	"CRAM Macros--AR Miscellaneous, ARL, and ARR"
; 2573	
; 2574	AR+ARX+MQ_0.M	"MEM/ARL IND,CLR/AR+ARX+MQ"
; 2575	AR+MQ_0.M	"MEM/ARL IND,CLR/AR+MQ"
; 2576	AR+MQ_0.S	"SPEC/ARL IND,CLR/AR+MQ"
; 2577	
; 2578	AR0-3 DISP	"SH/AR,DISP/SH0-3"
; 2579	AR0-8_#		"COND/LD AR0-8,AR/ARMM,ARMM/#"
; 2580	AR0-8_# AND AR0-8 "SCADA/#,SCADB/AR0-8,SCAD/AND,AR0-8_SCAD#"
; 2581	AR0-8_# OR AR0-8 "SCADA/#,SCADB/AR0-8,SCAD/OR,AR0-8_SCAD#"
; 2582	AR0-8_#+SC	"SCADA/#,SCADB/SC,SCAD/A+B,AR0-8_SCAD#"
; 2583	AR0-8_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,AR0-8_SCAD"
; 2584	AR0-8_FE	"SCADA/FE,SCAD/A,AR0-8_SCAD"
; 2585	AR0-8_FE OR #	"SCADA/FE,SCADB/#,SCAD/OR,AR0-8_SCAD#"
; 2586	AR0-8_FE OR SC	"SCADA/FE,SCADB/SC,SCAD/OR,AR0-8_SCAD.M"
; 2587	AR0-8_FE#	"SCADA/FE,SCAD/A,ARMM/SCAD EXP,AR/ARMM,COND/LD AR0-8"
; 2588	AR0-8_FE+#	"SCADA/FE,SCADB/#,SCAD/A+B,AR0-8_SCAD#"
; 2589	AR0-8_FE+1	"SCADA/FE,SCAD/A+1,AR0-8_SCAD"
; 2590	AR0-8_FE+SC	"SCADA/FE,SCADB/SC,SCAD/A+B,AR0-8_SCAD.M"
; 2591	AR0-8_FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B,AR0-8_SCAD.M"
; 2592	AR0-8_FE.M	"SCADA/FE,SCAD/A,AR0-8_SCAD.M"
; 2593	AR0-8_FE.R	"GEN FE,AR0-8_SCAD.R"
; 2594	AR0-8_SC	"SCADA EN/0S,SCADB/SC,SCAD/A+B,AR0-8_SCAD"
; 2595	AR0-8_SCAD	"SPEC/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD EXP"
; 2596	AR0-8_SCAD#	"ARMM/SCAD EXP,AR/ARMM,COND/LD AR0-8"
; 2597	AR0-8_SCAD.M	"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD EXP"
; 2598	AR0-8_SCAD.R	"ARMM/SCAD EXP,AR/ARMM,COND/REG CTL,AR CTL/AR0-8 LOAD"
; 2599	AR12-17_PC SEC	"AR/ARMM,VMAX/PC SEC,COND/LD AR9-17"
; 2600	AR12-17_PREV SEC	"AR/ARMM,VMAX/PREV SEC,COND/LD AR9-17"
; 2601	AR18-21 DISP	"SH/AR SWAP,DISP/SH0-3"
; 2602	
; 2603	ARL_0.C		"COND/ARL IND,CLR/ARL"
; 2604	ARL_0.M		"MEM/ARL IND,CLR/ARL"
; 2605	ARL_0.S		"SPEC/ARL IND,CLR/ARL"
; 2606	ARL_0S		"COND/ARL IND,CLR/ARL"
; 2607	ARL_1.M		"ADA EN/0S,AD/A+1,SPEC/GEN CRY18,MEM/ARL IND,ARL/AD"
; 2608	ARL_1S		"AD/1S,COND/ARL IND,ARL/AD"
; 2609	ARL_1S.M	"AD/1S,MEM/ARL IND,ARL/AD"
; 2610	ARL_AC0		"FMADR/AC0,ADB/FM,AD/B,COND/ARL IND,ARL/AD"
; 2611	ARL_ARL		"COND/ARL IND,ARL/ARL"
; 2612	ARL_ARL.M	"MEM/ARL IND,ARL/ARL"
; 2613	ARL_ARL.S	"SPEC/ARL IND,ARL/ARL"
; 2614	ARL_ARR		"COND/ARL IND,ARL/SH,SH/AR SWAP"
; 2615	ARL_ARR.M	"MEM/ARL IND,ARL/SH,SH/AR SWAP"
; 2616	ARL_ARR.S	"SPEC/ARL IND,ARL/SH,SH/AR SWAP"
; 2617	ARL_ARX (ADX)	"ADA EN/EN,AD/A,MEM/ARL IND,ARL/ADX"
; 2618	ARL_ARXL	"SPEC/ARL IND,SH/ARX,ARL/SH"
; 2619	ARL_ARXL.M	"MEM/ARL IND,SH/ARX,ARL/SH"
; 2620	ARL_BRL		"ADB/BR,AD/B,COND/ARL IND,ARL/AD"
; 2621	ARL_BRL.M	"ADB/BR,AD/B,MEM/ARL IND,ARL/AD"
; 2622	ARL_BRL.S	"ADB/BR,AD/B,SPEC/ARL IND,ARL/AD"
; 2623	ARL_MQL		"ADA/MQ,AD/A,MEM/ARL IND,ARL/AD"
; 2624	ARL_SHIFT	"MEM/ARL IND,SH/SHIFT AR!ARX,ARL/SH"
; 2625	ARL_SIGN	"AD/XCRY-1,SPEC/XCRY AR0,COND/ARL IND,ARL/AD"
; 2626	ARL+ARX+MQ_0.M	"MEM/ARL IND,CLR/ARL+ARX+MQ"
; 2627	ARL+ARX_0.M	"MEM/ARL IND,CLR/ARL+ARX"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--AR Miscellaneous, ARL, and ARR		

; 2628	
; 2629	ARR_0.C		"COND/ARL IND,CLR/ARR"
; 2630	ARR_0.M		"MEM/ARL IND,CLR/ARR"
; 2631	ARR_0.S		"SPEC/ARL IND,CLR/ARR"
; 2632	ARR_0S		"AR_0S"
; 2633	ARR_1S		"AR_1S"
; 2634	ARR_AC0		"AR_AC0"
; 2635	ARR_AC0.S	"SPEC/ARL IND,FMADR/AC0,ADB/FM,AD/B,AR/AD"
; 2636	ARR_AR+1	"AR_AR+1"				;[343]
; 2637	ARR_AR-1	"AR_AR-1"
; 2638	ARR_AR+BR	"AR_AR+BR"				;[343]
; 2639	ARR_ARL		"SH/AR SWAP,AR/SH"
; 2640	ARR_ARR		"AR/AR"
; 2641	ARR_ARX		"AR_ARX"
; 2642	ARR_ARX+1	"AR_ARX+1"
; 2643	ARR_ARX+BRX	"AR_ARX+BRX"
; 2644	ARR_ARX+BR	"AR_ARX+BR"				;[343]
; 2645	ARR_ARX-1	"AR_ARX-1"
; 2646	ARR_BR		"ADB/BR,AD/B,COND/ARL IND,AR/AD"	;[252]
; 2647	ARR_PC+1	"ADA/PC,AD/A+1,AR/AD"
; 2648	ARR_SHIFT	"AR_SHIFT"
; 2649	ARR+MQ_0.S	"SPEC/ARL IND,CLR/ARR+MQ"
; 2650	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--ARX					

; 2651	.TOC	"CRAM Macros--ARX"
; 2652	
; 2653	ARX_-2+MQ0	"AD/1S,ARX/ADX*2"			;[343] -2 if MQ0 = 0
; 2654	ARX_-AC0	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A-B,ARX/AD"
; 2655	ARX_-BRX	"ADB/BR,ADA EN/0S,AD/A-B,ARX/ADX"
; 2656	ARX_-FM[]	"ADA EN/0S,ADB/FM,@1,AD/A-B,ARX/AD"
; 2657	ARX_-SLEN	"SLEN,ADB/FM,ADA EN/0S,AD/A-B,ARX/AD"
; 2658	ARX_0.C		"COND/ARL IND,CLR/ARX"
; 2659	ARX_0.M		"MEM/ARL IND,CLR/ARX"
; 2660	ARX_0.S		"SPEC/ARL IND,CLR/ARX"
; 2661	ARX_0S		"AD/0S,ARX/AD"
; 2662	ARX_1		"ADA EN/0S,AD/A+1,ARX/AD"
; 2663	ARX_1B1		"ADA EN/0S,AD/A+1,ARX/ADX*.25"
; 2664	ARX_1B17-1	"ADA EN/0S,AD/A-1,SPEC/GEN CRY18,ARX/AD"
; 2665	ARX_1S		"AD/1S,ARX/AD"
; 2666	ARX_2+MQ0	"ADA EN/0S,AD/A+1,ARX/ADX*2"
; 2667	
; 2668	ARX_AC0		"FMADR/AC0,ADB/FM,AD/B,ARX/AD"
; 2669	ARX_AC0 COMP	"ADB/FM,FMADR/AC0,AD/SETCB,ARX/AD"
; 2670	ARX_AC0+1	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A+B+1,ARX/AD"
; 2671	ARX_AC1		"FMADR/AC1,ADB/FM,AD/B,ARX/AD"
; 2672	ARX_AC2		"FMADR/AC2,ADB/FM,AD/B,ARX/AD"
; 2673	ARX_AC3		"FMADR/AC3,ADB/FM,AD/B,ARX/AD"
; 2674	ARX_AC4		"AC4,ADB/FM,AD/B,ARX/AD"
; 2675	
; 2676	ARX_AR		"SH/AR,ARX/SH"
; 2677	ARX_AR (AD)	"ADA/AR,AD/A,ARX/AD"
; 2678	ARX_AR AND ADMSK "ADMSK,ADB/FM,ADA/AR,AD/AND,ARX/AD"
; 2679	ARX_AR ANDCA BR	"ADA/AR,ADB/BR,AD/ANDCA,ARX/AD"
; 2680	ARX_AR SIGN	"AD/XCRY-1,SPEC/XCRY AR0,ARX/AD"
; 2681	ARX_AR SWAP	"SH/AR SWAP,ARX/SH"
; 2682	ARX_AR*2	"ADA/AR,AD/A*2,ARX/AD"			;[343]
; 2683	ARX_AR*4 COMP	"ADB/AR*4,AD/SETCB,ARX/AD"
; 2684	ARX_AR*MSK	"MSK,ADB/FM,ADA/AR,ARX/AD"
; 2685	ARX_AR+1	"ADA/AR,AD/A+1,ARX/AD"
; 2686	ARX_AR+CBR	"CBR,ADB/FM,ADA/AR,AD/A+B,ARX/AD"
; 2687	ARX_AR+FM[]	"ADA/AR,ADB/FM,@1,AD/A+B,ARX/AD"
; 2688	ARX_AR-1	"ADA/AR,AD/A-1,ARX/AD"
; 2689	ARX_AR-BR	"ADA/AR,ADB/BR,AD/A-B,ARX/AD"		;[224]
; 2690	ARX_AR-FM[]	"ADA/AR,ADB/FM,@1,AD/A-B,ARX/AD"
; 2691	ARX_AR-FM[]-1	"ADA/AR,ADB/FM,@1,AD/A-B-1,ARX/AD"
; 2692	
; 2693	ARX_ARX AND ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/AND,ARX/AD"
; 2694	ARX_ARX ANDC ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/ANDCB,ARX/AD"
; 2695	ARX_ARX*-6	"ADA EN/EN,ADB/AR*4,AD/A-B,ARX/ADX*2"
; 2696	ARX_ARX*.25	"ADA EN/EN,AD/A,ARX/ADX*.25"
; 2697	ARX_ARX*.5	"ADA EN/EN,AD/A*2,ARX/ADX*.25"
; 2698	ARX_ARX*2	"ADA EN/EN,AD/A,ARX/ADX*2"
; 2699	ARX_ARX*2 COMP	"ADA EN/EN,AD/SETCA,ARX/ADX*2"
; 2700	ARX_ARX*4	"ADB/AR*4,AD/B,ARX/ADX"
; 2701	ARX_ARX*4 COMP	"ADB/AR*4,AD/SETCB,ARX/ADX"
; 2702	ARX_ARX*8	"ADB/AR*4,AD/B,ARX/ADX*2"
; 2703	ARX_ARX*BRX	"ADA/AR,ADB/BR,ARX/ADX"
; 2704	ARX_ARX*EXPMSK	"EXPMSK,ADB/FM,ADA/ARX,ARX/AD"		;[224]
; 2705	ARX_ARX+1	"ADA EN/EN,AD/A+1,ARX/ADX"
; 2706	ARX_ARX+1 (AD)	"ADA/ARX,AD/A+1,ARX/AD"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--ARX					

; 2707	ARX_ARX+AC0	"ADA/ARX,ADB/FM,FMADR/AC0,AD/A+B,ARX/AD"
; 2708	ARX_ARX+BRX	"ADA EN/EN,ADB/BR,AD/A+B,ARX/ADX"
; 2709	ARX_ARX+CBR	"CBR,ADB/FM,ADA/ARX,AD/A+B,ARX/AD"
; 2710	ARX_ARX+FM[]	"ADA/ARX,ADB/FM,@1,AD/A+B,ARX/AD"	;[343]
; 2711	ARX_ARX-1	"ADA EN/EN,AD/A-1,ARX/ADX"
; 2712	ARX_ARX-1 (AD)	"ADA/ARX,AD/A-1,ARX/AD"
; 2713	ARX_ARX-AR*4	"ADA/ARX,ADB/AR*4,AD/A-B,ARX/AD"	;[343]
; 2714	ARX_ARX-FM[]	"ADA/ARX,ADB/FM,@1,AD/A-B,ARX/AD"
; 2715	ARX_ARX-FM[]-1	"ADA/ARX,ADB/FM,@1,AD/A-B-1,ARX/AD"
; 2716	
; 2717	ARX_BR		"ADB/BR,AD/B,ARX/AD"
; 2718	ARX_BR*2	"ADB/BR*2,AD/B,ARX/AD"
; 2719	ARX_BR+1	"ADB/BR,ADA EN/0S,AD/A+B+1,ARX/AD"
; 2720	ARX_BRX		"ADB/BR,AD/B,ARX/ADX"
; 2721	ARX_BRX COMP	"ADB/BR,AD/SETCB,ARX/ADX"
; 2722	ARX_BRX+1	"ADA EN/0S,ADB/BR,AD/A+B+1,ARX/ADX"
; 2723	ARX_DSTP	"DSTP,ARX_FM"
; 2724	ARX_DSTP2	"DSTP2,ARX_FM"
; 2725	ARX_E1		"E1,ARX_FM"
; 2726	ARX_FILL	"FILL,ARX_FM"				;[310]
; 2727	ARX_FM		"ADB/FM,AD/B,ARX/AD"
; 2728	ARX_FM[]	"ADB/FM,@1,AD/B,ARX/AD"			;[343]
; 2729	ARX_FM[] COMP	"ADB/FM,@1,AD/SETCB,ARX/AD"
; 2730	ARX_FM[]+1	"ADA EN/0S,ADB/FM,@1,AD/A+B+1,ARX/AD"
; 2731	ARX_FM(VMA)	"FMADR/VMA,ADB/FM,AD/B,ARX/AD"
; 2732	ARX_MEM		"MEM/MB WAIT,FMADR/VMA,ADB/FM,AD/B,ARX/MEM"
; 2733	ARX_MQ		"ADA/MQ,AD/A,ARX/AD"
; 2734	ARX_MQ+1	"ADA/MQ,AD/A+1,ARX/AD"
; 2735	ARX_MQ-1	"ADA/MQ,AD/A-1,ARX/AD"			;[343]
; 2736	ARX_MQ-FM[]	"ADA/MQ,ADB/FM,@1,AD/A-B,ARX/AD"
; 2737	ARX_MQ-FM[]-1	"ADA/MQ,ADB/FM,@1,AD/A-B-1,ARX/AD"
; 2738	
; 2739	ARX_PC		"ADA/PC,AD/A,ARX/AD"
; 2740	ARX_PC+1	"ADA/PC,AD/A+1,ARX/AD,SPEC/SAVE FLAGS"
; 2741	ARX_SHIFT	"SH/SHIFT AR!ARX,ARX/SH"
; 2742	ARX_SRCP	"SRCP,ARX_FM"
; 2743	ARX_SRCP2	"SRCP2,ARX_FM"
; 2744	ARX_SV.AR	"SV.AR,ARX_FM"
; 2745	ARX_SV.ARX	"SV.ARX,ARX_FM"
; 2746	ARX_SV.BR	"SV.BR,ARX_FM"
; 2747	ARX_SV.VMA	"SV.VMA,ARX_FM"
; 2748	ARX_T0		"T0,ARX_FM"
; 2749	ARX_T2		"T2,ARX_FM"
;;2750	.IF/TRXDEF
;;2751	ARX_TRB		"TRB,ARX_FM"
; 2752	.ENDIF/TRXDEF
; 2753	ARX_VMA HELD	"COND/SEL VMA,ARX_PC"
; 2754	ARX+MQ_0.M	"MEM/ARL IND,CLR/ARX+MQ"
; 2755	ARX+MQ_0.S	"SPEC/ARL IND,CLR/ARX+MQ"
; 2756	ARX0_AR35	"ADA/AR,AD/A*2+1,ARX/ADX*.25"	;[337]
; 2757	ARX0_MQ35	"ADA/MQ,AD/A*2+1,ARX/ADX*.25"
; 2758	ARX0-3 DISP	"SH/ARX,DISP/SH0-3"
; 2759	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--B, C, D					

; 2760	.TOC	"CRAM Macros--B, C, D"
; 2761	
; 2762	B DISP		"DISP/DRAM B"
; 2763	B STORE		"MEM/B WRITE"			;[413] Store, no dispatch
; 2764	B WRITE		"DISP/DRAM B,MEM/B WRITE"
; 2765	BLKO TIM(L)	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/LD PA LEFT"
; 2766	BLKO TIM(R)	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/LD PA RIGHT"
; 2767	BR_AR LONG	"BR/AR,BRX/ARX"
; 2768	BYTE DISP	"DISP/BYTE"
; 2769	BYTE INDRCT	"MEM/EA CALC,EA CALC/BYTE IND,VMA/LOAD"
; 2770	BYTE LOAD	"MEM/EA CALC,EA CALC/BYTE LD,VMA/LOAD";[337]
; 2771	BYTE PREV & CLR SR3	"COND/SR_#,#/640"
; 2772	BYTE PREV & SET SR2	"COND/SR_#,#/622"
; 2773	BYTE PREV & SET SR3	"COND/SR_#,#/641"
; 2774	BYTE READ	"MEM/EA CALC,EA CALC/BYTE RD,VMA/LOAD"
; 2775	BYTE READ PC	"MEM/EA CALC,EA CALC/BYTE RD PC,VMA/LOAD"
; 2776	BYTE RPW	"MEM/EA CALC,EA CALC/BYTE RPW,VMA/LOAD"
; 2777	
; 2778	CALL		"CALL/CALL"
; 2779	CALL []		"CALL, J/@1"
; 2780	CALL[]		"CALL, J/@1"
; 2781	CALL.C		"CALL/CALL"		;Obsolete model A forms of CALL
; 2782	CALL.M		"CALL/CALL"
; 2783	CALL.S		"CALL/CALL"
; 2784	CBR		"P2"
; 2785	CLR ACC+SET UCODE	"COND/EBOX STATE,#/245"
; 2786	CLR ACCOUNT EN	"COND/EBOX STATE,#/145"
; 2787	CLR AR		"COND/AR CLR"
; 2788	CLR ARX		"COND/ARX CLR"
; 2789	CLR EBUS DEMAND	"COND/EBUS CTL,EBUS CTL/EBUS NODEMAND"
; 2790	CLR EXP		"SCADA EN/0S,SCAD/A,EXP_SCAD"
; 2791	CLR FE		"SCADA EN/0S,SCAD/A,FE/SCAD"
; 2792	CLR FPD		"SPEC/CLR FPD"
; 2793	CLR MQ		"COND/REG CTL,MQ/MQ SEL,MQ CTL/0S"
; 2794	CLR MTR PA EN	"COND/EBOX STATE,#/025"
; 2795	CLR P		"SCADA EN/0S,SCAD/A,P_SCAD"
; 2796	CLR PT LINE	"COND/MBOX CTL,MBOX CTL/CLR PT LINE"
; 2797	CLR PT LINE (KEEP) "COND/MBOX CTL,MBOX CTL/CLR PT LINE(NK)"
; 2798	CLR SC		"SCADA EN/0S,SCAD/A,SC/SCAD"
; 2799	CLR SPECIAL CYCLE	"COND/SPEC INSTR,SPEC INSTR/0"
; 2800	CLR SR2		"COND/SR_#,#/20"
; 2801	CLR SR3		"COND/SR_#,#/40"
; 2802	CLR TRACKS EN	"COND/EBOX STATE,#/121"
; 2803	CLR TRK+PA EN	"COND/EBOX STATE,#/021"
; 2804	CMS FETCH	"VMA/PC+1,MEM/FETCH,FETCH/SKIP"
; 2805	COMP FETCH	"AD/XOR,VMA/PC+1,MEM/FETCH,FETCH/COMP"
; 2806	CONI APR(L)	"DIAG IN,DIAG FUNC/CONI APR(L)"
; 2807	CONI APR(R)	"DIAG IN,DIAG FUNC/CONI APR(R)"
; 2808	CONI MTR	"DIAG IN,DIAG FUNC/CONI MTR"
; 2809	CONI PAG	"DIAG IN,DIAG FUNC/CONI PAG"
; 2810	CONI PI(L)	"DIAG IN,DIAG FUNC/CONI PI(L)"
; 2811	CONI PI(PAR)	"DIAG IN,DIAG FUNC/CONI PI(PAR)"
; 2812	CONI PI(R)	"DIAG IN,DIAG FUNC/CONI PI(R)"
; 2813	CONO APR	"DIAG OUT,DIAG FUNC/CONO APR"
; 2814	CONO MTR	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/CONO MTR"
; 2815	CONO PAG	"DIAG OUT,DIAG FUNC/CONO PAG"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--B, C, D					

; 2816	CONO PI		"DIAG OUT,DIAG FUNC/CONO PI"
; 2817	CONO TIM	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/CONO TIM"
; 2818	CONTINUE	"COND/SPEC INSTR,SPEC INSTR/CONT"
; 2819	CSMSK		"P0"
; 2820	
; 2821	DATAI APR(L)	"DIAG IN,DIAG FUNC/DATAI APR"
; 2822	DATAI PAG(L)	"DIAG IN,DIAG FUNC/DATAI PAG(L)"
; 2823	DATAO APR	"DIAG OUT,DIAG FUNC/DATAO APR"
; 2824	DATAO PAG(L)	"DIAG OUT,DIAG FUNC/DATAO PAG"
; 2825	DIAG IN		"COND/DIAG FUNC,TIME/5T,AR/EBUS"
; 2826	DIAG OUT	"COND/DIAG FUNC,TIME/5T,ADA/AR,AD/A"
; 2827	DISMISS		"SPEC/FLAG CTL,FLAG CTL/DISMISS"
; 2828	DIVIDE		"FE_FE-1,DISP/DIV,MQ/MQ*2"
; 2829	DLEN		"FMADR/AC3"
; 2830	DLEN_AR		"DLEN,FM_AR"
; 2831	DROP EBUS REQ	"COND/EBUS CTL,EBUS CTL/0"
; 2832	DSTP		"FMADR/AC+#,AC-OP/AC+#,AC#/4"
; 2833	DSTP_AR		"DSTP,FM_AR"
; 2834	DSTP2		"FMADR/AC+#,AC-OP/AC+#,AC#/5"
; 2835	DSTP2_AR	"DSTP2,FM_AR"
; 2836	DSTW		"R14"
; 2837	DSTW_AR		"DSTW,FM_AR"
; 2838	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--E, F					

; 2839	.TOC	"CRAM Macros--E, F"
; 2840	
; 2841	E0		"R16"
; 2842	E0_AR		"E0,FM_AR"
; 2843	E1		"R5"
; 2844	E1_AR		"E1,FM_AR"
; 2845	EA MOD DISP	"DISP/EA MOD,AD/1S"
; 2846	EPT FETCH	"MEM/LOAD ARX,SPEC/SP MEM CYCLE,SP MEM/EPT FETCH"
; 2847	EPT REF		"SPEC/SP MEM CYCLE,SP MEM/EPT"
; 2848	EPT REF	CACHE	"SPEC/SP MEM CYCLE,SP MEM/EPT CACHE"
; 2849	EXEC REF	"SPEC/SP MEM CYCLE,SP MEM/EXEC"
; 2850	EXIT		"DISP/DRAM B,MEM/B WRITE,J/ST0"
; 2851	EXP TEST	"COND/REG CTL,EXP TST/AR_EXP"
; 2852	EXP TST		"COND/REG CTL,EXP TST/AR_EXP"
; 2853	EXPMSK		"R4"		;[224][233]
; 2854	EXP_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,EXP_SCAD"
; 2855	EXP_-SC-1 TST	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,EXP_SCAD.C,EXP TST"
; 2856	EXP_1		"SCADA EN/0S,SCAD/A+1,EXP_SCAD"
; 2857	EXP_FE TST	"SCADA/FE,SCAD/A,EXP_SCAD.C,EXP TST"
; 2858	EXP_SC		"SCADA EN/0S,SCADB/SC,SCAD/A+B,EXP_SCAD"
; 2859	EXP_SC.MS	"MEM/ARL IND,ARL/ARMM,COND/LD AR0-8,ARMM/SCAD EXP,EXP_SCAD.MS";[224]
; 2860	EXP_SCAD	"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD EXP"
; 2861	EXP_SCAD.C	"COND/REG CTL,AR CTL/AR0-8 LOAD,AR/ARMM,ARMM/SCAD EXP"
; 2862	EXP_SCAD.MS	"SCADA EN/0S,SCADB/SC,SCAD/A+B"
; 2863	EXP_SIGN	"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/EXP_SIGN"
; 2864	EXP_SIGN.C	"COND/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/EXP_SIGN"
; 2865	EXP_SIGN.MS	"MEM/ARL IND,ARL/ARMM,COND/LD AR0-8,ARMM/EXP_SIGN";[224]
; 2866	EXP_SIGN.S	"SPEC/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/EXP_SIGN"
; 2867	EXT ADDR	"MEM/A RD,#/400,DISP/DRAM B"
; 2868	EXT BYTE READ	"MEM/EA CALC,EA CALC/BYTE RD,VMA/LOAD,GLOBAL"
; 2869	EXT BYTE RPW	"MEM/EA CALC,EA CALC/BYTE RPW,VMA/LOAD,GLOBAL";[337]
; 2870	EXT INDEX	"MEM/A RD,#/400,DISP/DRAM A RD"
; 2871	EXT INDRCT	"MEM/EA CALC,EA CALC/A IND,VMA/LOAD"
; 2872	
; 2873	FE_#		"SCADA/#,SCAD/A,FE/SCAD"
; 2874	FE_# AND S	"SCADA/#,SCADB/AR6-11,SCAD/AND,FE/SCAD"
; 2875	FE_#+AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/A+B,FE/SCAD"
; 2876	FE_#+SC		"SCADA/#,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2877	FE_#-SC		"SCADA/#,SCADB/SC,SCAD/A-B,FE/SCAD"
; 2878	FE_+#		"SCADA EN/0S,SCADB/#,SCAD/A+B,FE/SCAD"
; 2879	FE_-1		"SCADA EN/0S,SCAD/A-1,FE/SCAD"
; 2880	FE_-S		"SCADA EN/0S,SCADB/AR6-11,SCAD/A-B,FE/SCAD";[337]
; 2881	FE_-SC		"SCADA EN/0S,SCADB/SC,SCAD/A-B,FE/SCAD"
; 2882	FE_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,FE/SCAD"
; 2883	FE_0		"SCADA EN/0S,SCAD/A,FE/SCAD"
; 2884	FE_1		"SCADA EN/0S,SCAD/A+1,FE/SCAD"
; 2885	FE_AR0-8	"SCADA EN/0S,SCADB/AR0-8,SCAD/A+B,FE/SCAD"
; 2886	FE_AR0-8 AND #	"SCADA/#,SCADB/AR0-8,SCAD/AND,FE/SCAD"
; 2887	FE_AR0-8 COMP	"SCADA EN/0S,SCADB/AR0-8,SCAD/A-B-1,FE/SCAD"
; 2888	FE_EXP		"SCADA/AR EXP,SCAD/A,FE/SCAD"
; 2889	FE_EXP+1	"SCADA/AR EXP,SCAD/A+1,FE/SCAD"
; 2890	FE_EXP+SC	"SCADA/AR EXP,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2891	FE_EXP-#	"SCADA/AR EXP,SCADB/#,SCAD/A-B,FE/SCAD"
; 2892	FE_EXP-1	"SCADA/AR EXP,SCAD/A-1,FE/SCAD"
; 2893	FE_FE AND #	"SCADA/FE,SCADB/#,SCAD/AND,FE/SCAD"
; 2894	FE_FE AND AR0-8	"SCADA/FE,SCADB/AR0-8,SCAD/AND,FE/SCAD"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--E, F					

; 2895	FE_FE OR #	"SCADA/FE,SCADB/#,SCAD/OR,FE/SCAD"
; 2896	FE_FE OR AR0-8	"SCADA/FE,SCADB/AR0-8,SCAD/OR,FE/SCAD"
; 2897	FE_FE SHRT	"COND/FE SHRT,FE/0"
; 2898	FE_FE+#		"SCADA/FE,SCADB/#,SCAD/A+B,FE/SCAD"
; 2899	FE_FE+1		"SCADA/FE,SCAD/A+1,FE/SCAD"
; 2900	FE_FE+S		"SCADA/FE,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2901	FE_FE+SC	"SCADA/FE,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2902	FE_FE-#		"SCADA/FE,SCADB/#,SCAD/A-B,FE/SCAD"
; 2903	FE_FE-1		"SCADA/FE,SCAD/A-1,FE/SCAD"
; 2904	FE_FE-S		"SCADA/FE,SCADB/AR6-11,SCAD/A-B,FE/SCAD"
; 2905	FE_FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B,FE/SCAD"
; 2906	FE_FE-SC-1	"SCADA/FE,SCADB/SC,SCAD/A-B-1,FE/SCAD"	;[343]
; 2907	FE_P		"SCADA/AR0-5,SCAD/A,FE/SCAD"
; 2908	FE_P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND,FE/SCAD"
; 2909	FE_P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND,FE/SCAD"
; 2910	FE_P OR #	"SCADA/AR0-5,SCADB/#,SCAD/OR,FE/SCAD"
; 2911	FE_P+#		"SCADA/AR0-5,SCADB/#,SCAD/A+B,FE/SCAD"
; 2912	FE_P+1		"SCADA/AR0-5,SCAD/A+1,FE/SCAD"		;[343]
; 2913	FE_P+S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2914	FE_P+SC		"SCADA/AR0-5,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2915	FE_P-#		"SCADA/AR0-5,SCADB/#,SCAD/A-B,FE/SCAD"
; 2916	FE_P-S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A-B,FE/SCAD"
; 2917	FE_S		"SCADA EN/0S,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2918	FE_S+#		"SCADA/#,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2919	FE_SC		"SCADA EN/0S,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2920	
; 2921	FETCH		"MEM/IFET"
; 2922	FETCH+1		"COND/VMA INC,MEM/IFET"
; 2923	FETCH WAIT	"MEM/MB WAIT"		;See edit 111
; 2924	FILL		"R13"
; 2925	FILL_AR		"FILL,FM_AR"
; 2926	FIN LOAD	"ADB/FM,FMADR/VMA,AD/B"	;Finish load of AR or ARX, start new op
; 2927	FIN STORE	"FMADR/VMA"		;FINISH STORE, start new operation
; 2928	FIN XFER	"FMADR/VMA,ADB/FM,AD/B"	;Same as FIN LOAD
; 2929	FINISH		"J/FINI"	;USE INSTEAD OF NXT INSTR IF FM WRITE
; 2930	FM_AR		"COND/FM WRITE"
; 2931	FM[]_AR		"@1, FM_AR"
; 2932	FM(#)_AR	"FMADR/AC+#,AC-OP/AC+#,COND/FM WRITE"
; 2933	FORCE AR-ARX	"ADB/AR*4,AD/B,AR/AD*.25,ARX/ADX*.25"
; 2934	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--G, H, I, J, L				

; 2935	.TOC	"CRAM Macros--G, H, I, J, L"
; 2936	
; 2937	GEN # AND AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/AND"
; 2938	GEN # AND SC	"SCADA/#,SCADB/SC,SCAD/AND"
; 2939	GEN #+AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/A+B"
; 2940	GEN #+SC	"SCADA/#,SCADB/SC,SCAD/A+B"
; 2941	GEN #-S		"SCADA/#,SCADB/AR6-11,SCAD/A-B"
; 2942	GEN #-SC	"SCADA/#,SCADB/SC,SCAD/A-B"
; 2943	GEN -AC0	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A-B"
; 2944	GEN -AR LONG	"ADB/AR*4,ADA EN/0S,AD/A-B,SPEC/AD LONG"
; 2945	GEN -AR*4	"ADA EN/0S,ADB/AR*4,AD/A-B"
; 2946	GEN -BR LONG	"ADA EN/0S,ADB/BR,AD/A-B,SPEC/AD LONG"
; 2947	GEN -SC		"SCADB/SC,SCADA EN/0S,SCAD/A-B"
; 2948	GEN -SC-1	"SCADB/SC,SCADA EN/0S,SCAD/A-B-1"
; 2949	GEN 0S		"AD/0S"
; 2950	GEN 2AR		"ADA/AR, AD/A*2"
; 2951	
; 2952	GEN AC0		"FMADR/AC0,ADB/FM,AD/B"
; 2953	GEN AC0+1	"FMADR/AC0,ADB/FM,ADA EN/0S,AD/A+B+1"
; 2954	GEN AR		"ADA/AR,AD/A"
; 2955	GEN AR*2 LONG	"ADA/AR,AD/A*2,SPEC/AD LONG"
; 2956	GEN AR*AC0	"FMADR/AC0,ADB/FM,ADA/AR"
; 2957	GEN AR*BR	"ADA/AR,ADB/BR"
; 2958	GEN AR*T0	"T0,ADB/FM,ADA/AR"
; 2959	GEN AR+1	"ADA/AR,AD/A+1"
; 2960	GEN AR+2BR	"ADA/AR,ADB/BR*2,AD/A+B"
; 2961	GEN AR+BR	"ADA/AR,ADB/BR,AD/A+B"
; 2962	GEN AR+E1	"E1,ADB/FM,ADA/AR,AD/A+B"
; 2963	GEN AR+FM[]	"ADA/AR,ADB/FM,@1,AD/A+B"
; 2964	GEN AR+XR	"FMADR/XR,ADB/FM,ADA/AR,AD/A+B"
; 2965	GEN AR-2BR	"ADA/AR,ADB/BR*2,AD/A-B"
; 2966	GEN AR-AC3	"FMADR/AC3,ADB/FM,ADA/AR,AD/A-B"
; 2967	GEN AR-BR	"ADA/AR,ADB/BR,AD/A-B"
; 2968	GEN AR-BR-1	"GEN AR*BR,AD/A-B-1"
; 2969	GEN AR-FM[]	"ADA/AR,ADB/FM,@1,AD/A-B"
; 2970	GEN AR-FM[]-1	"ADA/AR,ADB/FM,@1,AD/A-B-1"
; 2971	GEN AR0-8	"SCADA EN/0S,SCADB/AR0-8,SCAD/OR"
; 2972	GEN ARX		"ADA/ARX,AD/A"
; 2973	GEN ARX COMP	"ADA/ARX,AD/SETCA"
; 2974	GEN ARX*BR	"ADA/ARX,ADB/BR"		;[224]
; 2975	GEN ARX*BRX	"ADA EN/EN,ADB/BR"
; 2976	GEN ARX+1	"ADA/ARX,AD/A+1"
; 2977	GEN ARX+XR	"FMADR/XR,ADB/FM,ADA/ARX,AD/A+B"
; 2978	GEN ARX-1	"ADA/ARX,AD/A-1"
; 2979	GEN ARX-FM[]	"ADA/ARX,ADB/FM,@1,AD/A-B"
; 2980	
; 2981	GEN BR		"ADB/BR,AD/B"
; 2982	GEN BR*2	"ADB/BR*2,AD/B"
; 2983	GEN BR+ARX	"ADA/ARX,ADB/BR,AD/A+B"		;[230]
; 2984	GEN BRX+1	"ADA EN/0S,ADB/BR,AD/A+B+1"
; 2985	GEN CRY18	"SPEC/GEN CRY18"
; 2986	GEN E1		"E1, ADB/FM, AD/B"
; 2987	GEN FE		"SCADA/FE,SCAD/A"
; 2988	GEN FE AND #	"SCADA/FE,SCADB/#,SCAD/AND"
; 2989	GEN FE AND AR0-8 "SCADA/FE,SCADB/AR0-8,SCAD/AND"
; 2990	GEN FE AND S	"SCADA/FE,SCADB/AR6-11,SCAD/AND"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--G, H, I, J, L				

; 2991	GEN FE AND SC	"SCADA/FE,SCADB/SC,SCAD/AND"
; 2992	GEN FE OR AR0-8	"SCADA/FE,SCADB/AR0-8,SCAD/OR"	;[347]
; 2993	GEN FE+#	"SCADA/FE,SCADB/#,SCAD/A+B"
; 2994	GEN FE-#	"SCADA/FE,SCADB/#,SCAD/A-B"
; 2995	GEN FE-1	"SCADA/FE,SCAD/A-1"
; 2996	GEN FE-S	"SCADA/FE,SCADB/AR6-11,SCAD/A-B"
; 2997	GEN FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B"
; 2998	GEN FE-SC-1	"SCADA/FE,SCADB/SC,SCAD/A-B-1"	; [303] For DPB to top byte
; 2999	GEN FM[]	"ADB/FM,@1,AD/B"
; 3000	GEN MQ		"ADA/MQ,AD/A"
; 3001	GEN MQ*AC0	"ADA/MQ,ADB/FM,FMADR/AC0"
; 3002	GEN MQ-BR	"ADA/MQ,ADB/BR,AD/A-B"
; 3003	GEN MQ-BR-1	"ADA/MQ,ADB/BR,AD/A-B-1"
; 3004	GEN P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND"
; 3005	GEN P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND"
; 3006	GEN P+SC	"SCADA/AR0-5,SCADB/SC,SCAD/A+B"
; 3007	GEN P-#		"SCADA/AR0-5,SCADB/#,SCAD/A-B"		;[337]
; 3008	GEN P-S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A-B"
; 3009	GEN P-SC	"SCADA/AR0-5,SCADB/SC,SCAD/A-B"
; 3010	GEN SC		"SCADB/SC,SCADA EN/0S,SCAD/A+B"
; 3011	GEN SCAD 0S	"SCADA EN/0S,SCAD/A"
; 3012	GEN T1		"T1,ADB/FM,AD/B"
; 3013	GEN T2		"T2,ADB/FM,AD/B"
; 3014	GET ECL EBUS	"COND/EBUS CTL,EBUS CTL/GRAB EEBUS"
; 3015	GLOBAL		"SH/1"
; 3016	
; 3017	HALT		"SPEC/FLAG CTL,FLAG CTL/HALT"
; 3018	HARDPFW		"R17"			;Hard page fail word
; 3019	I FETCH		"VMA/PC+1,MEM/IFET"
; 3020	INDEXED		"SH/2"
; 3021	INH CRY18	"SPEC/INH CRY18"
; 3022	IO INIT		"COND/EBUS CTL,EBUS CTL/IO INIT"
; 3023	IR DISP		"DISP/DRAM J"
; 3024	
; 3025	JFCL FETCH	"VMA/PC+1,MEM/FETCH,FETCH/JFCL"
; 3026	JFCL S		"SPEC/FLAG CTL,FLAG CTL/JFCL+LD"
; 3027	JFCL T		"SPEC/FLAG CTL,FLAG CTL/JFCL"
; 3028	JUMP FETCH	"VMA/PC+1,MEM/FETCH,FETCH/JUMP"
; 3029	
; 3030	LD PCS		"COND/DIAG FUNC,TIME/5T,DIAG FUNC/LD PCS+CWSX,ADA/ARX,AD/A"
; 3031	LD PREV CTXT	"COND/DIAG FUNC,TIME/5T,DIAG FUNC/LD PCS+CWSX,ADA/PC,AD/A"
; 3032	LOAD AR		"MEM/LOAD AR"
; 3033	LOAD AR (RPW)	"MEM/RPW"
; 3034	LOAD AR (WR TST)	"MEM/EA CALC,EA CALC/LD AR+WR"
; 3035	LOAD ARX	"MEM/LOAD ARX"
; 3036	LOAD ARX (WR TST)	"MEM/EA CALC,EA CALC/LD ARX+WR"
; 3037	LOAD EBR	"MEM/REG FUNC,MREG FNC/LOAD EBR"
; 3038	LOAD IR		"COND/LOAD IR,PXCT/0"
; 3039	LOAD UBR	"MEM/REG FUNC,MREG FNC/LOAD UBR"
; 3040	LOAD VMA(EA)	"MEM/EA CALC,EA CALC/LD AR(EA),VMA/LOAD"
; 3041	LONG EN		"COND/LONG EN"
; 3042	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--M, N, O, P					

; 3043	.TOC	"CRAM Macros--M, N, O, P"
; 3044	
; 3045	MAP		"MEM/REG FUNC,MREG FNC/MAP"
; 3046	MB WAIT		"MEM/MB WAIT"
; 3047	MEM_AR		"FMADR/VMA,MEM/MB WAIT"
; 3048	
; 3049	MQ_0.C		"COND/ARL IND,CLR/MQ"
; 3050	MQ_0.M		"MEM/ARL IND,CLR/MQ"
; 3051	MQ_0.S		"SPEC/ARL IND,CLR/MQ"
; 3052	MQ_1		"ADA EN/0S,AD/A+1,MQ_AD"
; 3053	MQ_1S		"COND/REG CTL,MQ/MQM SEL,MQ CTL/1S"
; 3054	MQ_AC0		"ADB/FM,FMADR/AC0,AD/B,MQ_AD"
; 3055	MQ_AD		"COND/REG CTL,MQ/MQM SEL,MQ CTL/AD"
; 3056	MQ_AR		"SH/AR,MQ/SH"
; 3057	MQ_AR (AD)	"ADA/AR,AD/A,MQ_AD"
; 3058	MQ_AR COMP	"ADA/AR,AD/SETCA,MQ_AD"
; 3059	MQ_AR SWAP	"SH/AR SWAP,MQ/SH"
; 3060	MQ_AR XOR BR	"ADA/AR,ADB/BR,AD/XOR,MQ_AD"
; 3061	MQ_AR-1		"ADA/AR,AD/A-1,MQ_AD"
; 3062	MQ_ARX		"SH/ARX,MQ/SH"
; 3063	MQ_ARX COMP	"ADA/ARX,AD/SETCA,MQ_AD"
; 3064	MQ_ARX*2	"ADA/ARX,AD/A*2,MQ_AD"
; 3065	MQ_ARX+BR	"ADA/ARX,ADB/BR,AD/A+B,MQ_AD"
; 3066	MQ_ARX-BR	"ADA/ARX,ADB/BR,AD/A-B,MQ_AD"
; 3067	MQ_BR		"ADB/BR,AD/B,MQ_AD"
; 3068	MQ_BR COMP	"ADB/BR,AD/SETCB,MQ_AD"
; 3069	MQ_FM[]		"ADB/FM,@1,AD/B,MQ_AD"			;[343]
; 3070	MQ_MQ*.25	"SPEC/MQ SHIFT,MQ/MQ*.25"
; 3071	MQ_MQ*2		"SPEC/MQ SHIFT,MQ/MQ*2"
; 3072	MQ_MQ*BR	"ADA/MQ, ADB/BR, MQ_AD"
; 3073	MQ_MQ+1		"ADA/MQ,AD/A+1,MQ_AD"
; 3074	MQ_MQ-1		"ADA/MQ,AD/A-1,MQ_AD"			;[343]
; 3075	MQ_SHIFT	"SH/SHIFT AR!ARX,MQ/SH"
; 3076	
; 3077	MSK		"R7"
; 3078	MSK_AR		"MSK,FM_AR"
; 3079	MUL		"FE_FE+1,DISP/MUL,MQ/MQ*.25"
; 3080	MUL DISP	"DISP/MUL,MQ/MQ*.25"
; 3081	
; 3082	NO CRY		"AD/SETCA"
; 3083	NORM		"DISP/NORM"
; 3084	NORM -AR	"ADA EN/0S,ADB/AR*4,AD/A-B,AR/AD*.25,ARX/ADX*.25,DISP/NORM"
; 3085	NORM AR		"ADB/AR*4,AD/B,DISP/NORM"
; 3086	NXT INSTR	"MEM/MB WAIT,DISP/NICOND,#/0,CLR SC,CLR FE,J/NEXT"
; 3087	
; 3088	OPTIONS		"KLPAGE/OPTIONS,LONGPC/OPTIONS,NONSTD/OPTIONS,PV/OPTIONS,PMOVE/OPTIONS,ISTAT/OPTIONS"
; 3089	
; 3090	P_#		"SCADA/#,SCAD/A,P_SCAD#"
; 3091	P_# AND SC	"SCADA/#,SCADB/SC,SCAD/AND,P_SCAD#"
; 3092	P_#+SC		"SCADA/#,SCADB/SC,SCAD/A+B,P_SCAD#"
; 3093	P_#-S		"SCADA/#,SCADB/AR6-11,SCAD/A-B,P_SCAD#"
; 3094	P_#-SC		"SCADA/#,SCADB/SC,SCAD/A-B,P_SCAD#"	;[343]
; 3095	P_-SC		"SCADA EN/0S,SCADB/SC,SCAD/A-B,P_SCAD"
; 3096	P_0		"SCADA EN/0S,SCAD/A,P_SCAD"
; 3097	P_1S		"SCADA EN/0S,SCAD/A-1,P_SCAD"
; 3098	P_FE		"SCADA/FE,SCAD/A,P_SCAD"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--M, N, O, P					

; 3099	P_FE AND #	"SCADA/FE,SCADB/#,SCAD/AND,P_SCAD#"
; 3100	P_FE OR SC	"SCADA/FE,SCADB/SC,SCAD/OR,P_SCAD"
; 3101	P_FE+S		"SCADA/FE,SCADB/AR6-11,SCAD/A+B,P_SCAD"
; 3102	P_FE+SC		"SCADA/FE,SCADB/SC,SCAD/A+B,P_SCAD#"
; 3103	P_FE-S		"SCADA/FE,SCADB/AR6-11,SCAD/A-B,P_SCAD.C"
; 3104	P_FE-S.S	"SCADA/FE,SCADB/AR6-11,SCAD/A-B,P_SCAD.S"
; 3105	P_FE.C		"SCADA/FE,SCAD/A,P_SCAD#"
; 3106	P_P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND,P_SCAD#"
; 3107	P_P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND,P_SCAD.C"
; 3108	P_P OR #	"SCADA/AR0-5,SCADB/#,SCAD/OR,P_SCAD#"
; 3109	P_P OR SC	"SCADA/AR0-5,SCADB/SC,SCAD/OR,P_SCAD#"
; 3110	P_P+#		"SCADA/AR0-5,SCADB/#,SCAD/A+B,P_SCAD#"
; 3111	P_P+1		"SCADA/AR0-5,SCAD/A+1,P_SCAD#"		;[337]
; 3112	P_P+S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,P_SCAD"
; 3113	P_P+S.C		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,P_SCAD#"
; 3114	P_P-S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A-B,P_SCAD"
; 3115	P_SC		"SCADA EN/0S,SCADB/SC,SCAD/A+B,P_SCAD"
; 3116	P_SC#		"SCADA EN/0S,SCADB/SC,SCAD/A+B,P_SCAD#"
; 3117	P_SCAD		"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD POS"
; 3118	P_SCAD#		"COND/LD AR0-8,AR/ARMM,ARMM/SCAD POS"
; 3119	P_SCAD.C	"COND/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD POS"
; 3120	P_SCAD.S	"SPEC/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD POS"
; 3121	
; 3122	P0		"FMADR/#B#,ACB/PAGB,AC#/0"	Paging AC 0
; 3123	P1		"FMADR/#B#,ACB/PAGB,AC#/1"
; 3124	P10		"FMADR/#B#,ACB/PAGB,AC#/10"
; 3125	P11		"FMADR/#B#,ACB/PAGB,AC#/11"
; 3126	P12		"FMADR/#B#,ACB/PAGB,AC#/12"
; 3127	P13		"FMADR/#B#,ACB/PAGB,AC#/13"
; 3128	P14		"FMADR/#B#,ACB/PAGB,AC#/14"
; 3129	P15		"FMADR/#B#,ACB/PAGB,AC#/15"
; 3130	P16		"FMADR/#B#,ACB/PAGB,AC#/16"
; 3131	P17		"FMADR/#B#,ACB/PAGB,AC#/17"
; 3132	P2		"FMADR/#B#,ACB/PAGB,AC#/2"
; 3133	P3		"FMADR/#B#,ACB/PAGB,AC#/3"
; 3134	P4		"FMADR/#B#,ACB/PAGB,AC#/4"
; 3135	P5		"FMADR/#B#,ACB/PAGB,AC#/5"
; 3136	P6		"FMADR/#B#,ACB/PAGB,AC#/6"
; 3137	P7		"FMADR/#B#,ACB/PAGB,AC#/7"
; 3138	PC_VMA		"SPEC/LOAD PC"
; 3139	PF DISP		"DISP/PG FAIL"
; 3140	PFA		"P4"
; 3141	PFA_AR		"PFA,COND/FM WRITE"
; 3142	PHYS REF	"SPEC/SP MEM CYCLE,SP MEM/UNPAGED"
; 3143	PHYS REF CACHE	"SPEC/SP MEM CYCLE,SP MEM/UNPAGED+CACHED"
; 3144	POP AR		"MEM/EA CALC,EA CALC/POP AR,VMA/LOAD"
; 3145	POP AR-ARX	"MEM/EA CALC,EA CALC/POP AR-ARX,VMA/LOAD"
; 3146	POP ARX		"MEM/EA CALC,EA CALC/POP ARX,VMA/LOAD"
; 3147	PORTAL		"SPEC/FLAG CTL,FLAG CTL/PORTAL"
; 3148	PT FETCH	"MEM/LOAD ARX,SPEC/SP MEM CYCLE,SP MEM/PT FETCH"
; 3149	PT REF		"SPEC/SP MEM CYCLE,SP MEM/PT"
; 3150	PT SEL_INVAL	"COND/MBOX CTL,MBOX CTL/PT DIR CLR"
; 3151	PT SEL_INVAL (KEEP) "COND/MBOX CTL,MBOX CTL/PT DIR CLR(NK)"
; 3152	PT SEL_NORMAL	"COND/MBOX CTL,MBOX CTL/NORMAL"
; 3153	PUR		"P1"
; 3154	PUSH		"MEM/EA CALC,EA CALC/PUSH,VMA/LOAD,SPEC/STACK UPDATE"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--R						

; 3155	.TOC	"CRAM Macros--R"
; 3156	
; 3157	R0		"FMADR/#B#,ACB/MICROB,AC#/0"	Scratch register 0
; 3158	R1		"FMADR/#B#,ACB/MICROB,AC#/1"	Scratch register 1
; 3159	R10		"FMADR/#B#,ACB/MICROB,AC#/10"	Scratch register 10
; 3160	R11		"FMADR/#B#,ACB/MICROB,AC#/11"	Scratch register 11
; 3161	R12		"FMADR/#B#,ACB/MICROB,AC#/12"	Scratch register 12
; 3162	R13		"FMADR/#B#,ACB/MICROB,AC#/13"	Scratch register 13
; 3163	R14		"FMADR/#B#,ACB/MICROB,AC#/14"	Scratch register 14
; 3164	R15		"FMADR/#B#,ACB/MICROB,AC#/15"	Scratch register 15
; 3165	R16		"FMADR/#B#,ACB/MICROB,AC#/16"	Scratch register 16
; 3166	R17		"FMADR/#B#,ACB/MICROB,AC#/17"	Scratch register 17
; 3167	R2		"FMADR/#B#,ACB/MICROB,AC#/2"	Scratch register 2
; 3168	R3		"FMADR/#B#,ACB/MICROB,AC#/3"	Scratch register 3
; 3169	R4		"FMADR/#B#,ACB/MICROB,AC#/4"	Scratch register 4
; 3170	R5		"FMADR/#B#,ACB/MICROB,AC#/5"	Scratch register 5
; 3171	R6		"FMADR/#B#,ACB/MICROB,AC#/6"	Scratch register 6
; 3172	R7		"FMADR/#B#,ACB/MICROB,AC#/7"	Scratch register 7
; 3173	
; 3174	RD+CLR C CNT	"SPEC/MTR CTL,AR_CACHE CNT"
; 3175	RD+CLR E CNT	"SPEC/MTR CTL,AR_EBOX CNT"
; 3176	RD+CLR PA	"SPEC/MTR CTL,AR_PERF CNT"
; 3177	RD+CLR TB	"SPEC/MTR CTL,AR_TIME BASE"
; 3178	READ BP2	"MEM/EA CALC,EA CALC/BYTE IND,VMA/VMA,COND/VMA INC"
; 3179	READ EBR	"MEM/REG FUNC,MREG FNC/READ EBR"
; 3180	READ ERA	"MEM/REG FUNC,MREG FNC/READ ERA"
; 3181	READ UBR	"MEM/REG FUNC,MREG FNC/READ UBR"
; 3182	REL EBUS	"COND/EBUS CTL,EBUS CTL/REL EBUS"
; 3183	REL ECL EBUS	"COND/EBUS CTL,EBUS CTL/REL EEBUS"
; 3184	REQ EBUS	"COND/EBUS CTL,EBUS CTL/REQ EBUS"
; 3185	REQ SV.VMA	"SV.VMA,ADB/FM,AD/B,VMA/1,MEM/AD FUNC"
; 3186	REQ VMA HELD	"COND/SEL VMA,ADA/PC,AD/A,VMA/1,MEM/AD FUNC"
; 3187	
; 3188	RET[]		"DISP/RETURN,J/@1"
; 3189	RETURN []	"DISP/RETURN,J/@1"
; 3190	RETURN0		"DISP/RETURN,J/0"
; 3191	RETURN1		"DISP/RETURN,J/1"
; 3192	RETURN10	"DISP/RETURN,J/10"
; 3193	RETURN12	"DISP/RETURN,J/12"
; 3194	RETURN15	"DISP/RETURN,J/15"			;[343]
; 3195	RETURN16	"DISP/RETURN,J/16"			;[337]
; 3196	RETURN17	"DISP/RETURN,J/17"			;[337]
; 3197	RETURN2		"DISP/RETURN,J/2"
; 3198	RETURN20	"DISP/RETURN,J/20"
; 3199	RETURN3		"DISP/RETURN,J/3"
; 3200	RETURN30	"DISP/RETURN,J/30"
; 3201	RETURN37	"DISP/RETURN,J/37"
; 3202	RETURN4		"DISP/RETURN,J/4"
; 3203	RETURN5		"DISP/RETURN,J/5"
; 3204	RETURN6		"DISP/RETURN,J/6"
; 3205	RETURN7		"DISP/RETURN,J/7"
; 3206	RSTR FLAGS_AR	"SPEC/FLAG CTL,FLAG CTL/RSTR FLAGS"
; 3207	RSTR VMA_ARX	"ADA/ARX,AD/A,VMA/LOAD,MEM/RESTORE VMA"
; 3208	RSTR VMA_MQ	"ADA/MQ,AD/A,VMA/LOAD,MEM/RESTORE VMA"
; 3209	RSTR VMA_SV.VMA	"SV.VMA,ADB/FM,AD/B,VMA/LOAD,MEM/RESTORE VMA"
; 3210	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--S						

; 3211	.TOC	"CRAM Macros--S"
; 3212	
; 3213	SBR		"P3"
; 3214	SBUS DIAG	"MEM/REG FUNC,MREG FNC/SBUS DIAG"
; 3215	
; 3216	SC_#		"SCADA/#,SCAD/A,SC/SCAD"
; 3217	SC_# AND AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/AND,SC/SCAD"
; 3218	SC_# AND S	"SCADA/#,SCADB/AR6-11,SCAD/AND,SC/SCAD"
; 3219	SC_# OR SC	"SCADA/#,SCADB/SC,SCAD/OR,SC/SCAD"
; 3220	SC_#+AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/A+B,SC/SCAD"
; 3221	SC_#+SC		"SCADA/#,SCADB/SC,SCAD/A+B,SC/SCAD"
; 3222	SC_#-S		"SCADA/#,SCADB/AR6-11,SCAD/A-B,SC/SCAD"
; 3223	SC_#-SC		"SCADA/#,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3224	SC_#-SC-1	"SCADA/#,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3225	SC_-S		"SCADA EN/0S,SCADB/AR6-11,SCAD/A-B,SC/SCAD";[343]
; 3226	SC_-SC		"SCADA EN/0S,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3227	SC_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3228	SC_0		"SCADA EN/0S,SCAD/A,SC/SCAD"
; 3229	SC_1		"SCADA EN/0S,SCAD/A+1,SC/SCAD"
; 3230	SC_1S		"SCADA EN/0S,SCAD/A-1,SC/SCAD"
; 3231	SC_AR0-8	"SCADA EN/0S,SCADB/AR0-8,SCAD/A+B,SC/SCAD" ;[231]
; 3232	SC_EA		"SPEC/SCM ALT,SC/AR SHIFT"
; 3233	SC_EXP		"SCADA/AR EXP,SCAD/A,SC/SCAD"
; 3234	SC_EXP+1	"SCADA/AR EXP,SCAD/A+1,SC/SCAD"
; 3235	SC_EXP+SC	"SCADA/AR EXP,SCADB/SC,SCAD/A+B,SC/SCAD"
; 3236	SC_EXP-#	"SCADA/AR EXP,SCADB/#,SCAD/A-B,SC/SCAD"
; 3237	SC_EXP-1	"SCADA/AR EXP,SCAD/A-1,SC/SCAD"
; 3238	SC_EXP-SC	"SCADA/AR EXP,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3239	SC_FE		"SPEC/SCM ALT,SC/FE"
; 3240	SC_FE AND #	"SCADA/FE,SCADB/#,SCAD/AND,SC/SCAD"
; 3241	SC_FE#		"SCADA/FE,SCAD/A,SC/SCAD"	  ;[337] If SPEC is in conflict
; 3242	SC_FE+#		"SCADA/FE,SCADB/#,SCAD/A+B,SC/SCAD"
; 3243	SC_FE+1		"SCADA/FE,SCAD/A+1,SC/SCAD"
; 3244	SC_FE+S		"SCADA/FE,SCADB/AR6-11,SCAD/A+B,SC/SCAD"
; 3245	SC_FE+SC	"SCADA/FE,SCADB/SC,SCAD/A+B,SC/SCAD"
; 3246	SC_FE-#		"SCADA/FE,SCADB/#,SCAD/A-B,SC/SCAD"
; 3247	SC_FE-1		"SCADA/FE,SCAD/A-1,SC/SCAD"
; 3248	SC_FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3249	SC_FE-SC-1	"SCADA/FE,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3250	SC_P		"SCADA/AR0-5,SCAD/A,SC/SCAD"
; 3251	SC_P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND,SC/SCAD"
; 3252	SC_P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND,SC/SCAD"
; 3253	SC_P+#		"SCADA/AR0-5,SCADB/#,SCAD/A+B,SC/SCAD"
; 3254	SC_P+1		"SCADA/AR0-5,SCAD/A+1,SC/SCAD"		;[337]
; 3255	SC_P+S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,SC/SCAD";[343]
; 3256	SC_P-#		"SCADA/AR0-5,SCADB/#,SCAD/A-B,SC/SCAD"
; 3257	SC_P-SC-1	"SCADA/AR0-5,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3258	SC_S		"SCADB/AR6-11,SCADA EN/0S,SCAD/A+B,SC/SCAD"
; 3259	SC_SC AND #	"SCADA/#,SCADB/SC,SCAD/AND,SC/SCAD"
; 3260	
; 3261	SEL AC4		"AC-OP/AC+#,AC#/4"
; 3262	SEL AC5		"AC-OP/AC+#,AC#/5"
; 3263	SEL DSTP	"AC-OP/AC+#,AC#/4"
; 3264	SEL DSTP2	"AC-OP/AC+#,AC#/5"
; 3265	SET ACC+CLR UCODE	"COND/EBOX STATE,#/005"
; 3266	SET ACCOUNT EN	"COND/EBOX STATE,#/105"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--S						

; 3267	SET AROV	"COND/PCF_#,PC FLAGS/AROV"
; 3268	SET CONS XCT	"COND/SPEC INSTR,SPEC INSTR/CONS XCT"
; 3269	SET DATAI	"COND/EBUS CTL,EBUS CTL/DATAI,AD/0S,AR/AD"
; 3270	SET DATAO	"COND/EBUS CTL,EBUS CTL/DATAO"
; 3271	SET EBUS DEMAND	"COND/EBUS CTL,EBUS CTL/EBUS DEMAND"
; 3272	SET FL NO DIV	"COND/PCF_#,PC FLAGS/FDV CHK"
; 3273	SET FLAGS_AR	"SPEC/FLAG CTL,FLAG CTL/SET FLAGS"
; 3274	SET FLOV	"COND/PCF_#,PC FLAGS/FLOV"
; 3275	SET FPD		"COND/PCF_#,PC FLAGS/FPD"
; 3276	SET FXU		"COND/PCF_#,PC FLAGS/FXU"	;[224]
; 3277	SET HALTED	"COND/SPEC INSTR,SPEC INSTR/HALTED"
; 3278	SET IO PF	"COND/MBOX CTL,MBOX CTL/SET IO PF ERR"
; 3279	SET MTR PA EN	"COND/EBOX STATE,#/225"
; 3280	SET NO DIVIDE	"COND/PCF_#,PC FLAGS/DIV CHK"
; 3281	SET PC+1 INH	"COND/SPEC INSTR,SPEC INSTR/INH PC+1"
; 3282	SET PI CYCLE	"COND/SPEC INSTR,SPEC INSTR/SET PI CYCLE"
; 3283	SET PXCT	"COND/SPEC INSTR,SPEC INSTR/PXCT"
; 3284	SET SR1		"COND/SR_#,#/64"
; 3285	SET SR2		"COND/SR_#,#/62"
; 3286	SET SR3		"COND/SR_#,#/61"
; 3287	SET SXCT	"COND/SPEC INSTR,SPEC INSTR/SXCT"
; 3288	SET TRACKS EN	"COND/EBOX STATE,#/131"
; 3289	SET TRK+PA EN	"COND/EBOX STATE,#/231"
; 3290	SFLGS		"FMADR/AC0"
; 3291	SFLGS_AR	"SFLGS,FM_AR"
; 3292	SH DISP		"SH/SHIFT AR!ARX,DISP/SH0-3"
; 3293	SIGNS DISP	"DISP/SIGNS"
; 3294	SKIP FETCH	"ADA/AR,AD/A,VMA/PC+1,MEM/FETCH,FETCH/SKIP"
; 3295	
; 3296	SKP -EBUS GRANT	"SKIP/-EBUS GRANT"
; 3297	SKP -EBUS XFER	"SKIP/-EBUS XFER"
; 3298	SKP -LOCAL AC ADDR	"SKIP/-LOCAL AC ADDR"
; 3299	SKP -START	"SKIP/-START"
; 3300	SKP -VMA SEC0	"SKIP/-VMA SEC0"
; 3301	SKP AC EQ 0	"SKIP/AC#0"			;[343] More mnemonic than AC#0
; 3302	SKP AC REF	"SKIP/AC REF"
; 3303	SKP AC#0	"SKIP/AC#0"
; 3304	SKP AC0+	"FMADR/AC0,ADB/FM,AD/SETCB,SKIP/AD0"
; 3305	SKP AC0-	"FMADR/AC0,ADB/FM,AD/B,SKIP/AD0"
; 3306	SKP AD NE	"SKIP/AD#0"
; 3307	SKP AD NZ	"SKIP/AD#0"			;Mnemonic synonym
; 3308	SKP AD0		"SKIP/AD0"
; 3309	SKP ADX0	"SKIP/ADX0"
; 3310	SKP AR EQ	"ADA EN/0S,ADB/AR*4,AD/ORCB+1,SKIP/AD CRY0"
; 3311	SKP AR EQ -1	"ADA/AR,AD/CRY A EQ -1,SKIP/AD CRY0"
; 3312	SKP AR GT BR	"ADA/AR,ADB/BR,AD/XOR,SKIP/AD CRY0"
; 3313	SKP AR NE	"ADA/AR,AD/CRY A#0,SKIP/AD CRY0"
; 3314	SKP AR NE BR	"ADA/AR,ADB/BR,AD/XOR,SKIP/AD#0"
; 3315	SKP AR NZ	"ADA/AR,AD/A,SKIP/AD#0"			;[343]
; 3316	SKP AR SIG	"ADA/AR,AD/A+XCRY,SPEC/XCRY AR0,SKIP/AD#0"
; 3317	SKP AR0		"SKIP/AR0"
; 3318	SKP AR1		"ADA/AR,AD/A*2,SKIP/AD0"
; 3319	SKP AR18	"SKIP/AR18"
; 3320	SKP AR2		"ADB/AR*4,AD/B,SKIP/AD0"
; 3321	SKP AR6		"SCADB/AR6-11,SCADA/#,#/40,SCAD/AND,SKIP/SCAD#0"
; 3322	SKP ARX LE BRX	"ADA EN/EN,ADB/BR,AD/A-B-1,SKIP/ADX0"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10-2
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--S						

; 3323	SKP ARX LT BRX	"ADA EN/EN,ADB/BR,AD/A-B,SKIP/ADX0"
; 3324	SKP ARX NE	"ADA/ARX,AD/CRY A#0,SKIP/AD CRY0"
; 3325	SKP ARX NZ	"ADA/ARX,AD/A,SKIP/AD#0"		;[343]
; 3326	SKP ARX+MQ NE	"ADA/MQ,AD/CRY A#0,SPEC/AD LONG,SKIP/AD CRY0"
; 3327	SKP ARX0	"SKIP/ARX0"
; 3328	SKP ARX2	"ADB/AR*4,AD/B,SKIP/ADX0"
; 3329	
; 3330	SKP BR EQ	"ADA EN/0S,ADB/BR,AD/CRY A GE B,SKIP/AD CRY0"
; 3331	SKP BR EQ -1	"ADA EN/0S,ADB/BR,AD/A+B+1,SKIP/AD CRY0"
; 3332	SKP BR0		"SKIP/BR0"
; 3333	SKP CRY0	"SKIP/AD CRY0"
; 3334	SKP EVEN PAR	"SKIP/EVEN PAR"
; 3335	SKP EXP NE	"SCADA/AR EXP,SCAD/A,SKIP/SCAD#0"
; 3336	SKP FE NZ	"SCADA/FE,SCAD/A,SKIP/SCAD#0"
; 3337	SKP FE0		"SCADA/FE,SCAD/A,SKIP/SCAD0"
; 3338	SKP FETCH	"SKIP/FETCH"
; 3339	SKP INTRPT	"SKIP/INTRPT"
; 3340	SKP IO LEGAL	"SKIP/IO LEGAL"
; 3341	SKP KERNEL	"SKIP/KERNEL"
; 3342	SKP MQ EQ -1	"ADA/MQ,AD/CRY A EQ -1,SKIP/AD CRY0"
; 3343	SKP MQ NE	"ADA/MQ,AD/CRY A#0,SKIP/AD CRY0"
; 3344	SKP MQ NE BR	"ADA/MQ,ADB/BR,AD/XOR,SKIP/AD#0"
; 3345	SKP MQ NZ	"ADA/MQ,AD/A,SKIP/AD#0"		;Better than above
; 3346	SKP P NE	"SCADA/AR0-5,SCAD/A,SKIP/SCAD#0"
; 3347	SKP P!S XCT	"SKIP/P!S XCT"
; 3348	SKP PC SEC0	"SKIP/PC SEC0"
; 3349	SKP PI CYCLE	"SKIP/PI CYCLE"
; 3350	SKP RPW		"SKIP/RPW REF"
; 3351	SKP RUN		"SKIP/RUN"
; 3352	SKP SC NE	"SCADB/SC,SCADA EN/0S,SCAD/A+B,SKIP/SCAD#0"
; 3353	SKP SC NZ	"SCADA EN/0S,SCADB/SC,SCAD/A+B,SKIP/SCAD#0"
; 3354	SKP SC0		"SKIP/SC0"
; 3355	SKP SCAD NE	"SKIP/SCAD#0"
; 3356	SKP SCAD NZ	"SKIP/SCAD#0"			;[347]
; 3357	SKP SCAD0	"SKIP/SCAD0"
; 3358	SKP USER	"SKIP/USER"
; 3359	
; 3360	SLEN		"R10"		;MUST BE 170
; 3361	SLEN_AR		"SLEN,FM_AR"
; 3362	SR DISP		"DISP/SR"
; 3363	SR_#		"COND/SR_#"		;USED FOR NON-PAGE-FAIL APPLICATIONS
; 3364	SR_0		"COND/SR_#,#/0"
; 3365	SR_1		"COND/SR_#,#/1"
; 3366	SR_2		"COND/SR_#,#/2"		;[224]
; 3367	SR_BDD		"COND/SR_#,#/206"	;B2D AFTER UPDATING DST PTR
; 3368	SR_BDF		"COND/SR_#,#/203"	;B2D STORING FILLERS
; 3369	SR_BDT		"COND/SR_#,#/010"	;B2D IN TRANSLATION
; 3370	SR_BLT		"COND/SR_#,#/7"		;[426]
; 3371	SR_BLT(AC)	"COND/SR_#,#/407"
; 3372	SR_BLT(PXCT DST) "COND/SR_#,#/107"
; 3373	SR_BLT(PXCT SRC) "COND/SR_#,#/307"
; 3374	SR_DB		"COND/SR_#,#/102"	;D2B ANYWHERE
; 3375	SR_DST		"COND/SR_#,#/212"
; 3376	SR_DSTF		"COND/SR_#,#/214"
; 3377	SR_ED(+D)	"COND/SR_#,#/224"
; 3378	SR_ED(PAT)	"COND/SR_#,#/0"		;PATTERN REF IS EXTENDED; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10-3
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--S						

; 3379	SR_ED(S)	"COND/SR_#,#/101"
; 3380	SR_MAP		"COND/SR_#,#/15"	;CATCH MAP PAGE FAILURES
; 3381	SR_SRC		"COND/SR_#,#/111"
; 3382	SR_SRC+DST	"COND/SR_#,#/213"
; 3383	SR_WORD		"COND/SR_#,#/17"	;WORD MOVES OF STRING INSTRS
; 3384	SR_XBLT(DST)	"COND/SR_#,#/316"
; 3385	SR_XBLT(SRC)	"COND/SR_#,#/216"
; 3386	SRCP		"FMADR/AC1"
; 3387	SRCP_AR		"SRCP,FM_AR"
; 3388	SRCP2		"FMADR/AC2"
; 3389	SRCP2_AR	"SRCP2,FM_AR"
; 3390	
; 3391	STACK UPDATE	"SPEC/STACK UPDATE"
; 3392	STORE		"MEM/WRITE"
; 3393	STORE VMA(EA)	"MEM/EA CALC,EA CALC/WRITE(E),VMA/LOAD"
; 3394	SV.AR		"P16"		;156 REQUIRED FOR PF.PAR HACK
; 3395	SV.AR_AR	"SV.AR,COND/FM WRITE"
; 3396	SV.ARX		"P17"		;157 REQUIRED FOR PF.PAR HACK
; 3397	SV.ARX_AR	"SV.ARX,COND/FM WRITE"
; 3398	SV.BR		"P10"
; 3399	SV.BR_AR	"SV.BR,COND/FM WRITE"
; 3400	SV.IOP		"R3"		;[233]
; 3401	SV.IOPF		"R2"
; 3402	SV.IOPF_AR	"SV.IOPF,COND/FM WRITE"	;IO PAGE FAIL WORD
; 3403	SV.PAR		"R0"		;Note not in PAGB block
; 3404	SV.PAR_AR	"SV.PAR,COND/FM WRITE"
; 3405	SV.PFW		"P12"
; 3406	SV.PFW_AR	"SV.PFW,COND/FM WRITE"
; 3407	SV.SC		"P11"
; 3408	SV.SC_AR	"SV.SC,COND/FM WRITE"
; 3409	SV.VMA		"P5"
; 3410	SV.VMA_AR	"SV.VMA,COND/FM WRITE"
; 3411	SWD		"R1"		;BUFFER FOR SOURCE BYTE WORD
; 3412	SWD_AR		"SWD,FM_AR"
; 3413	SWEEP CACHE	"MEM/REG FUNC,MREG FNC/LOAD CCA"
; 3414	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--T, U, V, W, X				

; 3415	.TOC	"CRAM Macros--T, U, V, W, X"
; 3416	
; 3417	T0		"R6"
; 3418	T0_AR		"T0,FM_AR"
; 3419	T1		"R11"
; 3420	T1_AR		"T1,FM_AR"
; 3421	T2		"R12"
; 3422	T2_AR		"T2,FM_AR"
; 3423	TAKE INTRPT	"SKIP/-MTR REQ,J/MTRINT"
; 3424	TEST AR		"ADA/AR,AD/CRY A#0"
; 3425	TEST AR.AC0	"FMADR/AC0,ADB/FM,ADA/AR,AD/CRY A.B#0"
; 3426	TEST AR.BR	"ADB/BR,ADA/AR,AD/CRY A.B#0"
; 3427	TEST AR.MSK	"MSK,ADB/FM,ADA/AR,AD/CRY A.B#0"
; 3428	TEST ARX	"ADA/ARX,AD/CRY A#0"
; 3429	TEST ARX.AR*4	"ADA/ARX,ADB/AR*4,AD/CRY A.B#0"
; 3430	TEST BRL	"ADA EN/0S,ADB/BR,AD/ORCB+1,GEN CRY18"
; 3431	TEST CBR	"CBR,ADB/FM,AD/B,SKP AD NE"	;[247]
; 3432	TEST FETCH	"VMA/PC+1,MEM/FETCH,FETCH/TEST"
; 3433	TRAP1		"COND/PCF_#,PC FLAGS/TRAP1"
; 3434	TRAP2		"COND/PCF_#,PC FLAGS/TRAP2"
; 3435	TRAP3		"COND/PCF_#,PC FLAGS/TRAP3"
;;3436	.IF/TRXDEF
;;3437	TRB		"E0"		;same as E0.
;;3438	TRB_AR		"TRB,FM_AR"
;;3439	TRX		"R17"
;;3440	TRX_AR		"TRX,FM_AR"
;;3441	TRX1		"R2"
;;3442	TRX1_AR		"TRX1,FM_AR"
;;3443	TRX2		"R1"
;;3444	TRX2_AR		"TRX2,FM_AR"
;;3445	TRX3		"R14"
;;3446	TRX3_AR		"TRX3,FM_AR"
; 3447	.ENDIF/TRXDEF
; 3448	
; 3449	UNCSH PHYS REF	"SPEC/SP MEM CYCLE,SP MEM/UNCSH+UNPAGE"
; 3450	UPT FETCH	"MEM/LOAD ARX,SPEC/SP MEM CYCLE,SP MEM/UPT FETCH"
; 3451	UPT REF		"SPEC/SP MEM CYCLE,SP MEM/UPT"
; 3452	USER REF	"SPEC/SP MEM CYCLE,SP MEM/USER"
; 3453	
; 3454	VMA_#		"VMA/LOAD,COND/VMA_#"
; 3455	VMA_#+AR32-35	"VMA/LOAD,COND/VMA_#+AR32-35"
; 3456	VMA_40		"VMA/LOAD,COND/VMA_#,#/40"
; 3457	VMA_40+PI*2	"VMA/LOAD,COND/VMA_#+PI*2,#/40"
; 3458	VMA_41		"VMA/LOAD,COND/VMA_#,#/41"
; 3459	VMA_41+PI*2	"VMA/LOAD,COND/VMA_#+PI*2,#/41"
; 3460	VMA_420+TRAP	"VMA/LOAD,COND/VMA_#+TRAP,#/420"
; 3461	VMA_430+MODE	"VMA/LOAD,COND/VMA_#+MODE,#/430"
; 3462	VMA_AC3		"FMADR/AC3,ADB/FM,AD/B,VMA/AD"
; 3463	VMA_AR		"ADA/AR,AD/A,VMA/AD"
; 3464	VMA_AR AND ADMSK "ADMSK,ADB/FM,ADA/AR,AD/AND,VMA/AD"
; 3465	VMA_AR+1	"ADA/AR,AD/A+1,VMA/AD"
; 3466	VMA_AR+BR	"ADA/AR,ADB/BR,AD/A+B,VMA/AD"
; 3467	VMA_AR+CBR	"CBR,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3468	VMA_AR+E0	"E0,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3469	VMA_AR+E0+1	"E0,ADB/FM,ADA/AR,AD/A+B+1,VMA/AD"
; 3470	VMA_AR+E1	"E1,ADB/FM,ADA/AR,AD/A+B,VMA/AD"; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11-1
; MACRO.MIC[4,24]	11:59 29-May-86			CRAM Macros--T, U, V, W, X				

; 3471	VMA_AR+SBR	"SBR,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
;;3472	.IF/TRXDEF
;;3473	VMA_AR+TRB	"TRB,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3474	.ENDIF/TRXDEF
; 3475	VMA_AR+XR	"GEN AR+XR,VMA/AD"
; 3476	VMA_AR-1	"ADA/AR,AD/A-1,VMA/AD"
; 3477	VMA_ARX		"ADA/ARX,AD/A,VMA/AD"
; 3478	VMA_ARX AND ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/AND,VMA/AD"
; 3479	VMA_ARX+1	"ADA/ARX,AD/A+1,VMA/AD"
; 3480	VMA_ARX+BR	"ADA/ARX,ADB/BR,AD/A+B,VMA/AD"
; 3481	VMA_ARX+CBR	"CBR,ADB/FM,ADA/ARX,AD/A+B,VMA/AD"
; 3482	VMA_ARX+FM[]	"ADA/ARX,ADB/FM,@1,AD/A+B,VMA/AD"
; 3483	VMA_ARX+XR	"GEN ARX+XR,VMA/AD"
; 3484	
; 3485	VMA_BR		"ADB/BR,AD/B,VMA/AD"
; 3486	VMA_E0+1	"E0,ADB/FM,ADA EN/0S,AD/A+B+1,VMA/AD"
; 3487	VMA_FM[]	"ADA EN/0S,ADB/FM,@1,AD/B,VMA/AD";[344]
; 3488	VMA_MQ		"ADA/MQ,AD/A,VMA/AD"
; 3489	VMA_MQ+1	"ADA/MQ,AD/A+1,VMA/AD"		;[310]
; 3490	VMA_PC		"VMA/PC"			;[252]
; 3491	VMA_PC+1	"VMA/PC+1"
; 3492	VMA_SV.VMA	"SV.VMA,ADB/FM,AD/B,VMA/AD"
;;3493	.IF/TRXDEF
;;3494	VMA_TRB		"TRB,ADB/FM,AD/B,VMA/AD"
; 3495	.ENDIF/TRXDEF
; 3496	VMA_VMA HELD	"COND/SEL VMA,ADA/PC,AD/A,VMA/AD,MEM/RESTORE VMA"
; 3497	VMA_VMA+1	"VMA/VMA,COND/VMA INC"
; 3498	VMA_VMA-1	"VMA/VMA,COND/VMA DEC"
; 3499	
; 3500	WR PT ENTRY	"COND/MBOX CTL,MBOX CTL/PT WR"
; 3501	WR REFILL RAM	"MEM/REG FUNC,MREG FNC/WR REFILL RAM"
; 3502	WRITE (E)	"MEM/EA CALC,EA CALC/WRITE(E),VMA/LOAD"
; 3503	
; 3504	XR		"FMADR/XR"
; 3505	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; MACRO.MIC[4,24]	11:59 29-May-86			DRAM Macros						

; 3506	.TOC	"DRAM Macros"
; 3507	
; 3508		.DCODE
; 3509	;
; 3510	;	These macros have not been sorted alphabetically, as (1) there are
; 3511	;	too few to bother, and (2) no one ever looks at the DRAM anyway!
; 3512	;
; 3513	;"A FIELD" MACROS
; 3514	; DECODED TO TELL WHAT TO DO WITH EFFECTIVE ADDRESS
; 3515	; AND WHETHER TO PREFETCH FROM PC+1
; 3516	
; 3517	I	"A/IMMED"
; 3518	I-PF	"A/IMMED-PF"
; 3519	EA	"A/ADDR"
; 3520	W	"A/WR-TST"
; 3521	R	"A/READ"
; 3522	R-PF	"A/READ-PF"
; 3523	RW	"A/RD-WR"
; 3524	RPW	"A/RD-P-WR"			;Was A/RD-WR if RPW was off
; 3525	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; MACRO.MIC[4,24]	11:59 29-May-86			DRAM Macros						

; 3526	;"B FIELD" MACROS
; 3527	; DECODED BY MOST INSTRUCTIONS TO TELL WHERE TO STORE RESULTS,
; 3528	; BUT USED BY OTHERS TO HOLD VARIOUS "MODE" INFORMATION
; 3529	
; 3530	AC	"B/AC"
; 3531	M	"B/MEM"
; 3532	S	"B/SELF"
; 3533	B	"B/BOTH"
; 3534	DBL AC	"B/DBL AC"
; 3535	DBL B	"B/DBL BOTH"
; 3536	FL-AC	"B1-2/AC"
; 3537	FL-MEM	"B1-2/MEM"
; 3538	FL-BOTH	"B1-2/BOTH"
; 3539	
; 3540	TN-	"B0/CRY0(1),B1-2/0"
; 3541	TNE	"B0/CRY0(0),B1-2/0"
; 3542	TNA	"B0/CRY0(0),B1-2/0"
; 3543	TNN	"B0/CRY0(1),B1-2/0"
; 3544	TZ-	"B0/CRY0(1),B1-2/1"
; 3545	TZE	"B0/CRY0(0),B1-2/1"
; 3546	TZA	"B0/CRY0(0),B1-2/1"
; 3547	TZN	"B0/CRY0(1),B1-2/1"
; 3548	TC-	"B0/CRY0(1),B1-2/2"
; 3549	TCE	"B0/CRY0(0),B1-2/2"
; 3550	TCA	"B0/CRY0(0),B1-2/2"
; 3551	TCN	"B0/CRY0(1),B1-2/2"
; 3552	TO-	"B0/CRY0(1),B1-2/3"
; 3553	TOE	"B0/CRY0(0),B1-2/3"
; 3554	TOA	"B0/CRY0(0),B1-2/3"
; 3555	TON	"B0/CRY0(1),B1-2/3"
; 3556	
; 3557	SJC-	"B/SJC-"
; 3558	SJCL	"B/SJCL"
; 3559	SJCE	"B/SJCE"
; 3560	SJCLE	"B/SJCLE"
; 3561	SJCA	"B/SJCA"
; 3562	SJCGE	"B/SJCGE"
; 3563	SJCN	"B/SJCN"
; 3564	SJCG	"B/SJCG"
; 3565	
; 3566	BLKI	"B0/CRY0(0),B1-2/2"
; 3567	BLKO	"B0/CRY0(0),B1-2/0"
; 3568	DATAI	"B/6"
; 3569	DATAO	"B/4"
; 3570	CONI	"B/6"
; 3571	CONO	"B/4"
; 3572	CONSO	"B0/CRY0(1),B1-2/1"
; 3573	CONSZ	"B0/CRY0(0),B1-2/1"
; 3574	
						; 3575	.BIN
						; 3576		.UCODE
						; 3577	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; BASIC.MIC[4,24]	11:56 29-May-86			THE INSTRUCTION LOOP					

						; 3578	.TOC	"THE INSTRUCTION LOOP"
						; 3579	
; 3580	.NOBIN
; 3581	;	Comments updated [302][344][410]
; 3582	;
; 3583	;INSTRUCTION DECODE, EA COMPUTATION, AND OPERAND FETCH
; 3584	;
; 3585	;	IN GENERAL, AN INSTRUCTION IS STARTED AT XCTGO.
; 3586	; AT THIS TIME THE INSTRUCTION IS IN ARX AND IR, AND PC HAS ITS ADDRESS.
; 3587	; THE DRAM OUTPUTS AND "AC" BITS WILL SETTLE DURING THIS
; 3588	; MICROINSTRUCTION, AND WILL BE LATCHED BY THE CLOCK WHICH ENDS
; 3589	; THE CYCLE.  XCTGO DISPATCHES ON THE STATE OF THE
; 3590	; INDIRECT AND INDEX BITS OF THE ARX (EA MOD DISP) TO COMPEA OR
; 3591	; ONE OF THE THREE LOCATIONS FOLLOWING IT.
; 3592	;	IF INDIRECT IS SPECIFIED, THE INDIRECT POINTER IS FETCHED (AT
; 3593	; COMPEA+2 OR +3 (DEPENDING ON WHETHER INDEXING IS ALSO SPECIFIED).
; 3594	; WE WAIT FOR IT AT INDRCT, AND THEN LOOP BACK TO EFIW, WHERE GLOBAL
; 3595	; INDEXING AND INDIRECTION, AS WELL AS BAD DATA, WILL BE CHECKED.  ALL
; 3596	; OF THIS WAS ADDED FOR THE MODEL B.  WHEN THIS IS DONE, OR IF NO
; 3597	; INDIRECT IS CALLED FOR, WE COMPUTE THE INSTRUCTION'S EFFECTIVE ADDRESS
; 3598	; (EA) AT COMPEA OR COMPEA+1 (DEPENDING ON WHETHER INDEXING IS CALLED
; 3599	; FOR), OR AT EFIW+4, 5, 6, OR 7 IF THE LAST WORD IN THE INDIRECTION CHAIN
; 3600	; WAS GLOBAL AND PERFORM THE FUNCTION "A READ", WHOSE OPERATION DEPENDS
; 3601	; ON THE DRAM A FIELD, AS FOLLOWS:
; 3602	;
; 3603	; MACRO	 A-FLD	MEM FUNCTION	VMA	DISPATCH
; 3604	;  I	  0	NONE		AD(=EA)  DRAM J
; 3605	; I-PF	  1	FETCH		PC+1	 DRAM J
; 3606	; EA	  2	30 BIT EA CALC	AD	 DRAM J
; 3607	;  W	  3	WR TST		AD	   3	(The model A dispatch
; 3608	;  R	  4	READ		AD	   4	 addresses were all 40
; 3609	; R-PF	  5	READ		AD	   5	 greater than these)
; 3610	; RW	  6	READ/WR TST	AD	   6
; 3611	; RPW	  7	RD-PSE/WR TST	AD	   7
; 3612	;
; 3613	;	A FIELD VALUES 0 AND 1 ARE USED FOR INSTRUCTIONS WHICH NEITHER
; 3614	; READ NOR WRITE THE CONTENTS OF EA (IMMEDIATE-MODE INSTRUCTIONS,
; 3615	; JUMPS, ETC).  THESE DISPATCH FROM "A READ" DIRECTLY TO THE MICROCODE
; 3616	; WHICH HANDLES THE INSTRUCTION.  IF THE A FIELD CONTAINS 1, "A READ"
; 3617	; CAUSES A PREFETCH (FROM PC+1), SO THAT THE MBOX CAN WORK ON GETTING
; 3618	; THE NEXT INSTRUCTION INTO ARX WHILE THE EBOX PERFORMS THIS ONE.
; 3619	;	IF THE A FIELD CONTAINS A 2, THE EA CALCULATION WILL PROVIDE
; 3620	; A FULL 30 BIT EFFECTIVE ADDRESS TO THE AD AT THE END.  THIS WAS
; 3621	; INTRODUCED WITH EXTENDED ADDRESSING, TO ALLOW SUCH INSTRUCTIONS AS
; 3622	; XMOVEI AND XHLLI TO COMPUTE A COMPLETE ADDRESS WITHOUT ACTUALLY
; 3623	; REFERENCING IT.  OTHERWISE, THIS IS SIMILAR TO AN A FIELD OF 0.  NOTE
; 3624	; THAT AN A FIELD OF 0 WILL STILL PROVIDE A 30 BIT ADDRESS TO THE VMA;
; 3625	; ONLY THE RESULT IN THE AD WILL BE DIFFERENT.
; 3626	;	IF THE A FIELD CONTAINS 3, THE MBOX PERFORMS A PAGING CHECK ON
; 3627	; EA, AND CAUSES A PAGE FAIL IF THAT LOCATION IS NOT WRITABLE.
; 3628	; THE MICROCODE GOES TO 3 TO WAIT FOR COMPLETION OF THE PAGE CHECK,
; 3629	; AND AT THAT LOCATION LOADS AC INTO AR.  THE WRITABILITY OF EA IS
; 3630	; VERIFIED AT THIS TIME TO PREVENT INCORRECTLY SETTING FLAGS OR
; 3631	; THE PROCESSOR STATE IF THE INSTRUCTION WILL BE ABORTED BY PAGE
; 3632	; FAILURE.  LOCATION 3 THEN DISPATCHES TO THE HANDLER FOR THE
; 3633	; CURRENT INSTRUCTION.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; BASIC.MIC[4,24]	11:56 29-May-86			THE INSTRUCTION LOOP					

; 3634	;	A FIELD VALUES 4 TO 7 PERFORM READS FROM EA.  6 AND 7 ALSO TEST
; 3635	; THE WRITABILITY OF THE LOCATION, AND 7 PERFORMS THE FIRST HALF OF
; 3636	; A READ-PAUSE-WRITE CYCLE IF EA IS AN UN-CACHED ADDRESS.  THE DISPATCH
; 3637	; IS TO A, WHERE WE WAIT FOR MEMORY DATA TO ARRIVE IN AR.  IF THE A
; 3638	; FIELD WAS 5, WE PREFETCH FROM PC+1 AS SOON AS THE DATA ARRIVES.
; 3639	; IN ANY CASE, WE DISPATCH ACCORDING TO THE DRAM J FIELD TO THE
; 3640	; HANDLER FOR THE INSTRUCTION.
; 3641	;	IF A PAGE FAIL OCCURS AT ANY TIME (EITHER IN THIS CODE OR DURING
; 3642	; INSTRUCTION EXECUTION) THE MICROPROCESSOR TRAPS TO CRAM LOCATION
; 3643	; 1777 OR 3777, WHERE IT CAUSES A PAGE FAIL TRAP.
; 3644	;
; 3645	;	MOST INSTRUCTIONS (THE MOVE, HALFWORD, AND BOOLEAN GROUPS,
; 3646	; PLUS ADD AND SUB) ARE PERFORMED BY HANDLERS CONSISTING OF ONE OR
; 3647	; TWO MICROINSTRUCTIONS WHICH LEAVE THE RESULT IN AR, AND COMPLETE
; 3648	; BY INVOKING THE "EXIT" MACRO.  EXIT USES THE MEM/B WRITE FUNCTION
; 3649	; TO BEGIN A STORE TO MEMORY FOR THOSE MODES IN WHICH THE RESULT
; 3650	; GOES TO MEMORY, AND DISP/DRAM B TO GET TO ONE OF THE MICROINSTRUCTIONS
; 3651	; FOLLOWING ST0.  THIS CODE DEPENDS ON A CERTAIN AMOUNT OF CORRELATION
; 3652	; BETWEEN THE DRAM A AND B FIELDS.  IN PARTICULAR, STAC (STORE AC)
; 3653	; ASSUMES THAT A PREFETCH HAS OCCURRED, WHILE THE OTHERS ASSUME THAT
; 3654	; NO PREFETCH HAS OCCURED.  THUS NORMAL AND IMMEDIATE MODES, WHOSE
; 3655	; RESULTS GO ONLY TO AC, MUST PREFETCH IN THE DRAM A FIELD, WHILE
; 3656	; MEM, BOTH, AND SELF MODES, WHOSE RESULTS GO TO MEMORY, MUST NOT.
; 3657	; (THIS RESTRICTION IS AVOIDED FOR THOSE INSTRUCTIONS WHICH NEVER
; 3658	; PREFETCH -- IN MUL, DIV, AND IDIV BY USE OF THE EXIT TO ST2AC,
; 3659	; AND IN IMUL AND THE SINGLE PRECISION FLOATING POINT
; 3660	; INSTRUCTIONS BY A RESTRICTED EXIT TO ST6.)
; 3661	;	ANOTHER LARGE SET OF INSTRUCTIONS (SKIP, AOS, SOS, JUMP, AOJ,
; 3662	; SOJ, AOBJ, CAI, CAM, AND THE TEST GROUP) KNOWS WHERE TO PUT THE
; 3663	; RESULTS WITHOUT MODE INFORMATION, AND THEY USE THE DRAM B FIELD TO
; 3664	; DETERMINE WHETHER TO SKIP OR JUMP, AS A FUNCTION OF THEIR OPERANDS.
; 3665	; SKIP, AOS, AND SOS ARE CONSIDERED SELF-MODE INSTRUCTIONS,
; 3666	; AND AFTER MAKING THE FETCH DECISION (AND RE-WRITING MEMORY, IN
; 3667	; THE CASE OF AOS OR SOS), JUMP TO STSELF TO DECIDE WHETHER OR NOT
; 3668	; TO PUT THE RESULT ALSO IN AC.  THE OTHER INSTRUCTIONS OF THIS SET
; 3669	; JUMP TO STORAC OR NOP AFTER MAKING THE FETCH DECISION, DEPENDING
; 3670	; ON WHETHER OR NOT THE OPCODE DEFINITION REQUIRES MODIFICATION OF AC.
; 3671	; (NOTE THE DIFFERENCE BETWEEN STAC AND FINI ON THE ONE HAND,
; 3672	; AND STORAC AND NOP ON THE OTHER -- STORAC AND NOP MUST BE USED WHEN
; 3673	; THE NEXT INSTRUCTION FETCH OCCURS ON THE PRECEDING EBOX CYCLE, BECAUSE
; 3674	; NICOND MUST NOT IMMEDIATELY FOLLOW A FETCH (ONE CYCLE REQUIRED FOR
; 3675	; VMA AC REF TO MAKE IT THROUGH THE NICOND LOGIC), STAC AND FINI ARE
; 3676	; USED WHEN THERE HAS BEEN AN INTERVENING CYCLE.)
; 3677	;
; 3678	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; BASIC.MIC[4,24]	11:56 29-May-86			NEXT INSTRUCTION DISPATCH				

; 3679	.TOC	"NEXT INSTRUCTION DISPATCH"
; 3680	
; 3681	;
; 3682	; [321] DISP/NICOND (THE "NXT INSTR" MACRO) BRINGS US TO ONE OF THE
; 3683	; LOCATIONS FOLLOWING "NEXT".  PC HAS BEEN UPDATED TO ADDRESS THE NEXT
; 3684	; INSTRUCTION IN THE NORMAL FLOW, AND IF IT IS FROM MEMORY
; 3685	; (AS OPPOSED TO AC'S), THE INSTRUCTION IS IN ARX AND IR.
; 3686	;	THE NICOND DISPATCH IS PRIORITY ENCODED, AS FOLLOWS: (see CON2)
; 3687	;(1)	IF PI CYCLE IS TRUE, GO TO NEXT FOR SECOND HALF
; 3688	; OF STANDARD OR VECTOR INTERRUPT.
; 3689	;(2)	IF THE RUN FLOP (CON RUN) IS OFF, GO TO NEXT+2, FROM WHICH THE
; 3690	; MICROCODE WILL ENTER THE HALT LOOP TO WAIT FOR THE CONSOLE TO RESTART
; 3691	; INSTRUCTION PROCESSING.
; 3692	;(3)	IF THE METER HAS A REQUEST, GO TO NEXT+4 (MTRINT) TO SERVE IT.
; 3693	;(4)	IF THE PI SYSTEM HAS A REQUEST READY, GO TO NEXT+6 (INTRPT)
; 3694	; TO START A PI CYCLE.
; 3695	;(5)	IF CON UCODE STATE 05 (TRACK EN) IS SET, GO TO NEXT+10 OR 11.
; 3696	; Normally NEXT+10 will be used; if a trap flag was set by the previous
; 3697	; instruction, however, NEXT+11 will be reached.  This is the only
; 3698	; way the trap will ever be detected, so be cautious of ignoring it.
; 3699	; THIS FLOP IS ENTIRELY UNDER CONTROL OF THE MICROCODE, AND IS ONLY
; 3700	; USED FOR THE SPECIAL STATISTICS-GATHERING MICROCODE.
; 3701	;(6)	IF THE LAST INSTRUCTION SET A TRAP FLAG, GO TO NEXT+13 OR +17,
; 3702	; IT DOESN'T MATTER WHICH.  (NEXT+17 will be reached if VMA contains
; 3703	; an AC address, probably irrelevant when a trap flag was set.)
; 3704	;(7)	IF VMA CONTAINS AN AC ADDRESS, IMPLYING THAT THE NEXT
; 3705	; INSTRUCTION IS TO COME OUT OF FAST MEMORY, GO TO NEXT+16 TO GET IT.
; 3706	;(10)	--NORMAL CASE-- THE INSTRUCTION IS IN ARX, READY TO GO, GO
; 3707	; TO NEXT+12 (XCTGO).
; 3708	;
; 3709	;	The NICOND dispatch yields the following:
; 3710	;
; 3711	;	+0	CON PI CYCLE
; 3712	;	+2	-CON RUN (i.e. halt)
; 3713	;	+4	CON MTR INT REQ (meter interrupt)
; 3714	;	+6	CON INT REQ (interrupt)
; 3715	;	+10	CON UCODE STATE 05 (tracks enable)
; 3716	;	+11	CON UCODE STATE 05+TRAP REQ (tracks enable+trap)
; 3717	;	+12	-VM AC REF (normal instruction)
; 3718	;	+13	-VM AC REF+TRAP REQ (normal instruction+trap)
; 3719	;	+16	-CON PI CYCLE (AC ref)
; 3720	;	+17	-CON PI CYCLE+TRAP REQ (AC ref+trap)
; 3721	;
; 3722	;	Other offsets are never reached.
; 3723	;
						; 3724	.BIN
						; 3725	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; BASIC.MIC[4,24]	11:56 29-May-86			NEXT INSTRUCTION DISPATCH				

						; 3726	;NICOND (NXT INSTR) DISPATCH BLOCK
						; 3727	
						; 3728	=11*0000				;USE LOC'NS INACCESSIBLE TO DRAM
						; 3729	NEXT:	AR_MQ+1,VMA/AD,			;2ND PART OF INTERRUPT
U 0140, 3641,4023,2000,0000,0320,1510,0714	; 3730			SET PI CYCLE,J/PICYC2	;CONTINUE WITH 41+2N
						; 3731	=0010	CLR AR,ARX_1S,SC_#,#/23.,	;HERE IF RUN FLOP OFF
U 0142, 0044,2301,0200,0302,0000,0450,0027	; 3732			CALL,J/ROTS		;BUILD ADDR MASK
						;;3733	.IFNOT/EXTEXP				;[230]
						;;3734	.IFNOT/OWGBP				;[344]
						;;3735		ADMSK,FM_AR,AR_AR+1,J/CHALT	;SAVE MASK, GO HALT
						;;3736	.IF/OWGBP				;[344]
						;;3737		FM[ADMSK]_AR,AR_AR*2,J/EXMSK	;[230] AR HAS 77,,777776
						;;3738	.ENDIF/OWGBP				;[344]
						; 3739	.IF/EXTEXP				;[230]
U 0143, 0030,3701,5007,0000,0000,1010,0175	; 3740		FM[ADMSK]_AR,AR_AR*2,J/EXMSK	;[230] AR HAS 77,,777776
						; 3741	.ENDIF/EXTEXP				;[230]
						; 3742	=0100
U 0144, 3615,0001,0000,0000,0000,2110,0145	; 3743	MTRINT:	CLR ACCOUNT EN,J/MTRREQ		;HERE IF METER REQUEST UP
U 0145, 3634,0001,3000,0302,0060,0010,0002	; 3744		AR_EBUS,SC_#,#/2,J/PICYC1	;HERE IF TAKE INTRPT DOESNT FIND
						; 3745	=0110					; A METER REQUEST
U 0146, 3634,0001,3000,0302,0060,0010,0002	; 3746	INTRPT:	AR_EBUS,SC_#,#/2,J/PICYC1	;HERE IF INTERRUPT PENDING
						; 3747	;
						; 3748	;	[321] Even if statistics are enabled, traps should not be lost,
						; 3749	;	so override TRACKS and friends when TRAP is set.
						; 3750	;
						;;3751	.IF/TRACKS
						;;3752	=1000	AR_TRX+1,GEN CRY18,SKP CRY0,J/TRK1;STORE PC BEFORE EXECUTING INSTR
						;;3753		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3754	.ENDIF/TRACKS
						;;3755	.IF/OP.CNT
						;;3756	=1000	SC_#,#/9.,SKP USER,J/OPCT1	;COUNT THIS INSTR
						;;3757		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3758	.ENDIF/OP.CNT
						;;3759	.IF/OP.TIME
						;;3760	=1000	AR_2,CLR TRK+PA EN,J/OPTM0	;TIME THIS INSTR
						;;3761		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3762	.ENDIF/OP.TIME
						;;3763	.IF/SO.CNT
						;;3764	=1000
						;;3765	TRK0:	ARX_TRB,BRX/ARX,SKP AC REF,J/TRK1;GET PREV INSTR HOLD THIS INSTR
						;;3766		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3767	.ENDIF/SO.CNT
						;;3768	.IF/SO2.CNT
						;;3769	=1000
						;;3770	TRK0:	ARX_TRB,BRX/ARX,SKP AC REF,J/TRK1;GET PREV INSTR HOLD THIS INSTR
						;;3771		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3772	.ENDIF/SO2.CNT
						; 3773	=1010
						; 3774	XCTGO:	BRX/ARX,AR_ARX,SET ACCOUNT EN,	;SAVE INSTR, SIGN EXTEND Y,
U 0152, 0174,2341,4022,0402,2000,2136,0105	; 3775		    CLR SC,XR,EA MOD DISP,J/COMPEA;[414] GO CALCULATE EA
U 0153, 0364,0001,0000,0000,0100,3110,0420	; 3776	TRAP:	VMA_420+TRAP,J/TRAPX		;HERE IF TRAP BITS SET
U 0156, 0152,3240,0203,0000,0020,1410,0000	; 3777	=1110	ARX_FM(VMA),TIME/3T,LOAD IR,J/XCTGO	;HERE IF INSTR IS IN FM
U 0157, 0364,0001,0000,0000,0100,3110,0420	; 3778		VMA_420+TRAP,J/TRAPX		;HERE IF TRAP BITS SET
						; 3779	
						; 3780	;HERE ON TRAPS, VMA SETUP WITH 420+TRAP CODE
						; 3781	=11****; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3-1
; BASIC.MIC[4,24]	11:56 29-May-86			NEXT INSTRUCTION DISPATCH				

U 0364, 0365,0001,0000,0000,0013,0026,0033	; 3782	TRAPX:	LOAD ARX,PT REF			;GET AND XCT TRAP INSTR
U 0365, 0366,2341,0200,0000,0000,1510,0100	; 3783	=11****	ARX_1S,SET PC+1 INH		;[412] DON'T INCREMENT PC
						; 3784	
						; 3785	;HERE AFTER FETCHING INSTR TO BE EXECUTED
						; 3786	
						; 3787	=11****
U 0366, 0152,3240,0003,0000,0022,1410,0000	; 3788	XCTW:	ARX_MEM,LOAD IR,J/XCTGO		;GET INSTR TO XCT
						; 3789	;
						; 3790	;	Start of everything.  PC word is in AR.
						; 3791	;
						; 3792	0:
U 0000, 1072,0001,0000,0000,0000,0024,0020	; 3793	START:	SET FLAGS_AR,J/SETPC		;LOAD FLAGS, USE REST AS ADDR
						; 3794	1:					;MUST BE AT START+1
U 0001, 0366,0001,0000,0000,0117,0010,0000	; 3795	CONT:	VMA/PC,FETCH,J/XCTW		;CONTINUE FROM PC
						; 3796	.IF/EXTEXP				;[230]
U 0030, 0041,4001,2000,0000,0020,0010,0000	; 3797	EXMSK:	AR_AR+1				;[230] GIVES 77,,777777
U 0041, 0620,0001,0007,0000,0000,1010,0164	; 3798		FM[EXPMSK]_AR,J/CHALT		;[230] MASK FOR FORTRAN EXT EXP
						;;3799	.IFNOT/EXTEXP				;[344]
						;;3800	.IF/OWGBP				;[344]
						;;3801	EXMSK:	AR_AR+1				;[230] GIVES 77,,777777
						;;3802		FM[EXPMSK]_AR,J/CHALT		;[230] MASK FOR FORTRAN EXT EXP
						;;3803	.ENDIF/OWGBP				;[344]
						; 3804	.ENDIF/EXTEXP				;[230]
						; 3805	;
						; 3806	;	Rotate subroutine.
						; 3807	;
U 0044, 0003,4001,4400,5302,0000,0003,0044	; 3808	ROTS:	AR_SHIFT,ARX_SHIFT,SC_#-SC,#/36.,RETURN3
						; 3809	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; BASIC.MIC[4,24]	11:56 29-May-86			EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH		

						; 3810	.TOC	"EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH"
						; 3811	
						; 3812	=11*0000
						; 3813	EFIW:	AR_ARX (AD),GLOBAL,A INDRCT,	;LONG EXTENDED ADDR
U 0160, 0360,3711,2000,0000,1111,7010,0230	; 3814			SKP INTRPT,J/INDRCT	;WITH INDIRECT SET
						; 3815		AR_ARX+XR,GLOBAL,A INDRCT,
U 0161, 0360,0610,2002,0000,1131,7010,0230	; 3816			SKP INTRPT,J/INDRCT
						; 3817		AR_ARX (AD),GLOBAL,A INDRCT,
U 0162, 0360,3711,2000,0000,1111,7010,0230	; 3818			SKP INTRPT,J/INDRCT
						; 3819		AR_ARX+XR,GLOBAL,A INDRCT,
U 0163, 0360,0610,2002,0000,1131,7010,0230	; 3820			SKP INTRPT,J/INDRCT
						; 3821	
U 0164, 0000,3713,2000,0000,1204,0002,0300	; 3822		AR_ARX (AD),GLOBAL,A READ	;LONG EXTENDED ADDR
U 0165, 0000,0610,2002,4000,1224,0002,0300	; 3823		AR_ARX+XR,GLOBAL,A READ		; WITH INDEXING IN 2-5
U 0166, 0000,3713,2000,0000,1204,0002,0300	; 3824		AR_ARX (AD),GLOBAL,A READ
U 0167, 0000,0610,2002,4000,1224,0002,0300	; 3825		AR_ARX+XR,GLOBAL,A READ
						; 3826	
U 0170, 3154,0001,0000,0000,0000,2210,0400	; 3827	PF24:	GET ECL EBUS,J/PFPAR		;[414] ARX BITS 0,1 = 11
U 0171, 3154,0001,0000,0000,0000,2210,0400	; 3828		GET ECL EBUS,J/PFPAR		;Illegal indirection
U 0172, 3154,0001,0000,0000,0000,2210,0400	; 3829		GET ECL EBUS,J/PFPAR
U 0173, 3154,0001,0000,0000,0000,2210,0400	; 3830		GET ECL EBUS,J/PFPAR
						; 3831	
						; 3832	=11*1100
U 0174, 0000,3701,0000,0000,0204,0002,0300	; 3833	COMPEA:	GEN AR,A READ			;LOCAL
U 0175, 0000,0600,0002,4000,2224,0002,0300	; 3834		GEN AR+XR,INDEXED,A READ	;LOCAL UNLESS XR>0
						; 3835		GEN AR,A INDRCT,
U 0176, 0360,3703,0000,0000,0111,7010,0230	; 3836			SKP INTRPT,J/INDRCT
						; 3837		GEN AR+XR,INDEXED,A INDRCT,
U 0177, 0360,0600,0002,0000,2131,7010,0230	; 3838			SKP INTRPT
						; 3839	=11***0
U 0360, 0370,3240,0003,0000,0022,2510,0000	; 3840	INDRCT:	ARX_MEM,LONG EN,J/INDR1
U 0361, 0144,3200,0003,0000,0022,7710,0000	; 3841	TAKINT:	ARX_MEM,TAKE INTRPT
						; 3842	;
						; 3843	=11****
						; 3844	INDR1:	AR_ARX,XR,EA MOD DISP,
U 0370, 0160,2341,4002,0301,2020,0036,0024	; 3845			FE_#,#/24,TIME/3T,J/EFIW
						; 3846	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; BASIC.MIC[4,24]	11:56 29-May-86			WAIT FOR (E)						

						; 3847	.TOC	"WAIT FOR (E)"
						; 3848	
						; 3849	;THE EXECUTE CODE FOR EACH INSTRUCTION IS ENTERED WITH
						; 3850	; THE OPCODE AND AC # IN BRX AND IR, THE LAST INDIRECT WORD
						; 3851	; IN ARX, AND AR AND VMA SETUP AS A FUNCTION OF THE A
						; 3852	; FIELD OF THE DISPATCH RAM. A PREFETCH IS IN PROGRESS IF THE
						; 3853	; DRAM A FIELD WAS 1 OR 5 (OR IF IR CONTAINS "JRST 0,").
						; 3854	
						; 3855	;ON "A READ", THE HARDWARE DISPATCHES TO THE EXECUTE CODE FOR
						; 3856	; THE INSTRUCTION IF THE DRAM A FIELD IS 0-2.  IF THE A FIELD
						; 3857	; CONTAINS 3-7, THE HARDWARE DISPATCHES TO A (MODEL B), OR 40+A
						; 3858	; (MODEL A), BELOW:
						; 3859	
						; 3860	;COME HERE ON "A READ" FUNCTION IF DRAM A FIELD IS 3 (W macro)
						; 3861	; A "WRITE TST" IS IN PROGRESS
						; 3862	
						; 3863	3:	BR/AR,AR_AC0,MB WAIT,		;WAIT FOR PERMISSION TO WRITE
U 0003, 0000,3240,2040,0000,0022,0001,0000	; 3864			TIME/3T,IR DISP,J/0	;AND GO TO EXECUTE CODE
						; 3865	
						; 3866	;HERE ON "A READ" FUNCTION IF DRAM A FIELD IS 4 (R macro)
						; 3867	; A "LOAD AR" IS IN PROGRESS.  We load FE with 2 for LDB and DPB. [337]
						; 3868	
						; 3869	4:	BR/AR,AR_MEM,TIME/3T,FE_#,#/2,	;GET OPERAND, set up FE for byte
U 0004, 0000,3200,0043,0301,0022,0001,0002	; 3870			IR DISP,J/0		; instructions [337], and go
						; 3871	
						; 3872	;HERE ON "A READ" IF A FIELD IS 5 (R-PF macro)
						; 3873	; A "LOAD AR" IS IN PROGRESS, AND WE MUST PREFETCH WHEN IT COMPLETES
						; 3874	
						; 3875	5:	BR/AR,FIN XFER,I FETCH,		;GET OPERAND, PREFETCH,
U 0005, 0000,3200,0043,0000,0237,0001,0000	; 3876			TIME/3T,IR DISP,J/0	; & START EXECUTE
						; 3877	
						; 3878	;HERE ON "A READ" IF A FIELD IS 6 (RW macro)
						; 3879	; A "LOAD AR" IS IN PROGRESS, PAGING IS TESTING WRITABILITY
						; 3880	; We load FE with 2 for ILDB and IDPB. [337]
						; 3881	
						; 3882	6:	BR/AR,AR_MEM,TIME/3T,FE_#,#/2,	;GET OPERAND, load FE for byte
U 0006, 0000,3200,0043,0301,0022,0001,0002	; 3883			IR DISP,J/0		; instructions [337], and go
						; 3884	
						; 3885	;HERE ON "A READ" IF A FIELD IS 7 (RPW macro)
						; 3886	; A "READ-PAUSE-WRITE" IS IN PROGRESS
						; 3887	
						; 3888	7:	BR/AR,AR_MEM,TIME/3T,		;GET OPERAND
U 0007, 0000,3200,0043,0000,0022,0001,0000	; 3889			IR DISP,J/0		; START EXECUTE
						; 3890	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; BASIC.MIC[4,24]	11:56 29-May-86			TERMINATION						

						; 3891	.TOC	"TERMINATION"
						; 3892	
						; 3893	;DISPATCH HERE WITH THE "EXIT" MACRO,
						; 3894	; OR JUMP DIRECTLY TO ONE OF THESE LOCATIONS.
						; 3895	
						; 3896	=0****00*000				;[430]
						; 3897	ST0:					;BASE FOR B DISP IN EXIT MACRO
						; 3898	=001
U 0011, 1162,5401,2010,0000,1237,0016,0000	; 3899	ST2AC:	MQ_AR,AR_SIGN,I FETCH,J/DSHIFT	;[430] HERE TO STORE AC0 & AC1
						; 3900		FIN STORE,MQ_AR,AR_SIGN,	;[430] MULB, DIVB, etc. Wait for
U 0012, 1162,5401,2013,0000,1237,0016,0000	; 3901		    I FETCH,J/DSHIFT		; mem, gen low sign bit, and store
						; 3902	SLFEND:	FIN STORE,I FETCH,		;[440] SELF MODE
U 0013, 0216,4001,0003,0000,0217,4610,0000	; 3903			SKP AC#0,J/STSELF	; RESULT TO AC TOO?
U 0014, 0015,4001,4000,0000,0000,0010,0000	; 3904	SHFLOD:	AR_SHIFT			;[337] Shift byte load result
U 0015, 0140,4001,0000,0403,0002,1006,0000	; 3905	STAC:	AC0_AR,NXT INSTR		;NORMAL AND IMMEDIATE MODES
						; 3906	ST6:
						; 3907	IFNOP:
U 0016, 0217,4001,0003,0000,0217,0010,0000	; 3908	STMEM:	FIN STORE,I FETCH,J/NOP		;MEM MODE
						; 3909	IFSTAC:
U 0017, 0216,0001,0003,0000,0217,0010,0000	; 3910	STBOTH:	FIN STORE,I FETCH,J/STORAC	;BOTH mode
						; 3911	=
						; 3912	;
						; 3913	;HERE TO FINISH, AFTER FETCHING NEXT INSTRUCTION.
						; 3914	; WE MUST GUARANTEE AT LEAST ONE EBOX CYCLE BETWEEN FETCH AND NICOND,
						; 3915	; TO ALLOW VMA AC REF TO MAKE IT THROUGH THE NICOND LOGIC.
						; 3916	; Most noops come here directly. [412]
						; 3917	216:					;[440]
						; 3918	STSELF:					;SKIP, AOS, SOS COME HERE
U 0216, 0015,4001,0000,0000,0000,1610,0000	; 3919	STORAC:	SR_0,J/STAC			;STORE AC, TOO
						; 3920	217:					;[440] TRN, CAI, etc. get here  
U 0217, 0133,4001,0000,0000,0000,1610,0000	; 3921	NOP:	SR_0				;Must be near CAIx instructions
U 0133, 0140,0001,0000,0403,0002,0006,0000	; 3922	FINI:	NXT INSTR			;Load IR and PC, test PI cycle, RUN,
						; 3923						; PI READY, TRAPS
						; 3924	;
						; 3925	;	Store AC1 and finish up.
						; 3926	;
U 0141, 0136,0001,4000,0000,0000,1610,0000	; 3927	STD1:	AR_SHIFT,SR_0			;BRING IN LOW PART
						; 3928	;
						; 3929	;	[356] The next two locations are fixed at 136 and 137.  We don't
						; 3930	;	really care where they are, but the front end looks at the # field
						; 3931	;	of these to get the microcode major version (# bits 0-5 of 136),
						; 3932	;	minor version (# bits 6-8 of 136), and edit number (# bits 0-8 of
						; 3933	;	137).
						; 3934	;
						; 3935	136:					;[357]
						; 3936	STAC1:	AC1_AR,FINISH,			;GO DO NEXT INSTRUCTION
U 0136, 0133,0001,0001,0000,0000,1010,0021	; 3937			MAJVER/MAJOR,MINVER/MINOR;[356]
						; 3938	;
						; 3939	;HERE TO GET MICRO-CODE VERSION #.  FIXED LOC'N SO SOFTWARE CAN FIND IT
						; 3940	;
						; 3941	137:
U 0137, 3646,4001,0040,0000,0000,0110,0442	; 3942	UVERS:	BR/AR,AR0-8_#,#/EDIT,J/GTAR08	;COPY VERSION TO AR
						; 3943	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; BASIC.MIC[4,24]	11:56 29-May-86			MOVE GROUP, EXCH, XMOVEI, XHLLI				

						; 3944	.TOC	"MOVE GROUP, EXCH, XMOVEI, XHLLI"
						; 3945	
						; 3946		.DCODE
D 0200, 5500,0100				; 3947	200:	R-PF,	AC,	J/MOVE		;MOVE
D 0201, 1501,0100				; 3948		I-PF,	AC,	J/MOVE		;MOVEI
D 0202, 0100,0314				; 3949		I,	B/1,	J/MOVEM		;MOVEM--no write test needed
D 0203, 6000,0313				; 3950		RW,		J/MOVES		;MOVES [440]
						; 3951	
D 0204, 5500,0002				; 3952	204:	R-PF,	AC,	J/MOVS		;MOVS
D 0205, 1501,0002				; 3953		I-PF,	AC,	J/MOVS		;MOVSI
D 0206, 3600,0002				; 3954		W,	M,	J/MOVS		;MOVSM
D 0207, 7301,0002				; 3955		RPW,	S,	J/MOVS		;MOVSS
						; 3956	
D 0210, 5500,0010				; 3957	210:	R-PF,	AC,	J/MOVN		;MOVN
D 0211, 1501,0010				; 3958		I-PF,	AC,	J/MOVN		;MOVNI
D 0212, 3600,0010				; 3959		W,	M,	J/MOVN		;MOVNM
D 0213, 7301,0010				; 3960		RPW,	S,	J/MOVN		;MOVNS
						; 3961	
D 0214, 5501,0202				; 3962	214:	R-PF,	AC,	J/MOVM		;MOVM
D 0215, 1501,0200				; 3963		I-PF,	AC,	J/HRRZ		;MOVMI <==> HRRZI <==> MOVEI
D 0216, 3601,0202				; 3964		W,	M,	J/MOVM		;MOVMM
D 0217, 7300,0202				; 3965		RPW,	S,	J/MOVM		;MOVMS
						; 3966	
D 0250, 7000,0314				; 3967	250:	RPW,	B/0,	J/EXCH		;EXCH--adjoins BLT
						; 3968		.UCODE
						; 3969	
						; 3970	=0****00****
U 0002, 0010,0001,4000,0000,3005,0033,0000	; 3971	MOVS:	AR_AR SWAP,EXIT			;ALSO USED BY HALFWORD GROUP
						; 3972	=
						; 3973	202:					;Must be near HRRZ
U 0202, 0100,0001,0040,0000,0000,4510,0000	; 3974	MOVM:	BR/AR,SKP AR0,J/MOVE		;FORCE POSITIVE
						; 3975	;
						; 3976	=0****00****
U 0010, 0101,0001,0040,0000,0000,0010,0000	; 3977	MOVN:	BR/AR,J/MOVNEG			;GET NEGATIVE
						; 3978	=
						; 3979	100:
U 0100, 0010,4001,0000,0000,0005,0033,0000	; 3980	MOVE:	EXIT				;STORE AS IS FROM AR
						; 3981	101:
U 0101, 0010,5142,2000,0000,0025,1333,0000	; 3982	MOVNEG:	AR_-BR,AD FLAGS,EXIT		;[412] Negate before storing
						; 3983	313:					;[440] Near SKIP, MOVEM
U 0313, 0216,4001,0000,0000,0217,4610,0000	; 3984	MOVES:	I FETCH,SKP AC#0,J/STSELF	;Like MOVE if AC not 0
						; 3985	314:					;[440] Near BLT, SKIP, MOVES
						; 3986	MOVEM:					;LIKE EXCH, EXCEPT NO STORE AC
U 0314, 1024,3240,2400,0000,1036,0010,0000	; 3987	EXCH:	ARX_AR,AR_AC0,STORE,J/STMAC	;PUT AC AT E, THEN STORE AC
						; 3988	;
						; 3989	102:					;Near SETM (MOVE)
U 0102, 0020,0001,0000,0000,0217,5710,0000	; 3990	XMOVEI:	SKP -LOCAL AC ADDR,I FETCH	;30 bit version of MOVEI
						; 3991	=0
U 0020, 0015,4041,0000,0000,0021,0017,0002	; 3992	XMOVEI1:ARL_1.M,ARR_ARR,J/STAC		;AC IN NON-ZERO SECTION
U 0021, 0015,4001,0000,0400,3001,0010,0200	; 3993		CLR P,J/STAC			;RETURN 30-BIT ADDRESS
						; 3994	317:					;[440] Near HLL
						; 3995	XHLLI:	SKP -LOCAL AC ADDR,I FETCH,
U 0317, 0020,3240,2000,0000,0237,5722,0000	; 3996			ARR_AC0.S,ARL_ARL.S,J/XMOVEI1
						; 3997	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; BASIC.MIC[4,24]	11:56 29-May-86			Physical MOVE Instructions--PMOVE, PMOVEM		

						; 3998	.TOC	"Physical MOVE Instructions--PMOVE, PMOVEM"
						; 3999	;
						; 4000	;	PMOVE and PMOVEM were added to the architecture as something of an
						; 4001	;	afterthought (as their opcodes may suggest) primarily to improve
						; 4002	;	the monitor performance of certain I/O devices which require
						; 4003	;	manipulation of data structures in physical memory.  Note that
						; 4004	;	the effective address of the instruction CONTAINS the physical
						; 4005	;	address to move from or to, somewhat akin to XJRST.  This extra
						; 4006	;	level of indirection is necessary to make section zero physical
						; 4007	;	addresses easily accessible from a non zero section and vice versa.
						; 4008	;	(Without this mechanism, the EA calculation logic would consider
						; 4009	;	addresses in section zero to be local, and would insert the current
						; 4010	;	section into the VMA.)
						; 4011	;
						; 4012	
						; 4013		.DCODE
D 0052, 4001,0103				; 4014	052:	R,	B/0,	J/PMOVE		;PMOVE (Was reserved to DEC)
D 0053, 4200,0103				; 4015		R,	B/2,	J/PMOVEM	;PMOVEM (Ditto)
						; 4016		.UCODE
						; 4017	
						; 4018	=0****00****
						; 4019	PMOVE:					;Share the code with PMOVEM
U 0103, 0120,3701,0000,0000,0300,7333,0000	; 4020	PMOVEM:	VMA_AR,SKP IO LEGAL,B DISP	;Set VMA for PMOVEM. OK to do?
						; 4021	=
U 0120, 1002,3242,2000,0000,0000,0010,0000	; 4022	=00	AR_BR,J/UUO			;Illegal to do a PMOVE
U 0121, 0017,3703,0000,0000,0312,0026,0103	; 4023		VMA_AR,LOAD AR,PHYS REF,J/STBOTH;PMOVE OK. Do a physical load
U 0122, 1002,3242,2000,0000,0000,0010,0000	; 4024		AR_BR,J/UUO			;Illegal to do a PMOVEM
U 0123, 0016,3200,2000,0000,0036,0026,0103	; 4025		AR_AC0,STORE,PHYS REF,J/STMEM	;PMOVEM OK. Do the physical store
						; 4026	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; BASIC.MIC[4,24]	11:56 29-May-86			DMOVE, DMOVN, DMOVEM, DMOVNM				

						; 4027	.TOC	"DMOVE, DMOVN, DMOVEM, DMOVNM"
						; 4028	
						; 4029	;	Double word moves to ACs.
						; 4030	
						; 4031		.DCODE
D 0120, 0000,0106				; 4032	120:	I,	B/0,	J/DMOVE		;DMOVE [413]
D 0121, 0101,0106				; 4033		I,	B/1,	J/DMOVN		;DMOVN [413]
						; 4034		.UCODE
						; 4035	
						; 4036	=0****00****
						; 4037	DMOVN:
U 0106, 0147,0001,0000,0000,0012,3610,0000	; 4038	DMOVE:	VMA_VMA+1,LOAD AR		;PICK UP (E+1)
						; 4039	=
						; 4040	TWJUNK:	FIN LOAD,VMA_VMA-1,LOAD ARX,	;And (E)--redundant if from ADJBP
U 0147, 0224,3240,0003,0000,0033,3533,0000	; 4041		    B DISP			;Split DMOVE from DMOVN
U 0224, 0226,3240,0003,0000,0237,0010,0000	; 4042	=100	FIN LOAD,I FETCH,J/DLOAD	;DMOVE. Start I FETCH and load
U 0225, 0241,3240,0003,0000,0022,0062,0100	; 4043		ARX_MEM,MQ_0.S,CALL [DNSUB]	;DMOVN. Do the negation
						; 4044	=111	AR_SHIFT,ARX/MQ,GEN -BR LONG,	;Force 0 into AC1 high bit
U 0227, 0226,5142,4300,0000,0237,1327,0000	; 4045		    AD FLAGS,I FETCH		; and set carry/overflow flags
U 0226, 0015,0001,4001,0000,2000,1010,0000	; 4046	DLOAD:	AC1_AR,AR_ARX,J/STAC		;Now load AC1, then AC0
						; 4047	;
						; 4048	;	DNSUB--Subroutine to swallow the high bit of the low order word of
						; 4049	;	a double pair, copy both words to BR/BRX, negate the pair and store
						; 4050	;	the high order word (if DMOVNM), and set up to zero the high bit of
						; 4051	;	the low order word.  Return 2.
						; 4052	;
U 0241, 0244,0301,4200,0302,2020,0010,0043	; 4053	DNSUB:	ARX_AR*2,AR_ARX,SC_#,#/35.	;Eat low order bit 0, set count
U 0244, 0322,5143,7760,0000,0025,0027,0000	; 4054		BR_AR LONG,AR_-AR LONG,B STORE	;Negate, store if DMOVNM
U 0322, 0002,3401,2013,0000,1002,0003,0000	; 4055		MEM_AR,MQ_AR,AR_0S,RETURN [2]	;Set up for shift, keep AC0
						; 4056	
						; 4057	;	DOUBLE MOVES TO MEMORY
						; 4058	
						; 4059		.DCODE
D 0124, 0001,0104				; 4060	124:	I,		J/DMOVEM	;DMOVEM [413]
D 0125, 3201,0105				; 4061		W,	B/2,	J/DMOVNM	;DMOVNM
						; 4062		.UCODE
						; 4063	
						; 4064	=0****00**00
U 0104, 0331,3200,2000,0302,0036,0010,0044	; 4065	DMOVEM:	AR_AC0,STORE,SC_#,#/36.,J/GETAC1;Store AC0 and set for later grab
						; 4066	;
						; 4067	DMOVNM:	MQ_0.S,AR_AC1,ARL/AD,ARX_AR,	;[421] Get second word, do
U 0105, 0241,3240,2401,0000,1020,0062,0102	; 4068		    CALL [DNSUB]		; negation, and store high word
U 0107, 0357,5162,0000,0000,0020,1327,0000	; 4069	=11	GEN -BR LONG,AD FLAGS,J/STLOW	;Set carry/overflow flags
						; 4070	=
						; 4071	;
U 0331, 0357,3240,0201,0000,0020,0010,0000	; 4072	GETAC1:	ARX_AC1				;For DMOVEM, ready second word
						; 4073	STLOW:	FIN STORE,VMA_VMA+1,AR_SHIFT,	;Store, then done
U 0357, 0016,0001,4003,0000,0016,3610,0000	; 4074		    STORE,J/STMEM
						; 4075	;
						; 4076	;	Come in here from meter code (DMVM1) and floating point (DBLST).
						; 4077	;	These are hacks which should be killed, if possible.
						; 4078	;
U 0372, 0357,3441,2003,0000,0002,0010,0000	; 4079	DMVM1:	MEM_AR,AR_0S,J/STLOW		;Set up to zero AC1 high bit
						; 4080	;
U 0431, 0141,3401,2000,0000,0217,1010,0000	; 4081	DBLST:	AC0_AR,AR_0S,I FETCH,J/STD1	;Store high word, ready low
						; 4082	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; BASIC.MIC[4,24]	11:56 29-May-86			HALFWORD GROUP						

						; 4083	.TOC	"HALFWORD GROUP"
						; 4084	;	DESTINATION LEFT HALF
						; 4085	
						; 4086		.DCODE
D 0500, 5500,0316				; 4087	500:	R-PF,	AC,	J/HLL		;HLL
D 0501, 2500,0317				; 4088		EA,	AC,	J/XHLLI		;XHLLI--Use 30 bit address
D 0502, 7601,0315				; 4089		RPW,	M,	J/HRR		;HLLM = HRR EXCEPT FOR STORE
D 0503, 6000,0313				; 4090		RW,		J/MOVES		;HLLS <==> MOVES [440]
						; 4091	
D 0504, 5501,0207				; 4092		R-PF,	AC,	J/HRL		;HRL
D 0505, 1500,0207				; 4093		I-PF,	AC,	J/HRL		;HRLI
D 0506, 7601,0114				; 4094		RPW,	M,	J/HRLM		;HRLM
D 0507, 7300,0115				; 4095		RPW,	S,	J/HRLS		;HRLS
						; 4096	
D 0510, 5501,0704				; 4097	510:	R-PF,	AC,	J/HLLZ		;HLLZ
D 0511, 1500,0704				; 4098		I-PF,	AC,	J/HLLZ		;HLLZI <==> SETZ
D 0512, 3601,0704				; 4099		W,	M,	J/HLLZ		;HLLZM
D 0513, 7300,0704				; 4100		RPW,	S,	J/HLLZ		;HLLZS
						; 4101	
D 0514, 5501,0300				; 4102		R-PF,	AC,	J/HRLZ		;HRLZ
D 0515, 1500,0300				; 4103		I-PF,	AC,	J/HRLZ		;HRLZI <==> MOVSI
D 0516, 3601,0300				; 4104		W,	M,	J/HRLZ		;HRLZM
D 0517, 7300,0300				; 4105		RPW,	S,	J/HRLZ		;HRLZS
						; 4106	
D 0520, 5500,0705				; 4107	520:	R-PF,	AC,	J/HLLO		;HLLO
D 0521, 1501,0705				; 4108		I-PF,	AC,	J/HLLO		;HLLOI
D 0522, 3600,0705				; 4109		W,	M,	J/HLLO		;HLLOM
D 0523, 7301,0705				; 4110		RPW,	S,	J/HLLO		;HLLOS
						; 4111	
D 0524, 5500,0301				; 4112		R-PF,	AC,	J/HRLO		;HRLO
D 0525, 1501,0301				; 4113		I-PF,	AC,	J/HRLO		;HRLOI
D 0526, 3600,0301				; 4114		W,	M,	J/HRLO		;HRLOM
D 0527, 7301,0301				; 4115		RPW,	S,	J/HRLO		;HRLOS
						; 4116	
D 0530, 5500,0703				; 4117	530:	R-PF,	AC,	J/HLLE		;HLLE
D 0531, 1500,0704				; 4118		I-PF,	AC,	J/SETZ		;HLLEI <==> SETZ [412]
D 0532, 3600,0703				; 4119		W,	M,	J/HLLE		;HLLEM
D 0533, 7301,0703				; 4120		RPW,	S,	J/HLLE		;HLLES
						; 4121	
D 0534, 5501,0401				; 4122		R-PF,	AC,	J/HRLE		;HRLE
D 0535, 1500,0401				; 4123		I-PF,	AC,	J/HRLE		;HRLEI
D 0536, 3601,0401				; 4124		W,	M,	J/HRLE		;HRLEM
D 0537, 7300,0401				; 4125		RPW,	S,	J/HRLE		;HRLES
						; 4126	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; BASIC.MIC[4,24]	11:56 29-May-86			HALFWORD GROUP						

						; 4127	;	DESTINATION RIGHT HALF
						; 4128	
D 0540, 5500,0315				; 4129	540:	R-PF,	AC,	J/HRR		;HRR
D 0541, 1501,0315				; 4130		I-PF,	AC,	J/HRR		;HRRI
D 0542, 7601,0316				; 4131		RPW,	M,	J/HLL		;HRRM = HLL EXCEPT FOR STORE
D 0543, 6000,0313				; 4132		RW,		J/MOVES		;HRRS <==> MOVES [440]
						; 4133	
D 0544, 5500,0212				; 4134		R-PF,	AC,	J/HLR		;HLR
D 0545, 1501,0212				; 4135		I-PF,	AC,	J/HLR		;HLRI
D 0546, 7600,0210				; 4136		RPW,	M,	J/HLRM		;HLRM
D 0547, 7301,0211				; 4137		RPW,	S,	J/HLRS		;HLRS
						; 4138	
D 0550, 5500,0200				; 4139	550:	R-PF,	AC,	J/HRRZ		;HRRZ
D 0551, 1501,0200				; 4140		I-PF,	AC,	J/HRRZ		;HRRZI <==> MOVEI
D 0552, 0000,0203				; 4141		I,		J/HRRZM		;HRRZM [442]
D 0553, 7301,0200				; 4142		RPW,	S,	J/HRRZ		;HRRZS
						; 4143	
D 0554, 5500,0502				; 4144		R-PF,	AC,	J/HLRZ		;HLRZ
D 0555, 1501,0502				; 4145		I-PF,	AC,	J/HLRZ		;HLRZI <==> SETZ
D 0556, 3600,0502				; 4146		W,	M,	J/HLRZ		;HLRZM
D 0557, 7301,0502				; 4147		RPW,	S,	J/HLRZ		;HLRZS
						; 4148	
D 0560, 5501,0201				; 4149	560:	R-PF,	AC,	J/HRRO		;HRRO
D 0561, 1500,0201				; 4150		I-PF,	AC,	J/HRRO		;HRROI
D 0562, 3601,0201				; 4151		W,	M,	J/HRRO		;HRROM
D 0563, 7300,0201				; 4152		RPW,	S,	J/HRRO		;HRROS
						; 4153	
D 0564, 5501,0503				; 4154		R-PF,	AC,	J/HLRO		;HLRO
D 0565, 1500,0503				; 4155		I-PF,	AC,	J/HLRO		;HLROI
D 0566, 3601,0503				; 4156		W,	M,	J/HLRO		;HLROM
D 0567, 7300,0503				; 4157		RPW,	S,	J/HLRO		;HLROS
						; 4158	
D 0570, 5500,0310				; 4159	570:	R-PF,	AC,	J/HRRE		;HRRE
D 0571, 1501,0310				; 4160		I-PF,	AC,	J/HRRE		;HRREI
D 0572, 3600,0310				; 4161		W,	M,	J/HRRE		;HRREM
D 0573, 7301,0310				; 4162		RPW,	S,	J/HRRE		;HRRES
						; 4163	
D 0574, 5501,0702				; 4164		R-PF,	AC,	J/HLRE		;HLRE
D 0575, 1500,0704				; 4165		I-PF,	AC,	J/SETZ		;HLREI <==> SETZ [412]
D 0576, 3601,0702				; 4166		W,	M,	J/HLRE		;HLREM
D 0577, 7300,0702				; 4167		RPW,	S,	J/HLRE		;HLRES
						; 4168	
						; 4169		.UCODE
						; 4170	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; BASIC.MIC[4,24]	11:56 29-May-86			HALFWORD GROUP						

						; 4171	;FIRST, THE 16 OPS WHICH DO NOT AFFECT THE "OTHER" HALF.
						; 4172	;THESE MUST BE TREATED SEPARATELY, BECAUSE THEY COMBINE MEMORY DATA
						; 4173	;IN AR WITH DATA FROM THE FM.  ENTER WITH 0,E OR (E) IN AR.
						; 4174	
						; 4175	315:					;[440] Near MOVES (for HLLS)
U 0315, 0010,3240,0000,0000,0025,0633,0002	; 4176	HRR:	ARL_AC0,ARR_ARR,EXIT		;HRR, HRRI, HLLM
						; 4177	316:					;[440] Near XHLLI, MOVES (for HRRS)
U 0316, 0010,3240,2000,0000,0025,0633,0000	; 4178	HLL:	ARR_AC0,ARL_ARL,EXIT		;HLL, HRRM
						; 4179				;HRRS, HLLS ARE BOTH EQUIVALENT TO MOVES
						; 4180	=0****00****
U 0207, 0010,3200,2000,0000,3025,0633,0004	; 4181	HRL:	ARL_ARR,ARR_AC0,EXIT		;HRL, HRLI
						; 4182	=
						; 4183	=0****00****
U 0212, 0010,3200,4000,0000,3025,0633,0002	; 4184	HLR:	ARR_ARL,ARL_AC0,EXIT		;HLR, HLRI
						; 4185	=
						; 4186	=0****00***0
U 0114, 0002,3240,2000,0000,3020,0610,0004	; 4187	HRLM:	ARL_ARR,ARR_AC0,J/MOVS		;HRLM
U 0115, 0010,0001,0000,0000,3005,0633,0004	; 4188	HRLS:	ARL_ARR,ARR_ARR,EXIT		;HRLS
						; 4189	=
						; 4190	=0****00***0
U 0210, 0002,3240,4000,0000,3020,0610,0002	; 4191	HLRM:	ARR_ARL,ARL_AC0,J/MOVS		;HLRM
U 0211, 0010,0001,4000,0000,3005,0633,0000	; 4192	HLRS:	ARR_ARL,ARL_ARL,EXIT		;HLRS
						; 4193	=
						; 4194	;NOW THE HALFWORD OPS WHICH CONTROL THE "OTHER" HALF
						; 4195	; ENTER WITH 0,E, (E), OR (AC) IN AR
						; 4196	
						; 4197	=0****00****
U 0310, 0200,0001,0000,0000,0000,4410,0000	; 4198	HRRE:	SKP AR18			;SELECT HRRZ OR HRRO ON SIGN
						; 4199	=
						; 4200	200:					;Must be near MOVMI, HRRZM
U 0200, 0010,0001,0000,0000,0005,0633,0020	; 4201	HRRZ:	ARL_0S,ARR_ARR,EXIT
						; 4202	201:
U 0201, 0010,2341,0000,0000,0005,0633,0002	; 4203	HRRO:	ARL_1S,ARR_ARR,EXIT
						; 4204	203:					;[442] Near HRRZ
U 0203, 0016,3200,2000,0000,0036,0610,0020	; 4205	HRRZM:	ARL_0S,ARR_AC0,STORE,J/STMEM	;[442] No need for write test
						; 4206	;
						; 4207	=0****00****
U 0401, 0300,4001,0000,0000,0000,4410,0000	; 4208	HRLE:	SKP AR18
						; 4209	=
						; 4210	=0****00***0
U 0300, 0010,3401,2000,0000,3005,0633,0004	; 4211	HRLZ:	ARL_ARR,ARR_0S,EXIT
U 0301, 0010,2301,2000,0000,3005,0633,0004	; 4212	HRLO:	ARL_ARR,ARR_1S,EXIT
						; 4213	=
						; 4214	702:					;Must be near SETZ
U 0702, 0502,4001,0000,0000,0000,4510,0000	; 4215	HLRE:	SKP AR0
						; 4216	=0****00***0
U 0502, 0010,4001,4000,0000,3005,0633,0020	; 4217	HLRZ:	ARR_ARL,ARL_0S,EXIT
U 0503, 0010,2301,4000,0000,3005,0633,0002	; 4218	HLRO:	ARR_ARL,ARL_1S,EXIT
						; 4219	=
						; 4220	703:					;Must be near SETZ
U 0703, 0704,0001,0000,0000,0000,4510,0000	; 4221	HLLE:	SKP AR0
						; 4222	704:					;Must be near HLRE and HLLE
						; 4223	SETZ:					;[421] ARL already 0 for SETZ
U 0704, 0010,3441,2000,0000,0005,0633,0000	; 4224	HLLZ:	ARR_0S,ARL_ARL,EXIT
						; 4225	705:					;Must follow HLLZ
U 0705, 0010,2341,2000,0000,0005,0633,0000	; 4226	HLLO:	ARR_1S,ARL_ARL,EXIT; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; BASIC.MIC[4,24]	11:56 29-May-86			BOOLEAN GROUP						

						; 4227	.TOC	"BOOLEAN GROUP"
						; 4228		.DCODE
D 0400, 1500,0704				; 4229	400:	I-PF,	AC,	J/SETZ		;SETZ <==> HLLZI--use that code
D 0401, 1500,0704				; 4230		I-PF,	AC,	J/SETZ		;SETZI <==> SETZ
D 0402, 0601,0704				; 4231		I,	M,	J/SETZ		;SETZM [421]
D 0403, 0700,0704				; 4232		I,	B,	J/SETZ		;SETZB [421]
						; 4233	
D 0404, 5500,0405				; 4234	404:	R-PF,	AC,	J/AND		;AND
D 0405, 1501,0405				; 4235		I-PF,	AC,	J/AND		;ANDI
D 0406, 7601,0405				; 4236		RPW,	M,	J/AND		;ANDM
D 0407, 7700,0405				; 4237		RPW,	B,	J/AND		;ANDB
						; 4238		.UCODE
						; 4239	
						; 4240	=0****00****
U 0405, 0010,3600,2000,0000,0025,0033,0000	; 4241	AND:	AR_AR*AC0,AD/AND,EXIT
						; 4242	=
						; 4243		.DCODE
D 0410, 5500,0406				; 4244	410:	R-PF,	AC,	J/ANDCA		;ANDCA
D 0411, 1501,0406				; 4245		I-PF,	AC,	J/ANDCA		;ANDCAI
D 0412, 7601,0406				; 4246		RPW,	M,	J/ANDCA		;ANDCAM
D 0413, 7700,0406				; 4247		RPW,	B,	J/ANDCA		;ANDCAB
						; 4248		.UCODE
						; 4249	
						; 4250	=0****00****
U 0406, 0010,3500,2000,0000,0025,0033,0000	; 4251	ANDCA:	AR_AR*AC0,AD/ANDCB,EXIT
						; 4252	=
						; 4253		.DCODE
D 0414, 5500,0100				; 4254	414:	R-PF,	AC,	J/MOVE		;SETM <==> MOVE
D 0415, 2500,0102				; 4255		EA,	AC,	J/XMOVEI	;XMOVEI (SETMI)--Use 30 bit EA
D 0416, 3000,0016				; 4256		W,		J/IFNOP		;[430] SETMM = NOP+memory write
D 0417, 6001,0017				; 4257		RW,		J/IFSTAC	;[430] SETMB = MOVE+memory write
						; 4258	
D 0420, 5501,0110				; 4259	420:	R-PF,	AC,	J/ANDCM		;ANDCM
D 0421, 1500,0110				; 4260		I-PF,	AC,	J/ANDCM		;ANDCMI
D 0422, 7600,0110				; 4261		RPW,	M,	J/ANDCM		;ANDCMM
D 0423, 7701,0110				; 4262		RPW,	B,	J/ANDCM		;ANDCMB
						; 4263		.UCODE
						; 4264	
						; 4265	110:					;Must be near TLX
U 0110, 0010,3000,2000,0000,0025,0033,0000	; 4266	ANDCM:	AR_AR*AC0,AD/ANDCA,EXIT
						; 4267	
						; 4268		.DCODE
D 0424, 1001,0217				; 4269	424:	I-PF,		J/NOP		;SETA <==> TRN (NOP) [413]
D 0425, 1001,0217				; 4270		I-PF,		J/NOP		;SETAI <==> TRN
D 0426, 0100,0314				; 4271		I,	B/1,	J/MOVEM		;SETAM <==> MOVEM [412]
D 0427, 0100,0314				; 4272		I,	B/1,	J/MOVEM		;SETAB <==> MOVEM [412]
						; 4273	
D 0430, 5500,0111				; 4274	430:	R-PF,	AC,	J/XOR		;XOR
D 0431, 1501,0111				; 4275		I-PF,	AC,	J/XOR		;XORI
D 0432, 7601,0111				; 4276		RPW,	M,	J/XOR		;XORM
D 0433, 7700,0111				; 4277		RPW,	B,	J/XOR		;XORB
						; 4278		.UCODE
						; 4279	
						; 4280	111:					;Must be near TLX
U 0111, 0010,3100,2000,4000,0025,0033,0000	; 4281	XOR:	AR_AR*AC0,AD/XOR,EXIT
						; 4282	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14
; BASIC.MIC[4,24]	11:56 29-May-86			BOOLEAN GROUP						

						; 4283		.DCODE
D 0434, 5500,0112				; 4284	434:	R-PF,	AC,	J/IOR		;OR
D 0435, 1501,0112				; 4285		I-PF,	AC,	J/IOR		;ORI
D 0436, 7601,0112				; 4286		RPW,	M,	J/IOR		;ORM
D 0437, 7700,0112				; 4287		RPW,	B,	J/IOR		;ORB
						; 4288		.UCODE
						; 4289	
						; 4290	112:					;Must be near TLX
U 0112, 0010,3300,2000,0000,0025,0033,0000	; 4291	IOR:	AR_AR*AC0,AD/OR,EXIT
						; 4292	
						; 4293		.DCODE
D 0440, 5501,0413				; 4294	440:	R-PF,	AC,	J/ANDCB		;ANDCB
D 0441, 1500,0413				; 4295		I-PF,	AC,	J/ANDCB		;ANDCBI
D 0442, 7600,0413				; 4296		RPW,	M,	J/ANDCB		;ANDCBM
D 0443, 7701,0413				; 4297		RPW,	B,	J/ANDCB		;ANDCBB
						; 4298		.UCODE
						; 4299	
						; 4300	=0****00****
U 0413, 0010,2400,2000,0000,0025,0033,0000	; 4301	ANDCB:	AR_AR*AC0,AD/ANDC,EXIT		;NOR function
						; 4302	=
						; 4303		.DCODE
D 0444, 5500,0501				; 4304	444:	R-PF,	AC,	J/EQV		;EQV
D 0445, 1501,0501				; 4305		I-PF,	AC,	J/EQV		;EQVI
D 0446, 7601,0501				; 4306		RPW,	M,	J/EQV		;EQVM
D 0447, 7700,0501				; 4307		RPW,	B,	J/EQV		;EQVB
						; 4308		.UCODE
						; 4309	
						; 4310	=0****00****
U 0501, 0010,2600,2000,4000,0025,0033,0000	; 4311	EQV:	AR_AR*AC0,AD/EQV,EXIT
						; 4312	=
						; 4313		.DCODE
D 0450, 1500,0505				; 4314	450:	I-PF,	AC,	J/SETCA		;SETCA
D 0451, 1500,0505				; 4315		I-PF,	AC,	J/SETCA		;SETCAI <==> SETCA
D 0452, 0601,0505				; 4316		I,	M,	J/SETCA		;SETCAM
D 0453, 0700,0505				; 4317		I,	B,	J/SETCA		;SETCAB
						; 4318		.UCODE
						; 4319	
						; 4320	=0****00****
U 0505, 0010,2540,2000,0000,0025,0033,0000	; 4321	SETCA:	AR_AC0 COMP,EXIT		;[442]
						; 4322	=
						; 4323		.DCODE
D 0454, 5501,0511				; 4324	454:	R-PF,	AC,	J/ORCA		;ORCA
D 0455, 1500,0511				; 4325		I-PF,	AC,	J/ORCA		;ORCAI
D 0456, 7600,0511				; 4326		RPW,	M,	J/ORCA		;ORCAM
D 0457, 7701,0511				; 4327		RPW,	B,	J/ORCA		;ORCAB
						; 4328		.UCODE
						; 4329	
						; 4330	=0****00****
U 0511, 0010,2700,2000,0000,0025,0033,0000	; 4331	ORCA:	AR_AR*AC0,AD/ORCB,EXIT
						; 4332	=
						; 4333	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15
; BASIC.MIC[4,24]	11:56 29-May-86			BOOLEAN GROUP						

						; 4334		.DCODE
D 0460, 5501,0701				; 4335	460:	R-PF,	AC,	J/SETCM		;SETCM
D 0461, 1500,0701				; 4336		I-PF,	AC,	J/SETCM		;SETCMI
D 0462, 7600,0701				; 4337		RPW,	M,	J/SETCM		;SETCMM
D 0463, 7701,0701				; 4338		RPW,	B,	J/SETCM		;SETCMB
						; 4339		.UCODE
						; 4340	
						; 4341	=0****00****
U 0701, 0010,2003,2000,0000,0005,0033,0000	; 4342	SETCM:	ADA/AR,AD/SETCA,AR/AD,EXIT
						; 4343	=
						; 4344		.DCODE
D 0464, 5500,0711				; 4345	464:	R-PF,	AC,	J/ORCM		;ORCM
D 0465, 1501,0711				; 4346		I-PF,	AC,	J/ORCM		;ORCMI
D 0466, 7601,0711				; 4347		RPW,	M,	J/ORCM		;ORCMM
D 0467, 7700,0711				; 4348		RPW,	B,	J/ORCM		;ORCMB
						; 4349		.UCODE
						; 4350	
						; 4351	=0****00****
U 0711, 0010,2200,2000,0000,0025,0033,0000	; 4352	ORCM:	AR_AR*AC0,AD/ORCA,EXIT
						; 4353	=
						; 4354		.DCODE
D 0470, 5501,0715				; 4355	470:	R-PF,	AC,	J/ORCB		;ORCB
D 0471, 1500,0715				; 4356		I-PF,	AC,	J/ORCB		;ORCBI
D 0472, 7600,0715				; 4357		RPW,	M,	J/ORCB		;ORCBM
D 0473, 7701,0715				; 4358		RPW,	B,	J/ORCB		;ORCBB
						; 4359		.UCODE
						; 4360	
						; 4361	=0****00****
U 0715, 0010,2100,2000,0000,0025,0033,0000	; 4362	ORCB:	AR_AR*AC0,AD/ORC,EXIT		;NAND function
						; 4363	=
						; 4364		.DCODE
D 0474, 1500,1007				; 4365	474:	I-PF,	AC,	J/SETO		;SETO
D 0475, 1500,1007				; 4366		I-PF,	AC,	J/SETO		;SETOI <==> SETO
D 0476, 0601,1007				; 4367		I,	M,	J/SETO		;SETOM
D 0477, 0700,1007				; 4368		I,	B,	J/SETO		;SETOB
						; 4369		.UCODE
						; 4370	
						; 4371	=0****00****
U 1007, 0010,2341,2000,0000,0005,0033,0000	; 4372	SETO:	AR_1S,EXIT
						; 4373	=
						; 4374	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; SKPJMP.MIC[4,24]	11:57 29-May-86			TEST GROUP						

						; 4375	.TOC	"TEST GROUP"
						; 4376		.DCODE
D 0600, 1001,0217				; 4377	600:	I-PF,		J/NOP		;TRN--a noop
D 0601, 1001,0217				; 4378		I-PF,		J/NOP		;TLN <==> TRN
D 0602, 0000,0507				; 4379		I,	TNE,	J/TDXX		;TRNE
D 0603, 0001,0506				; 4380		I,	TNE,	J/TSXX		;TLNE
D 0604, 0001,0213				; 4381		I,	TNA,	J/TDX		;TRNA--unconditional skip
D 0605, 0001,0213				; 4382		I,	TNA,	J/TDX		;TLNA <==> TRNA
D 0606, 0401,0507				; 4383		I,	TNN,	J/TDXX		;TRNN
D 0607, 0400,0506				; 4384		I,	TNN,	J/TSXX		;TLNN
						; 4385	
D 0610, 5000,0217				; 4386	610:	R-PF,		J/NOP		;TDN--a noop that references memory
D 0611, 5000,0217				; 4387		R-PF,		J/NOP		;TSN <==> TDN
D 0612, 4001,0507				; 4388		R,	TNE,	J/TDXX		;TDNE
D 0613, 4000,0506				; 4389		R,	TNE,	J/TSXX		;TSNE
D 0614, 4000,0213				; 4390		R,	TNA,	J/TDX		;TDNA--skip with memory reference
D 0615, 4000,0213				; 4391		R,	TNA,	J/TDX		;TSNA <==> TDNA
D 0616, 4400,0507				; 4392		R,	TNN,	J/TDXX		;TDNN
D 0617, 4401,0506				; 4393		R,	TNN,	J/TSXX		;TSNN
						; 4394	
D 0620, 1500,0110				; 4395	620:	I-PF,	AC,	J/ANDCM		;TRZ <==> ANDCMI
D 0621, 1500,0113				; 4396		I-PF,	TZ-,	J/TLX		;TLZ [440]
D 0622, 0101,0507				; 4397		I,	TZE,	J/TDXX		;TRZE
D 0623, 0100,0506				; 4398		I,	TZE,	J/TSXX		;TLZE
D 0624, 0100,0213				; 4399		I,	TZA,	J/TDX		;TRZA
D 0625, 0101,0214				; 4400		I,	TZA,	J/TLXA		;TLZA
D 0626, 0500,0507				; 4401		I,	TZN,	J/TDXX		;TRZN
D 0627, 0501,0506				; 4402		I,	TZN,	J/TSXX		;TLZN
						; 4403	
D 0630, 5501,0110				; 4404	630:	R-PF,	AC,	J/ANDCM		;TDZ <==> ANDCM
D 0631, 5501,0113				; 4405		R-PF,	TZ-,	J/TLX		;TSZ [440]
D 0632, 4100,0507				; 4406		R,	TZE,	J/TDXX		;TDZE
D 0633, 4101,0506				; 4407		R,	TZE,	J/TSXX		;TSZE
D 0634, 4101,0213				; 4408		R,	TZA,	J/TDX		;TDZA
D 0635, 4100,0214				; 4409		R,	TZA,	J/TLXA		;TSZA
D 0636, 4501,0507				; 4410		R,	TZN,	J/TDXX		;TDZN
D 0637, 4500,0506				; 4411		R,	TZN,	J/TSXX		;TSZN
						; 4412	
D 0640, 1501,0111				; 4413	640:	I-PF,	AC,	J/XOR		;TRC <==> XORI
D 0641, 1600,0113				; 4414		I-PF,	TC-,	J/TLX		;TLC [440]
D 0642, 0201,0507				; 4415		I,	TCE,	J/TDXX		;TRCE
D 0643, 0200,0506				; 4416		I,	TCE,	J/TSXX		;TLCE
D 0644, 0200,0213				; 4417		I,	TCA,	J/TDX		;TRCA
D 0645, 0201,0214				; 4418		I,	TCA,	J/TLXA		;TLCA
D 0646, 0600,0507				; 4419		I,	TCN,	J/TDXX		;TRCN
D 0647, 0601,0506				; 4420		I,	TCN,	J/TSXX		;TLCN
						; 4421	
D 0650, 5500,0111				; 4422	650:	R-PF,	AC,	J/XOR		;TDC <==> XOR
D 0651, 5601,0113				; 4423		R-PF,	TC-,	J/TLX		;TSC [440]
D 0652, 4200,0507				; 4424		R,	TCE,	J/TDXX		;TDCE
D 0653, 4201,0506				; 4425		R,	TCE,	J/TSXX		;TSCE
D 0654, 4201,0213				; 4426		R,	TCA,	J/TDX		;TDCA
D 0655, 4200,0214				; 4427		R,	TCA,	J/TLXA		;TSCA
D 0656, 4601,0507				; 4428		R,	TCN,	J/TDXX		;TDCN
D 0657, 4600,0506				; 4429		R,	TCN,	J/TSXX		;TSCN
						; 4430	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; SKPJMP.MIC[4,24]	11:57 29-May-86			TEST GROUP						

D 0660, 1501,0112				; 4431	660:	I-PF,	AC,	J/IOR		;TRO <==> ORI
D 0661, 1701,0113				; 4432		I-PF,	TO-,	J/TLX		;TLO [440]
D 0662, 0300,0507				; 4433		I,	TOE,	J/TDXX		;TROE
D 0663, 0301,0506				; 4434		I,	TOE,	J/TSXX		;TLOE
D 0664, 0301,0213				; 4435		I,	TOA,	J/TDX		;TROA
D 0665, 0300,0214				; 4436		I,	TOA,	J/TLXA		;TLOA
D 0666, 0701,0507				; 4437		I,	TON,	J/TDXX		;TRON
D 0667, 0700,0506				; 4438		I,	TON,	J/TSXX		;TLON
						; 4439	
D 0670, 5500,0112				; 4440	670:	R-PF,	AC,	J/IOR		;TDO <==> OR
D 0671, 5700,0113				; 4441		R-PF,	TO-,	J/TLX		;TSO [440]
D 0672, 4301,0507				; 4442		R,	TOE,	J/TDXX		;TDOE
D 0673, 4300,0506				; 4443		R,	TOE,	J/TSXX		;TSOE
D 0674, 4300,0213				; 4444		R,	TOA,	J/TDX		;TDOA
D 0675, 4301,0214				; 4445		R,	TOA,	J/TLXA		;TSOA
D 0676, 4700,0507				; 4446		R,	TON,	J/TDXX		;TDON
D 0677, 4701,0506				; 4447		R,	TON,	J/TSXX		;TSON
						; 4448		.UCODE
						; 4449	
						; 4450	;MOST OF THESE 64 INSTRUCTIONS ARE DECODED BY MASK MODE (IMMEDIATE OR
						; 4451	; MEMORY) IN THE A FIELD, DISPATCH TO HERE ON THE J FIELD, AND RE-DISPATCH
						; 4452	; FOR THE MODIFICATION ON THE B FIELD.  A few of these are synonyms for
						; 4453	; other instructions, and use the synonym microcode for speed (see above).
						; 4454	; Those that never skip will always prefetch.
						; 4455	; ENTER WITH 0,E OR (E) IN AR, B FIELD BITS 1 AND 2 AS FOLLOWS:
						; 4456	; 0 0	NO MODIFICATION
						; 4457	; 0 1	ZEROS
						; 4458	; 1 0	COMPLEMENT
						; 4459	; 1 1	ONES
						; 4460	; THIS ORDER HAS NO SIGNIFICANCE EXCEPT THAT IT CORRESPONDS TO THE
						; 4461	; ORDER OF INSTRUCTIONS AT TGROUP.  THE HIGH ORDER BIT OF THE B FIELD
						; 4462	; (B0) IS XOR'D WITH AD CRY0 TO DETERMINE THE SENSE OF THE SKIP:
						; 4463	; 0	SKIP IF CRY0=1 (TXX- AND TXXN)
						; 4464	; 1	SKIP IF CRY0=0 (TXXA AND TXXE)
						; 4465	
						; 4466	213:					;[412][440] Near TLXA/CAIM
U 0213, 0304,2041,0000,0000,0226,0033,0203	; 4467	TDX:	TEST FETCH,NO CRY,B DISP,J/TDN	;TDXA and TRXA
						; 4468	214:					;[412][440] Near TDX
						; 4469	TLXA:	AR_AR SWAP,TEST FETCH,NO CRY,	;TSXA and TLXA
U 0214, 0304,2001,4000,0000,3226,0033,0203	; 4470		    B DISP,J/TDN
						; 4471	113:					;[440] Near ANDCM/XOR/IOR
U 0113, 0304,2041,4000,0000,3000,0033,0000	; 4472	TLX:	AR_AR SWAP,NO CRY,B DISP,J/TDN	;[440] TLX and TSX
						; 4473	=0****00***0				;[412]
U 0506, 0507,4001,4000,0000,3000,0010,0000	; 4474	TSXX:	AR_AR SWAP			;TSXE, TSXN, TLXE, AND TLXN
U 0507, 0304,3600,0000,4000,0246,0033,0203	; 4475	TDXX:	TEST AR.AC0,TEST FETCH,B DISP	;TDXE, TDXN, TRXE, AND TRXN
						; 4476	=
						; 4477	304:					;[412][441]
U 0304, 0133,0001,0000,0000,0000,0010,0000	; 4478	TDN:	J/FINI				;NO MODIFICATION
U 0305, 0015,3000,2000,0000,0000,0010,0000	; 4479	305:	AR_AR*AC0,AD/ANDCA,TIME/2T,J/STAC;[441] ZEROS
U 0306, 0015,3100,2000,4000,0000,0010,0000	; 4480	306:	AR_AR*AC0,AD/XOR,TIME/2T,J/STAC	;[441] COMP
U 0307, 0015,3300,2000,0000,0000,0010,0000	; 4481	307:	AR_AR*AC0,AD/OR,TIME/2T,J/STAC	;[441] ONES
						; 4482	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; SKPJMP.MIC[4,24]	11:57 29-May-86			COMPARE -- CAI, CAM					

						; 4483	.TOC	"COMPARE -- CAI, CAM"
						; 4484	
						; 4485		.DCODE
D 0300, 1001,0217				; 4486	300:	I-PF,		J/NOP		;CAI <==> TRN
D 0301, 0200,0215				; 4487		I,	SJCL,	J/CAIM		;CAIL
D 0302, 0100,0215				; 4488		I,	SJCE,	J/CAIM		;CAIE
D 0303, 0001,0215				; 4489		I,	SJCLE,	J/CAIM		;CAILE
D 0304, 0001,0213				; 4490		I,	TNA,	J/TDX		;CAIA <==> TRNA
D 0305, 0601,0215				; 4491		I,	SJCGE,	J/CAIM		;CAIGE
D 0306, 0501,0215				; 4492		I,	SJCN,	J/CAIM		;CAIN
D 0307, 0400,0215				; 4493		I,	SJCG,	J/CAIM		;CAIG
						; 4494	
						; 4495	
D 0310, 5000,0217				; 4496	310:	R-PF,		J/NOP		;CAM <==> TDN
D 0311, 4201,0215				; 4497		R,	SJCL,	J/CAIM		;CAML
D 0312, 4101,0215				; 4498		R,	SJCE,	J/CAIM		;CAME
D 0313, 4000,0215				; 4499		R,	SJCLE,	J/CAIM		;CAMLE
D 0314, 4000,0213				; 4500		R,	TNA,	J/TDX		;CAMA <==> TDNA
D 0315, 4600,0215				; 4501		R,	SJCGE,	J/CAIM		;CAMGE
D 0316, 4500,0215				; 4502		R,	SJCN,	J/CAIM		;CAMN
D 0317, 4401,0215				; 4503		R,	SJCG,	J/CAIM		;CAMG
						; 4504		.UCODE
						; 4505	
						; 4506	215:					;[440] Near NOP and TDX
U 0215, 0217,3100,0000,0000,0246,0010,0201	; 4507	CAIM:	GEN AR*AC0,COMP FETCH,J/NOP
						; 4508	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; SKPJMP.MIC[4,24]	11:57 29-May-86			ARITHMETIC SKIPS -- AOS, SOS, SKIP			

						; 4509	.TOC	"ARITHMETIC SKIPS -- AOS, SOS, SKIP"
						; 4510	;ENTER WITH (E) IN AR
						; 4511	
						; 4512		.DCODE
D 0330, 4001,0313				; 4513	330:	R,		J/MOVES		;SKIP--rather like MOVES [440]
D 0331, 4201,0312				; 4514		R,	SJCL,	J/SKIP		;SKIPL
D 0332, 4101,0312				; 4515		R,	SJCE,	J/SKIP		;SKIPE
D 0333, 4000,0312				; 4516		R,	SJCLE,	J/SKIP		;SKIPLE
D 0334, 4701,0312				; 4517		R,	SJCA,	J/SKIP		;SKIPA
D 0335, 4600,0312				; 4518		R,	SJCGE,	J/SKIP		;SKIPGE
D 0336, 4500,0312				; 4519		R,	SJCN,	J/SKIP		;SKIPN
D 0337, 4401,0312				; 4520		R,	SJCG,	J/SKIP		;SKIPG
						; 4521		.UCODE
						; 4522	
						; 4523	312:					;Must be near MOVES
						; 4524	SKIP:	FIN STORE,SKIP FETCH,
U 0312, 0216,3703,0003,0000,0246,4610,0202	; 4525			SKP AC#0,J/STSELF	;STORE IN SELF MODE
						; 4526	
						; 4527		.DCODE
D 0350, 7001,0706				; 4528	350:	RPW,		J/AONS		;AOS--never skip [440]
D 0351, 7201,0707				; 4529		RPW,	SJCL,	J/AOS		;AOSL
D 0352, 7101,0707				; 4530		RPW,	SJCE,	J/AOS		;AOSE
D 0353, 7000,0707				; 4531		RPW,	SJCLE,	J/AOS		;AOSLE
D 0354, 7701,0707				; 4532		RPW,	SJCA,	J/AOS		;AOSA
D 0355, 7600,0707				; 4533		RPW,	SJCGE,	J/AOS		;AOSGE
D 0356, 7500,0707				; 4534		RPW,	SJCN,	J/AOS		;AOSN
D 0357, 7401,0707				; 4535		RPW,	SJCG,	J/AOS		;AOSG
						; 4536		.UCODE
						; 4537	
						; 4538	=0****00***0
U 0706, 0013,4003,2000,0000,0036,1310,0000	; 4539	AONS:	AR_AR+1,AD FLAGS,STORE,J/SLFEND	;[440] AOS only
U 0707, 0312,4001,2000,0000,0036,1310,0000	; 4540	AOS:	AR_AR+1,AD FLAGS,STORE,J/SKIP	;Other AOSx
						; 4541	=
						; 4542	
						; 4543		.DCODE
D 0370, 7001,0712				; 4544	370:	RPW,		J/SONS		;SOS--never skip [440]
D 0371, 7201,0713				; 4545		RPW,	SJCL,	J/SOS		;SOSL
D 0372, 7101,0713				; 4546		RPW,	SJCE,	J/SOS		;SOSE
D 0373, 7000,0713				; 4547		RPW,	SJCLE,	J/SOS		;SOSLE
D 0374, 7701,0713				; 4548		RPW,	SJCA,	J/SOS		;SOSA
D 0375, 7600,0713				; 4549		RPW,	SJCGE,	J/SOS		;SOSGE
D 0376, 7500,0713				; 4550		RPW,	SJCN,	J/SOS		;SOSN
D 0377, 7401,0713				; 4551		RPW,	SJCG,	J/SOS		;SOSG
						; 4552		.UCODE
						; 4553	
						; 4554	=0****00***0
U 0712, 0013,1701,2000,0000,0036,1310,0000	; 4555	SONS:	AR_AR-1,AD FLAGS,STORE,J/SLFEND	;[440] SOS only
U 0713, 0312,1703,2000,0000,0036,1310,0000	; 4556	SOS:	AR_AR-1,AD FLAGS,STORE,J/SKIP	;Other SOSx
						; 4557	=
						; 4558	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; SKPJMP.MIC[4,24]	11:57 29-May-86			CONDITIONAL JUMPS -- JUMP, AOJ, SOJ, AOBJ		

						; 4559	.TOC	"CONDITIONAL JUMPS -- JUMP, AOJ, SOJ, AOBJ"
						; 4560	
						; 4561		.DCODE
D 0320, 1001,0304				; 4562	320:	I-PF,		J/TDN		;JUMP <==> TRN (Do not jump!)
D 0321, 0200,0303				; 4563		I,	SJCL,	J/JUMP		;JUMPL
D 0322, 0100,0303				; 4564		I,	SJCE,	J/JUMP		;JUMPE
D 0323, 0001,0303				; 4565		I,	SJCLE,	J/JUMP		;JUMPLE
D 0324, 0000,0302				; 4566		I,		J/JUMPA		;JUMPA--always jump [441]
D 0325, 0601,0303				; 4567		I,	SJCGE,	J/JUMP		;JUMPGE
D 0326, 0501,0303				; 4568		I,	SJCN,	J/JUMP		;JUMPN
D 0327, 0400,0303				; 4569		I,	SJCG,	J/JUMP		;JUMPG
						; 4570		.UCODE
						; 4571	302:					;[441]
U 0302, 0217,0001,0000,0000,0017,0010,0000	; 4572	JUMPA:	FETCH,J/NOP			;JUMPA only
						; 4573	303:					;Other JUMPx--near TDN
U 0303, 0217,3200,2000,0000,0246,0010,0502	; 4574	JUMP:	AR_AC0,JUMP FETCH,J/NOP		; E IS IN VMA (HERE AND BELOW)
						; 4575	
						; 4576		.DCODE
D 0340, 1001,0205				; 4577	340:	I-PF,		J/AONJ		;AOJ--never jump [440]
D 0341, 0201,0206				; 4578		I,	SJCL,	J/AOJ		;AOJL
D 0342, 0101,0206				; 4579		I,	SJCE,	J/AOJ		;AOJE
D 0343, 0000,0206				; 4580		I,	SJCLE,	J/AOJ		;AOJLE
D 0344, 0001,0204				; 4581		I,		J/AOJA		;AOJA--always jump [440]
D 0345, 0600,0206				; 4582		I,	SJCGE,	J/AOJ		;AOJGE
D 0346, 0500,0206				; 4583		I,	SJCN,	J/AOJ		;AOJN
D 0347, 0401,0206				; 4584		I,	SJCG,	J/AOJ		;AOJG
						; 4585		.UCODE
						; 4586	=0****00**00
U 0204, 0205,0001,0000,0000,0017,0010,0000	; 4587	AOJA:	FETCH				;[441] AOJA only
U 0205, 0015,4640,2000,0000,0020,1310,0000	; 4588	AONJ:	AR_AC0+1,AD FLAGS,J/STAC	;[440] AOJ only
U 0206, 0216,4640,2000,0000,0266,1310,0502	; 4589	AOJ:	AR_AC0+1,AD FLAGS,JUMP FETCH,J/STORAC;Other AOJx
						; 4590	=
						; 4591	
						; 4592		.DCODE
D 0360, 1500,0410				; 4593	360:	I-PF,	AC,	J/SONJ		;SOJ--never jump [440]
D 0361, 0201,0412				; 4594		I,	SJCL,	J/SOJ		;SOJL
D 0362, 0101,0412				; 4595		I,	SJCE,	J/SOJ		;SOJE
D 0363, 0000,0412				; 4596		I,	SJCLE,	J/SOJ		;SOJLE
D 0364, 0500,0411				; 4597		I,	AC,	J/SOJA		;SOJA--always jump [440]
D 0365, 0600,0412				; 4598		I,	SJCGE,	J/SOJ		;SOJGE
D 0366, 0500,0412				; 4599		I,	SJCN,	J/SOJ		;SOJN
D 0367, 0401,0412				; 4600		I,	SJCG,	J/SOJ		;SOJG
						; 4601		.UCODE
						; 4602	=0****00**00
U 0410, 1014,2301,2000,0000,0000,0010,0000	; 4603	SONJ:	AR_1S,J/ADD			;[440] SOJ only--add -1 to AC0
U 0411, 1014,2301,2000,0000,0017,0010,0000	; 4604	SOJA:	AR_1S,FETCH,J/ADD		;[440] SOJA only--add -1 and jump
U 0412, 0455,3200,2000,0000,0020,0010,0000	; 4605	SOJ:	AR_AC0				;Other SOJx
U 0455, 0216,1703,2000,0000,0266,1310,0502	; 4606	=	AR_AR-1,AD FLAGS,JUMP FETCH,J/STORAC
						; 4607	
						; 4608		.DCODE
D 0252, 0601,1013				; 4609	252:	I,	SJCGE,	J/AOBJ		;AOBJP
D 0253, 0200,1013				; 4610		I,	SJCL,	J/AOBJ		;AOBJN
						; 4611		.UCODE
						; 4612	=0****00****
U 1013, 0216,4640,2000,0000,0266,0017,0502	; 4613	AOBJ:	AR_AC0+1,GEN CRY18,JUMP FETCH,J/STORAC
						; 4614	=; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; SKPJMP.MIC[4,24]	11:57 29-May-86			AC DECODE JUMPS -- JRST					

						; 4615	.TOC	"AC DECODE JUMPS -- JRST"
						; 4616	
						; 4617		.DCODE
D 0254, 2000,0600				; 4618	254:	EA,		J/JRST		;JRST--must be at JFCL-100
						; 4619		.UCODE
						; 4620	;
						; 4621	;	A READ detects JRST, and dispatches to one of 16 loc'ns on AC bits.
						; 4622	;	These have been completely rewritten to save space.  [414]
						; 4623	;	Note that the instruction dispatch must have cleared SC
						; 4624	;	for XJRSTF to work.
						; 4625	;
						; 4626	600:					;Multiple of 200, at JFCL-100
U 0600, 0133,0001,0000,0000,0000,0010,0000	; 4627	JRST:	J/FINI				;(0) JRST--A READ has prefetched
U 0601, 0217,3703,0000,0000,0317,0024,0412	; 4628	601:	PORTAL,VMA_AR,FETCH,J/NOP	;(1) PORTAL
						; 4629	602:
U 0602, 0464,0001,0000,0000,0000,5050,0000	; 4630	JRSTF:	SKP PC SEC0,CALL [JRSTOK]	;(2) JRSTF--Must be in section 0
U 0603, 1002,4001,0000,0000,0000,0010,0000	; 4631	603:	J/UUO				;(3)
						; 4632	604:
U 0604, 0464,4001,0000,0000,0000,6150,0000	; 4633	HALT:	SKP KERNEL,CALL [JRSTOK]	;(4) HALT--Must be kernel mode
U 0605, 0574,0001,0000,0000,0012,0050,0000	; 4634	605:	LOAD AR,CALL [GETNXT]		;(5) XJRSTF. Fetch flags
U 0606, 0604,4001,0000,3402,0000,7310,0000	; 4635	606:	SC_1S,SKP IO LEGAL,J/HALT	;(6) XJEN. Set switch
U 0607, 0464,4031,0200,0000,0020,7365,0000	; 4636	607:	ARX_PC+1,SKP IO LEGAL,CALL [JRSTOK];(7) XPCW
U 0610, 0464,4001,0000,0000,0000,7350,0000	; 4637	610:	SKP IO LEGAL,CALL [JRSTOK]	;(10) JRST 10, dismiss only
U 0611, 1002,4001,0000,0000,0000,0010,0000	; 4638	611:	J/UUO				;(11)
U 0612, 0464,4001,0000,0000,0000,7350,0000	; 4639	612:	SKP IO LEGAL,CALL [JRSTOK]	;(12) JEN
U 0613, 1002,4001,0000,0000,0000,0010,0000	; 4640	613:	J/UUO				;(13)
U 0614, 0444,4033,0200,0000,0020,0625,0030	; 4641	614:	AR_0.C,ARX_PC+1,J/XSFM		;(14) XSFM
U 0615, 0551,0001,0000,0000,0012,0010,0000	; 4642	615:	LOAD AR,J/XJRST			;(15) XJRST
U 0616, 1002,4001,0000,0000,0000,0010,0000	; 4643	616:	J/UUO				;(16)
U 0617, 1002,4001,0000,0000,0000,0010,0000	; 4644	617:	J/UUO				;(17)
						; 4645	;
						; 4646	;	JRSTOK will either drop out to UUO or return 20.
						; 4647	;	GETNXT starts reading the next word and returns 40.
						; 4648	;
U 0622, 0116,2301,0040,0000,0000,0036,0000	; 4649	622:	BR/AR,EA MOD DISP,J/JRSNDX	;JRSTF.  Is it indexed?
						; 4650	;
U 0624, 0620,0001,0000,0000,0000,0024,0442	; 4651	624:	HALT,J/CHALT			;HALT. Stop and loop
						; 4652	;
						; 4653	645:	RSTR FLAGS_AR,ARX_AR SWAP,	;XJRSTF/XJEN. Restore flags, get
U 0645, 0324,3240,0403,0000,3022,4724,0420	; 4654		    AR_MEM,SKP SC0,J/XJSPLT	; address, and dismiss if XJEN
						; 4655	;
						; 4656	627:	BRX/ARX,AR_ARX ANDC ADMSK,	;XPCW. Get flags
U 0627, 0424,3510,2227,4000,0020,0010,0175	; 4657		    ARX/AD,J/XPCW
						; 4658	;
U 0630, 0217,3703,0000,0000,0317,0024,0502	; 4659	630:	DISMISS,VMA_AR,FETCH,J/NOP	;JRST 10. Just dismiss and jump
						; 4660	;
U 0632, 0602,0001,0000,0000,0000,0024,0502	; 4661	632:	DISMISS,J/JRSTF			;JEN. Dismiss, then restore flags
						; 4662	;
						; 4663	;	The more complex forms of JRST end up here.  Most of these were
						; 4664	;	substantially rewritten. [414]
						; 4665	;
						; 4666	=1110
U 0116, 0456,0001,4000,0000,2000,0010,0000	; 4667	JRSNDX:	AR_ARX,J/RFLAGS			;JRSTF/JEN, no index. Flags in mem
U 0117, 0456,3240,2002,0000,0020,0010,0000	; 4668		AR_XR				;Indexed. Flags in AC
U 0456, 0335,3202,2000,0000,0000,0024,0420	; 4669	RFLAGS:	RSTR FLAGS_AR,AR_BR,J/ARJMP	;[436] Restore flags; avoid race
						; 4670	;; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6-1
; SKPJMP.MIC[4,24]	11:57 29-May-86			AC DECODE JUMPS -- JRST					

						; 4671	=0
U 0324, 0334,4001,0000,0000,0000,6210,0000	; 4672	XJSPLT:	SKP USER,J/LDPCS		;XJRSTF, no dismiss. Enter user?
U 0325, 0334,4001,0000,0000,0000,6224,0502	; 4673		DISMISS,SKP USER		;XJEN. Dismiss interrupt. User mode?
						; 4674	=0
U 0334, 0461,0001,0000,0000,0000,2210,0400	; 4675	LDPCS:	GET ECL EBUS,J/PCSLOD		;No. Get EBUS and load PCS
U 0335, 0217,3703,0000,0000,0317,0010,0000	; 4676	ARJMP:	VMA_AR,FETCH,J/NOP		;Yes. We're done
						; 4677	;
U 0461, 0531,3713,0000,0000,0060,2010,0426	; 4678	PCSLOD:	LD PCS				;Do the load (from ARX)
U 0531, 0335,0001,0000,0000,0000,2210,0002	; 4679		COND/EBUS CTL,EBUS CTL/2,J/ARJMP;Release ECL EBUS and go
						; 4680	;
						; 4681	=0
						; 4682	XPCW:	BR/AR,AR_0S,SKP USER,		;XPCW. Save flags and fetch previous
U 0424, 0474,3441,2040,0000,0000,6250,0000	; 4683		    CALL [PRVSEC]		; section if not user mode
U 0425, 0535,3202,0000,0000,0016,0610,0002	; 4684		ARL_BRL,ARR_ARR,STORE		;Store flags word
						; 4685		FIN STORE,AR_ARX COMP AND BRX,	;Store PC word
U 0535, 0634,3022,6003,0000,0016,3610,0000	; 4686		    VMA_VMA+1,STORE
						; 4687	=0*****	FIN STORE,VMA_VMA+1,LOAD AR,	;Fetch new flags
U 0634, 0574,0001,0003,0000,0012,3650,0000	; 4688		    CALL [GETNXT]		; and PC
U 0674, 0335,3200,0003,0000,0022,0024,0420	; 4689		RSTR FLAGS_AR,AR_MEM,J/ARJMP	;Restore flags, then jump
						; 4690	;
						; 4691	=0
						; 4692	XSFM:	ARX_ARX ANDC ADMSK,SKP USER,	;Isolate flags and previous
U 0444, 0474,3510,0207,4000,0020,6250,0175	; 4693		    CALL [PRVSEC]		; section if not user mode
U 0445, 0016,0001,0000,0000,2016,0022,0004	; 4694		ARL_ARXL,ARR_ARR,STORE,J/STMEM	;Store flags and exit
						; 4695	;
U 0551, 0335,3200,0003,0000,0022,2110,0105	; 4696	XJRST:	AR_MEM,SET ACCOUNT EN,J/ARJMP	;XJRST. Get jump address and go
						; 4697	;
						; 4698	;	JRSTOK--Subroutine to check whether a JRST X, is legal.  Enter
						; 4699	;	skipping legal.  This exits to UUO if not, and returns 20
						; 4700	;	otherwise.
						; 4701	;
						; 4702	=0
U 0464, 1002,4001,0000,0000,0000,0010,0000	; 4703	JRSTOK:	J/UUO				;No go
U 0465, 0020,4001,0000,0000,0000,0003,0000	; 4704		RETURN20			;Looks OK
						; 4705	;
						; 4706	;	GETNXT--Subroutine to await a load and then start loading
						; 4707	;	the next word in sequence.  Return 40.
						; 4708	;
U 0574, 0040,3200,0003,0000,0032,3603,0000	; 4709	GETNXT:	FIN LOAD,VMA_VMA+1,LOAD AR,RETURN [40];Await, then load
						; 4710	;
						; 4711	;	PRVSEC--Subroutine to load the previous section and then
						; 4712	;	swap it into the right half of AR.  This is paired with
						; 4713	;	ARSWAP (below).  Return 1.
						; 4714	;
						; 4715	=0
U 0474, 0475,4001,0000,0000,2000,0210,0000	; 4716	PRVSEC:	AR12-17_PREV SEC		;Load PCS
						; 4717	;
						; 4718	;	[334] Subroutine to swap AR halves.  Used in a couple of places.
						; 4719	;
U 0475, 0001,0001,4000,0000,3000,0003,0000	; 4720	ARSWAP:	AR_AR SWAP,RETURN1		;[334] Rearrange things
						; 4721	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; SKPJMP.MIC[4,24]	11:57 29-May-86			HALT LOOP						

						; 4722	.TOC	"HALT LOOP"
						; 4723	;HERE WHILE PROCESSOR IS "HALTED"
						; 4724	
						; 4725	CHALT:	AR_0S,CLR SC,CLR FE,SET HALTED,	;KERNEL OR CONSOLE HALT
U 0620, 0554,3401,2000,0403,0100,1515,0302	; 4726			VMA/PC,PC_VMA		; IF JRST 4, COPY EA TO PC
						;;4727	.IF/PAGCNT				;[327] PFH, DATAO PAG bit 2 counts
						;;4728		TRX2_AR				;[327] Zero count registers
						;;4729		TRX3_AR
						; 4730	.ENDIF/PAGCNT				;[327]
						; 4731	;
						; 4732	;	The halt loop must be an odd number of ticks to allow diagnostics
						; 4733	;	to synchronize EBOX with E and SBUS clock phases.
						; 4734	;
						; 4735	=0
						; 4736	HALT1:	SKP -START,TIME/3T,		;CHECK FOR CONTINUE BUTTON
U 0554, 0654,4001,0400,2421,1020,7110,0000	; 4737			FE_AR0-8,ARX_AR,J/HALT2	;PICK UP OPCODE IN CASE XCT
U 0555, 0144,0001,0000,0000,0000,7710,0000	; 4738		TAKE INTRPT			;HERE IF EXAMINE/DEPOSIT UP
						; 4739	=0
U 0654, 0326,0001,0000,3000,0020,1534,0000	; 4740	HALT2:	GEN FE-1,BYTE DISP,CONTINUE,J/UNHALT	;INSTR FROM SWITCHES?
U 0655, 0554,4001,0000,0000,0000,7010,0000	; 4741		SKP INTRPT,TIME/2T,J/HALT1	;Still halted
						; 4742	=110
U 0326, 1235,4001,0000,0401,0000,1510,0310	; 4743	UNHALT:	SET CONS XCT,CLR FE,J/UXCT	;XCT ONE FROM "SWITCHES"
U 0327, 0000,5063,0000,0000,0040,5410,0000	; 4744		SKP AR EQ,J/START		;NOT AN INSTR.  START, OR CONT?
						; 4745	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; SKPJMP.MIC[4,24]	11:57 29-May-86			AC DECODE JUMPS -- JFCL					

						; 4746	.TOC	"AC DECODE JUMPS -- JFCL"
						; 4747	
						; 4748		.DCODE
D 0255, 0401,0700				; 4749	255:	I,	TNN,	J/JFCL		;JFCL--must be at JRST+100
						; 4750		.UCODE
						; 4751	;
						; 4752	;	JFCL--Often a slow noop.
						; 4753	;
						; 4754	700:					;JFCL MUST BE AT JRST+100
U 0700, 0130,3202,0600,0302,0000,4610,0015	; 4755	JFCL:	ARX_BRX,SC_#,#/13.,SKP AC EQ 0	;[440] Grab AC field. Is it 0?
						; 4756	=00	AR_SHIFT,ARX_0S,		;No. MOVE AC TO AR32-35
U 0130, 2604,3441,4200,0302,0000,0050,0040	; 4757			SC_#,#/32.,CALL,J/SHIFT	;SHIFTER WILL MOVE TO 0-3
U 0131, 0217,4001,0000,0000,0217,0010,0000	; 4758		I FETCH,J/NOP			;[440] Yes. Speed up noop
U 0132, 0625,3733,2040,0000,0000,0024,0602	; 4759		BR/AR,AR_PC,JFCL T		;[440] GET PC FLAGS INTO AR
U 0625, 0631,3602,0000,0000,0246,0010,0503	; 4760	=	TEST AR.BR,JFCL FETCH		;JUMP IF TEST SATISFIED
U 0631, 0641,3502,2000,0000,0000,0010,0000	; 4761		AR_AR*BR,AD/ANDCB		;CLEAR TESTED FLAGS IN AR
U 0641, 0133,4001,0000,0000,0000,0024,0622	; 4762		JFCL S,J/FINI			;SET PC FROM THEM
						; 4763	
						; 4764	.TOC	"MAP"
						; 4765	
						; 4766		.DCODE
D 0257, 0500,1311				; 4767	257:	I,	AC,	J/MAP		;MAP--256 is XCT
						; 4768		.UCODE
						; 4769	
						; 4770	1311:					;Must be near XCT
U 1311, 0653,4001,0040,0000,0007,0010,0140	; 4771	MAP:	MAP,BR/AR			;MAP E, GO READ BACK EBRG
U 0653, 0740,4001,0000,0000,0000,1610,0015	; 4772	=	SR_MAP				;With KL PAGING, MAP CAN PAGE FAIL
						; 4773	=0
						; 4774	RDEBRG:	AR_0S,SKP IO LEGAL,MB WAIT,	;FINISH READ REG FUNC
U 0740, 3132,3401,2000,0000,0002,7350,0000	; 4775			CALL,J/GETEEB		;AND GET EBUS
U 0741, 0672,4001,3000,0000,0060,2010,0567	; 4776		AR_EBUS REG			;READ DATA
U 0672, 0016,4001,0000,0000,0005,2233,0000	; 4777		REL ECL EBUS,B WRITE,J/ST6	;GIVE IT TO USER
						; 4778	;
						; 4779	;	MAP never generates a hard page fail.  If the MAP microorder
						; 4780	;	faults, CLEAN returns here with
						; 4781	;
						; 4782	;	AR_SV.PFW,SKP IO LEGAL		;Return PFW in AC
						; 4783	=0
U 0744, 1002,0001,0000,0000,0000,1610,0000	; 4784	MAP2:	SR_0,J/UUO			;NO MAPS IN USER MODE
U 0745, 0754,0001,0000,0000,0000,7010,0000	; 4785		SKP INTRPT			;DO NOT BUM THIS CODE OUT
U 0754, 0216,0001,0000,0000,0217,0010,0000	; 4786	=0	I FETCH,J/STORAC		;MAP must do nothing interesting if
U 0755, 0361,4001,0000,0000,0000,2110,0105	; 4787		SET ACCOUNT EN,J/TAKINT		; an interrupt is pending
						; 4788	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; SKPJMP.MIC[4,24]	11:57 29-May-86			STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ		

						; 4789	.TOC	"STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ"
						; 4790	
						; 4791		.DCODE
D 0260, 2001,1017				; 4792	260:	EA,		J/PUSHJ		;PUSHJ
D 0261, 4000,1016				; 4793		R,	B/0,	J/PUSH		;PUSH
D 0262, 2001,1104				; 4794		EA,	B/0,	J/POP		;POP
D 0263, 0001,1105				; 4795		I,		J/POPJ		;POPJ
						; 4796		.UCODE
						; 4797	
						; 4798	;PUSHJ
						; 4799	; ENTER WITH E IN AR
						; 4800	;PUSH
						; 4801	; ENTER WITH (E) IN AR
						; 4802	
						; 4803	=0****00***0
U 1016, 1024,4660,0200,0000,0151,5420,0041	; 4804	PUSH:	ARX_AC0+1,PUSH,SKP CRY0,J/STMAC	;BUMP AC ACCORDING TO FORMAT
						; 4805						; AND SECTION NUMBER
U 1017, 1020,4033,2040,0000,0020,5025,0000	; 4806	PUSHJ:	BR/AR,AR_PC+1,SKP PC SEC0	;GET PC WITH FLAGS
						; 4807	=
U 1020, 1021,3600,2007,0000,0020,0010,0175	; 4808	=0	AR_AR AND ADMSK			;STRIP OFF FLAGS IF NOT SEC0
U 1021, 0222,4660,0200,0000,0151,5420,0041	; 4809		ARX_AC0+1,PUSH,SKP CRY0,J/JSTAC	;UPDATE STACK POINTER, STORE
						; 4810	=00
U 0220, 1045,3701,0000,0000,0313,0050,0000	; 4811	JRA1:	VMA_AR,LOAD ARX,CALL,J/XFERW	;GET SAVED AC
						; 4812	=10
						; 4813	JSTAC:	FIN STORE,VMA_BR,FETCH,		;STORE PC, JUMP ADDR TO VMA
U 0222, 0216,3202,4003,0000,2317,0010,0000	; 4814			AR_ARX,J/STORAC		;PREPARE TO STORE AC VALUE
U 0223, 0222,0001,0003,0000,0002,1110,0040	; 4815		TRAP2,MEM_AR,J/JSTAC		;CAUSE PDL OVRFLO
						; 4816	
						; 4817	=0
						; 4818	STMAC:	FIN STORE,I FETCH,		;STORE RESULT, GET NEXT INSTR
U 1024, 0216,4001,4003,0000,2217,0033,0000	; 4819			AR_ARX,B DISP,J/STSELF	;STORE AC IF B=0
						; 4820		MEM_AR,TRAP2,			;PDL OVFLO, CAUSE TRAP
U 1025, 0017,4001,4003,0000,2002,1110,0040	; 4821			AR_ARX,J/IFSTAC		;UPDATE AC BEFORE TRAPPING
						; 4822	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; SKPJMP.MIC[4,24]	11:57 29-May-86			STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ		

						; 4823	;POP, POPJ
						; 4824	;ENTER WITH E IN AR
						; 4825	
						; 4826	=0****00***0
U 1104, 0034,3240,2040,0000,0131,0010,0421	; 4827	POP:	BR/AR,AR_AC0,POP AR,J/POP2	;GET FROM STACK
						; 4828	
U 1105, 0675,3240,2000,0000,0131,0010,0621	; 4829	POPJ:	AR_AC0,POP AR-ARX		;GET STACK TO AR AND ARX
						; 4830	=	AR_AR-1,TIME/3T,AC0,		;BACK OFF POINTER
U 0675, 1030,1703,2000,0000,0020,5420,0000	; 4831		    STACK UPDATE,SKP CRY0	; UNDERFLOW?
						; 4832	=0	MQ_AR,AR_MEM,ARX_MEM,TIME/3T,	;[413] Yes. AC to MQ, PC to AR and
U 1030, 0731,3200,0013,0000,1022,1110,0040	; 4833		    TRAP2,J/POPJT		;[414] ARX, set trap
						; 4834		MQ_AR,AR_MEM,ARL_0.S,ARX_MEM,	;[413] AC TO BR, HALFWORD PC TO AR
U 1031, 1034,3240,0013,0000,1022,5022,0020	; 4835		    TIME/3T,SKP PC SEC0		;FULL PC TO ARX. Extended form?
						; 4836	=0
						; 4837	POPJ2:	VMA_ARX,FETCH,ARX/MQ,		;[413] YES.  LOAD ENTIRE ADDR
U 1034, 0014,3711,0300,0302,0317,0010,0044	; 4838		    SC_#,#/36.,J/SHFLOD
						; 4839		VMA_AR,FETCH,ARX/MQ,SC_#,#/36.,	;[413] NO, LOAD HALFWORD ADDRESS
U 1035, 0014,3703,0300,0302,0317,0010,0044	; 4840		    J/SHFLOD
						; 4841	;
						; 4842	;	POPJ gets stack underflow.  Unfortunately, we can't clear ARL and
						; 4843	;	set TRAP2 at the same time (to say nothing of testing PC SEC0).
						; 4844	;
						; 4845	POPJT:	ARL_0.S,ARR_ARR,		;[414] Halfword PC to AR. Test for
U 0731, 1034,4001,0000,0000,0000,5022,0020	; 4846		    SKP PC SEC0,J/POPJ2		; extended form
						; 4847	;
						; 4848	=0*
						; 4849	POP2:	ARX_AR-1,AC0,STACK UPDATE,	;BACK UP POINTER
U 0034, 1044,1701,0200,0000,0040,5460,0000	; 4850			SKP CRY0,CALL [POPTRP]	; and test for trap [413]
U 0036, 1024,3202,0000,0000,0111,0010,0042	; 4851		GEN BR,WRITE (E),J/STMAC	;STORE RESULT & AC
						; 4852	;
						; 4853	;	Subroutine to test for POP trap and wait for memory.  Return 2.
						; 4854	;
						; 4855	=0
U 1044, 0002,3200,0003,0000,0022,1103,0040	; 4856	POPTRP:	AR_MEM,TRAP2,RETURN [2]		;POP stack underflowed.
						; 4857	;
						; 4858	;	A one line subroutine to wait for a memory fetch (either AR
						; 4859	;	or ARX) and return.  Used by all sorts of things.  Must adjoin
						; 4860	;	POPTRP.
						; 4861	;
U 1045, 0002,3240,0003,0000,0022,0003,0000	; 4862	XFERW:	AR_MEM,ARX_MEM,TIME/3T,RETURN2	;Cross reference both macros [313]
						; 4863	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; SKPJMP.MIC[4,24]	11:57 29-May-86			SUBROUTINE CALL/RETURN -- JSR, JSP, JSA, JRA		

						; 4864	.TOC	"SUBROUTINE CALL/RETURN -- JSR, JSP, JSA, JRA"
						; 4865	
						; 4866		.DCODE
D 0264, 2000,1205				; 4867	264:	EA,		J/JSR		;JSR
D 0265, 2001,1204				; 4868		EA,		J/JSP		;JSP
D 0266, 0001,1214				; 4869		I,		J/JSA		;JSA
D 0267, 0000,1215				; 4870		I,		J/JRA		;JRA
						; 4871		.UCODE
						; 4872	
						; 4873	=0****00***0
U 1204, 1064,4031,2000,0000,0037,5025,0000	; 4874	JSP:	AR_PC+1,FETCH,SKP PC SEC0,J/JSP1
						; 4875	
U 1205, 1054,4031,2000,0000,0020,5025,0000	; 4876	JSR:	AR_PC+1,SKP PC SEC0
						; 4877	=
U 1054, 0751,3600,2007,0000,0036,0010,0175	; 4878	=0	AR_AR AND ADMSK,STORE,J/JSR1
U 1055, 0751,0001,0000,0000,0016,0010,0000	; 4879		STORE				;IN SECT 0, SAVE FLAGS, TOO
U 0751, 0217,0001,0003,0000,0017,3610,0000	; 4880	JSR1:	FIN STORE,VMA_VMA+1,FETCH,J/NOP
						; 4881	
						; 4882	=0
U 1064, 0015,3600,2007,0000,0020,0010,0175	; 4883	JSP1:	AR_AR AND ADMSK,J/STAC		;NON-ZERO SEC, NO FLAGS
U 1065, 0133,4001,0000,0000,0000,1010,0000	; 4884		AC0_AR,J/FINI
						; 4885	
						; 4886	
						; 4887	=0****00***0
U 1214, 1032,3240,2400,0000,3036,0010,0000	; 4888	JSA:	ARX_AR SWAP,AR_AC0,STORE,J/JSA1	;SAVE E IN ARX LEFT, GET AC
						; 4889	
U 1215, 1015,4001,0000,0000,1000,0210,0000	; 4890	JRA:	AR12-17_PC SEC			;[235] put section in jump address.
U 1015, 1026,3200,2040,0000,0020,0010,0000	; 4891	=	BR/AR,AR_AC0			;[235][414] Grab AC, keep jump addr
U 1026, 0220,3202,4000,0000,3000,0610,0002	; 4892		ARR_ARL,ARL_BRL,J/JRA1		;[235][414] Generate memory address
						; 4893	
U 1032, 1036,0001,0003,0000,0017,3610,0000	; 4894	JSA1:	FIN STORE,VMA_VMA+1,FETCH	;JUMP TO E+1
U 1036, 0015,4031,2000,0000,2020,0022,0004	; 4895		ARR_PC+1,ARL_ARXL,J/STAC	;PC+1,,E GOES TO AC
						; 4896	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; SKPJMP.MIC[4,24]	11:57 29-May-86			UUO'S							

						; 4897	.TOC	"UUO'S"
						; 4898	;LUUO'S TRAP TO CURRENT CONTEXT
						; 4899	; EXTENDED INSTRUCTION SET IS "HIDDEN" BENEATH LUUO OPCODES
						; 4900	;	The general format is
						; 4901	;
						; 4902	;	EA,	Bfield,	J/EXTEND op code - 1000
						; 4903	;
						; 4904	;	so that a jump to LUUO can be put as the direct DRAM object
						; 4905	;	and the EXTEND dispatch can ship it off to the appropriate
						; 4906	;	extended op code processor.  All of the legal EXTEND op codes
						; 4907	;	are assembled adjacent to their handlers.
						; 4908	;
						; 4909	;	WARNING:  use extreme caution if E1 for MOVSRJ or CMPSE should
						; 4910	;	ever be used for anything, as they are sign smeared if they are
						; 4911	;	> 377777 (they fall in with MOVSO and friends at EXT2). [301]
						; 4912	;	Use similar caution if new EXTEND codes are created which
						; 4913	;	must have the DCODE B field be 1 or 3.
						; 4914	;
						; 4915		.DCODE
D 0000, 2000,1002				; 4916	000:	EA,		J/UUO
						; 4917	.IF/EXTEXP
D 0020, 2001,1110				; 4918	020:	EA,	J/BLUUO			;XBLT LUUO must adjoin GSNGL
						; 4919	.IFNOT/GFTCNV
D 0023, 2001,1110				; 4920	023:	EA,	J/BLUUO			;G floating converts
D 0024, 2001,1110				; 4921		EA,	J/BLUUO			;decommited due to
D 0025, 2001,1110				; 4922		EA,	J/BLUUO			;lack of space
D 0026, 2001,1110				; 4923		EA,	J/BLUUO
						; 4924	.ENDIF/GFTCNV
						;;4925	.IFNOT/EXTEXP
						;;4926	020:	EA,	J/LUUO			;XBLT no longer dispatched
						;;4927		EA,	J/LUUO
						;;4928		EA,	J/LUUO
						;;4929		EA,	J/LUUO
						;;4930	024:	EA,	J/LUUO
						;;4931		EA,	J/LUUO
						;;4932		EA,	J/LUUO
						;;4933		EA,	J/LUUO
						;;4934	030:	EA,	J/LUUO
						;;4935		EA,	J/LUUO
						; 4936	.ENDIF/EXTEXP
D 0032, 2001,1005				; 4937	032:	EA,	J/LUUO			;These are reserved to Cobol.
D 0033, 2001,1005				; 4938		EA,	J/LUUO
D 0034, 2001,1005				; 4939		EA,	J/LUUO
D 0035, 2001,1005				; 4940		EA,	J/LUUO
D 0036, 2001,1005				; 4941		EA,	J/LUUO
D 0037, 2001,1005				; 4942		EA,	J/LUUO
						; 4943	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; SKPJMP.MIC[4,24]	11:57 29-May-86			UUO'S							

						; 4944	;MONITOR UUO'S -- TRAP TO EXEC
						; 4945	
D 0040, 2000,1002				; 4946	040:	EA,	J/MUUO		;CALL
D 0041, 2000,1002				; 4947		EA,	J/MUUO		;INIT
D 0042, 2000,1002				; 4948		EA,	J/MUUO
D 0043, 2000,1002				; 4949		EA,	J/MUUO
D 0044, 2000,1002				; 4950		EA,	J/MUUO
D 0045, 2000,1002				; 4951		EA,	J/MUUO
D 0046, 2000,1002				; 4952		EA,	J/MUUO
D 0047, 2000,1002				; 4953		EA,	J/MUUO		;CALLI
D 0050, 2000,1002				; 4954		EA,	J/MUUO		;OPEN
D 0051, 2000,1002				; 4955		EA,	J/MUUO		;TTCALL
						; 4956	;
						; 4957	;	052 and 053 are now PMOVE and PMOVEM.
						; 4958	;
D 0054, 2000,1002				; 4959	054:	EA,	J/MUUO
D 0055, 2000,1002				; 4960		EA,	J/MUUO		;RENAME
D 0056, 2000,1002				; 4961		EA,	J/MUUO		;IN
D 0057, 2000,1002				; 4962		EA,	J/MUUO		;OUT
D 0060, 2000,1002				; 4963		EA,	J/MUUO		;SETSTS
D 0061, 2000,1002				; 4964		EA,	J/MUUO		;STATO
D 0062, 2000,1002				; 4965		EA,	J/MUUO		;GETSTS
D 0063, 2000,1002				; 4966		EA,	J/MUUO		;STATZ
D 0064, 2000,1002				; 4967		EA,	J/MUUO		;INBUF
D 0065, 2000,1002				; 4968		EA,	J/MUUO		;OUTBUF
D 0066, 2000,1002				; 4969		EA,	J/MUUO		;INPUT
D 0067, 2000,1002				; 4970		EA,	J/MUUO		;OUTPUT
D 0070, 2000,1002				; 4971		EA,	J/MUUO		;CLOSE
D 0071, 2000,1002				; 4972		EA,	J/MUUO		;RELEAS
D 0072, 2000,1002				; 4973		EA,	J/MUUO		;MTAPE
D 0073, 2000,1002				; 4974		EA,	J/MUUO		;UGETF
D 0074, 2000,1002				; 4975		EA,	J/MUUO		;USETI
D 0075, 2000,1002				; 4976		EA,	J/MUUO		;USETO
D 0076, 2000,1002				; 4977		EA,	J/MUUO		;LOOKUP
D 0077, 2000,1002				; 4978		EA,	J/MUUO		;ENTER
						; 4979	
						; 4980	;EXPANSION OPCODES
						; 4981	
D 0100, 2000,1002				; 4982	100:	EA,	J/UUO		;UJEN
D 0101, 2000,1002				; 4983		EA,	J/UUO
D 0247, 2000,1002				; 4984	247:	EA,	J/UUO		;[430] Adjoins LSHC
						; 4985	
						;;4986	.IFNOT/EXTEXP
						;;4987	102:	EA,	J/UUO
						;;4988		EA,	J/UUO
						;;4989	106:	EA,	J/UUO
						;;4990		EA,	J/UUO
						; 4991	.ENDIF/EXTEXP
						; 4992		.UCODE
						; 4993	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14
; SKPJMP.MIC[4,24]	11:57 29-May-86			UUO'S							

						; 4994	;HERE FOR UNDEFINED OPS (UUO'S) AND ILLEGAL INSTRUCTIONS
						; 4995	;E IS IN AR, OPCODE AND AC IN BRX
						; 4996	
						; 4997	;HERE ON LUUO'S
						; 4998	; E IN AR, INSTR IN BRX
						; 4999	;
						; 5000	;	All LUUOs which have corresponding EXTEND op codes must dispatch
						; 5001	;	to their own first word.  In all cases, it is the same as LUUO.
						; 5002	;
						; 5003	.IF/EXTEXP				;[337]
						; 5004	1116:
						;;5005	.IF/GFTCNV				;[427]
						;;5006	BLUUO:					;[427] Define this for XBLT
						; 5007	.ENDIF/GFTCNV				;[427]
U 1116, 0440,3202,0600,0000,0000,5010,0000	; 5008	L-GTPI:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GSNGL
						; 5009	1114:
U 1114, 0440,3202,0600,0000,0000,5010,0000	; 5010	L-SFTE:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GDBLE
						;;5011	.IF/GFTCNV
						;;5012	1117:
						;;5013	L-GTDI:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GDFIX
						;;5014	1106:
						;;5015	L-GTSI:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GFIX
						;;5016	1107:
						;;5017	L-GTDR:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GDFIXR
						;;5018	1110:
						;;5019	L-GTSR:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GFIXR
						; 5020	.IFNOT/GFTCNV
						; 5021	1110:
U 1110, 0440,3202,0600,0000,0000,5010,0000	; 5022	BLUUO:	ARX_BRX,SKP PC SEC0,J/LUUO1	;Conditioned out EXTEND ops
U 3110, 1002,4001,0000,0000,0000,0010,0000	; 5023	3110:	J/MUUO				;Force EXTEND to UUO
						; 5024	.ENDIF/GFTCNV
						; 5025	1111:
U 1111, 0440,3202,0600,0000,0000,5010,0000	; 5026	L-DITE:	ARX_BRX,SKP PC SEC0,J/LUUO1	;DGFLTR
						; 5027	1112:
U 1112, 0440,3202,0600,0000,0000,5010,0000	; 5028	L-SITE:	ARX_BRX,SKP PC SEC0,J/LUUO1	;DGLTR
						; 5029	1113:
U 1113, 0440,3202,0600,0000,0000,5010,0000	; 5030	L-EFSC:	ARX_BRX,SKP PC SEC0,J/LUUO1	;GFSC
						;;5031	.IFNOT/EXTEXP
						;;5032	1110:
						;;5033	BLUUO:	ARX_BRX,SKP PC SEC0,J/LUUO1	;Conditioned out EXTEND ops
						;;5034	3110:	J/MUUO				;Force EXTEND to UUO
						; 5035	.ENDIF/EXTEXP
						; 5036	1006:
U 1006, 0440,3202,0600,0000,0000,5010,0000	; 5037	L-EDIT:	ARX_BRX,SKP PC SEC0,J/LUUO1	;EDIT
						; 5038	1010:
U 1010, 0440,3202,0600,0000,0000,5010,0000	; 5039	L-DBIN:	ARX_BRX,SKP PC SEC0,J/LUUO1	;DBIN AT 2010
						; 5040	1011:
U 1011, 0440,3202,0600,0000,0000,5010,0000	; 5041	L-BDEC:	ARX_BRX,SKP PC SEC0,J/LUUO1	;BDEC AT 2011
						; 5042	1012:
U 1012, 0440,3202,0600,0000,0000,5010,0000	; 5043	L-MVS:	ARX_BRX,SKP PC SEC0,J/LUUO1	;MOVE STRING AT 2012
						; 5044	1005:
						; 5045	L-CMS:					;STRING COMPARE
U 1005, 0440,3202,0600,0000,0000,5010,0000	; 5046	LUUO:	ARX_BRX,SKP PC SEC0		;WHICH KIND OF UUO?
						; 5047	=0***0
						; 5048	LUUO1:	CLR P,SKP -LOCAL AC ADDR,	;[414] Extended. Generate clean  
U 0440, 1134,4001,0000,0400,3001,5750,0200	; 5049		    CALL [UUOCOM]		; section number; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14-1
; SKPJMP.MIC[4,24]	11:57 29-May-86			UUO'S							

						; 5050		BR/AR,AR_ARX ANDC ADMSK,	;COMPATiBLE.  ADDR TO BR
U 0441, 1074,3510,2047,4000,0020,7010,0175	; 5051			SKP INTRPT,J/LUUO2	; DO IT THE OLD WAY
U 0460, 0240,0001,0000,0000,0100,3010,0420	; 5052		VMA_#,#/420			;PT LOC FOR LUUO BLOCK POINTER
						; 5053	=
U 0240, 1045,4001,0000,0000,0013,0066,0033	; 5054	=00	LOAD ARX,PT REF,CALL,J/XFERW	;GET LUUO BLOCK ADDRESS
U 0242, 1132,3711,0000,0000,0316,0050,0000	; 5055	=10	VMA_ARX,STORE,CALL,J/UUOC2	;STORE UUO OPCODE AND FLAGS
						; 5056		FIN STORE,VMA_VMA+1,LOAD AR,	;NOW GET A NEW PC
U 0243, 0551,0001,0003,0000,0012,3610,0000	; 5057			J/XJRST			; [414]
						; 5058	
						; 5059	;HERE FOR COMPATIBLE UUO
						; 5060	=0
						; 5061	LUUO2:	AR_AR*BR,AD/OR,VMA_#,#/40,	;SAVE OPCODE AND EA
U 1074, 1046,3302,2004,0000,0116,3010,0040	; 5062			STORE,J/LUUO3		;THEN GET INSTR FROM 41
U 1075, 0144,0001,0000,0000,0000,7710,0000	; 5063		TAKE INTRPT			;ONE MOMENT, PLEASE
						; 5064	LUUO3:	FIN STORE,VMA_VMA+1,LOAD ARX,
U 1046, 0366,0001,0003,0000,0013,3610,0000	; 5065		    J/XCTW
						; 5066	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15
; SKPJMP.MIC[4,24]	11:57 29-May-86			UUO'S							

						; 5067	;HERE ON MUUO'S
						; 5068	; E IN AR, OP AND AC IN BRX
						; 5069	
						; 5070	1002:					;Fixed for EXTEND, other ops
						; 5071	UUO:					;A PEDANTIC DISTINCTION...
						; 5072	MUUO:	ARX_BRX,CLR P,SKP -LOCAL AC ADDR,;[414] Get clean section number
U 1002, 1134,3202,0600,0400,3001,5750,0200	; 5073		    CALL [UUOCOM]		; and pull together pieces of UUO
U 1022, 1056,4001,0000,0000,0013,0026,0223	; 5074	1022:	LOAD ARX,UPT REF		;GET NEW PC
U 1056, 1124,3240,0003,0000,0122,3010,0424	; 5075		ARX_MEM,VMA_#,#/424		;LOC'N OF MUUO DATA BLOCK
						; 5076	=0	BRX/ARX,STORE,UPT REF,		;STORE OPCODE, FLAGS
U 1124, 1132,4001,0020,0000,0016,0066,0223	; 5077			CALL,J/UUOC2		;NOW RETURN TO COMMON CODE
U 1125, 0320,3733,2003,0302,0002,0010,0004	; 5078		MEM_AR,AR_PC,SC_#,#/4		;READY TO SETUP NEW FLAGS
						; 5079	=00	VMA_VMA+1,SC_#,#/60,		;SET UP FOR CONTEXT WORD
						; 5080			SH DISP,AR_0S,		;TEST USER AND PUBLIC FLAGS
U 0320, 0232,3401,2000,0302,0020,3647,0060	; 5081			CALL,J/MUUOF		;SET NEW PREV FLAGS, GET EBUS
						; 5082		DATAI PAG(L),ARX_1B17-1,	;GO COLLECT DATAI PAG INFO
U 0321, 3576,1761,3200,0000,0060,2057,0511	; 5083			CALL,J/PCTXT
U 0323, 1062,3733,0000,0000,0060,2010,0426	; 5084	=11	LD PREV CTXT			;PCS FROM PC, CWSX FROM SXCT
						; 5085		AR_SHIFT,ARL_BRL.S,		;COMBINE UBR WITH AC BLKS, CWSX
						; 5086			STORE,			; STORE THAT AT 426 (XADDR =427)
U 1062, 1066,3242,4000,0000,0016,2222,0002	; 5087			COND/EBUS CTL,EBUS CTL/2; & RELEASE ECL EBUS
U 1066, 1072,3242,6003,0000,0002,1610,0000	; 5088		MEM_AR,AR_BRX,SR_0		;NOW GET NEW PC
U 1072, 0217,3600,0007,4000,0337,0010,0175	; 5089	SETPC:	VMA_AR AND ADMSK,FETCH,J/NOP
						; 5090	
						; 5091	=0					;[414]
U 1134, 1135,4061,0000,0000,0021,0017,0002	; 5092	UUOCOM:	ARL_1.M				;FORCE AC ADDRESS
U 1135, 1076,3510,2017,0000,1020,0010,0175	; 5093		MQ_AR,AR_ARX ANDC ADMSK		;SAVE ADDR IN MQ.  GET OPCODE
U 1076, 1144,0001,0040,0000,0000,6222,0030	; 5094		BR/AR,AR_0.S,SKP USER		;SAVE OPCODE IN BR
U 1144, 1145,0001,0000,0000,2000,0210,0000	; 5095	=0	AR12-17_PREV SEC		;GET PCS
U 1145, 1117,3302,2004,0000,0100,3210,0430	; 5096		AR_AR*BR,AD/OR,VMA_430+MODE	;[414] OPCODE+PCS, UUO new PC loc
U 1117, 1126,3530,2407,4000,3020,0010,0175	; 5097		ARX_AR SWAP,AR_PC FLAGS		;GET FLAGS FROM PC
U 1126, 0020,4001,4000,0000,2000,0603,0000	; 5098		ARL_ARL,ARR_ARX,RETURN20	;[414] FLAGS AND OPCODE COMBINED
						; 5099	
U 1132, 1136,4033,0203,0000,0022,0025,0000	; 5100	UUOC2:	MEM_AR,ARX_PC+1			;FINISH STORE
						; 5101		AR_ARX AND ADMSK,		;PC+1 ADDRESS TO AR
U 1136, 1142,3610,2307,4000,0036,3610,0175	; 5102			VMA_VMA+1,STORE,ARX/MQ	;PUT PC AWAY, GET EFFECTIVE ADDR
						; 5103		FIN STORE,AR_ARX,
U 1142, 0001,0001,4003,0000,2016,3603,0000	; 5104			VMA_VMA+1,STORE,RETURN1	;PUT EA AWAY.
						; 5105	
						; 5106	=1010					;HERE TO SETUP NEW FLAGS
U 0232, 3133,0001,0000,0000,0000,0024,0020	; 5107	MUUOF:	SET FLAGS_AR,J/GTEEB1		;GO GET ECL EBUS
U 0233, 0232,4001,0000,0000,0000,0110,0400	; 5108		AR0-8_#,#/400,J/MUUOF		;PREV CTXT SUPERVISOR
U 0236, 0232,4001,0000,0000,0000,0110,0004	; 5109		AR0-8_#,#/004,J/MUUOF		;  USER/CONCEALED
U 0237, 0232,0001,0000,0000,0000,0110,0404	; 5110		AR0-8_#,#/404,J/MUUOF		;  USER/PUBLIC
						; 5111	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 16
; SKPJMP.MIC[4,24]	11:57 29-May-86			JSYS, ADJSP, XCT, PXCT					

						; 5112	.TOC	"JSYS, ADJSP, XCT, PXCT"
						; 5113	
						; 5114		.DCODE
D 0104, 2000,1002				; 5115	104:	EA,		J/UUO		;JSYS
D 0105, 0000,1000				; 5116		I,		J/ADJSP		;ADJSP [431]
						; 5117		.UCODE
						; 5118	
						; 5119	;
						; 5120	;	ADJSP has been completely rewritten to start the I FETCH
						; 5121	;	quicker in all cases.  [431]
						; 5122	;
						; 5123	1000:					;Must adjoin JSYS (UUO)
U 1000, 1146,4041,0200,0000,3000,0610,0004	; 5124	ADJSP:	ARL_ARR,ARR_ARR,ARX_1,TIME/2T	;Gen adjustment for short stack
						; 5125		AC0,STACK UPDATE,GEN ARX-1,	;Short stack enables CRY18; thus
U 1146, 1154,1711,0000,0000,0257,5420,0000	; 5126		    SKP CRY0,I FETCH		; skip on long pointer
						; 5127	=0	AR_AR+FM[AC0],INH CRY18,	;Short pointer. Adjust both halves
U 1154, 1164,0600,2000,4000,0020,4511,0000	; 5128		    SKP AR0,J/ADJPDL		; and set for proper overflow test
U 1155, 1151,5441,0000,0000,0020,0616,0002	; 5129		ARL_SIGN,ARR_ARR		;Long pointer. Sign extend E
U 1151, 0015,0600,2000,4000,0020,0010,0000	; 5130		AR_AR+FM[AC0],J/STAC		;Adjust and store
						; 5131	;
						; 5132	=0
						; 5133	ADJPDL:	GEN AR*AC0,AD/ANDCA,SKP AD0,	;Positive adjustment. Test for
U 1164, 1174,3000,0000,4000,0020,5510,0000	; 5134		    J/ADJTRP			; - to + change
U 1165, 1174,3500,0000,4000,0020,5510,0000	; 5135		GEN AR*AC0,AD/ANDCB,SKP AD0	;Negative. Look for + to - change
						; 5136	=0
U 1174, 0140,4001,0000,0403,0002,1006,0000	; 5137	ADJTRP:	AC0_AR,NXT INSTR		;No overflow. All done
U 1175, 0015,0001,0000,0000,0002,1110,0040	; 5138		FETCH WAIT,TRAP2,J/STAC		;Overflow. Set trap; don't lose PF
						; 5139	
						; 5140		.DCODE
D 0256, 4000,1310				; 5141	256:	R,		J/XCT		;XCT--257 is MAP
						; 5142		.UCODE
						; 5143	
						; 5144	1310:					;Must be near MAP
U 1310, 1224,0001,0000,0000,0000,7010,0000	; 5145	XCT:	SKP INTRPT			;CHECK FOR XCT . LOOP
U 1224, 1234,4001,0000,0000,0000,6210,0000	; 5146	=0	SKP USER,J/PXCT			;HERE ON XCT, NO INTERRUPT
U 1225, 0144,0001,0000,0000,0000,7710,0000	; 5147		TAKE INTRPT			;GET OUT OF LONG XCT CHAIN
						; 5148	=0
U 1234, 1235,4001,0000,0000,0000,1510,0020	; 5149	PXCT:	SET PXCT			;SETUP PXCT CONTROLS FROM 9-12
U 1235, 0152,3703,0200,0000,0000,1410,0000	; 5150	UXCT:	ARX_AR (AD),LOAD IR,#/0,J/XCTGO	;COPY INSTR TO ARX, IR
						; 5151	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; SHIFT.MIC[4,24]	16:52 3-Apr-86			Rotate and Logical Shift -- LSH, ROT			

						; 5152	.TOC	"Rotate and Logical Shift -- LSH, ROT"
						; 5153	
						; 5154		.DCODE
D 0240, 0001,1414				; 5155	240:	I,		J/ASH		;ASH [423]
D 0241, 0000,1415				; 5156		I,		J/ROT		;ROT
D 0242, 0001,1412				; 5157		I,		J/LSH		;LSH
D 0243, 0000,1413				; 5158		I,		J/JFFO		;JFFO
D 0244, 0000,1416				; 5159		I,		J/ASHC		;ASHC [423]
D 0245, 0001,1417				; 5160		I,		J/ROTC		;ROTC
D 0246, 0001,1004				; 5161		I,		J/LSHC		;LSHC--Adjoins UUO (247)
						; 5162		.UCODE
						; 5163	;
						; 5164	;	Single word Logical SHift.  If |count| > 36, we just zero AC0.
						; 5165	;
						; 5166	1412:					;[413] Must adjoin JFFO
U 1412, 1250,3240,2000,0002,0237,4413,0000	; 5167	LSH:	AR_AC0,SC_EA,SKP AR18,I FETCH	;Get count, data, shift direction
U 1250, 0014,3441,0200,0000,0000,0010,0000	; 5168	=0	ARX_0S,J/SHFLOD			;Left. Zeros come in from right
						; 5169		ARX_AR,AR_0S,SC_#+SC,#/36.,	;Right. Swap data to right, adjust
U 1251, 0014,3441,2400,2302,1020,5110,0044	; 5170		    SKP SCAD0,J/SHFLOD		; count, and shift if needed
						; 5171	;
						; 5172	;	Single word ROTate.  We must always do the shift (even if the count
						; 5173	;	is ridiculously huge), so we cannot have an I FETCH pending if we
						; 5174	;	must normalize the count.  Still, we can optimize if -37 < count
						; 5175	;	< 36. [413]
						; 5176	;
						; 5177	1415:					;Must adjoin ASH
						; 5178	ROT:	AR_AC0,ARX/AD,SC_EA,SKP AR18,	;Get data and count. Left or right
U 1415, 1254,3240,2200,0002,0237,4413,0000	; 5179		    I FETCH			; rotation?
						; 5180	=0	AR_SHIFT,SC_#+SC,#/-36.,	;Left rotation. Try to do it, and
U 1254, 1324,4001,4000,2302,0020,5110,0734	; 5181		    SKP SCAD0,J/ROTL		; test range
U 1255, 1270,0001,0000,2302,0020,5110,0044	; 5182		SC_#+SC,#/36.,SKP SCAD0		;Right rotation. In range?
U 1270, 0015,4001,4000,0000,0000,0010,0000	; 5183	=0	AR_SHIFT,J/STAC			;[417] Yes. Shift and store
						; 5184		FETCH WAIT,SC_#+SC,#/36.,	;No. Force next instruction into
U 1271, 1274,0001,0000,2302,0022,5110,0044	; 5185		    SKP SCAD0			; ARX and try to normalize count
						; 5186	=0
U 1274, 1156,3703,0210,0000,2000,0010,0000	; 5187	RTRLUP:	MQ_ARX,ARX_AR (AD),J/ROTDUN	;[417] OK. Save inst, restore data
U 1275, 1274,4001,0000,2302,0020,5110,0044	; 5188		SC_#+SC,#/36.,SKP SCAD0,J/RTRLUP;Loop until count in range
						; 5189	;
						; 5190	=0					;Left rotation
						; 5191	ROTL:	FETCH WAIT,SC_#+SC,#/-36.,	;Count too big. Wait for I FETCH
U 1324, 1334,0001,0000,2302,0022,5110,0734	; 5192		    SKP SCAD0,J/RTLLUP		; and normalize count
U 1325, 0140,4001,0000,0403,0002,1006,0000	; 5193		AC0_AR,NXT INSTR		;Count OK. Done!
						; 5194	=0
U 1334, 1334,4001,0000,2302,0020,5110,0734	; 5195	RTLLUP:	SC_#+SC,#/-36.,SKP SCAD0,J/RTLLUP;Keep normalizing until in range
U 1335, 1156,3703,0210,2302,2000,0010,0044	; 5196		MQ_ARX,ARX_AR (AD),SC_#+SC,#/36.;Keep inst, restore data and count
U 1156, 0015,4001,4300,0000,0000,0010,0000	; 5197	ROTDUN:	AR_SHIFT,ARX/MQ,J/STAC		;[417] Shift, snarf inst, and store
						; 5198	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; SHIFT.MIC[4,24]	16:52 3-Apr-86			Rotate and Logical Shift Combined (ROTC, LSHC)		

						; 5199	.TOC	"Rotate and Logical Shift Combined (ROTC, LSHC)"
						; 5200	
						; 5201	;
						; 5202	;	ROTate Combined--Normalize the count first, then do rotation.
						; 5203	;
						; 5204	1417:					;Next to ASHC
						; 5205	ROTC:	ARX_AC1,SC_EA,FE_#,#/-36.,	;Get low word
U 1417, 1354,3200,0201,0303,0020,4413,0734	; 5206		    SKP AR18			; Which way do we rotate?
						; 5207	=0	AR_AC0,MQ_AC0,FE_FE+SC,		;Left. Put high word in both low
U 1354, 0426,3240,2010,2001,0020,0734,0003	; 5208		    BYTE DISP,TIME/3T,J/ROTCL	; and high words. Test count range
						; 5209		AR_ARX,MQ_ARX,ARX_AC0,SC_#+SC,	;Right. Put low word in both places,
U 1355, 1364,3240,4210,2302,2040,5110,0044	; 5210		    #/36.,SKP SCAD0		; force count into range
						; 5211	=0
						; 5212	ROTCR:	MQ_SHIFT,AR_ARX (AD),ARX/MQ,	;Count all set. Generate left word,
U 1364, 1162,3713,2310,0000,0217,0010,0000	; 5213		    I FETCH,J/DSHIFT		; set up for right word
						; 5214		AR_ARX,MQ_ARX,ARX/MQ,SC_#+SC,	;Not yet in range. Swap shift words
U 1365, 1364,0001,4310,2302,2020,5110,0044	; 5215		    #/36.,SKP SCAD0,J/ROTCR	; and keep forcing count up
						; 5216	;
						; 5217	=110
						; 5218	ROTCL:	AR_ARX,MQ_ARX,ARX_AR (AD),SC_FE,;Long left. Swap shift words, keep
U 0426, 0426,3703,4210,5031,2020,5113,0044	; 5219		    FE_FE-#,#/36.,SKP SCAD0,J/ROTCL; adjusted count, and test again
						; 5220		MQ_SHIFT,AR_ARX (AD),ARX/MQ,	;Count now in range. Shift left
U 0427, 1162,3713,2310,0000,0217,0010,0000	; 5221		    I FETCH,J/DSHIFT		; half and set for right half
						; 5222	;
						; 5223	;	Logical SHift Combined.  Normalize the count before we do
						; 5224	;	any shifting.  This is very similar to ROTC.
						; 5225	;
						; 5226	1004:					;Must adjoin UUO (247)
U 1004, 1460,3240,0201,0002,0021,4413,0130	; 5227	LSHC:	ARX_AC1,AR+MQ_0.M,SC_EA,SKP AR18;Get low word. Left or right?
						; 5228	=0	AR_AC0,FE_#+SC,#/-36.,		;Left. Get high word; test count
U 1460, 1570,3200,2000,2301,0040,5110,0734	; 5229		    SKP SCAD0,J/CLEFT		; range
						; 5230		MQ_ARX,ARX_AC0,SC_#+SC,#/36.,	;Right. Shuffle data; try to force
U 1461, 1534,3200,0210,2302,2040,5110,0044	; 5231		    SKP SCAD0			; shift count into 0-35 range
						; 5232	=0
						; 5233	CRIGHT:	MQ_SHIFT,AR_ARX (AD),ARX/MQ,	;Count OK. Generate left word
U 1534, 1162,3713,2310,0000,0217,0010,0000	; 5234		    I FETCH,J/DSHIFT		; and set up for right word
						; 5235		MQ_ARX,ARX_AR (AD),SC_#+SC,	;Long shift. Adjust one word to
U 1535, 1534,3703,0210,2302,2020,5110,0044	; 5236		    #/36.,SKP SCAD0,J/CRIGHT	; right and loop again
						; 5237	;
						; 5238	=0
						; 5239	CLEFT:	AR_ARX,ARX_0S,SC_FE,FE_FE-#,	;Long left shift. Adjust one word
U 1570, 1570,3401,4200,5031,2020,5113,0044	; 5240		    #/36.,SKP SCAD0,J/CLEFT	; left, force count into range
						; 5241		MQ_SHIFT,AR_ARX (AD),ARX/MQ,	;Count set. Shift left word and set
U 1571, 1162,3713,2310,0000,0217,0010,0000	; 5242		    I FETCH			; for right half shift
U 1162, 0226,0001,4300,0000,0000,0010,0000	; 5243	DSHIFT:	AR_SHIFT,ARX/MQ,J/DLOAD		;Shift right word, store, and exit
						; 5244	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; SHIFT.MIC[4,24]	16:52 3-Apr-86			Arithmetic Shifts (ASH, ASHC) and JFFO			

						; 5245	.TOC	"Arithmetic Shifts (ASH, ASHC) and JFFO"
						; 5246	
						; 5247	;
						; 5248	;	[423] ASH completely rewritten for speed.
						; 5249	;
						; 5250	1414:					;[413] Must adjoin ROT
						; 5251	ASH:	AR_AC0,ARL/AD,MQ_0.M,SC_EA,	;ASH. Fetch count and data, test
U 1414, 1572,3240,2000,0002,0221,4413,0102	; 5252		    VMA/PC+1,SKP AR18		; left/right, set VMA for NICOND
						; 5253	=0	ARX_AR,AR_SIGN,SC_#+SC,#/1,	;Left. Set up to examine bits
U 1572, 1166,5441,2400,2302,1020,0016,0001	; 5254		    J/ASHL			; shifted out
						; 5255		ARX_AR,AR_SIGN,SC_#+SC,#/36.,	;Right. Adjust count and set up
U 1573, 0014,5441,2400,2302,1037,5116,0044	; 5256		    SKP SCAD0,FETCH,J/SHFLOD	; to shift in sign bits
						; 5257	;
						; 5258	ASHL:	BR/AR,MQ_SHIFT,AR_ARX (AD),	;MQ_bits shifted out, BR_sign bits
U 1166, 1574,3713,2350,2300,0020,5110,0733	; 5259		    ARX/MQ,GEN #+SC,#/-37.,SKP SCAD0; Set for shift. Is count huge?
U 1574, 1575,3102,0010,0000,0000,0710,0003	; 5260	=0	MQ_AR XOR BR			;Yes. Force overflow unless 0 data
						; 5261		ARX_SHIFT,SC_#,#/35.,		;Shift all but sign bit. Was
U 1575, 1620,3122,0400,0302,0237,5610,0043	; 5262		    SKP MQ NE BR,I FETCH	; significant data shifted out?
U 1620, 0014,3242,2000,0000,0000,0010,0000	; 5263	=0	AR_BR,J/SHFLOD			;No. Set to shift in sign and store
U 1621, 0014,3242,2000,0000,0000,1110,0420	; 5264		SET AROV,AR_BR,J/SHFLOD		;Yes. Set overflow and sign shift
						; 5265	;
						; 5266	;	Arithmetic SHift Combined.  This has been left alone due to its
						; 5267	;	low execution frequency.
						; 5268	;
						; 5269	1416:					;[415] Must adjoin ROTC
U 1416, 1624,3240,5001,0002,0020,4413,0000	; 5270	ASHC:	SC_EA,SKP AR18,AR_AC1*2		;Set up shift count, get low word
U 1624, 1634,3200,2400,2400,1040,5210,0000	; 5271	=0	ARX_AR,AR_AC0,SKP SC NE,J/ASHL1	;[423] Left shift. Check for null
						; 5272		ARX_AR,AR_AC0,SC_#+SC,#/36.,	;HERE IF RIGHT SHIFT
U 1625, 1630,3240,2400,2302,1040,5110,0044	; 5273		    SKP SCAD0			;CHECK FOR LONG ONE
						; 5274	=0
U 1630, 1171,5401,2440,0000,0020,0016,0000	; 5275	ASHR1:	BR/AR,ARX_SHIFT,AR_SIGN,J/ASHR2	;LOW OUTPUT TO ARX
						; 5276		ARX_AR,AR_SIGN,SC_#+SC,#/36.,	;HERE IF SHIFT COUNT .GT. 36
U 1631, 1630,5401,2400,2302,1020,5116,0044	; 5277		    SKP SCAD0,J/ASHR1		; Count it down to reasonable
						; 5278	
U 1171, 1222,3202,0220,0000,0000,0010,0000	; 5279	ASHR2:	BRX/ARX,ARX_BR,J/ASHX		;[423] HIGH INPUT TO ARX
						; 5280	;
						; 5281	;	[423] Here for left double arithmetic shift.
						; 5282	;
						; 5283	=0
U 1634, 0217,4001,0000,0000,0217,0010,0000	; 5284	ASHL1:	I FETCH,J/NOP			;SHIFT 0 IS A NOP
U 1635, 1176,5441,2060,0000,0020,0016,0000	; 5285		BR_AR LONG,AR_SIGN		;SAVE INPUT, GEN SIGN WORD
U 1176, 0124,3241,2640,0000,0000,0010,0000	; 5286		BR/AR,AR_BR*2 LONG		;SAVE SIGN, GET MAGNITUDE BITS
						; 5287	=0*
						; 5288	ASHL2:	BRX/ARX,ARX_AR,AR_BR,		;HI IN TO ARX, LOW TO BRX
U 0124, 2604,3242,2420,0000,1000,0050,0000	; 5289		    CALL,J/SHIFT		;CALL SHIFTER TO GET BITS LOST
U 0126, 1642,3102,0000,0000,0020,5610,0000	; 5290		SKP AR NE BR			;ANY BITS DIFFERENT FROM SIGN?
						; 5291	=0
						; 5292	ASHL3:	AR_ARX,ARX_BRX,GEN #+SC,#/-36.,	;RESTORE HI TO AR, LOW TO ARX
U 1642, 1650,3202,4600,2300,2020,5110,0734	; 5293		    SKP SCAD0,J/ASHL4
U 1643, 1642,0001,0000,0000,0000,1110,0420	; 5294		SET AROV,J/ASHL3		;BITS SHIFTED OUT NE SIGN
						; 5295	=0
						; 5296	ASHL4:	AR_ARX,ARX_0S,SC_#+SC,#/-36.,	;HERE IF E .GT. 36
U 1650, 0124,3401,4200,2302,2000,0010,0734	; 5297		    J/ASHL2			;SHIFT 36 PLACES, TRY AGAIN
						; 5298		MQ_SHIFT,AR_BRX,CLR ARX,	;HIGH OUTPUT TO MQ,
U 1651, 1203,3202,6010,2302,0000,0510,0777	; 5299		    SC_#+SC,#/-1		;COMPENSATE FOR EXTRA SHIFT
U 1203, 1216,4001,0400,0000,0000,0010,0000	; 5300		ARX_SHIFT			;LOW OUTPUT TO ARX; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3-1
; SHIFT.MIC[4,24]	16:52 3-Apr-86			Arithmetic Shifts (ASH, ASHC) and JFFO			

U 1216, 1222,3202,2320,0302,0000,0010,0043	; 5301		AR_BR,BRX/ARX,ARX/MQ,SC_#,#/35.	;SIGN TO AR, HIGH OUT TO ARX
						; 5302	ASHX:	AR_SHIFT,ARX_BRX,SC_#,#/35.,	;[423] Generate high word and
U 1222, 0011,3242,4600,0302,0000,0010,0043	; 5303		    J/ST2AC			; set up low for finish
						; 5304	;
						; 5305	;	Jump if Find First One (!)--This is implemented by first finding
						; 5306	;	the group of six bits which contains the first one, and then
						; 5307	;	isolating the actual bit.  The bit isolation has been rewritten
						; 5308	;	to speed up the slower cases (actually, the faster cases were
						; 5309	;	helped a bit, too).  [432]
						; 5310	;
						; 5311	1413:					;Must be near LSH
U 1413, 1652,3200,2000,0302,0020,5610,0006	; 5312	JFFO:	SC_#,#/6,AR_AC0,SKP AD NZ	;JFFO! Any bits set at all?
U 1652, 0217,4001,0001,0000,0217,1010,0000	; 5313	=0	AC1_AR,I FETCH,J/NOP		;AC was zero, no jump
						; 5314		ARX+MQ_0.M,FE_P,SKP SCAD NZ,	;Test first 6 bits
U 1653, 1654,0001,4000,0101,0021,5210,0144	; 5315			AR_SHIFT,ARL/SH		; and discard them
						; 5316	=0
						; 5317	JFGRUP:	AR_SHIFT,FE_P,SKP SCAD NZ,	;[432] Loop through six bit groups
U 1654, 1654,1701,4600,0101,0020,5210,0000	; 5318		    ARX_ARX-1,J/JFGRUP		; until we find a one bit
						; 5319	;
						; 5320	;	We now have the negative of the group number in ARX.  Convert this
						; 5321	;	to a bit number, and test the FE in two bit groups to find which
						; 5322	;	bit we hit on.
						; 5323	;
						; 5324		ARX_ARX*-6,GEN FE AND #,#/60,	;Convert group to bit number
U 1655, 1670,5103,0500,7030,0020,5210,0060	; 5325		    SKP SCAD NZ			; Is one in first two bits?
						; 5326	=0	BRX/ARX,ARX_2+MQ0,GEN FE AND #,	;No. Must add at least 2 to group
U 1670, 1672,4041,0520,7030,0020,5210,0014	; 5327		    #/14,SKP SCAD NZ,J/JFBITS	; count. Is one in second two bits?
						; 5328		BRX/ARX,ARX_0S,GEN FE AND #,#/40,;Yes. Set up 0 and test bit 0 in
U 1671, 1710,3441,0220,7030,0037,5210,0040	; 5329		    SKP SCAD NZ,FETCH,J/LOWBIT	; group
						; 5330	;
						; 5331	=0
						; 5332	JFBITS:	ARX_ARX*2,GEN FE AND #,#/2,	;Must be in third two bits. Set up
U 1672, 1710,3721,0500,7030,0037,5210,0002	; 5333		    SKP SCAD NZ,FETCH,J/LOWBIT	; 4 and test bit 4 in group
						; 5334		GEN FE AND #,#/10,SKP SCAD NZ,	;In second two bits; leave bit
U 1673, 1710,0001,0000,7030,0037,5210,0010	; 5335		    FETCH			; number at 2. Is bit 2 set?
						; 5336	=0
U 1710, 0136,4622,6000,0000,0000,0010,0000	; 5337	LOWBIT:	AR_ARX+BRX+1,TIME/2T,J/STAC1	;No, so low bit is. Add 1 to bit #
U 1711, 0136,0602,6000,0000,0000,0010,0000	; 5338		AR_ARX+BRX,TIME/2T,J/STAC1	;Yes. Add group+bit number in group
						; 5339	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; ARITH.MIC[4,24]	16:07 19-Mar-86			ADD, SUB						

						; 5340	.TOC	"ADD, SUB"
						; 5341	
						; 5342		.DCODE
D 0270, 5500,1014				; 5343	270:	R-PF,	AC,	J/ADD		;ADD
D 0271, 1501,1014				; 5344		I-PF,	AC,	J/ADD		;ADDI
D 0272, 7601,1014				; 5345		RPW,	M,	J/ADD		;ADDM
D 0273, 7700,1014				; 5346		RPW,	B,	J/ADD		;ADDB
						; 5347		.UCODE
						; 5348	
						; 5349	=0****00****
U 1014, 0010,0600,2000,4000,0025,1333,0000	; 5350	ADD:	AR_AR*AC0,AD/A+B,AD FLAGS,EXIT
						; 5351	=
						; 5352	
						; 5353	
						; 5354		.DCODE
D 0274, 5500,1115				; 5355	274:	R-PF,	AC,	J/SUB		;SUB
D 0275, 1501,1115				; 5356		I-PF,	AC,	J/SUB		;SUBI
D 0276, 7601,1115				; 5357		RPW,	M,	J/SUB		;SUBM
D 0277, 7700,1115				; 5358		RPW,	B,	J/SUB		;SUBB
						; 5359		.UCODE
						; 5360	
						; 5361	=0****00****
U 1115, 1226,3240,2040,0000,0020,0010,0000	; 5362	SUB:	AR_AC0,BR/AR
U 1226, 0010,5102,2004,0000,0025,1333,0000	; 5363	=	AR_AR-BR,AD FLAGS,EXIT
						; 5364	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; ARITH.MIC[4,24]	16:07 19-Mar-86			MUL, IMUL						

						; 5365	.TOC	"MUL, IMUL"
						; 5366	
						; 5367		.DCODE
D 0220, 4501,0510				; 5368	220:	R,	AC,	J/IMUL		;IMUL
D 0221, 0500,0510				; 5369		I,	AC,	J/IMUL		;IMULI [416]
D 0222, 6600,0513				; 5370		RW,	M,	J/IMULM		;IMULM [424]
D 0223, 6701,0513				; 5371		RW,	B,	J/IMULM		;IMULB [424]
						; 5372		.UCODE
						; 5373	;
						; 5374	;	In days of old when knights were bold and PDP-10s were alive and
						; 5375	;	kicking, someone decided to try to optimize IMULtIplication of a
						; 5376	;	positive number by a positive number.  That attempt failed because
						; 5377	;	the code necessary to test for overflow in the general case becomes
						; 5378	;	complex enough to cost much of the time saved by shortening the
						; 5379	;	multiplication loop.  We are now improving on the original idea a
						; 5380	;	bit by optimizing all IMUL(I)s of a positive by a positive, as long
						; 5381	;	as we can quickly guarantee that no overflow will occur.  This
						; 5382	;	requires that the high order 19 bits of the multiplier and the
						; 5383	;	high order 18 bits of the multiplicand be zero.  [416][424]
						; 5384	;
						; 5385	=0****00**00
						; 5386	IMUL:	MQ_AR,ARX_AC0,SC_#,#/18.,	;MQ_multiplier, ARX_multiplicand
U 0510, 0512,3240,0210,0303,1020,4410,0022	; 5387		    FE/SCAD,SKP AR18		; Too much multiplier?
U 0512, 1232,0001,4000,3002,0000,0610,0000	; 5388	=10	ARL_ARL,ARR_SHIFT,SC_FE-1,J/IMFAST;Maybe not. Test further, set SC
U 0513, 0463,3200,2010,3401,1020,0510,0000	; 5389	IMULM:	MQ_AR,AR_AC0,CLR ARX,FE_-1,J/IMLONG;[424] Yes, or IMULM/B. No opt
						; 5390	=
						; 5391	;
						; 5392	IMFAST:	AR_ARX,ARL/SH,ARX_0.M,FE_-1,	;Finish setup (FE to force MUL DISP
U 1232, 0462,3701,4000,3401,2021,5610,0044	; 5393		    SKP AR NZ			; to work.) Can we optimize?
						; 5394	=010	BR_AR LONG,AR_0S,FE_#,#/-8,	;Yes. Set for truncated loop and
U 0462, 0540,3401,2070,0301,0000,0070,0770	; 5395		    MUL DISP,CALL [MULP]	; start (note that SC has 17.)
						; 5396	IMLONG:	BR_AR LONG,AR_0S,FE_#,#/-17.,	;No. Do the full loop, starting
U 0463, 0540,3401,2070,0301,0000,0070,0757	; 5397		    MUL DISP,CALL [MULP]	; here
U 0466, 0216,4001,4000,0000,0217,0010,0000	; 5398		AR_SHIFT,I FETCH,J/STORAC	;[424] Short IMUL(I). Load AC
U 0467, 1712,0001,0000,0302,0040,5616,0043	; 5399		SC_#,#/35.,SKP AR SIG		;Long IMULx. Did we overflow?
						; 5400	=0
U 1712, 0016,0001,4000,0000,0005,0033,0000	; 5401	IMDONE:	AR_SHIFT,B WRITE,J/ST6		;[424] No. Just store product
U 1713, 1712,5401,2000,0000,0020,1116,0420	; 5402		SET AROV,AR_SIGN,J/IMDONE	;Yes. Set overflow, force good sign
						; 5403	
						; 5404	
						; 5405		.DCODE
D 0224, 4101,0710				; 5406	224:	R,	DBL AC,	J/MUL		;MUL
D 0225, 0100,0710				; 5407		I,	DBL AC,	J/MUL		;MULI
D 0226, 6601,0710				; 5408		RW,	M,	J/MUL		;MULM
D 0227, 6200,0710				; 5409		RW,	DBL B,	J/MUL		;MULB
						; 5410		.UCODE
						; 5411	
						; 5412	=0****00*000
						; 5413	MUL:	MQ_AR,CLR ARX,AR_AC0,		;MULTIPLIER TO MQ. Set multiplicand
U 0710, 1236,3200,2010,0301,1020,0550,0756	; 5414		    FE_#,#/-18.,CALL,J/MULSUB	; and step count. Call subroutine
						; 5415	=100	SC_#,#/35.,GEN AR*BR,AD/AND,	;[421] M'IER NEG, CHECK M'CAND
U 0714, 0716,3602,0000,0302,0020,5510,0043	; 5416		    SKP AD0			; and product
U 0716, 0010,4001,0000,0302,0005,0033,0043	; 5417	=110	SC_#,#/35.,EXIT			;STORE DOUBLE RESULT
U 0717, 0010,4001,0000,0000,0005,1133,0420	; 5418		SET AROV,EXIT			;[421] MUST HAVE SQUARED 400000,,0
						; 5419	=
						; 5420	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; ARITH.MIC[4,24]	16:07 19-Mar-86			MULTIPLY SUBROUTINE					

						; 5421	.TOC	"MULTIPLY SUBROUTINE"
						; 5422	; ENTER WITH MULTIPLIER IN MQ,
						; 5423	; MULTIPLICAND IN AR!ARX, MINUS STEP COUNT IN FE
						; 5424	; RETURNS PRODUCT IN AR!ARX!MQ.
						; 5425	; RETURN 4, 6 TELLS SIGN OF MULTIPLIER
						; 5426	; 4 AND 6 ARE USED SO CALLER CAN IGNORE
						; 5427	; DIFFERENCE BY ALIGNMENT OF CALL LOC'N
						; 5428	;[TIME=4+2(-FE)+(# OF ARITH STEPS)] ... IF FE=-18, 40-58.
						; 5429	;
						; 5430	;Recall:
						; 5431	; MUL		"FE_FE+1,DISP/MUL,MQ/MQ*.25"
						; 5432	;
						; 5433	
						; 5434	MULSUB:	BR_AR LONG,AR_0S,ARX_0S,	;M'CAND TO BR LONG, CLEAR PROD
U 1236, 0540,3441,2270,4001,0000,0030,0000	; 5435			MUL,J/MULP		;START THE MULTIPLICATION
						; 5436	=000					;GRAB AN 8-WORD BLOCK
U 0540, 0006,3701,5500,2401,0000,0703,0001	; 5437	MULP:	(AR+ARX+MQ)*2,FE_SC,RETURN6	;XADDR MACHINE HAS
U 0541, 0006,3701,5500,2401,0000,0703,0001	; 5438		(AR+ARX+MQ)*2,FE_SC,RETURN6	; NO "CRA MUL DONE"
U 0542, 0006,3701,5500,2401,0000,0703,0001	; 5439		(AR+ARX+MQ)*2,FE_SC,RETURN6
U 0543, 0006,3701,5500,2401,0000,0703,0001	; 5440		(AR+ARX+MQ)*2,FE_SC,RETURN6	;DISCARD REDUNDANT SIGN BIT
						; 5441	
U 0544, 0540,3703,7710,4001,0000,0030,0000	; 5442		AR_AR*.25 LONG,MUL,J/MULP	;M'IER BITS 00 AFTER POS STEP
						; 5443		AR_(AR+BR)*.25,ARX/ADX*.25,	;01 AFTER +
U 0545, 0540,0602,7714,4001,0020,0030,0000	; 5444			MUL,J/MULP
						; 5445		AR_(AR-2BR)*.25,ARX/ADX*.25,	;10 AFTER +
U 0546, 0560,5101,7714,4001,0020,0030,0000	; 5446			MUL,J/MULM
						; 5447		AR_(AR-BR)*.25,ARX/ADX*.25,
U 0547, 0560,5102,7714,4001,0020,0030,0000	; 5448			MUL,J/MULM		;11 AFTER +
						; 5449	
						; 5450	=000					;ANOTHER 8-WORD BLOCK FOR
U 0560, 0004,3703,5500,2401,0000,0703,0001	; 5451	MULM:	(AR+ARX+MQ)*2,FE_SC,RETURN4	; AFTER SUBTRACTION STEPS
U 0561, 0004,3703,5500,2401,0000,0703,0001	; 5452		(AR+ARX+MQ)*2,FE_SC,RETURN4
U 0562, 0004,3703,5500,2401,0000,0703,0001	; 5453		(AR+ARX+MQ)*2,FE_SC,RETURN4
U 0563, 0004,3703,5500,2401,0000,0703,0001	; 5454		(AR+ARX+MQ)*2,FE_SC,RETURN4	;M'IER WAS NEGATIVE
						; 5455	
						; 5456		AR_(AR+BR)*.25,ARX/ADX*.25,	;M'IER BITS 00 AFTER NEG STEP
U 0564, 0540,0602,7714,4001,0020,0030,0000	; 5457			MUL,J/MULP
						; 5458		AR_(AR+2BR)*.25,ARX/ADX*.25,	;01 AFTER -
U 0565, 0540,0601,7714,4001,0020,0030,0000	; 5459			MUL,J/MULP
						; 5460		AR_(AR-BR)*.25,ARX/ADX*.25,	;10 AFTER -
U 0566, 0560,5102,7714,4001,0020,0030,0000	; 5461			MUL,J/MULM
U 0567, 0560,3701,7710,4001,0000,0030,0000	; 5462		AR_AR*.25 LONG,MUL,J/MULM	;11 AFTER -
						; 5463	
						; 5464	;HERE TO CONTINUE A LONG MULTIPLICATION
						; 5465	; WITH PARTIAL PRODUCT IN AR LONG
						; 5466	
U 1241, 0540,3441,0010,4001,0000,0030,0000	; 5467	MULREE:	AD/0S,MUL,J/MULP		;DIVE IN WITHOUT CLOBBERING AR
						; 5468	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; ARITH.MIC[4,24]	16:07 19-Mar-86			DIV, IDIV						

						; 5469	.TOC	"DIV, IDIV"
						; 5470	
						; 5471		.DCODE
D 0230, 4100,1102				; 5472	230:	R,	DBL AC,	J/IDIV		;IDIV
D 0231, 0101,1102				; 5473		I,	DBL AC,	J/IDIV		;IDIVI
D 0232, 6600,1102				; 5474		RW,	M,	J/IDIV		;IDIVM
D 0233, 6201,1102				; 5475		RW,	DBL B,	J/IDIV		;IDIVB
						; 5476	
D 0234, 4101,1100				; 5477	234:	R,	DBL AC,	J/DIV		;DIV
D 0235, 0100,1100				; 5478		I,	DBL AC,	J/DIV		;DIVI
D 0236, 6601,1100				; 5479		RW,	M,	J/DIV		;DIVM
D 0237, 6200,1100				; 5480		RW,	DBL B,	J/DIV		;DIVB
						; 5481		.UCODE
						; 5482	
						; 5483	=0****00*000
						; 5484	DIV:	BR/AR,AR_AC1*2,ARL/AD*2,	;DIVISOR TO BR, LOW DIVIDEND TO AR
U 1100, 1245,3240,5041,0000,0021,0050,0145	; 5485		    ARX+MQ_0.M,CALL.M,J/DIV1	;[422] GET HIGH DIVIDEND
						; 5486	=010					;[422]
						; 5487	IDIV:	BR/AR,ARR+MQ_0.S,ARX_AC0,ARL/AD,;[421] BR_divisor; fetch dividend
U 1102, 1714,3200,0240,4402,0020,5562,0112	; 5488		    SC_1,SKP AD0,CALL [IDIVGO]	; Isolate top half. Is it < 0?
U 1106, 0010,5162,6400,0302,1025,0033,0044	; 5489	=110	ARX_AR,AR_-BRX,SC_#,#/36.,EXIT	;Remainder to ARX, negate quotient
U 1107, 0010,3242,6400,0302,1005,0033,0044	; 5490		ARX_AR,AR_BRX,SC_#,#/36.,EXIT	;HERE FOR POS QUOTIENT
						; 5491	=		
						; 5492	
						; 5493	;HERE ON DIVIDE TO SET UP DIVIDEND
						; 5494	
						; 5495	DIV1:	BRX/ARX,ARX_AR,AR_AC0,		;CLR BRX, DIVIDEND IN AR LONG
						; 5496			FE_#,#/33.,TIME/3T,	;SETUP ITERATION COUNT
U 1245, 0374,3240,2420,0301,1020,0032,0041	; 5497			SIGNS DISP,J/DIVS1	;ENTER SUBR
						; 5498	;
						; 5499	;	Start of divide subroutine for IDIVx.  We will optimize the
						; 5500	;	division by taking only 19 (instead of 36) divide steps if the
						; 5501	;	top half of the absolute value of the dividend is zero.  Enter
						; 5502	;	skipping if the dividend is positive.  This routine will set
						; 5503	;	up for the division and go to DIVS1 (or DIVS2) to begin the
						; 5504	;	actual division.  The latter take care of the subroutine return.
						; 5505	;
						; 5506	=0
						; 5507	IDIVGO:	AR_ARX,ARX/MQ,FE_#,#/33.,	;Recover positive dividend, set
U 1714, 1732,3703,4300,0301,2020,5610,0041	; 5508		    SKP AR NZ,J/IDVOPT		; long step count. Can we optimize?
						; 5509		ARX_-AC0,ARR_0.M,ARL/AD,	;Negative dividend. Start building
U 1715, 1730,5140,0200,0301,0041,5510,0012	; 5510		    FE_#,#/12,SKP AD0		; step count.  Is it max neg?
						; 5511	=0	AR_ARX,ARX/MQ,FE_#,#/33.,	;No. Set long step count and
U 1730, 1732,3703,4300,0301,2020,5610,0041	; 5512		    SKP AR NZ,J/IDVOPT		; test for optimization
						; 5513		MQ_1,TIME/2T,ARX/MQ,FE_FE+#,#/27,;Yes. Set up for positive version
U 1731, 1733,4061,0310,2031,0000,0710,0027	; 5514		    AR CTL/0,EXP TST/0,J/IDVLNG	;and long count (avoid conflict) 
						; 5515	;
						; 5516	=0
						; 5517	IDVOPT:	BRX/ARX,AR_0S,ARX_AR SWAP,FE_#,	;Can optimize. Left adjust dividend
U 1732, 0374,3401,2420,0301,3020,0032,0020	; 5518		    #/16.,SIGNS DISP,J/DIVS1	; Set short divide count and go
						; 5519	IDVLNG:	BRX/ARX,AR_MQ,ARL/AD,ARX_SHIFT,	;Dividend too big. Kill sign bit,
U 1733, 0374,3721,2420,0000,0021,0032,0102	; 5520		    MQ_0.M,SIGNS DISP,J/DIVS1	; clear MQ, set AR (FE already 33.)
						; 5521	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; ARITH.MIC[4,24]	16:07 19-Mar-86			INTEGER DIVIDE SUBROUTINE				

						; 5522	.TOC	"INTEGER DIVIDE SUBROUTINE"
						; 5523	; ENTER WITH SIGNS DISPATCH OF DIVISOR AND DIVIDEND,
						; 5524	; DIVISOR IN BR, BRX CLR; DIVIDEND IN AR!ARX
						; 5525	; STEP COUNT IN FE (# OF QUOTIENT BITS -2)
						; 5526	; If no divide, check for the maximum negative number as a
						; 5527	; quotient, and force it if it is there; otherwise, just set
						; 5528	; no divide.  Exit the instruction in either case. [420]
						; 5529	; OTHERWISE, RETURN WITH SIGNED REMAINDER IN AR,
						; 5530	; POSITIVE QUOTIENT IN BRX AND MQ.
						; 5531	; RETURN 6 IF QUOTIENT SHOULD BE NEGATIVE,
						; 5532	; RETURN 7 IF QUOTIENT SHOULD BE POSITIVE.
						; 5533	;[TIME=14+3(FE)+3(D'END NEG)+3(RESTORE REQ'D)+1(REMAINDER NEG)]
						; 5534	; ... IF FE=33, 113-120
						; 5535	;
						; 5536	;Recall:
						; 5537	; DIVIDE	"FE_FE-1,DISP/DIV,MQ/MQ*2"
						; 5538	;
						; 5539	=1100
						; 5540	DIVS1:	DIVIDE,AR_2(AR-BR),
U 0374, 0442,5102,5504,3001,0020,0031,0000	; 5541			ARX/ADX*2,J/DIVS3	;BOTH D'END AND D'SOR POS
U 0375, 0374,5143,7700,0000,0020,0027,0000	; 5542		AR_-AR LONG,J/DIVS1		;MAKE POS DIVIDEND, THEN CHK
						; 5543	DIVS2:	DIVIDE,AR_2(AR+BR),
U 0376, 0522,0602,5504,3001,0020,0031,0000	; 5544			ARX/ADX*2,J/DIVS4	;D'END POS, D'SOR NEG
U 0377, 0376,5163,7700,0000,0020,0027,0000	; 5545		AR_-AR LONG,J/DIVS2
						; 5546	
						; 5547	=0010
						; 5548	DIVS3:	DIVIDE,AR_2(AR+BR),ARX/ADX*2,
U 0442, 0660,0602,5500,3001,0020,0071,0005	; 5549			ARL/AD*2,CALL.M,J/DIVLP	;START DIVIDING
						; 5550		AR_AR*.25 LONG,FE_#,#/40,	;[420] Possible overflow, but
U 0443, 1252,3701,7700,0301,0000,0050,0040	; 5551		    CALL [MAXDIV]		; might be -2**-35 quotient
U 0446, 0006,5162,2020,0000,0020,0003,0000	; 5552		AR_-BR,BRX/ARX,RETURN6		;D'END NEG, SO NEGATE QUO & REM
U 0447, 0007,0001,0020,0000,0000,0003,0000	; 5553		BRX/ARX,RETURN7			;EVERYTHING POSITIVE
U 0457, 0476,5112,0010,0000,0020,0710,0003	; 5554	=1111	MQ_ARX-BR			;Possible legal quotient. Check rem
						; 5555	=1110	SC_#,#/36.,GEN MQ*AC0,AD/ORC,	;and make sure dividend was negative
U 0476, 1750,2120,0000,0302,0020,5550,0044	; 5556		    SKP AD0,CALL [MAXCHK]	;with remainder < divisor
U 0477, 0010,5142,0600,0000,0025,0033,0000	; 5557		ARX_-BRX,EXIT			;Looks OK. Negate remainder
						; 5558	;
						; 5559	=0010
						; 5560	DIVS4:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,
U 0522, 0660,5102,5504,3001,0020,0071,0005	; 5561			ARL/AD*2,CALL.M,J/DIVLP	;BEGIN DIVISION FOR REAL BITS
						; 5562		AR_AR*.25 LONG,FE_#,#/40,	;[420] Usually overflow, but might
U 0523, 1252,3701,7700,0301,0000,0050,0040	; 5563		    CALL [MAXDIV]		; be in range
U 0526, 0006,4001,0020,0000,0000,0003,0000	; 5564		BRX/ARX,RETURN6			;NEGATE QUO
U 0527, 0007,5142,2020,0000,0020,0003,0000	; 5565		AR_-BR,BRX/ARX,RETURN7		;NEGATE REM
U 0537, 0556,0612,0014,0000,0020,0710,0003	; 5566	=1111	MQ_ARX+BR			;Look at bottom of dividend (must
						; 5567	=1110	SC_#,#/36.,GEN MQ*AC0,AD/ORCA,	; < |divisor|); also original dividend
U 0556, 1750,2220,0000,0302,0020,5550,0044	; 5568		    SKP AD0,CALL [MAXCHK]	; must > 0
U 0557, 0010,4001,0000,0000,0005,0033,0000	; 5569		EXIT				;All conditions met. Force quotient
						; 5570	;
						; 5571	;	MAXDIV, MAXCHK--Helper subroutines to check for a quotient of
						; 5572	;	-2**35.  This copies the low dividend half to BRX, sets up the
						; 5573	;	quotient (in case we have to generate it) and tests the high
						; 5574	;	dividend half to see if the first DIVIDE step generated a zero
						; 5575	;	result.  If it did not, we have a true overflow and we exit
						; 5576	;	sideways; otherwise we return 17.  The latter portion is reused
						; 5577	;	for a subsequent test of the remainder.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5-1
; ARITH.MIC[4,24]	16:07 19-Mar-86			INTEGER DIVIDE SUBROUTINE				

						; 5578	;
						; 5579	MAXDIV:	BRX/ARX,P_FE,GEN AR*2 LONG,	;Set up quotient, save remainder
U 1252, 1750,0301,0020,0000,3041,5627,0200	; 5580		    SKP AD NZ			; Did first step generate a zero?
						; 5581	=0
U 1750, 0017,0001,0000,0000,0000,0003,0000	; 5582	MAXCHK:	RETURN17			;Yes. Look more closely
U 1751, 0217,0001,0000,0000,0217,1110,0424	; 5583		SET NO DIVIDE,I FETCH,J/NOP	;No. Must be a real overflow
						; 5584	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; ARITH.MIC[4,24]	16:07 19-Mar-86			BASIC DIVIDE LOOP					

						; 5585	.TOC	"BASIC DIVIDE LOOP"
						; 5586	; THE LOOP ITSELF IS AN INNER SUBROUTINE, TO MAKE IT SUITABLE
						; 5587	; FOR USE IN DOUBLE-LENGTH DIVISION.
						; 5588	; THE DOUBLE LENGTH REMAINDER IS RETURNED IN BR!BRX (RESTORED)
						; 5589	; THE SINGLE LENGTH QUOTIENT (LOW PART IF DBL-LEN DIVISION) IN ARX
						; 5590	; RETURN 6 IF QUOTIENT (REALLY AC0.XOR.BR) NEGATIVE, OR 7 IF POSITIVE
						; 5591	;[TIME=12+3(FE)+3(RESTORE REQ'D)] ... IF FE=33, 111-114.
						; 5592	
						; 5593	=000
U 0660, 0660,0602,5504,3001,0020,0031,0000	; 5594	DIVLP:	DIVIDE,AR_2(AR+BR),ARX/ADX*2,J/DIVLP
U 0661, 0660,5102,5500,3001,0020,0031,0000	; 5595		DIVIDE,AR_2(AR-BR),ARX/ADX*2,J/DIVLP
U 0662, 0660,5102,5500,3001,0020,0031,0000	; 5596	DIV-:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,J/DIVLP
U 0663, 0660,0602,5504,3001,0020,0031,0000	; 5597	DIV+:	DIVIDE,AR_2(AR+BR),ARX/ADX*2,J/DIVLP
U 0664, 0724,0602,2604,3001,0020,0031,0000	; 5598		DIVIDE,AR_AR+BR,ARX/ADX,J/DIVX
U 0665, 0724,5102,2600,3001,0020,0031,0000	; 5599		DIVIDE,AR_AR-BR,ARX/ADX,J/DIVX
U 0666, 0724,5102,2600,3001,0020,0031,0000	; 5600		DIVIDE,AR_AR-BR,ARX/ADX,J/DIVX		;NO SHIFT ON FINAL STEP
U 0667, 0724,0602,2604,3001,0020,0031,0000	; 5601		DIVIDE,AR_AR+BR,ARX/ADX,J/DIVX
						; 5602	
						; 5603	;HERE AFTER FINAL DIVIDE STEP
						; 5604	; MQ HAS POSITIVE FORM QUOTIENT
						; 5605	; AR!ARX HAS REMAINDER, EXCEPT THAT IT MUST BE RESTORED IF IT IS
						; 5606	; NEGATIVE (IT'S NEGATIVE IF THERE WAS NO CARRY ON FINAL STEP)
						; 5607	; THE ORIGINAL DIVIDEND IS STILL IN AC0, SO WE CHECK ITS SIGN
						; 5608	; TO DETERMINE WHETHER TO NEGATE THE (RESTORED) REMAINDER.
						; 5609	
						; 5610	=100
U 0724, 0725,0602,2600,0000,0020,0027,0000	; 5611	DIVX:	AR_AR+BR LONG			;RESTORE REMAIN WITH POS D'SOR
						; 5612		BR_AR LONG,ARX/MQ,FE_SC,	;LONG REMAIN TO BR, QUO TO ARX
U 0725, 0006,2500,0360,2401,0020,5503,0000	; 5613			SKP AC0+,RETURN6	;RETURN TESTING D'END SIGN
U 0726, 0727,5102,2600,0000,0020,0027,0000	; 5614		AR_AR-BR LONG			;RESTORE REMAIN WITH NEG D'SOR
						; 5615		BR_AR LONG,ARX/MQ,FE_SC,
U 0727, 0006,3200,0360,2401,0020,5503,0000	; 5616			SKP AC0-,RETURN6
						; 5617	
						; 5618	
						; 5619	;SUBROUTINE FOR FIRST PART OF LONG DIVISIONS
						; 5620	; ENTER AT DDVSUB WITH SKP BR0
						; 5621	; RETURN3 IF SHOULD RESUME WITH ADD STEP
						; 5622	; RETURN5 IF SHOULD RESUME WITH SUBTRACT
						; 5623	
						; 5624	=000
U 0760, 0760,0602,5500,3001,0020,0031,0000	; 5625	DDVLP:	AR_2(AR+BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0761, 0760,5102,5504,3001,0020,0031,0000	; 5626		AR_2(AR-BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0762, 0760,5102,5504,3001,0020,0031,0000	; 5627	DDVSUB:	AR_2(AR-BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0763, 0760,0602,5500,3001,0020,0031,0000	; 5628		AR_2(AR+BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0764, 0003,3723,2010,0301,1000,0003,0040	; 5629		AR_MQ,MQ_AR,FE_#,#/32.,RETURN3
U 0765, 0005,3723,2010,0301,1000,0003,0040	; 5630		AR_MQ,MQ_AR,FE_#,#/32.,RETURN5
U 0766, 0005,3723,2010,0301,1000,0003,0040	; 5631		AR_MQ,MQ_AR,FE_#,#/32.,RETURN5
U 0767, 0003,3723,2010,0301,1000,0003,0040	; 5632		AR_MQ,MQ_AR,FE_#,#/32.,RETURN3
						; 5633	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; ARITH.MIC[4,24]	16:07 19-Mar-86			DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV	

						; 5634	.TOC	"DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV"
						; 5635	
						; 5636		.DCODE
D 0114, 4001,1305				; 5637	114:	R,	B/0,	J/DASMD		;DADD
D 0115, 4200,1305				; 5638		R,	B/2,	J/DASMD		;DSUB
D 0116, 4400,1305				; 5639		R,	B/4,	J/DASMD		;DMUL
D 0117, 4000,1304				; 5640		R,		J/DDIV		;DDIV
						; 5641		.UCODE
						; 5642	
						; 5643	;HERE FOR DOUBLE WORD ADD, SUBTRACT, MULTIPLY, OR DIVIDE
						; 5644	;ENTER WITH (E) IN AR, E IN VMA
						; 5645	
						; 5646	=0****00**00
U 1304, 1332,3200,0205,0000,0020,0710,0003	; 5647	DDIV:	ARX_AC3,CLR MQ,J/DDIV0		;GET LOWEST PART OF D'END
						; 5648	
						; 5649	DASMD:	BR/AR,AR_AC1*2,ARL/AD*2,	;HIGH MEM WORD TO BR
						; 5650			VMA_VMA+1,LOAD ARX,	;ASK FOR LOW WORD
U 1305, 1045,3200,5041,0000,0033,3662,0105	; 5651			MQ_0.S,CALL.S,J/XFERW	;AND WAIT FOR IT
U 1307, 1256,3701,0500,0000,0000,0010,0000	; 5652	=11	ARX_ARX*2			;SHIFT LOW MEM WORD LEFT
						; 5653	=	BRX/ARX,ARX_AR,AR_AC0,		;ALL DATA IN PLACE
U 1256, 0450,3200,2420,0302,1020,0033,0043	; 5654			SC_#,#/35.,B DISP	;DO THE OPERATION
						; 5655	
						; 5656	;HERE WITH (E) IN BR, (E+1)*2 IN BRX
						; 5657	; (AC) IN AR, (AC+1)*2 IN ARX
						; 5658	
U 0450, 0011,0602,2604,0000,0020,1327,0000	; 5659	=00*	AR_AR+BR LONG,AD FLAGS,J/ST2AC	;[430] DADD
						; 5660	
U 0452, 0011,5102,2600,0000,0020,1327,0000	; 5661		AR_AR-BR LONG,AD FLAGS,J/ST2AC	;[430] DSUB
						; 5662	
						; 5663		MQ_SHIFT,AR_0S,ARX_0S,		;DMUL, USE AC1 AS INITIAL M'IER
U 0454, 0530,3441,2210,0301,0000,0010,0756	; 5664			FE_#,#/-18.,J/DMULT	;SETUP STEP COUNT
						; 5665	=
						; 5666	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; ARITH.MIC[4,24]	16:07 19-Mar-86			DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV	

						; 5667	;HERE FOR DOUBLE WORD MULTIPLY
						; 5668	
						; 5669	=00*
U 0530, 0540,3401,0010,4001,0000,0070,0000	; 5670	DMULT:	AD/0S,MUL,CALL.M,J/MULP		;BEGIN MULTIPLY
U 0534, 0536,0602,2600,0000,0020,0027,0000	; 5671	=10*	AR_AR+BR LONG			;CANCEL EFFECTS OF LOW BIT 0
U 0536, 1262,3723,2010,0000,1000,0010,0000	; 5672		MQ_AR,AR_MQ			;EXCH HI AND LOW PRODUCT WORDS
						; 5673	
						; 5674	;HERE AFTER 1ST CALL ON MPY SUBR.  SAVE LOW WORD OF PROD, GET HIGH M'IER
						; 5675	
U 1262, 1264,4001,0005,0000,0000,1010,0000	; 5676		AC3_AR				;LOW WORD OF PRODUCT
U 1264, 1240,3200,2000,0000,0020,0010,0000	; 5677		AR_AC0				;GET HIGH M'IER WORD
						; 5678	=000	MQ_AR,AR_MQ,CALL,		;DIVE IN AGAIN
U 1240, 1241,3723,2010,0301,1000,0050,0756	; 5679			FE_#,#/-18.,J/MULREE	;CONTINUE THE MULTIPLY
U 1244, 1246,3602,0004,0000,0020,5510,0000	; 5680	=100	GEN AR*BR,AD/AND,SKP AD0	;SKP IF M'IER, M'CAND, & PROD NEG
						; 5681	=110
						; 5682	DMUL1:	AC0_AR,AR_SIGN,
U 1246, 1272,5441,2000,0302,0020,1016,0043	; 5683			SC_#,#/35.,J/DMUL2	;STORE HIGH WORD OF PRODUCT
U 1247, 1246,0001,0000,0000,0000,1110,0420	; 5684		SET AROV,J/DMUL1
						; 5685	
						; 5686	;MULTIPLY NOW COMPLETE, STORE RESULTS WITH PROPER SIGN IN BIT 0
						; 5687	
U 1272, 1276,0001,4040,0000,0000,0010,0000	; 5688	DMUL2:	BR/AR,AR_SHIFT			;GET 2ND WITH SIGN, SAVE SIGN
U 1276, 1306,0001,4301,0000,2000,1010,0000	; 5689		AC1_AR,AR_ARX,ARX/MQ		;READY TO BUILD 3RD WORD
U 1306, 1322,3202,2400,0000,0000,0012,0000	; 5690		ARX_SHIFT,AR_BR,MQ_MQ*2		;SIGNIFICANT BITS TO ARX, SIGN TO AR
						; 5691		AR_SHIFT,ARX_AC3,		;3RD WORD IN AR, GET LOW
U 1322, 1326,3200,4215,0000,0020,0012,0000	; 5692			MQ_MQ*.25		;EXTRA PROD BIT TO MQ 35
U 1326, 0470,3721,2004,0000,0000,1010,0000	; 5693		AC2_AR,AR_MQ			;,I FETCH WHEN TIMING FIXED
						; 5694	=0*	ARX_SHIFT,AR_BR,I FETCH,	;LOW WORD AND SIGN READY
U 0470, 2604,3202,2400,0000,0217,0050,0000	; 5695			CALL,J/SHIFT		; GET LOW WORD TO AR
U 0472, 0133,4001,0005,0000,0000,1010,0000	; 5696	STRAC3:	AC3_AR,FINISH			;GANZ GETAN
						; 5697	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; ARITH.MIC[4,24]	16:07 19-Mar-86			DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV	

						; 5698	;HERE FOR DOUBLE INTEGER DIVISION
						; 5699	;AR HAS (E), ARX HAS (AC3), AND MQ IS CLEAR
						; 5700	
U 1332, 1336,3203,4507,4402,2000,1010,0166	; 5701	DDIV0:	T0_AR,AR_ARX,ARX_ARX*8,SC_1	;SAVE (E) IN T0
						; 5702		BRX/ARX,ARX_SHIFT,		;AC3 3-35 TO BRX, 1-2 TO ARX
U 1336, 1345,3200,2424,0302,0020,0010,0002	; 5703			AR_AC2,SC_#,#/2		;GET AC2 READY
						; 5704		AR_SHIFT,BR/AR,			;AC2 BITS 2-35 WITH AC3 1-2
U 1345, 1361,3200,4241,0000,0020,3610,0000	; 5705			ARX_AC1,VMA_VMA+1	;READY TO GET (E+1)
						; 5706		BR/AR,AR_ARX,ARX_BR*2,		;LOW DOUBLE WORD NOW IN BR LONG
U 1361, 1370,3201,4240,4403,2000,0010,0000	; 5707			SC_1,FE_1
U 1370, 1752,3240,2400,0000,0020,5510,0000	; 5708		ARX_SHIFT,AR_AC0,SKP AD0	;HIGH DOUBLEWORD IN AR LONG
						; 5709	=0
						; 5710	DDIV1:	BR_AR LONG,AR_BRX,ARX_BR,	;HI POS D'END TO BR
U 1752, 0774,3202,6260,0000,0012,0010,0000	; 5711			LOAD AR,J/DDIV2		;GET LOW D'SOR READY
						; 5712		BR_AR LONG,AR_-BR LONG,		;NEGATE LOW D'END
U 1753, 1754,5142,2660,3401,0040,5427,0000	; 5713			FE_-1,SKP CRY0		;TEST FOR CARRY PROPAGATION
U 1754, 1752,2502,2660,0000,0000,0010,0000	; 5714	=0	BR_AR LONG,AR_BR COMP LONG,J/DDIV1
U 1755, 1752,5162,2660,0000,0020,0027,0000	; 5715		BR_AR LONG,AR_-BR LONG,J/DDIV1	;FINISH NEGATION OF D'END
						; 5716	=0*
						; 5717	DDIV2:	T1_AR,MQ_ARX,ARX_0S,		;LOWEST D'END TO T1, NEXT TO MQ
U 0774, 1045,3441,0217,0000,2000,1050,0171	; 5718			CALL,J/XFERW		; WAIT FOR (E+1)
U 0776, 1760,3240,2407,0000,0040,5110,0166	; 5719		ARX_SHIFT,AR_T0,SKP FE0		;DIVISOR NOW IN AR LONG
						; 5720	=0	AR_BR LONG,BR_AR LONG,		;PUT OPERANDS IN PLACE FOR DIV
U 1760, 0573,3242,2660,0000,0020,0032,0000	; 5721			SIGNS DISP,J/DDIV3	;TEST D'SOR SIGN
						; 5722		AR_BR LONG,BR_AR LONG,SET SR2,	;NOTE D'END NEGATIVE
U 1761, 0573,3242,2660,0000,0020,1632,0062	; 5723			SIGNS DISP,J/DDIV3
						; 5724	
						; 5725	;HERE WITH THE DIVISOR IN BR LONG,
						; 5726	; THE HIGH PART OF THE MAGNITUDE OF THE DIVIDEND IN AR LONG,
						; 5727	; AND THE LOW PART OF THE MAGNITUDE OF THE DIVIDEND IN MQ AND T1
						; 5728	; SKIP IF DIVISOR NEGATIVE, & CHECK FOR NO-DIVIDE.
						; 5729	=1011
						; 5730	DDIV3:	AR_2(AR-BR),ARX/ADX*2,MQ_MQ*2,	;SEE IF FIRST DIVIDE STEP
U 0573, 1260,5102,5504,0000,0040,5412,0000	; 5731			SKP CRY0,J/DDIV4	; GENERATES A 1
U 0577, 1260,0602,5500,0000,0040,5412,0000	; 5732		AR_2(AR+BR),ARX/ADX*2,MQ_MQ*2,SKP CRY0
						; 5733	=000
U 1260, 0760,0001,0000,0301,0000,4250,0041	; 5734	DDIV4:	FE_#,#/33.,SKP BR0,CALL,J/DDVLP	;GO DO FIRST HALF OF DIVIDE
U 1261, 0217,0001,0000,0000,0217,1110,0424	; 5735		SET NO DIVIDE,I FETCH,J/NOP	;[422] TOO MANY QUOTIENT BITS
U 1263, 1401,0001,0001,0402,0000,1010,0000	; 5736	=011	AC1_AR,CLR SC,J/DDIV6		;SAVE HI QUOTIENT IN AC1
U 1265, 1401,0001,0001,3402,0000,1010,0000	; 5737	=101	AC1_AR,SC_1S			;SET FLAG FOR RESUMPTION
						; 5738	=
U 1401, 1344,3200,2007,0000,0020,0010,0171	; 5739	DDIV6:	AR_T1				;GET LOWEST DIVIDEND BITS
						; 5740	=100	MQ_AR,AR_MQ,CALL,		;FINISH DIVISION, GENERATING
U 1344, 0660,3723,2010,0000,1000,4750,0000	; 5741			SKP SC0,J/DIVLP		; 35 MORE QUOTIENT BITS
U 1346, 0635,3240,2001,0000,0020,1605,0061	; 5742	=110	AR_AC1,SR DISP,SET SR3,J/DDVX1	;QUOTIENT NEGATIVE.  NOTE
U 1347, 0635,3240,2001,0000,0020,0005,0000	; 5743		AR_AC1,SR DISP			;HERE'S HIGH PART OF QUOTIENT
						; 5744	=1101
U 0635, 1402,3202,2660,0000,0000,0010,0000	; 5745	DDVX1:	BR_AR LONG,AR_BR LONG,J/DDVX2	;POS REMAINDER.  GO STORE
U 0637, 1402,5162,2660,0000,0020,0027,0000	; 5746		BR_AR LONG,AR_-BR LONG,J/DDVX2	;NEGATE REMAINDER
U 1402, 1405,5401,2004,0302,0020,1016,0043	; 5747	DDVX2:	AC2_AR,AR_SIGN,SC_#,#/35.
U 1405, 0656,4001,4000,0000,0000,0005,0000	; 5748		AR_SHIFT,SR DISP		;GET LOW WORD OF REM. TEST QUO SIGN
U 0656, 0011,3202,2505,0000,0000,1010,0000	; 5749	=1110	AC3_AR,AR_BR,ARX/ADX*2,J/ST2AC	;[430] GET QUO, SQUEEZE OUT HOLE
						; 5750		AC3_AR,AR_-BR,ARX/ADX*2,AD LONG,;GET NEGATIVE QUOTIENT
U 0657, 0011,5162,2505,0000,0020,1027,0000	; 5751		    J/ST2AC			;[430]
						; 5752	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR	

						; 5753	.TOC	"SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR"
						; 5754	
						; 5755		.DCODE
						; 5756	
D 0140, 4101,1001				; 5757	140:	R,	FL-AC,	B0/0,	J/FAD	;FAD
						;;5758	.IF/FPLONG
						;;5759		R,		B0/0,	J/FADL	;FADL [414]
						; 5760	.IFNOT/FPLONG
D 0141, 2000,1002				; 5761		EA,			J/UUO	;(FADL) [414]
						; 5762	.ENDIF/FPLONG
D 0142, 6200,1001				; 5763		RW,	FL-MEM,	B0/0,	J/FAD	;FADM
D 0143, 6301,1001				; 5764		RW,	FL-BOTH,B0/0,	J/FAD	;FADB
						; 5765	
D 0144, 4101,1313				; 5766		R,	FL-AC,	B0/0,	J/FADR	;FADR [414]
D 0145, 0101,1312				; 5767		I,	FL-AC,	B0/0,	J/FADRI	;FADRI
D 0146, 6200,1313				; 5768		RW,	FL-MEM,	B0/0,	J/FADR	;FADRM [414]
D 0147, 6301,1313				; 5769		RW,	FL-BOTH,B0/0,	J/FADR	;FADRB [414]
						; 5770	
D 0150, 4500,1001				; 5771	150:	R,	FL-AC,	B0/1,	J/FSB	;FSB
						;;5772	.IF/FPLONG
						;;5773		R,		B0/1,	J/FSBL	;FSBL [414]
						; 5774	.IFNOT/FPLONG
D 0151, 2000,1002				; 5775		EA,			J/UUO	;(FSBL) [414]
						; 5776	.ENDIF/FPLONG
D 0152, 6601,1001				; 5777		RW,	FL-MEM,	B0/1,	J/FSB	;FSBM
D 0153, 6700,1001				; 5778		RW,	FL-BOTH,B0/1,	J/FSB	;FSBB
						; 5779	
D 0154, 4500,1313				; 5780		R,	FL-AC,	B0/1,	J/FSBR	;FSBR [414]
D 0155, 0500,1312				; 5781		I,	FL-AC,	B0/1,	J/FSBRI	;FSBRI
D 0156, 6601,1313				; 5782		RW,	FL-MEM,	B0/1,	J/FSBR	;FSBRM [414]
D 0157, 6700,1313				; 5783		RW,	FL-BOTH,B0/1,	J/FSBR	;FSBRB [414]
						; 5784		.UCODE
						; 5785	
						; 5786	.IFNOT/FPLONG
						; 5787	1001:					;[414] Must be near UUO
						; 5788	FAD:
U 1001, 1313,4001,0000,0000,0000,1610,0001	; 5789	FSB:	SR_#,#/1,J/FADR			;[414] FLAG NO ROUND, GO FAD/FSB
						;;5790	.IF/FPLONG
						;;5791	=0****00***0
						;;5792	FAD:
						;;5793	FSB:	SR_#,#/1,J/FADR			;[414] FLAG TRUNCATE MODE, GO FAD
						;;5794	FADL:
						;;5795	FSBL:	SR_#,#/2,J/FADR			;[414] FLAG LONG MODE
						;;5796	=
						; 5797	.ENDIF/FPLONG
						; 5798	=0****00***0
						; 5799	FADRI:
U 1312, 1313,0001,4000,0000,3000,0010,0000	; 5800	FSBRI:	AR_AR SWAP			;Orient immediate operand
						; 5801	FADR:
						; 5802	FSBR:	FE_EXP,EXP_SIGN,SC/SCAD,ARX_0S,	;Grab exponent, test for subtract
U 1313, 0643,3441,0200,0203,1001,0033,0200	; 5803		    B DISP			;[414]
						; 5804	=
						; 5805	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR	

						; 5806	; FIND OPERAND WITH LARGER EXP, LEAVING IT IN BR,
						; 5807	; AND ITS EXP-1 IN FE.  THE SMALLER OPERAND IS LEFT IN AR,
						; 5808	; SHIFTED RIGHT BY THE DIFFERENCE BETWEEN THE EXPONENTS -1
						; 5809	
						; 5810	=011					;[414]
U 0643, 1406,3240,2060,0000,0020,0010,0000	; 5811	FAS:	BR/AR,BRX/ARX,AR_AC0,J/FADFSB	;[414] SAVE MEM OP IN BR, GET AC
U 0647, 0643,5163,7000,0000,0020,0010,0000	; 5812		AR_-AR,J/FAS			;NEGATE SUBTRAHEND
						; 5813	;
U 1406, 1762,4001,0000,5202,1021,5110,0200	; 5814	FADFSB:	SC_EXP-SC,EXP_SIGN,SKP SCAD0	;[414] FIND LARGER OPERAND
U 1762, 1430,3201,2040,2001,0000,0010,0000	; 5815	=0	FE_FE+SC,BR/AR,AR_BR*2,J/FAS1	;AC EXP .GE. MEM
						; 5816		MQ_AR,SC_#+SC,#/37.,		;MEM OP LARGER, SHIFT AC OP
U 1763, 1764,4001,0010,2302,1020,5110,0045	; 5817			SKP SCAD0,J/FAS2	;COMPUTE SHIFT AMOUNT
						; 5818	
U 1430, 1764,4001,0010,5302,1020,5110,0044	; 5819	FAS1:	MQ_AR,SC_#-SC,#/36.,SKP SCAD0	;CHECK SHIFT AMOUNT
						; 5820	=0
U 1764, 1442,5441,2310,0000,0020,0016,0000	; 5821	FAS2:	MQ_SHIFT,ARX/MQ,AR_SIGN,J/FAS3	;LOW TO MQ, READY TO GET HI
						; 5822		AR_SIGN,ARX_AR,			;HERE IF EXP DIFF .GT. 36
U 1765, 1770,5401,2400,2302,1020,5116,0044	; 5823			SC_#+SC,#/36.,SKP SCAD0	; .GT. 72?
U 1770, 1451,4001,0400,4001,0001,0010,0100	; 5824	=0	ARX_SHIFT,MQ_0.M,FE_FE+1,J/FAS5
U 1771, 1451,0001,0400,4001,1001,0010,0100	; 5825		ARX_AR,MQ_0.M,FE_FE+1,J/FAS5	;SHIFTED CLEAR OUT
						; 5826	
						; 5827	FAS3:	AR_SHIFT,ARL/SH,ARX/MQ,
U 1442, 1451,0001,4300,4001,0001,0010,0104	; 5828			MQ_0.M,FE_FE+1		;READY TO ADD
						; 5829	
						; 5830	FAS5:	AR_(AR+2BR)*.25,ARX/ADX*.25,	;HERE FOR ADD OR SUB
U 1451, 1420,0601,7704,0000,0060,0035,0000	; 5831			NORM,J/SNORM
						; 5832	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE FLOATING MULTIPLY -- FMP, FMPR			

						; 5833	.TOC	"SINGLE FLOATING MULTIPLY -- FMP, FMPR"
						; 5834	
						; 5835		.DCODE
D 0160, 4501,1003				; 5836	160:	R,	FL-AC,	B0/1,	J/FMP	;FMP [414]
						;;5837	.IF/FPLONG
						;;5838		R,			J/FMPL	;FMPL [414]
						; 5839	.IFNOT/FPLONG
D 0161, 2000,1002				; 5840		EA,			J/UUO	;(FMPL) [414]
						; 5841	.ENDIF/FPLONG
D 0162, 6600,1003				; 5842		RW,	FL-MEM,	B0/1,	J/FMP	;FMPM [414]
D 0163, 6701,1003				; 5843		RW,	FL-BOTH,B0/1,	J/FMP	;FMPB [414]
						; 5844	
D 0164, 4101,0407				; 5845		R,	FL-AC,		J/FMPR	;FMPR
D 0165, 0501,0402				; 5846		I,	FL-AC,	B0/1,	J/FMPRI	;FMPRI [414]
D 0166, 6200,0407				; 5847		RW,	FL-MEM,		J/FMPR	;FMPRM
D 0167, 6301,0407				; 5848		RW,	FL-BOTH,	J/FMPR	;FMPRB
						; 5849		.UCODE
						; 5850	
						;;5851	.IF/FPLONG
						;;5852	=0****00**00				;[414]
						;;5853	FDV:					;[414] FMPx and FDVx start here
						;;5854	FMP:	SR_#,#/1,B DISP,J/FDVR		;[414] FLAG TRUNCATE MODE and split
						;;5855	FMPL:	SR_#,#/2,J/FMPR			;LONG MODE
						;;5856	FDVL:	FE_EXP-1,EXP_SIGN,ARX+MQ_0.M,J/FDVL1;[414]
						;;5857	=
						; 5858	.IFNOT/FPLONG				;[414]
						; 5859	1003:					;[414] Must adjoin UUO
						; 5860	FDV:					;[414]
U 1003, 0403,4001,0000,0000,0000,1633,0001	; 5861	FMP:	SR_#,#/1,B DISP,J/FDVR		;[414] Flag for truncate mode
						; 5862	.ENDIF/FPLONG
						; 5863	=0****00*010
						; 5864	FDVRI:					;[414]
U 0402, 0403,0001,4000,0000,3000,0033,0000	; 5865	FMPRI:	AR_AR SWAP,B DISP		;[414] Orient op. FMP or FDV?
						; 5866	FDVR:	SC_EXP+1,EXP_SIGN,ARX+MQ_0.M,	;[414] FDV. SETUP DIVISOR
U 0403, 1360,0001,0000,4202,1001,0010,0340	; 5867		    J/FDVGO
						; 5868	;
						; 5869	=111
U 0407, 1464,3441,0200,0202,1001,0010,0200	; 5870	FMPR:	SC_EXP,EXP_SIGN,ARX_0S		;FMP. PREPARE M'IER FRACTION
U 1464, 0572,3200,2010,0301,1020,0010,0762	; 5871	=	MQ_AR,AR_AC0,FE_#,#/-14.	;M'IER TO MQ, GET M'CAND
						; 5872	=01*	SC_EXP+SC,EXP_SIGN,		;SEPARATE M'CAND FRACTION FROM EXP
U 0572, 1236,0001,0000,2202,1001,0050,0200	; 5873			CALL.M,J/MULSUB		;[414] AND BEGIN MULTIPLY
U 0576, 1420,3203,0000,2301,0040,0035,0600	; 5874	=11*	FE_#+SC,#/-200,NORM AR,J/SNORM
						; 5875	=
						; 5876	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE FLOATING DIVIDE -- FDV, FDVR			

						; 5877	.TOC	"SINGLE FLOATING DIVIDE -- FDV, FDVR"
						; 5878	
						; 5879		.DCODE
D 0170, 4100,1003				; 5880	170:	R,	FL-AC,	B0/0,	J/FDV	;FDV [414]
						;;5881	.IF/FPLONG
						;;5882		R,	FL-AC,		J/FDVL	;FDVL [414]
						; 5883	.IFNOT/FPLONG
D 0171, 2000,1002				; 5884		EA,			J/UUO	;(FDVL) [414]
						; 5885	.ENDIF/FPLONG
D 0172, 6201,1003				; 5886		RW,	FL-MEM,	B0/0,	J/FDV	;FDVM [414]
D 0173, 6300,1003				; 5887		RW,	FL-BOTH,B0/0,	J/FDV	;FDVB [414]
						; 5888	
D 0174, 4100,0403				; 5889		R,	FL-AC,		J/FDVR	;FDVR
D 0175, 0100,0402				; 5890		I,	FL-AC,	B0/0,	J/FDVRI	;FDVRI [414]
D 0176, 6201,0403				; 5891		RW,	FL-MEM,		J/FDVR	;FDVRM
D 0177, 6300,0403				; 5892		RW,	FL-BOTH,	J/FDVR	;FDVRB
						; 5893		.UCODE
						; 5894	
						; 5895	;FDV:	SR_#,#/1,J/FDVR			;This stuff is with FMP
						; 5896	;
						; 5897	;FDVRI:	AR_AR SWAP			;[414] Orient op
						; 5898	;FDVR:	SC_EXP+1,EXP_SIGN,ARX+MQ_0.M	;[414] SETUP DIVISOR
						; 5899	
						; 5900	=000
						; 5901	FDVGO:	BR/AR,BRX/ARX,AR_AC0,FE_#,#/27.,;[414] DIVISOR TO BR, CLR BRX
U 1360, 1774,3240,2060,0301,0020,5550,0033	; 5902		    SKP AD0,CALL,J/FDVCHK	;GET DIVIDEND, STEP COUNT
U 1362, 0662,4001,0000,0000,0000,4250,0000	; 5903	=10	SKP BR0,CALL,J/DIV-		;OK, BEGIN DIVISION
U 1363, 0016,0001,0000,0000,0000,1110,0624	; 5904		SET FL NO DIV,J/IFNOP		;NO DIVIDE, SORRY
						; 5905	
						; 5906	;RETURN HERE WITH QUOTIENT IN ARX.  WE TOOK 29 DIVIDE STEPS, TO
						; 5907	; GUARANTEE HAVING A ROUNDING BIT EVEN IF THE FIRST STEP GENERATES
						; 5908	; A QUOTIENT BIT OF ZERO.  THEREFORE, THE MSB OF QUOTIENT IS EITHER
						; 5909	; IN BIT 7 OR 8, AND NORM WILL FIND IT IN ONE STEP.
						; 5910	
						; 5911	=110	AR_ARX,FE_FE+#,#/2,		;NEGATIVE QUOTIENT
U 1366, 1772,7162,4000,2031,2040,5410,0002	; 5912			SKP BR EQ,J/FDVNEG	;CHECK FOR MORE QUO TO COME
						; 5913		AR_ARX*.25,ARX_ARX*.25,NORM,	;JUNK IS 36 BITS AWAY FROM MSB
U 1367, 1420,3713,7700,2031,0040,0035,0002	; 5914			FE_FE+#,#/2,J/SNORM	;POS QUOTIENT, NORMALIZE
						; 5915	=
						; 5916	;HERE IF QUOTIENT SHOULD BE NEGATIVE, WITH POSITIVE FORM IN
						; 5917	; AR AND ARX.  SKIP IF REMAINDER (IN BR) IS ZERO.  IN THIS CASE,
						; 5918	; WE CLEAR ARX, BECAUSE AR CONTAINS THE ENTIRE QUOTIENT.
						; 5919	; IF, HOWEVER, THE REMAINDER IS NOT ZERO, WE INFER
						; 5920	; THAT AN INFINITE PRECISION DIVISION WOULD GENERATE MORE ONES
						; 5921	; IN THE QUOTIENT.  IF THAT IS THE CASE, WE LEAVE ARX WITH THE
						; 5922	; QUOTIENT, SO THE NEGATION PROCESS WILL WORK CORRECTLY TO RETURN
						; 5923	; THE HIGH ORDER PART OF THE INFINITE-PRECISION NEGATIVE QUOTIENT.
						; 5924	=0
U 1772, 1420,3703,7700,0000,0040,1635,0064	; 5925	FDVNEG:	SET SR1,AR_AR*.25 LONG,NORM,J/SNORM
U 1773, 1772,3441,0200,0000,0000,0010,0000	; 5926		ARX_0S,J/FDVNEG			;REMAINDER WENT TO ZERO
						; 5927	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE FLOATING DIVIDE -- FDV, FDVR			

						; 5928	;HERE FOR FDVL
						; 5929	
						;;5930	.IF/FPLONG
						;;5931	
						;;5932	;FDVL:	FE_EXP-1,EXP_SIGN,ARX+MQ_0.M
						;;5933	=000
						;;5934	FDVL1:	AR_AC1,BR_AR LONG,		;SAVE DIVISOR IN BR LONG
						;;5935			SC_#,#/9.,CALL		;READY TO SHIFT LOW DIVIDEND
						;;5936		ARX_SHIFT,AR_AC0,		;DIVIDEND IN PLACE
						;;5937			SC_FE,FE_#,#/24.,	;EXP TO SC, STEP COUNT TO FE
						;;5938			SKP AD0,J/FDVCHK	;GO CHECK FOR NO DIVIDE
						;;5939	=010	CALL,SKP BR0,J/FDVL2		;GO BEGIN DIVIDE
						;;5940		SET FL NO DIV,J/IFNOP		;CAN'T DIVIDE, ABORT
						;;5941	
						;;5942	=110	AR_AC0,SR_#,#/5,		;NEG QUO, FLAG TRUNCATE MODE
						;;5943			SR DISP,J/FDVL4		; WAS IT 26 OR 27 STEPS?
						;;5944		AR_AC0,SR_#,#/1,		;POS QUO
						;;5945			SR DISP,J/FDVL4
						;;5946	=
						;;5947	
						;;5948	
						;;5949	;COME HERE TO START THE DIVISION.  ON THE FIRST STEP, WE CHECK
						;;5950	; TO SEE WHETHER A 1 HAS BEEN GENERATED IN THE QUOTIENT.  IF SO,
						;;5951	; 26 ADDITIONAL STEPS WILL GENERATE THE FULL 27 SIGNIFICANT BITS
						;;5952	; OF THE QUOTIENT.  IF NOT, 27 STEPS ARE REQUIRED.
						;;5953	
						;;5954	=0
						;;5955	FDVL2:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,J/FDVL3	;FIRST DIVIDE STEP
						;;5956		DIVIDE,AR_2(AR+BR),ARX/ADX*2		; DOES IT GENERATE A 1?
						;;5957	=00
						;;5958	FDVL3:	DISP/DIV,MQ/MQ*2,		;NO, TAKE AN EXTRA DIVIDE STEP
						;;5959			AR_2(AR+BR),ARX/ADX*2,J/DIVLP	; WITHOUT COUNTING FE
						;;5960		SR_1,SC_#+SC,#/1,J/DIV-		;YES, 27 STEPS WILL NORMALIZE QUO
						;;5961		DISP/DIV,MQ/MQ*2,AR_2(AR-BR),ARX/ADX*2,J/DIVLP
						;;5962		SR_1,SC_#+SC,#/1,J/DIV+
						;;5963	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE FLOATING DIVIDE -- FDV, FDVR			

						;;5964	;WE COME HERE AFTER DOING THE DIVISION, EITHER 26 OR 27 STEPS
						;;5965	; AS REQUIRED TO GENERATE A NORMALIZED QUOTIENT FROM NORMALIZED
						;;5966	; OPERANDS.  NOW FIGURE OUT WHAT EXPONENT THE REMAINDER SHOULD HAVE.
						;;5967	
						;;5968	=0
						;;5969	FDVL4:	SC_EXP-#,#/27.,			;DIVIDEND EXP-27
						;;5970			AR_BR,SKP AR0,J/FDVL6	;GET REMAINDER, TEST D'END SIGN
						;;5971		SC_EXP-#,#/26.,			;D'END EXP-26
						;;5972			AR_BR,SKP AR0
						;;5973	
						;;5974	;HERE WITH REMAINDER IN AR, ITS EXP IN SC
						;;5975	; SKIP IF D'END (AND THEREFORE REM) NEGATIVE.
						;;5976	
						;;5977	=0
						;;5978	FDVL6:	EXP_SC,BYTE DISP,		;TEST FOR UNDERFLOW
						;;5979			SKP AR EQ,J/FDVL7	; OR REM =0
						;;5980		AR_-BR,SKP CRY0,		;NEGATE REM, CHECK =0
						;;5981			GEN SC,BYTE DISP	; AND LOOK FOR EXP UFLO
						;;5982	=110	EXP_-SC-1,J/FDVL7		;ONE'S COMPLEMENT EXP
						;;5983		AR_0S				;REM =0 OR EXP UFLO
						;;5984	=110
						;;5985	FDVL7:	AC1_AR,ARX+MQ_0.M,		;SAVE REMAINDER
						;;5986			AR_MQ,ARL/AD,J/SNR2	;GO NORMALIZE QUOTIENT
						;;5987		AR_0S,J/FDVL7
						; 5988	.ENDIF/FPLONG
						; 5989	
						; 5990	
						; 5991	;SUBR TO CHECK FOR FLOATING NO DIVIDE
						; 5992	; ENTER WITH SKP ON DIVIDEND SIGN, IN AR LONG, WITH
						; 5993	; DIVISOR EXP IN SC, DIVISOR IN BR
						; 5994	
						; 5995	=0
U 1774, 2010,0001,0000,5202,1001,4210,0200	; 5996	FDVCHK:	SC_EXP-SC,EXP_SIGN,SKP BR0,J/FDVCK1
U 1775, 1774,5143,7700,0000,0020,0027,0000	; 5997		AR_-AR LONG,J/FDVCHK		;GET POSITIVE DIVIDEND
						; 5998	=0
						; 5999	FDVCK1:	GEN AR-2BR,SKP CRY0,		;TEST FOR NO DIVIDE
U 2010, 0002,5101,0004,2302,0040,5403,0177	; 6000			SC_#+SC,#/177,RETURN2	;AND CORRECT EXP
						; 6001		GEN AR+2BR,SKP CRY0,		;SAME TEST, NEG DIVISOR
U 2011, 0002,0601,0000,2302,0040,5403,0177	; 6002			SC_#+SC,#/177,RETURN2	;AND SAME EXP CORRECTION
						; 6003	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; FP.MIC[4,24]	15:33 8-Feb-86				UFA, DFN, FSC						

						; 6004	.TOC	"UFA, DFN, FSC"
						; 6005	
						; 6006		.DCODE
						;;6007	.IF/FPLONG
						;;6008	130:	R,		J/UFA		;UFA
						;;6009		RPW,		J/DFN		;DFN
						; 6010	.IFNOT/FPLONG
D 0130, 2000,1002				; 6011	130:	EA,	J/UUO			;UFA
D 0131, 2000,1002				; 6012		EA,	J/UUO			;DFN
						; 6013	.ENDIF/FPLONG
D 0132, 0100,1502				; 6014	132:	I,	FL-AC,	J/FSC		;Must adjoin 133 (IBP/ADJBP)
						; 6015		.UCODE
						; 6016	
						;;6017	.IF/FPLONG
						;;6018	=0****00***0
						;;6019	DFN:	FE_AR0-8,AR0-8_#,#/0,		;SAVE LOW EXP, CLR SO CAN
						;;6020			ARX_0S,J/DFN1		; DETECT FRACTION = 0
						;;6021	UFA:	FE_EXP,SC/SCAD,EXP_SIGN,ARX_0S
						;;6022	=
						;;6023	=000	BR_AR LONG,AR_AC0,CALL,J/EXPD
						;;6024	=100	ARX_AR,AR_SIGN,ARL/AD,		;READY TO UNNORMALIZE SMALLER OP
						;;6025			CALL.M,J/SHIFT
						;;6026		AR_SIGN,ARX/AD			;LOST SMALLER OP, USE ITS SIGN
						;;6027		AR_AR+BR,SKP AD NE,		;IS RESULT SIGNIFICANT?
						;;6028			SC_FE,I FETCH
						;;6029	=
						;;6030	=0	AC1_AR,J/FINI			;NO, CLEAR RESULT AC
						;;6031		SKP EXP NE,BR/AR		;IS RIGHT SHIFT REQ'D?
						;;6032	=0	SKP AR0,FETCH WAIT,J/UFA4	;NO, IS RESULT NEG?
						;;6033		AR_BR*.5,GEN FE-#,#/377,SKP SCAD NE,FETCH WAIT
						;;6034	=0	FE_-1,SET FLOV
						;;6035		FE_FE+1,SC/SCAD,SKP AR0
						;;6036	=0
						;;6037	UFA4:	AR0-8_SC,J/STAC1		;POS, PUT IN EXP STRAIGHT
						;;6038		AR0-8_-SC-1,J/STAC1		;NEG, USE COMPLEMENT OF EXP
						;;6039	
						;;6040	
						;;6041	DFN1:	AR_-AR,SKP CRY0			; LOW FRACTION =0?
						;;6042	=0	AR0-8_FE,STORE,			;STORE LOW WORD BACK TO MEM
						;;6043			ARX_AC0 COMP,J/STMAC	; GET COMPLEMENTED HIGH WORD
						;;6044		AR0-8_FE,STORE,			;LOW WORD WAS ZERO, INSTALL EXP
						;;6045			ARX_-AC0,J/STMAC	; GET NEGATED HIGH WORD
						; 6046	.ENDIF/FPLONG
						; 6047	;
						; 6048	;FSC
						; 6049	;ENTER WITH E IN AR
						; 6050	1502:					;[345] Next to IBP because of DRAM
						; 6051	FSC:	SC_EA,ARX+MQ_0.M,
U 1502, 1471,3200,2000,0002,0021,0013,0142	; 6052			AR_AC0,ARL/AD
U 1471, 1422,4001,0000,2201,1001,0010,0200	; 6053	=	FE_EXP+SC,EXP_SIGN,J/SNR2	;NORMALIZE SCALED RESULT
						; 6054	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; FP.MIC[4,24]	15:33 8-Feb-86				FIX, FIXR, FLTR						

						; 6055	.TOC	"FIX, FIXR, FLTR"
						; 6056	
						; 6057		.DCODE
D 0122, 4001,1410				; 6058	122:	R,		J/FIX		;FIX--Unrounded. Adjoins EXTEND
D 0126, 4000,1501				; 6059	126:	R,		J/FIXR		;FIXR--Rounded
D 0127, 4100,1500				; 6060		R,	FL-AC,	J/FLTR		;FLTR
						; 6061		.UCODE
						; 6062	;FLTR
						; 6063	;ENTER WITH (E) IN AR
						; 6064	=0****00***0
						; 6065	FLTR:	FE_#,#/277,ARX_AR,SKP AR0,	;BINARY POINT TO RIGHT OF ARX
U 1500, 1420,5401,2400,0301,1020,4516,0277	; 6066			AR_SIGN,J/SNORM		; SIGN EXTENDED.  GO NORMALIZE
						; 6067	
						; 6068	;FIX AND FIXR
						; 6069	;ENTER WITH (E) IN AR
						; 6070	;	FIX AND FIXR DIFFER ONLY IN THE ROUNDING CRITERION:
						; 6071	;FIXR ADDS 1 TO THE INTEGER PART IF THE FRACTION PART IS ONE-HALF
						; 6072	;OR GREATER.  FIX DROPS THE FRACTION PART OF POSITIVE NUMBERS, BUT ADDS
						; 6073	;1 TO THE INTEGER PART OF NEGATIVE NUMBERS IF THE FRACTION PART IS NOT
						; 6074	;ALL ZERO.
						; 6075	;	THIS IS IMPLEMENTED BY CHOOSING A FRACTION (THE ROUNDING
						; 6076	;CONSTANT) TO ADD TO THE INPUT, SUCH THAT A CARRY WILL OCCUR INTO THE
						; 6077	;INTEGER PART UNDER THE APPROPRIATE CONDITIONS.  FOR FIXR, THE ROUNDING
						; 6078	;CONSTANT IS EXACTLY ONE-HALF.  FOR FIX, IT IS ZERO ON POSITIVE INPUT,
						; 6079	;OR THE LARGEST POSSIBLE FRACTION (ALL 1S) ON NEGATIVE INPUT.
						; 6080	
						; 6081	FIXR:	FE_EXP-#,#/244,SKP SCAD0,	;GET BINARY POINT POSITION
U 1501, 2012,4041,0700,5231,0020,5110,0244	; 6082			ARX_1B1,J/FIX1		;GET ROUNDING CONSTANT
						; 6083	=
						; 6084	1410:					;Must be near EXTEND
						; 6085	FIX:	FE_EXP-#,#/244,SKP SCAD0,	;GET BINARY POINT POSITION
U 1410, 2012,5441,0200,5231,0020,5116,0244	; 6086			ARX_AR SIGN		;SET ROUNDING CONSTANT, GO FIX
						; 6087	=0
U 2012, 0016,0001,0000,0000,0000,1110,0420	; 6088	FIX1:	SET AROV,J/IFNOP		;CAN'T DO IT, GIVE UP
U 2013, 1522,3721,0540,0000,0000,0410,0000	; 6089		BR/AR,CLR AR,ARX_ARX*2		;ROUNDING CONSTANT READY IN ARX
						; 6090		BR_AR LONG,AR_BR,CLR ARX,	;MANTISSA TO AR LONG
U 1522, 1531,3202,2060,0302,0000,0510,0011	; 6091			SC_#,#/9.		;READY TO SHIFT OFF EXPONENT
						; 6092		ARX_SHIFT,AR_SIGN,		;MANTISSA LEFT ALIGNED IN ARX
U 1531, 2014,5441,2400,2032,0020,5116,0044	; 6093			SC_FE+#,#/36.,SKP SCAD0	;ANY INTEGER BITS?
						; 6094	=0	MQ_SHIFT,			;YES, PUT THEM IN MQ
						; 6095			AR_ARX (ADX),CLR ARX,	;SHIFT MANTISSA LEFT 36 PLACES
U 2014, 1552,3701,6010,0000,0217,0510,0000	; 6096			I FETCH,J/FIX2		;AND PREFETCH NEXT
U 2015, 0216,3401,2000,0000,0217,0010,0000	; 6097		AR_0S,I FETCH,J/STORAC		;ALL SIGNIFICANCE LOST
U 1552, 1622,3721,2400,0000,0000,0010,0000	; 6098	FIX2:	ARX_SHIFT,AR_MQ			;INTEGER IN AR, FRACTION IN ARX
U 1622, 0015,0602,2004,0000,0020,0027,0000	; 6099		AR_AR+BR,AD LONG,J/STAC		;ROUND AND STORE
						; 6100	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE PRECISION FLOATING NORMALIZATION			

						; 6101	.TOC	"SINGLE PRECISION FLOATING NORMALIZATION"
						; 6102	
						; 6103	;HERE TO NORMALIZE SINGLE PRECISION RESULTS
						; 6104	;SR2-3 TELL HOW TO STORE RESULTS:
						; 6105	;XX00 ... ROUND, SINGLE PRECISION
						; 6106	;XX01 ... TRUNCATE, SINGLE PRECISION
						; 6107	;XX10 ... LONG MODE (IMPLIES TRUNCATION)
						; 6108	;IN ADDITION, THIS CODE SETS SR 1 IF ANSWER IS NEGATIVE, SO X1YZ
						; 6109	; CORRESPONDS TO X0YZ EXCEPT THAT THE RESULT MUST BE NEGATED.
						; 6110	
						; 6111	;DISPATCH TO SNORM WITH "DISP/NORM,AR/AD*.25"
						; 6112	; THUS THE 8 POSSIBILITIES ARE:
						; 6113	;SNORM		AD=0	AR=0	EITHER ANSWER IS ZERO, OR MSB IS IN ARX
						; 6114	;SNORM+1	AD0	AR NEG	RESULT IS NEG.  MAKE POS, TRY AGAIN
						; 6115	;SNORM+2	AD1-6	AR3-8	MSB TOO FAR LEFT, SHIFT RIGHT & RETRY
						; 6116	;SNORM+3	AD7	AR9	RESULT IS CORRECTLY NORMALIZED
						; 6117	;SNORM+4	AD8	AR10	SHIFT LEFT ONCE FOR NORMALIZATION
						; 6118	;SNORM+5	AD9	AR11	SHIFT LEFT 2 PLACES
						; 6119	;SNORM+6	AD10	AR12	SHIFT LEFT THRICE
						; 6120	;SNORM+7	AD11-35	AR13-35	SHIFT LEFT A LOT, TRY AGAIN
						; 6121	
						; 6122	=000
						; 6123	SNORM:	AR_ARX,ARL/SH,SKP ARX NE,	;AR IS ZERO, GET ARX
U 1420, 2022,3713,4000,0000,2041,5410,0044	; 6124			ARX_0.M,J/SNZERO
U 1421, 1420,5143,7700,0000,0060,1635,0064	; 6125		NORM -AR,SET SR1,J/SNORM	;REMEMBER NEGATIVE, GO POSITIVE
						; 6126	SNR2:	AR_AR*.25 LONG,FE_FE+#,#/2,	;SHIFT RIGHT,
U 1422, 1420,3701,7700,2031,0040,0035,0002	; 6127			NORM,J/SNORM		;TRY AGAIN
U 1423, 0024,0001,0000,0000,0000,0005,0000	; 6128		SR DISP,J/SROUND		;AD7 -> AR9, IS ROUND REQ'D?
						; 6129		AR_AR*2 LONG,FE_FE-1,		;AD8 -> AR10, ONCE LEFT AND DONE
U 1424, 0024,3701,5500,3001,0000,0005,0000	; 6130			SR DISP,J/SROUND
						; 6131		AR_AR*4 LONG,FE_FE-#,#/2,	;AD9 -> AR11
U 1425, 0024,3243,2600,5031,0000,0005,0002	; 6132			SR DISP,J/SROUND
						; 6133		AR_AR*8 LONG,FE_FE-#,#/3,	;AD10 -> AR12
U 1426, 0024,3243,5500,5031,0000,0005,0003	; 6134			SR DISP,J/SROUND
						;;6135	.IFNOT/SNORM.OPT
						;;6136		SKP AR NE,INH CRY18,SC_#,#/7	;LOOK FOR AR13-17
						;;6137	=0	SC_#,#/13.			;LH IS 0.  SHIFT FARTHER
						;;6138		MQ_SHIFT,AR_ARX (ADX),CLR ARX,	;HIGH TO MQ, GET READY FOR LOW
						;;6139			FE_FE-SC		; ADJUST EXPONENT
						;;6140		ARX_SHIFT,AR_MQ,J/SNR2		;FRACTION REPOSITIONED. GO AGAIN
						;;6141	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE PRECISION FLOATING NORMALIZATION			

						;;6142	;HERE IS THE FASTER VERSION OF LONG NORMALIZATION SHIFTS,
						;;6143	; WHICH TAKES FOUR WORDS MORE BUT IS A BIT QUICKER IN THE
						;;6144	; INTERMEDIATE NORMALIZATION CASES.
						;;6145	
						; 6146	.IF/SNORM.OPT
						; 6147		ADA EN/0S,ADB/AR*4,AD/ANDCA,	;GENERATE AR*4
						; 6148			AR/AD*2,ARX/ADX*2,	; AR_AR*8 LONG
						; 6149			SC_#,#/12.,		;READY TO SHIFT FARTHER
U 1427, 2020,3063,5500,0302,0020,5417,0014	; 6150			GEN CRY18,SKP CRY0	; TEST AR0-19 FOR ZERO
						; 6151	
						; 6152	=0	AR_AR*8 LONG,BR_AR LONG,	;IT WAS IN AR13-19
U 2020, 1444,3203,5560,5031,0040,0035,0006	; 6153			FE_FE-#,#/6,NORM,J/SN1	; NOW IN AR10-16, AD8-14
						; 6154		MQ_SHIFT,AR_ARX (ADX),		;13-19=0, SHIFT TO TRY 20-35
U 2021, 1626,3701,6010,0302,0000,0510,0012	; 6155			CLR ARX,SC_#,#/10.
						; 6156		ARX_SHIFT,AR_MQ*.25,		;REPOSITION FRACTION IN AR LONG
						; 6157			FE_FE-#,#/13.,		;COMPENSATE EXPONENT
U 1626, 1420,3721,7400,5031,0040,0035,0015	; 6158			NORM,J/SNORM
						; 6159	=100
						; 6160	SN1:	AR_BR*2 LONG,FE_FE+#,#/2,	;MSB IN AD8, SO IN BR10
U 1444, 0024,3241,2600,2031,0000,0005,0002	; 6161			SR DISP,J/SROUND
						; 6162		AR_BR*4 LONG,FE_FE+1,		;MSB IN AD9, THUS IN BR11
U 1445, 0024,3241,5500,4001,0000,0005,0000	; 6163			SR DISP,J/SROUND
U 1446, 0024,0001,0000,0000,0000,0005,0000	; 6164		SR DISP,J/SROUND		;AD10 -> AR9, A LUCKY GUESS
						; 6165		AR_AR*8 LONG,BR_AR LONG,	;TRY SHIFTING 3 MORE
U 1447, 1444,3203,5560,5031,0040,0035,0003	; 6166			FE_FE-#,#/3,NORM,J/SN1
						; 6167	.ENDIF/SNORM.OPT
						; 6168	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE PRECISION FLOATING NORMALIZATION			

						; 6169	;HERE WHEN AD ENTIRELY ZERO ON NORMALIZE ATTEMPT.  SKIP IF ARX
						; 6170	; IS NOT ZERO, HAVING COPIED IT TO AR (IE, LEFT SHIFT 36 PLACES).
						; 6171	; OTHERWISE, THE ENTIRE RESULT IS ZERO, SO WE STORE THAT.
						; 6172	=0
						; 6173	SNZERO:	CLR FE,AR+ARX+MQ_0.M,		;RESULT = 0
U 2022, 0151,4001,0000,0401,0001,0005,0170	; 6174			SR DISP,J/SRND5
						; 6175		AR_AR*.25 LONG,FE_FE-#,#/34.,	;HAVE MOVED LEFT 36, GO RIGHT 2
U 2023, 1420,3701,7700,5031,0040,0035,0042	; 6176			NORM,J/SNORM		;AND TRY THAT
						; 6177	
						; 6178	
						; 6179	;WE GET HERE WITH A NORMALIZED POSITIVE FRACTION IN AR'ARX,
						; 6180	; THE CORRECTED EXPONENT IN FE, AND SR INDICATES THE PROPER SIGN
						; 6181	; FOR THE RESULT AND WHETHER THE ANSWER SHOULD BE ROUNDED,
						; 6182	; TRUNCATED, OR LONG.
						; 6183	
						;;6184	.IF/FPLONG
						;;6185	=100
						; 6186	.IFNOT/FPLONG
						; 6187	=1*0
						; 6188	.ENDIF/FPLONG
U 0024, 0035,3441,2060,0000,0000,0010,0000	; 6189	SROUND:	BR_AR LONG,AR_0S,J/SRND2	;PREPARE TO ROUND BY ADDING THE
						; 6190						; PART OF THE FRACTION WE WILL
						; 6191						; DISCARD (CARRY IF ARX0)
						; 6192		BR_AR LONG,CLR AR,ARX_1S,	;TRUNCATE MODE
U 0025, 0031,2301,0260,0000,0000,0405,0000	; 6193			SR DISP,J/STRNC		; HANDLING DEPENDS ON SIGN
						;;6194	.IF/FPLONG
						;;6195		BR_AR LONG,CLR AR,ARX_1S,	;LONG MODE
						;;6196			SC_#,#/9.
						;;6197	=	ARX_SHIFT,SR DISP		;MASK = 0,,000777 TO ARX
						;;6198	=01*
						;;6199		BR_AR LONG,AR_BR LONG,J/SRND4	;POS, TRUNCATE BY ANDING
						;;6200		AR_AR+BR,ARX/ADX,BR_AR LONG,	;NEG, MUST DIDDLE
						;;6201			NORM,J/SRND3		; NORM FORCES LONG ARITH
						; 6202	.ENDIF/FPLONG
						; 6203	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; FP.MIC[4,24]	15:33 8-Feb-86				SINGLE PRECISION FLOATING NORMALIZATION			

						; 6204	;HERE TO PERFORM ROUNDING OR TRUNCATION OF SINGLE-PRECISION RESULTS,
						; 6205	; AND CHECK FOR CARRY INTO EXPONENT FIELD REQUIRING RENORMALIZATION
						; 6206	
						; 6207	=0*1
U 0031, 0135,3202,2000,0000,0000,0510,0000	; 6208	STRNC:	AR_BR,CLR ARX,J/SRND4		;POS TRUNCATE, GO STUFF IN EXP
U 0035, 0134,0602,2004,0000,0060,0535,0000	; 6209	SRND2:	AR_AR+BR,NORM,CLR ARX		;NORM FORCES LONG ARITH
						; 6210						; SO THIS ADDS ARX TO BR'BRX
						; 6211	=1*0
U 0134, 0135,0301,7000,4001,0020,0010,0000	; 6212	SRND3:	AR_AR*.5,FE_FE+1		;RENORMALIZE
						; 6213	SRND4:	EXP_FE TST,SR DISP,		;STUFF EXP, CHECK NEG OR LONG
U 0135, 0151,3502,0600,0000,2000,0705,0410	; 6214			ARX_ARX*BRX,AD/ANDCB	;CLEAR TRUNCATED FRACTION
						; 6215	
						; 6216	;HERE TO STORE RESULT AS A FUNCTION OF SINGLE OR LONG PRECISION
						; 6217	; AND POSITIVE OR NEGATIVE...
						;;6218	.IF/FPLONG
						;;6219	=001
						; 6220	.IFNOT/FPLONG
						; 6221	=0*1
						; 6222	.ENDIF/FPLONG
U 0151, 0016,0001,0000,0000,0005,1633,0000	; 6223	SRND5:	SR_0,B WRITE,J/ST6		;POS & NOT LONG
						;;6224	.IF/FPLONG
						;;6225	SLNG3:	AC0_AR,AR_0S,SC_#,#/27.,J/SLNG4	;STORE HIGH PART OF LONG ANS
						; 6226	.ENDIF/FPLONG
U 0155, 0016,5163,7000,0000,0025,1633,0000	; 6227		AR_-AR,SR_0,B WRITE,J/ST6	;NEG & NOT LONG
						;;6228	.IF/FPLONG
						;;6229		AR_-AR LONG,J/SLNG3		;LONG NEG, MAKE IT SO
						;;6230	
						;;6231	SLNG4:	AR_SHIFT,I FETCH
						;;6232		AR0-8_FE-SC,BYTE DISP,		;TEST FOR EXP UNDERFLOW
						;;6233			SKP AR EQ		; OR LOW WORD ZERO
						;;6234	
						;;6235	=110
						; 6236	.ENDIF/FPLONG
U 1632, 0136,4001,0000,0000,0000,1610,0000	; 6237	STRAC1:	SR_0,J/STAC1			;PUT AWAY LOW WORD OF LONG RESULT
						;;6238	.IF/FPLONG
						;;6239		AR_0S,SR_0,J/STAC1		;CLEAR LOW WORD IN AC1
						; 6240	.ENDIF/FPLONG
						; 6241	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; FP.MIC[4,24]	15:33 8-Feb-86				DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV	

						; 6242	.TOC	"DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV"
						; 6243	
						; 6244		.DCODE
D 0110, 4001,1101				; 6245	110:	R,	B/0,	J/DFLOAT	;DFAD
D 0111, 4200,1101				; 6246		R,	B/2,	J/DFLOAT	;DFSB
D 0112, 4400,1101				; 6247		R,	B/4,	J/DFLOAT	;DFMP
D 0113, 4601,1101				; 6248		R,	B/6,	J/DFLOAT	;DFDV
						; 6249		.UCODE
						; 6250	
						; 6251	=0****00**0*
						; 6252	DFLOAT:	FE_EXP,EXP_SIGN.S,SC/SCAD,MQ_0.S,;[414]
						; 6253			VMA_VMA+1,LOAD ARX,
U 1101, 1045,0001,0000,0203,1013,3662,0300	; 6254			CALL.S,J/XFERW		;GET LOW WORD
U 1103, 0640,3701,0500,0000,0000,0033,0000	; 6255		ARX_ARX*2,B DISP		;LOW BIT 0 IGNORED
						; 6256	=
						; 6257	=00*
U 0640, 0230,3200,5061,0000,0020,0010,0000	; 6258	DFAS:	BR_AR LONG,AR_AC1*2,J/DFAS1	;MEM OP READY, GET AC OP
						; 6259	
U 0642, 0640,5163,7700,0000,0020,0027,0000	; 6260		AR_-AR LONG,J/DFAS		;DFSB, NEGATE AND ADD
						; 6261	
						; 6262		BR_AR LONG,GEN ARX,SKP AD NE,	;[241]HERE FOR DOUBLE FLT MUL
U 0644, 2044,3713,0060,0301,0020,5610,0756	; 6263			FE_#,#/-18.,J/DFMP	;[241]BEGIN TEST FOR STICKY BIT
						; 6264	
						; 6265		GEN AR*AC0,AD/XOR,SKP AD0,	;DFDV.  WILL QUO BE NEG?
						; 6266			BR_AR LONG,		;SAVE D'SOR IN BR, BRX
U 0646, 1520,3100,0060,3022,0020,5510,0000	; 6267			SC_FE-1,J/DFDV
						; 6268	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14
; FP.MIC[4,24]	15:33 8-Feb-86				DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV	

						; 6269	;HERE FOR DFAD AND DFSB
						; 6270	; MEM OPERAND IS IN BR (NEGATED IF DFSB)
						; 6271	; FE AND SC HAVE ITS EXPONENT
						; 6272	
						; 6273	=0*0
U 0230, 2026,3200,2400,0000,1020,0050,0000	; 6274	DFAS1:	ARX_AR,AR_AC0,CALL,J/EXPD	;AC OPERAND IN PLACE
						; 6275	=1*0
						; 6276	DFAS2:	ARX_AR,AR_SIGN,			;GET SHIFTED HIGH WORD
						; 6277			GEN #+SC,#/-36.,	;IS ANY SHIFT REQUIRED?
U 0234, 2032,5401,2400,2300,1020,5116,0734	; 6278			SKP SCAD0,J/DFAS3
						; 6279		ARX_AR,AR_SIGN,			;DIFF IS > 36
U 0235, 2030,5401,2400,2302,1020,5116,0044	; 6280			SC_#+SC,#/36.,SKP SCAD0	;CHECK FOR >72
						; 6281	=0	AC0_AR,MQ_SHIFT,AR_ARX (ADX),
U 2030, 1662,3701,6310,0000,0000,1010,0000	; 6282			ARX/MQ,J/DFAS6		;[241]36 < DIFF < 72
						; 6283		AR_BR,ARL/AD,ARX_BRX,		;DIFF >72
U 2031, 1466,3202,2600,0000,0001,0010,0102	; 6284			MQ_0.M,J/DNTRY		;NORMALIZE LARGER OP
						; 6285	=0
						; 6286	DFAS3:	AR_ARX,ARL/SH,ARX/MQ,		;NO SHIFT REQUIRED
U 2032, 1661,0001,4300,0000,2001,0010,0104	; 6287			MQ_0.M,J/DFAS5
U 2033, 1636,4001,4000,0000,0000,0010,0000	; 6288		AR_SHIFT			;BEGIN SHIFTING SMALLER OP
U 1636, 1641,4001,4300,0000,2000,1010,0000	; 6289		AC0_AR,AR_ARX,ARX/MQ		;HI PART TO AC
						; 6290		MQ_SHIFT,AR_ARX (ADX),		;MID PART TO MQ
U 1641, 1645,3721,6010,0000,0000,0510,0000	; 6291			CLR ARX			;SHIFT ZEROS IN FROM RIGHT
U 1645, 1661,3200,2310,0000,0020,0010,0000	; 6292	DFAS4:	MQ_SHIFT,ARX/MQ,AR_AC0		;ALL PIECES NOW IN PLACE
						; 6293	DFAS5:	AR_AR+BR,ARX/ADX,SC_#,#/4,	;HERE WHEN OPERANDS ALIGNED
U 1661, 1540,0602,2604,0302,0060,0035,0004	; 6294			NORM,J/DNORM		;ADD, AND NORMALIZE RESULT
						; 6295	
U 1662, 1722,3723,2010,0000,0000,0010,0000	; 6296	DFAS6:	MQ_SHIFT,AR_MQ			;[241]GET H,L, PUT S,H IN AR
U 1722, 1776,3401,4201,0000,2000,1010,0000	; 6297		AC1_AR,AR_ARX,ARX_0S		;[241]STORE S,H
U 1776, 2001,3240,4201,0000,0020,0010,0000	; 6298		ARX_AC1,AR_SHIFT		;[241]GET L,0, GET S,H BACK
U 2001, 2034,3703,0000,0000,0020,5610,0000	; 6299		GEN AR,SKP AD NE		;[241]TEST FOR 0'S,
U 2034, 1661,3240,2000,0000,0020,1610,0040	; 6300	=0	CLR SR3,AR_AC0,J/DFAS5		;[241]DO 2'S COMP, ALL IN PLACE
U 2035, 1661,3240,2000,0000,0020,1610,0061	; 6301		SET SR3,AR_AC0,J/DFAS5		;[241]DO 1'S COMP, ALL IN PLACE
						; 6302	
						; 6303	;SUBROUTINE TO CHOOSE OPERAND WITH SMALLER EXPONENT, AND
						; 6304	; PREPARE FOR SHIFTING IT.
						; 6305	; ENTER WITH ONE OPERAND FRACTION IN BR, ITS EXPONENT IN FE & SC,
						; 6306	; THE OTHER OP IN AR WITH ITS EXPONENT IN AR0-8
						; 6307	; RETURN THE LARGER EXPONENT IN FE, AND 36-(MAGNITUDE OF DIFFERENCE)
						; 6308	; IN SC.  RETURN 4 IF SC POSITIVE, 5 IF NEGATIVE.
						; 6309	
U 2026, 2040,0001,0000,5202,1021,5110,0200	; 6310	EXPD:	SC_EXP-SC,EXP_SIGN,SKP SCAD0	;COMPARE MAGNITUDES
						; 6311	=0	AR_BR,ARX_BRX,BR/AR,BRX/ARX,	;AC OP IS LARGER MAGNITUDE
U 2040, 2052,3242,2660,2001,0000,0010,0000	; 6312			FE_FE+SC,J/EXPD1	;ITS EXP TO FE
						; 6313		MQ_ARX,SC_#+SC,#/36.,		;CHECK FOR EXP DIFF > 36
U 2041, 0004,4001,0010,2302,2020,5103,0044	; 6314			SKP SCAD0,RETURN4
						; 6315	EXPD1:	MQ_ARX,SC_#-SC,#/36.,		;AC EXP .GE. MEM
U 2052, 0004,0001,0010,5302,2020,5103,0044	; 6316			SKP SCAD0,RETURN4	;SHIFT MEM OP
						; 6317	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15
; FP.MIC[4,24]	15:33 8-Feb-86				DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV	

						; 6318	;DFMP
						; 6319	; DO TESTS FOR STICKY BITS FIRST THEN
						; 6320	; GET HERE WITH MEM OPERAND (M'CAND) IN BR!BRX
						; 6321	; AR HAS (AC1), LOW HALF OF M'IER
						; 6322	
						; 6323	=0
U 2044, 0730,3240,2001,0000,0020,0010,0000	; 6324	DFMP:	AR_AC1,J/DFMP1			;NO STICKY BIT
U 2045, 2050,3240,2001,0000,0020,5610,0000	; 6325		AR_AC1,SKP AD NE		;GET AC LOW AND TEST
U 2050, 0730,0001,0000,0000,0000,0010,0000	; 6326	=0	J/DFMP1				;NO STICKY BIT
U 2051, 0730,0001,0000,0000,0000,1610,0061	; 6327		SET SR3				;WORRY ABOUT IT IN NORM
						; 6328	=00*
						; 6329	DFMP1:	MQ_AR,AR_0S,ARX_0S,		;SETUP LOW M'IER
						; 6330			SC_#+SC,#/-200,		;CORRECT EXPONENT
U 0730, 1241,3441,2210,2302,1000,0050,0600	; 6331			CALL,J/MULREE		;MULTIPLY BY THE LOW PART
U 0734, 0736,0602,2604,0000,0020,0027,0000	; 6332	=10*	AR_AR+BR LONG			;OOPS, LOW SIGN WAS SET
U 0736, 2056,3240,2010,0301,1020,0010,0762	; 6333		MQ_AR,AR_AC0,FE_#,#/-14.	;READY TO CONTINUE WITH HIGH PART
						; 6334	
						; 6335	;HERE TO USE HIGH MULTIPLIER
						; 6336	
						; 6337		SC_EXP+SC,EXP_SIGN,		;[414] EXTRACT EXP FROM HIGH WORD
U 2056, 1462,4001,0000,2202,1001,4510,0200	; 6338			SKP AR0			;CHECK FOR NEG M'IER
						; 6339	=010
U 1462, 1241,3723,2010,0000,1000,0050,0000	; 6340	DFMP2:	MQ_AR,AR_MQ,CALL,J/MULREE	;GO BACK IN FOR HIGH PART
U 1463, 1462,4001,0000,4400,2001,0010,0200	; 6341		EXP_1,J/DFMP2			;OOPS, NEG, MOVE SIGN TO BIT 8
						; 6342	=110
U 1466, 1540,3701,0000,0302,0040,0035,0004	; 6343	DNTRY:	SC_#,#/4,GEN AR,NORM,J/DNORM	;NORMALIZE THE ANSWER
						; 6344	=
						; 6345	
						; 6346	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 16
; FP.MIC[4,24]	15:33 8-Feb-86				DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV	

						; 6347	
						; 6348	;DFDV
						; 6349	; GET HERE WITH DIVISOR IN BR!BRX, ITS EXP-1 IN SC
						; 6350	; SKIP IF D'SOR AND D'END SIGNS DIFFER
						; 6351	
						; 6352	=000
U 1520, 0330,3200,5001,0000,0020,0050,0000	; 6353	DFDV:	AR_AC1*2,CALL,J/DFDV1		;GET LOW D'END, GO START DIVIDE
U 1521, 0330,3200,5001,0000,0020,1650,0062	; 6354		SET SR2,AR_AC1*2,CALL,J/DFDV1	;NOTE NEG QUO
						; 6355	
						; 6356	=011	AC1_AR,AR_MQ,ARL/AD,FE_FE+1,	;HERE FROM DDVSUB. NEW STEP CNT
U 1523, 0663,3721,2001,4001,0001,1050,0102	; 6357			MQ_0.M,CALL.M,J/DIV+	; SAVE HIGH QUO, RESUME
						; 6358	=101	AC1_AR,AR_MQ,ARL/AD,FE_FE+1,
U 1525, 0662,3723,2001,4001,0001,1050,0102	; 6359			MQ_0.M,CALL.M,J/DIV-
						; 6360	
						; 6361	=111	AR_AC1,ARX/MQ,SC_#,#/4,		;POSITIVE QUOTIENT TO AR LONG
U 1527, 1540,3240,2301,0302,0040,0035,0004	; 6362			NORM,J/DNORM		;NORMALIZE AND ROUND
						; 6363	
						; 6364	=00
						; 6365	DFDV1:	ARX_AR,AR_AC0,SKP AD0,		;TEST DIVIDEND SIGN
						; 6366			FE_#,#/26.,		;SETUP COUNT FOR HIGH QUO
U 0330, 1774,3200,2400,0301,1020,5550,0032	; 6367			CALL,J/FDVCHK		;GO CHECK DIVIDABILITY
U 0332, 0762,4001,0000,0000,0000,4210,0000	; 6368	=10	SKP BR0,J/DDVSUB		;BEGIN DIVISION (RETURN ABOVE)
U 0333, 0016,0001,0000,0000,0000,1110,0624	; 6369		SET FL NO DIV,J/IFNOP		;ABORT THE DIVISION
						; 6370	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17
; FP.MIC[4,24]	15:33 8-Feb-86				DOUBLE PRECISION NORMALIZATION				

						; 6371	.TOC	"DOUBLE PRECISION NORMALIZATION"
						; 6372	
						; 6373	=000
U 1540, 2060,3723,0000,0302,0040,5427,0043	; 6374	DNORM:	SKP ARX+MQ NE,SC_#,#/35.,J/DNZERO	;AR=0
						; 6375		BR/AR,BRX/ARX,AR_MQ COMP,	;RESULT NEG, MAKE POS
U 1541, 2243,2023,2060,0000,0000,1610,0062	; 6376			SET SR2,J/DNNEG		;[241]FLAG NEGATIVE
						; 6377		AR_AR*.25 LONG,MQ_MQ*.25,
U 1542, 2244,3703,7710,2031,0000,0012,0004	; 6378			FE_FE+#,#/4,J/DNHI	;MSB IN AR 1-6
						; 6379		AR_AR*.25 LONG,
U 1543, 1545,3703,7700,2031,0000,0010,0002	; 6380			FE_FE+#,#/2,J/DROUND	;MSB IN AR7
U 1544, 1545,0303,7700,4001,0020,0027,0000	; 6381		AR_AR*.5 LONG,FE_FE+1		;MSB IN AR8
						; 6382	DROUND:	AR_AR+1,ARX/ADX,NORM,		;MSB IS AR9, RIGHT ON
U 1545, 1266,4001,2600,0302,0060,0035,0043	; 6383			SC_#,#/35.,J/DRND1
U 1546, 1545,3703,5500,3001,0000,0710,0001	; 6384		(AR+ARX+MQ)*2,FE_FE-1,J/DROUND	;MSB IN AR10
U 1547, 2073,4001,4000,5001,0000,0010,0000	; 6385		AR_SHIFT,FE_FE-SC		;SOMEWHERE IN AR 11-35
						; 6386	
U 2073, 2074,4001,4340,0000,2000,0010,0000	; 6387	DNSHFT:	BR/AR,AR_ARX,ARX/MQ		;SHIFT THE WHOLE THING
U 2074, 2207,3701,6010,0000,0000,0510,0000	; 6388		MQ_SHIFT,AR_ARX (ADX),CLR ARX
						; 6389		MQ_SHIFT,ARX/MQ,AR_BR,SC_#,#/10.,
U 2207, 1540,3242,2310,0302,0040,0035,0012	; 6390			NORM,J/DNORM		;GIVE IT ANOTHER GO
						; 6391	
U 2243, 0676,4001,0000,0000,0000,0005,0000	; 6392	DNNEG:	SR DISP				;[241]TEST FOR 1'S COMP
U 0676, 2054,4003,2000,0000,0040,5410,0000	; 6393	=1110	AR_AR+1,SKP CRY0,J/DNNEG1	;[241]COMPLETE NEGATION OF MQ
						; 6394		MQ_AR,AR_BR COMP,ARX_BRX COMP,
U 0677, 1540,2542,2610,0000,1040,0035,0000	; 6395			NORM,J/DNORM		;NORMALIZE THE POS FORM
						; 6396	=0
						; 6397	DNNEG1:	MQ_AR,AR_BR COMP,ARX_BRX COMP,
U 2054, 1540,2542,2610,0000,1040,0035,0000	; 6398			NORM,J/DNORM		;NORMALIZE THE POS FORM
U 2055, 1540,5162,2610,0000,1060,0035,0000	; 6399		MQ_AR,AR_-BR,ARX/ADX,NORM,J/DNORM
						; 6400	
U 2244, 1466,3703,7710,0000,0000,0710,0001	; 6401	DNHI:	(AR+ARX+MQ)*.25,J/DNTRY		;GO TRY AGAIN
						; 6402	
						; 6403	=0
U 2060, 0226,3401,2200,0000,0217,1610,0000	; 6404	DNZERO:	SR_0,AR_0S,ARX_0S,I FETCH,J/DLOAD;[413] RESULT = 0, STORE THAT
U 2061, 2073,4001,4000,5001,0000,0010,0000	; 6405		AR_SHIFT,FE_FE-SC,J/DNSHFT	;NOT ZERO, SHIFT AND TRY AGAIN
						; 6406	
						; 6407	=110
U 1266, 1267,0301,7700,4001,0020,0027,0000	; 6408	DRND1:	AR_AR*.5 LONG,FE_FE+1		;ROUNDING BLEW THE NORM, GO RIGHT
						; 6409		EXP_FE TST,SR DISP,CLR MQ,	;STUFF EXP IN, CHECK RESULT SIGN
U 1267, 0735,4041,0220,0000,2020,0705,0413	; 6410			BRX/ARX,ARX_1		;READY IF NEGATION NECESSARY
						; 6411	
						; 6412	=1101	AC0_AR,AR_SHIFT,ARX_BRX,	;[241] STORE HIGH WORD, READY LOW
U 0735, 0141,3202,4600,0000,0217,1010,0000	; 6413			I FETCH,J/STD1
						; 6414		ARX_ARX*BRX,AD/ANDCA,SR_0,	;CLEAR ROUNDING BIT
U 0737, 2252,3002,0620,0000,0000,1610,0000	; 6415		    BRX/ARX			;[413] and force BRX non zero
U 2252, 0431,5143,7700,0000,0020,0027,0000	; 6416	CDBLST:	AR_-AR LONG,J/DBLST		;[345] NEGATE RESULT AND STORE
						; 6417	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DOUBLE PRECISION ARITHMETIC			

						; 6418	.TOC	"GFLT DOUBLE PRECISION ARITHMETIC"
						; 6419	
						; 6420	.IF/EXTEXP
						; 6421	
						; 6422		.DCODE
D 0102, 4000,1700				; 6423	102:	R,	B/0,	J/EDFLOT	;GFAD
D 0103, 4201,1700				; 6424	103:	R,	B/2,	J/EDFLOT	;GFSB
D 0106, 4401,1700				; 6425	106:	R,	B/4,	J/EDFLOT	;GFMP
D 0107, 4600,1700				; 6426	107:	R,	B/6,	J/EDFLOT	;GFDV
						; 6427		.UCODE
						; 6428	
						; 6429	=0****00**0*
						; 6430	EDFLOT:	VMA_VMA+1, LOAD ARX,
U 1700, 1045,4001,0000,0000,0013,3662,0100	; 6431			MQ_0.S, CALL [XFERW]
U 1702, 1560,3701,0507,0000,0000,1033,0176	; 6432		FM[E0]_AR, ARX_ARX*2, B DISP	;mem high to E0, do instruction
						; 6433	=
						; 6434	
						; 6435	=000
U 1560, 2212,4001,0000,0000,0000,4550,0000	; 6436	EDFL1:	SKP AR0, CALL [ISOEXP]		;save mem high word in E0.
U 1561, 2062,0001,0007,0000,0000,1010,0172	; 6437		FM[T2]_AR, J/EF1		;save mem exp in T2.
U 1562, 1563,5163,7700,0000,0020,0027,0000	; 6438		AR_-AR LONG			;subtract now same as add.
U 1563, 1560,0001,0007,0000,0000,1010,0176	; 6439		FM[E0]_AR, J/EDFL1		;save "positive" exponent
U 1564, 2212,4001,0000,0000,0000,4550,0000	; 6440	=100	SKP AR0, CALL [ISOEXP]		;isolate mem exp in AR.
						; 6441		BR/AR, GEN ARX, SKP AD NE,	;start test for sticky bits.
U 1565, 2130,3713,0040,0000,0020,5610,0000	; 6442			J/EFMP
						; 6443	=110	BR/AR, GEN AR, SKP AD0, 	;save mem high in br.
U 1566, 2212,3703,0040,0000,0020,5550,0000	; 6444			CALL [ISOEXP]		;get mem exp
U 1567, 2614,3441,2007,0000,0000,1010,0172	; 6445		FM[T2]_AR, AR_0S, J/EFDV0	;save mem exp in T2. No sticky bits.
						; 6446	=0
U 2062, 2212,3200,2000,0000,0020,5550,0000	; 6447	EF1:	AR_AC0, SKP AD0, CALL [ISOEXP]	;get AC op
U 2063, 2256,4001,0047,0000,0000,1010,0165	; 6448		FM[E1]_AR, BR/AR		;save AC exp in E1
						; 6449			
						; 6450	;Now have positive mem exponent in T2, pos AC exp in E1.
						; 6451	;Save larger exp in T2 and exp diff if less than 340 in SC.
						; 6452		[AR]_[AR]*FM[T2], AD/A-B,	;AR gets exp diff.
U 2256, 0420,5100,2007,4000,0040,5510,0172	; 6453			SKP AD0			;AR get exp diff, BRX gets exp.
U 0420, 2366,0001,0000,0302,0000,0050,0003	; 6454	=00	SC_#, #/3, CALL [EXPDIF]	;test for exp diff >72.
U 0421, 0430,4001,0040,0000,0000,0010,0000	; 6455		BR/AR, J/EF3A			;mem op larger.
U 0422, 2326,3242,2000,0000,0000,0010,0000	; 6456		AR_BR, J/EF5			;restore exp to AR.
U 0423, 2656,3260,2007,0401,0020,0010,0165	; 6457		[AR]_FM[E1], CLR FE, J/ACNORM	;exp diff too large, norm AC op.
						; 6458	=00
U 0430, 2366,5142,2000,0302,0020,0050,0003	; 6459	EF3A:	AR_-BR, SC_#, #/3, CALL [EXPDIF];mem larger, get positive diff.
U 0432, 2272,3242,2000,0000,0000,0010,0000	; 6460	=10	AR_BR, J/EF3B			;restore exponent to AR.
U 0433, 2652,3240,2007,0401,0020,0010,0172	; 6461		[AR]_FM[T2], CLR FE, J/MEMNRM	;exp diff > 72. norm mem op.
U 2272, 2276,3203,5000,0000,0000,0010,0000	; 6462	EF3B:	AR_AR*8				;move exp difference into AR0-8.
						; 6463		FE_AR0-8,			;mem larger, op doable.
U 2276, 2100,3240,2000,2421,0020,0010,0000	; 6464			AR_AC0			;save smaller AC op in T0,T1
U 2100, 2362,4001,0007,0000,0000,1050,0166	; 6465	=0	FM[T0]_AR, CALL [EF5B]
U 2101, 2312,4001,0007,0000,0000,1010,0171	; 6466		FM[T1]_AR
U 2312, 2316,3260,2007,0000,0020,0010,0176	; 6467		[AR]_FM[E0]			;save larger mem op in AC0,AC1
U 2316, 2324,4001,4000,0000,2000,1010,0000	; 6468		AC0_AR, AR_ARX
U 2324, 2110,0001,0001,0000,0000,1010,0000	; 6469		AC1_AR, J/EF5A			;all set to shift and add.
						; 6470	
U 2326, 2330,3203,5000,0000,0000,0010,0000	; 6471	EF5:	AR_AR*8				;move exp difference into AR0-8.
U 2330, 2332,3260,2007,2421,0020,0010,0176	; 6472		FE_AR0-8, [AR]_FM[E0]		;smaller mem op to T0,T1
U 2332, 2104,4001,4007,0000,2000,1010,0166	; 6473		FM[T0]_AR, AR_ARX; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DOUBLE PRECISION ARITHMETIC			

U 2104, 2362,4001,0007,0000,0000,1050,0171	; 6474	=0	FM[T1]_AR, CALL [EF5B]
U 2105, 2334,4001,0001,0000,0000,1010,0000	; 6475		AC1_AR				;we expect AC1*2 to be saved.
U 2334, 2336,3260,2007,0000,0020,0010,0165	; 6476		[AR]_FM[E1]			;save larger AC exp in T2
U 2336, 2110,4001,0007,0000,0000,1010,0172	; 6477		FM[T2]_AR, J/EF5A
						; 6478	
U 2362, 0001,3240,5001,0000,0020,0003,0000	; 6479	EF5B:	AR_AC1*2, RETURN1
						; 6480	
						; 6481	;EXPDIF determines if the exponent difference is too large-ie >110 oct.
						; 6482	;The largest allowed value for shifting is 72 decimal. This is 110 octal.
						; 6483	;Since the exponent is in AR1-11, 110 octal has the value 11 in AR1-8.
						; 6484	;It expects the exponent difference in AR0-8.
						; 6485	;It uses AR0-8 and the BR.
						; 6486	;Returns 2 if the difference is ok (<=110).
						; 6487	;Returns 3 if the difference is too large (>110).
U 2366, 2371,0001,0040,0000,0000,0410,0000	; 6488	EXPDIF:	BR/AR, CLR AR			;zero all of those bits.
U 2371, 2372,0001,0000,0000,0000,0110,0010	; 6489		AR0-8_#, #/10			;put in 100 in AR0-11.
U 2372, 0002,5102,0004,0000,0040,5503,0000	; 6490		GEN AR-BR, SKP AD0, RETURN2	;<max diff>-<actual diff>
						; 6491	
						; 6492	;We now have:
						; 6493	; AC0	/ larger op high
						; 6494	; AC1	/ larger op low
						; 6495	; T0	/ smaller op high
						; 6496	; T1	/ smaller op low
						; 6497	; T2	/ larger exponent
						; 6498	; FE	/ exp difference
						; 6499	;We must now sign extend both high ops.
						; 6500	=0
U 2110, 2250,3200,2000,0000,0020,5550,0000	; 6501	EF5A:	AR_AC0, SKP AD0, CALL [SGNEXT]	;get larger high op
U 2111, 2112,0001,0000,0000,0000,1010,0000	; 6502		AC0_AR				;save larger extended op in AC0
						; 6503	=0	[AR]_FM[T0], SKP AD0,		;get smaller high op
U 2112, 2250,3260,2007,0000,0020,5550,0166	; 6504			CALL [SGNEXT]		; and sign extend into AR1-11.
U 2113, 2376,0001,0007,0000,0000,1010,0166	; 6505		FM[T0]_AR			;save smaller extended op in T0
						; 6506	;We are now set to shift the smaller op to align it with the larger op.
U 2376, 2404,3260,2007,0000,0020,0010,0171	; 6507		[AR]_FM[T1]
U 2404, 2406,3240,2407,0000,1020,0013,0166	; 6508		[AR]_FM[T0], ARX_AR, SC_FE	;move diff to SC for next line.
U 2406, 2114,0001,0000,5302,0020,5110,0044	; 6509		SC_#-SC, #/36., SKP SCAD0
U 2114, 2444,5401,2400,0000,1020,0016,0000	; 6510	=0	ARX_AR, AR_SIGN, J/EF10		;FE < 37.
U 2115, 2414,4001,0060,0000,0000,0013,0000	; 6511		BR/AR, BRX/ARX, SC_FE
U 2414, 2416,5441,2400,0000,1020,0016,0000	; 6512		AR_SIGN, ARX_AR
U 2416, 2426,4001,0000,5302,0000,0010,0110	; 6513		SC_#-SC, #/72.
U 2426, 0371,0001,0400,0000,0000,0010,0000	; 6514		ARX_SHIFT			;high is sign, low is sign,,high.
						; 6515	=01	AR_BR LONG, BR/AR, BRX/ARX,	;save new stuff in BR long.
U 0371, 2514,3242,2660,0000,0000,0050,0000	; 6516			CALL [EF12]		;MQ gets lowest word.
U 0373, 2436,0001,0007,0000,0000,1010,0165	; 6517		FM[E1]_AR			;save sticky bits.
U 2436, 2474,3200,2001,0000,0020,0010,0000	; 6518		AR_AC1, J/EF11			;now prepare to add.
U 2444, 2446,3240,4207,0000,0020,0010,0171	; 6519	EF10:	AR_SHIFT, [ARX]_FM[T1]		;shift high op, load low word.
U 2446, 2456,3260,2047,0000,0020,0010,0166	; 6520		BR/AR, [AR]_FM[T0]		;shift low op, load high word.
U 2456, 0451,3713,2400,0000,0000,0010,0000	; 6521		AR_ARX (AD), ARX_SHIFT		;get shifted low word into ARX.
U 0451, 2514,0001,0020,0000,0000,0550,0000	; 6522	=01	BRX/ARX, CLR ARX, CALL [EF12]	;save low word, shift end bits.
U 0453, 2464,4001,0007,0000,0000,1010,0165	; 6523		FM[E1]_AR			;save sticky bits. (word 4 of sum).
U 2464, 2474,3200,2001,0000,0020,0010,0000	; 6524		AR_AC1, J/EF11			;prepare to add.
U 2466, 2474,3200,2061,0000,0020,0010,0000	; 6525		BR/AR, BRX/ARX, AR_AC1		;get larger op in AR,ARX
U 2474, 2476,3240,2400,0301,1020,0010,0000	; 6526	EF11:	ARX_AR, AR_AC0, FE_#, #/0	;smaller op in BR,BRX
						; 6527		AR_AR+BR, ARX/ADX, SC_#, #/3,	;operation done, now normalize.
U 2476, 1740,0602,2604,0302,0060,0035,0003	; 6528			NORM, J/ENORM
						; 6529	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-2
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DOUBLE PRECISION ARITHMETIC			

U 2514, 2604,3713,2010,0000,0000,0510,0000	; 6530	EF12:	MQ_SHIFT, AR_ARX (AD), CLR ARX,J/SHIFT
						;;6531	.IF/GFTCNV		;[273]
						;;6532	EF12A:	AR_SHIFT, RETURN10
						; 6533	.ENDIF/GFTCNV		;[273]
						; 6534	.ENDIF/EXTEXP
						; 6535	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT MULTIPLY						

						; 6536	.TOC	"GFLT MULTIPLY"
						; 6537	.IF/EXTEXP
						; 6538	=0
U 2130, 2133,0001,0000,0000,0000,0410,0000	; 6539	EFMP:	CLR AR, J/EFMP1			;mem low op is zero, no sticky bits.
U 2131, 2132,3200,5001,0000,0020,5610,0000	; 6540		AR_AC1*2, SKP AD NE		;is AC low op non-zero as well ?
U 2132, 2133,0001,0000,0000,0000,0410,0000	; 6541	=0	CLR AR				;yes, no sticky bits today.
U 2133, 2526,3401,2007,0000,0000,1010,0165	; 6542	EFMP1:	FM[E1]_AR, AR_0S		;set sticky bits.
U 2526, 2536,0001,0000,0000,0000,0110,0200	; 6543		AR0-8_#, #/200			;subtract 200.
U 2536, 2542,3242,2040,0000,0000,0010,0000	; 6544		BR/AR, AR_BR			;swap around exp and 2000.
U 2542, 2140,5102,2004,0402,0020,0010,0000	; 6545		AR_AR-BR, CLR SC		;done, and SC is cleared.
						; 6546	=0	BR/AR, AR_AC0, SKP AD0,		;save exp-2000 in BR.
U 2140, 2212,3240,2040,0000,0020,5550,0000	; 6547			CALL [ISOEXP]		;get AC high and isolate exp.
U 2141, 2544,0602,2000,0000,0020,0010,0000	; 6548		AR_AR+BR			;add exponents together.
U 2544, 2142,0001,0007,0000,0000,1010,0172	; 6549		FM[T2]_AR			;and store the sum in T2.
						; 6550	=0	[AR]_FM[E0], SKP AD0,		;get mem high op sign extended.
U 2142, 2250,3240,2007,0000,0020,5550,0176	; 6551			CALL [SGNEXT]
U 2143, 2567,4001,0000,0301,0000,0010,0756	; 6552		FE_#, #/-18.			;
U 2567, 1640,3200,2061,0000,0020,0010,0000	; 6553		BR/AR, BRX/ARX, AR_AC1		;move mem ops to BR!BRX.
						; 6554	=000	MQ_AR, AR_0S, ARX_0S,		;multiply by low word.
U 1640, 1241,3441,2210,0000,1000,0050,0000	; 6555			CALL [MULREE]
U 1644, 1646,0602,2600,0000,0020,0027,0000	; 6556	=100	AR_AR+BR LONG			;low sign was set, add results.
						; 6557	=110	MQ_AR, AR_AC0, FE_#, #/-13.,	;now continue with high part.
U 1646, 2250,3200,2010,0301,1020,5550,0763	; 6558			SKP AD0, CALL [SGNEXT]	;sign extend the ac high op.
U 1647, 2144,0001,0007,0000,0000,1010,0166	; 6559		FM[T0]_AR			;save sign extended AC op.
						; 6560	;	SKP AR0				;test sign bit to adjust FE.
						; 6561	=0
U 2144, 1660,3723,2010,0000,1000,0010,0000	; 6562	EFMPP1:	MQ_AR, AR_MQ, J/EFMPP2		;swap AR+MQ.
U 2145, 2144,4001,0000,4001,0000,0010,0000	; 6563		FE_FE+1, J/EFMPP1		;inc the FE if number is neg.
						; 6564	=000
						; 6565	EFMPP2:	AD/0S, FE_FE+1, DISP/MUL,	;now multiply by the high word.
U 1660, 0540,3401,0010,4001,0000,0070,0000	; 6566			MQ/MQ*.25, CALL [MULP]
						; 6567	;Since our last multiply step used 2 signs bits instead of a sign bit
						; 6568	;and the MSB, our answer is too low by a power of two for positive numbers
						; 6569	;and too low by a power of 4 for negative numbers.
U 1664, 2574,3703,5500,0000,0000,0710,0001	; 6570	=100	(AR+ARX+MQ)*2, J/EFMPP3		;try this correction factor.
U 1665, 2574,3701,7710,0000,0000,0710,0001	; 6571	=101	(AR+ARX+MQ)*.25, J/EFMPP3	;shouldn't ever get here.
U 1666, 2574,3703,5500,0000,0000,0710,0001	; 6572	=110	(AR+ARX+MQ)*2			;and this for postive numbers.
						; 6573	=
U 2574, 0471,2301,0260,0000,0000,0610,0030	; 6574	EFMPP3:	BR_AR LONG, AR_0.C, ARX_1S	;result to BR!BRX. Build mask.
						; 6575	=01	SC_#, #/10.,			;load SC with shift count.
U 0471, 2604,4001,0000,0302,0000,0050,0012	; 6576			CALL [SHIFT]		;Now have mask of 0,,1777
U 0473, 2576,3242,2660,0000,0000,0010,0000	; 6577		AR_BR LONG, BR_AR LONG		;mask to BR, result TO AR!ARX.
U 2576, 2603,3522,0014,0000,0000,0710,0003	; 6578		MQ_MQ*BR, AD/ANDCB		;clear the last 10 MQ bits.
						; 6579		GEN AR, SC_#, #/3,		;generate NORM bits.
U 2603, 1740,3701,0000,0302,0040,0035,0003	; 6580			NORM, J/ENORM		;conditions set for EE norm.
						; 6581	.ENDIF/EXTEXP
						; 6582	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DIVIDE						

						; 6583	.TOC	"GFLT DIVIDE"
						; 6584	.IF/EXTEXP
U 2614, 2150,0001,0007,0000,0000,1010,0165	; 6585	EFDV0:	FM[E1]_AR			;no sticky bits on divide.
U 2150, 2250,3242,2000,0000,0020,5550,0000	; 6586	=0	AR_BR, SKP AD0, CALL [SGNEXT]	;sign extend mem high.
						; 6587		GEN AR*AC0, AD/XOR, SKP AD0,	;determine sign of result.
U 2151, 1720,3100,0060,4000,0020,5510,0000	; 6588			BR_AR LONG		;mem op to BR!BRX.
U 1720, 0550,3200,5001,0000,0020,0050,0000	; 6589	=000	AR_AC1*2, CALL [EFDV1]		;start division.
U 1721, 0550,3200,5001,0000,0020,1650,0001	; 6590		SR_1, AR_AC1*2, CALL [EFDV1]	;note result if negative.
						; 6591	
						; 6592	=011	AC1_AR, AR_MQ, ARL/AD, FE_FE+1,	;set step count to 35-2.
U 1723, 0663,3721,2001,4001,0001,1050,0102	; 6593			MQ_0.M, CALL [DIV+]	
						; 6594	=101	AC1_AR, AR_MQ, ARL/AD, FE_FE+1,
U 1725, 0662,3723,2001,4001,0001,1050,0102	; 6595			MQ_0.M, CALL [DIV-]
U 1727, 2616,0001,0000,0401,0000,0410,0000	; 6596	=111	CLR AR, CLR FE			;exp must be adjusted-
U 2616, 2632,4001,0000,0000,0000,0110,0200	; 6597		AR0-8_#, #/200			;  it is currently 2000 too low
U 2632, 2634,0600,2007,0000,0020,0010,0172	; 6598		[AR]_[AR]*FM[T2], AD/A+B	;add in the correction.
U 2634, 2636,4001,0007,0000,0000,1010,0172	; 6599		FM[T2]_AR			;store the corrected exp in T2.
						; 6600		AR_AC1, ARX/MQ, SC_#, #/3,	;get answer ready for norm.
U 2636, 1740,3240,2301,0302,0040,0035,0003	; 6601			NORM, J/ENORM
						; 6602	
						; 6603	=00
						; 6604	EFDV1:	ARX_AR, AR_AC0, SKP AD0, FE_#,	;AC low*2 to ARX, AC high to AR.
U 0550, 2152,3200,2400,0301,1020,5550,0027	; 6605			#/23., CALL [EDVCHK]
U 0552, 0762,4001,0000,0000,0000,4210,0000	; 6606	=10	SKP BR0, J/DDVSUB
U 0553, 0016,0001,0000,0000,0000,1110,0624	; 6607		SET FL NO DIV, J/IFNOP		;no division this time.
						; 6608	
						; 6609	=0
U 2152, 2154,0001,0010,0000,1000,0010,0000	; 6610	EDVCHK:	MQ_AR, J/EDVCH1			;go to an even address.
U 2153, 2152,5163,7700,0000,0020,0027,0000	; 6611		AR_-AR LONG, J/EDVCHK		;make ac op positive.
						; 6612	
						; 6613	=0
U 2154, 2212,4001,0000,0000,0000,4550,0000	; 6614	EDVCH1:	SKP AR0, CALL [ISOEXP]		;op saved in MQ, get exp in AR.
						; 6615		[AR]_[AR]*FM[T2], AD/A-B,	;subtract exponents.
U 2155, 2160,5100,2007,4000,0040,5510,0172	; 6616			SKP AD0			;did this cause an underflow ?
						; 6617	=0
U 2160, 2161,0001,0000,0000,0000,1610,0062	; 6618		SET SR2 			;no, let SR2 denote this.
U 2161, 2162,4001,0007,0000,0000,1010,0172	; 6619	EDVCH2:	FM[T2]_AR			;yes, save exponent in T2 for ENORM.
						; 6620	
						; 6621	=0
U 2162, 2250,3721,2000,0000,0020,5550,0000	; 6622	EDVCH3:	AR_MQ, SKP AD0, CALL [SGNEXT]	;now sign extend the op.
U 2163, 2010,4001,0000,0000,0001,4210,0100	; 6623		SKP BR0, MQ_0.M, J/FDVCK1
						; 6624	.ENDIF/EXTEXP
						; 6625	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT NORMALIZATION					

						; 6626	.TOC	"GFLT NORMALIZATION"
						; 6627	.IF/EXTEXP
						; 6628	;Normalization is done here.
						; 6629	;	The are 8 addresses the can be reached when doing a
						; 6630	;	NORM dispatch. The following table describes the
						; 6631	;	dispatching and how to normalize the fraction and
						; 6632	;	exponent.
						; 6633	;
						; 6634	;	=000	AR=0			AR is zero, check ARX,MQ
						; 6635	;	=001	AR00=1			sign bit on, complement
						; 6636	;	=010	MSB in AR 1-6		shf 4 rt.(a guess)
						; 6637	;	=011	MSB in AR07		sht 2 rt.
						; 6638	;	=100	MSB in AR08		sht 3 rt.
						; 6639	;	=101	MSB in AR09		right on!
						; 6640	;	=110	MSB in AR10		sht 1 lf.
						; 6641	;	=111	MSB in AR 11-35		sht 4 lf.(a guess)
						; 6642	;
						; 6643	;The normalization routine for double precision assumes that
						; 6644	;	the exponent can be found in the FE. As it goes through
						; 6645	;	the normalization process, it adjusts the fraction and
						; 6646	;	the FE by the correct amounts to normalize the number.
						; 6647	;	In GFLT numbers, the exponent may not fit
						; 6648	;	into the FE, so it has to be saved in an accumulator.
						; 6649	;	However, if one assumes initially that the exponent is
						; 6650	;	zero and that it is in the FE, then the same normalization
						; 6651	;	algorithm can be used as in double precision numbers
						; 6652	;	with the realization that at the end of the normalization
						; 6653	;	process the FE contains the correction (EC)  that must be
						; 6654	;	added into the saved exponent (ES)  to produce a 'bit-9'
						; 6655	;	normalized number. Once this correction value is obtained,
						; 6656	;	the 'bit-12' normalized exponent (EN)  is given by
						; 6657	;			EN = ES + EC + 3
						; 6658	
U 2652, 2654,0001,0007,0000,0000,1010,0172	; 6659	MEMNRM:	FM[T2]_AR			;save larger exponent.
U 2654, 2170,3240,2007,0000,0020,5510,0176	; 6660		[AR]_FM[E0], SKP AD0, J/ACNRM1	;get high word, sign extend it
						; 6661	
U 2656, 2676,0001,0007,0000,0000,1010,0172	; 6662	ACNORM:	FM[T2]_AR			;save larger exponent.
U 2676, 2704,3200,5001,0401,0020,0010,0000	; 6663		AR_AC1*2, CLR FE		;get low word*2 into AR.
U 2704, 2170,3200,2400,0000,1020,5510,0000	; 6664		ARX_AR, AR_AC0, SKP AD0		;get high word, sign extend it.
						; 6665	=0
						; 6666	ACNRM1:	[AR]_[AR]*FM[EXPMSK], AD/AND,	;sign extend with 0's.
U 2170, 1740,3600,2007,4000,0040,0035,0164	; 6667		NORM, J/ENORM
						; 6668		[AR]_[AR]*FM[EXPMSK], AD/ORCB,	;sign extend with 1's.
U 2171, 1740,2700,2007,4000,0040,0035,0164	; 6669			NORM			;fall into the normalize routine.
						; 6670	
						; 6671	=000
						; 6672	ENORM:	SKP ARX+MQ NE, SC_#, #/35.,	;AR=0,check ARX,+MQ.
U 1740, 2210,3723,0000,0302,0040,5427,0043	; 6673			J/ENZERO
						; 6674		BR/AR, BRX/ARX, AR_MQ COMP,	;result neg, complement.
U 1741, 2721,2021,2060,0000,0000,1610,0061	; 6675			SET SR3, J/ENNEG	;flag negative seen.
						; 6676		AR_AR*.25 LONG, MQ_MQ*.25,	;MSB in AR 1-6.
U 1742, 2746,3701,7710,2031,0000,0012,0004	; 6677			FE_FE+#, #/4, J/ENHI
						; 6678		AR_AR*.25 LONG, FE_FE+#,	;MSB in AR07.
U 1743, 1745,3701,7700,2031,0000,0010,0002	; 6679			#/2, J/EROUND		;
U 1744, 1745,0301,7700,4001,0020,0027,0000	; 6680		AR_AR*.5 LONG, FE_FE+1		;MSB in AR08.
						; 6681	EROUND:	BR_AR LONG, AR+MQ_0.S,	 	;MSB in AR09, where we want it.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4-1
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT NORMALIZATION					

U 1745, 2751,0001,0060,0000,0000,0022,0130	; 6682			J/ERND1			;put result in BR!BRX.
						; 6683		(AR+ARX+MQ)*2, FE_FE-1,		;MSB in AR10.
U 1746, 1745,3701,5500,3001,0000,0710,0001	; 6684			J/EROUND
U 1747, 2706,4001,4000,5001,0000,0010,0000	; 6685		AR_SHIFT, FE_FE-SC		;MSB somewhere in AR 11-35.
						; 6686	
U 2706, 2714,0001,4340,0000,2000,0010,0000	; 6687	ENSHFT:	BR/AR, AR_ARX, ARX/MQ		;shift everyone.
U 2714, 2716,3701,6010,0000,0000,0510,0000	; 6688		MQ_SHIFT, AR_ARX (ADX), CLR ARX
						; 6689		MQ_SHIFT, ARX/MQ, AR_BR,	;go aroung again.
						; 6690			SC_#, #/10.,
U 2716, 1740,3202,2310,0302,0040,0035,0012	; 6691			NORM, J/ENORM
						; 6692	
U 2721, 2172,3200,0007,0000,0020,5610,0165	; 6693	ENNEG:	GEN E1, SKP AD NE		;any sticky bits left around?
U 2172, 2202,4001,2000,0000,0040,5410,0000	; 6694	=0	AR_AR+1, SKP CRY0, J/ENNEG1	;no, 2's comp MQ.
						; 6695		MQ_AR, AR_BR COMP, ARX_BRX COMP,
U 2173, 1740,2502,2610,0000,1040,0035,0000	; 6696			NORM, J/ENORM		;one's complement to finish.
						; 6697	=0
						; 6698	ENNEG1:	MQ_AR, AR_BR COMP, ARX_BRX COMP,
U 2202, 1740,2502,2610,0000,1040,0035,0000	; 6699			NORM, J/ENORM		;one's complement to finish.
						; 6700		MQ_AR, AR_-BR, ARX/ADX,		;carry happened, do two's comp.
U 2203, 1740,5142,2610,0000,1060,0035,0000	; 6701			NORM, J/ENORM
						; 6702	
U 2746, 3227,3701,7710,0000,0000,0710,0001	; 6703	ENHI:	(AR+ARX+MQ)*.25, J/ENTRY	;go try again after setting SC.
						; 6704	=0
U 2210, 0226,3401,2200,0000,0217,1610,0000	; 6705	ENZERO:	SR_0,AR_0S,ARX_0S,I FETCH,J/DLOAD;[413] result 0, store in AC,AC+1
U 2211, 2706,4001,4000,5001,0000,0010,0000	; 6706		AR_SHIFT, FE_FE-SC, J/ENSHFT	;not zero, try next 35 bits.
						; 6707	
U 2751, 2753,4041,0500,0000,0020,0010,0000	; 6708	ERND1:	ARX_2+MQ0			;[407] build rounding constant.
U 2753, 2775,3203,0600,0000,0000,0010,0000	; 6709		ARX_ARX*4			;gen a 10 in the ARX for rounding.
U 2775, 1766,0602,2604,0000,0060,0035,0000	; 6710		AR_AR+BR, ARX/ADX, NORM		;do the rounding and test norm.
U 1766, 1767,0301,7700,4001,0020,0027,0000	; 6711	=110	AR_AR*.5 LONG, FE_FE+1		;rounding blew norm, correct it.
						; 6712	
						; 6713	; When we get here the number is 'bit-9' normalized
						; 6714	; in the AR,ARX.  Add the FE + 3 to the exponent
						; 6715	; saved in T2.
						; 6716	; At this point the Extended Exponent must be put
						; 6717	; into the AR after everything is shifted right 3 bits.
						; 6718	; The double precision norm routine does this by:
						; 6719	; EXP_FE TST, SR DISP, CLR MQ, BRX/ARX, ARX_1
						; 6720	
						; 6721	
						; 6722	ERND2:	AR_AR*.25 LONG,		;shift everything 2 bits right.
						; 6723			MQ_MQ*.25,	;	"	"	"
U 1767, 3004,3701,7710,0302,0000,0012,0003	; 6724			SC_#, #/3	;add in correction to FE.
						; 6725		AR_AR*.5 LONG,		;now shift the final bit position.
U 3004, 3007,0301,7700,2002,0020,0027,0000	; 6726			SC_FE+SC	;total exponent correction.
U 3007, 3013,4001,0060,0000,0000,0410,0000	; 6727		BR/AR, BRX/ARX, CLR AR	;save answer in BR,BRX.
U 3013, 3024,0001,0000,2400,2001,0110,0000	; 6728		EXP_SC.MS		;get exp corr in AR.
						; 6729		ARX_AR, AR_SIGN,	;get exp into ARX 1-8.
U 3024, 3033,5441,2400,0302,1020,0016,0041	; 6730			SC_#,#/33.	;prepare to shift 3 places.
						; 6731		ARX_SHIFT,		;move exponent into ARX 1-11.
U 3033, 3034,3260,2407,0000,0020,0010,0164	; 6732			[AR]_FM[EXPMSK]	;prepare to build mask in AR.
U 3034, 3042,4001,0000,0000,0000,0110,0400	; 6733		AR0-8_#, #/400		;include AR00 in EXPMSK==>400077,,-1
						; 6734		AR_AR*BR, AD/AND,	;zero AR1-11 to make room for exp.
U 3042, 3046,3602,2004,0302,0000,0010,0043	; 6735			SC_#, #/35.
						; 6736	
						; 6737	; I am sure a few lines of code can be saved around here.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4-2
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT NORMALIZATION					

						; 6738	
U 3046, 3051,3240,2047,0000,0020,0010,0172	; 6739		[AR]_FM[T2], BR/AR	;save high word in BR, load larger exp.
U 3051, 3056,3202,2040,0000,0000,0010,0000	; 6740		AR_BR, BR/AR		;swap around so we can add.
						; 6741		AR_ARX+BR, BR/AR,	;have final exponent, check for problems.
U 3056, 3115,0612,2040,0302,0020,0010,0000	; 6742			SC_#,#/0
U 3115, 0743,0001,0000,0000,0020,0007,0000	; 6743		SH DISP			;any exponent problems ?
						; 6744	=0011	ARX_AR, SC_#, #/35.,	; no problems.
U 0743, 3176,4001,0400,0302,1000,0010,0043	; 6745			J/ENFNL1
						; 6746	ENFNL0:	ARX_AR, SC_#, #/35.,	; no problems.
U 0747, 3176,4001,0400,0302,1000,0010,0043	; 6747			J/ENFNL1
U 0753, 3117,4001,0000,0000,0000,1110,0620	; 6748		SET FLOV,  J/EEOV	; an overflow occurred.
						; 6749			
U 0757, 0775,0001,0000,0000,0000,0005,0000	; 6750		SR DISP			;floating underflow - is it real ?
						; 6751	=1101	;test SR2.
U 0775, 3117,0001,0000,0000,0000,1110,0630	; 6752		SET FXU, J/EEOV		;yes, it is a real underflow.
U 0777, 3117,4001,0000,0000,0000,1110,0620	; 6753		SET FLOV		;no, GFDV saw an overflow before.
						; 6754	
						; 6755	EEOV:	P_P AND #, #/37,	;turn off AR00.
U 3117, 0747,0001,0000,7130,3000,0110,0037	; 6756			J/ENFNL0
						; 6757	
U 3176, 3226,3312,2004,0000,0000,0010,0000	; 6758	ENFNL1:	AR_ARX*BR, AD/OR	;AR now has high word, BRX has low.
U 3226, 0532,4061,0200,0000,0021,0005,0100	; 6759		ARX_1, MQ_0.M, SR DISP	;incase negation of lower word needed.
						; 6760	=10	AC0_AR, AR_SHIFT,	;store high word,
						; 6761			ARX_BRX,	;move low word to ARX.
U 0532, 0141,3202,4600,0000,0217,1010,0000	; 6762			I FETCH, J/STD1	;prepare to store low word and exit.
						; 6763		ARX_ARX*BRX, AD/ANDCA,	; clear rounding bit.
U 0533, 2252,3002,0604,0000,0000,1610,0000	; 6764			SR_0,J/CDBLST	;negate result and store double result.
						; 6765	
U 3227, 1740,3701,0000,0302,0040,0035,0003	; 6766	ENTRY:	SC_#, #/3, GEN AR, NORM, J/ENORM; go normalize again.
						; 6767	.ENDIF/EXTEXP
						; 6768	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT TO INTEGER CONVERSION				

						; 6769	.TOC	"GFLT TO INTEGER CONVERSION"
						; 6770	.IF/EXTEXP
						;;6771	.IF/GFTCNV		;[273]
						;;6772	
						;;6773	;ETXIX routine is used when converting extended exponent data to
						;;6774	;single/double precision integers with rounding/truncation.
						;;6775	;This routine assumes that the AR/ARX contain the extended exponent
						;;6776	;data. It also assumes that the maximum exponent value + 1 of either
						;;6777	;36 or 70 (decimal) are already in the FE. This is the positive exponent
						;;6778	;maximum; the code adjusts for the fact that a negative number can have
						;;6779	;an exponent one greater than a positive number. 
						;;6780	;It uses all of the registers in the EBOX and returns 4 if the
						;;6781	;result is positive and returns 5 if the result is negative
						;;6782	;with the AR/ARX containing the double word integer. It is the
						;;6783	;responsibility of the calling routine to determine whether
						;;6784	;rounding or truncation should be performed and how many words
						;;6785	;to store.
						;;6786	
						;;6787	ETXIX:	ARX_ARX*2		;get low word*2 into ARX.
						;;6788	=0	MQ_AR, SKP AR0,		; get a positive exp in AR.
						;;6789			CALL [ISOEXP]
						;;6790		CLR AR, BR/AR		;clear extraneous bits, save exp.
						;;6791		AR0-8_#, #/200		;test for positive exp.
						;;6792		GEN AR+BR, SKP AD0,	;skip on positive exponent(sum has AD0 on).
						;;6793			AR_0.M		;so exponent test has a clean register.
						;;6794	=0	MEM/ARL IND, CLR/AR+ARX,;exponent must be positive.
						;;6795			RETURN4		;return to caller.
						;;6796		AR0-8_#, #/212, J/ET1	;start range check of positive exponent
						;;6797	
						;;6798	;At this point the exponent is in BR 1-11 and it is positive.
						;;6799	;Now we must determine if it is a small enough positive number
						;;6800	;to make the conversion to integer meaningful.
						;;6801	ET1:	GEN AR-BR, SKP AD0	;do the exponent test.
						;;6802	=0	AR_BR*4, J/ET2		;exp fits in AR0-8, now for final test!
						;;6803		SET AROV, I FETCH, J/NOP;exponent out of range.
						;;6804	ET2:	AR_AR*2			;finish moving exponent into AR0-8.
						;;6805		SC_AR0-8, GEN MQ,	;exponent to SC.
						;;6806			SKP AD0		;max neg exponent is 1 gtr than max pos exp.
						;;6807	=0
						;;6808	ET2A:	AR_MQ, GEN FE-SC,	;shift low word into ARX00-34, caller
						;;6809			SKP SCAD0,	;put max exponent+1 in FE. range check.
						;;6810			J/ET2B
						;;6811		FE_FE+1, J/ET2A		;max neg exp is 1 gtr than max pos exp.
						;;6812	=0
						;;6813	ET2B:	FE_SC, J/ET3		;save exp in FE.
						;;6814		SET AROV, I FETCH, J/NOP;exponent is too large.
						;;6815	ET3:	SC_#, #/12.		;prepare to map AR12 into AR00.
						;;6816	
						;;6817	;We now have the high word in the AR and
						;;6818	;the low word*2 in the ARX. The SC has 12 (dec) to let the
						;;6819	;shifter strip off the sign and exponent of the high word.
						;;6820		AR_SIGN, MQ_SHIFT	;put high 36 integer bits into MQ.
						;;6821		AR_ARX, BR/AR, CLR ARX	;generate low 36 integer bits and
						;;6822		AR_BR, ARX/MQ, MQ_SHIFT,;  put in MQ. High bits to ARX.
						;;6823			SC_FE-#, #/36.,	;check the size of the exponent.
						;;6824			SKP SCAD0	;if exp<36. then high result is sign.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5-1
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT TO INTEGER CONVERSION				

						;;6825	=0	GEN SC, SKP SCAD NE,	;is exponent gtr or geq to 36 ?
						;;6826			J/ET3A
						;;6827		SC_#+SC, #/37., J/ET5	;exponent less than 36.
						;;6828	=0
						;;6829	ET3A:	(AR+ARX+MQ)*2, J/ET3B	;must shift left 1 bit.
						;;6830		BRX/ARX, SC_#+SC, #/1,	;adjust exp, save low word in BRX.
						;;6831			J/ET4
						;;6832	ET3B:	BR_AR LONG, AR_ARX,	;high and low to BR!BRX
						;;6833			SC_#, #/35.,	;get a good exponent for final shifting.
						;;6834			ARX/MQ, J/ET4A	;rest of fraction to ARX.
						;;6835	ET4:	AR_ARX (AD), ARX/MQ,	;exp gtr 36. High result has integer bits.
						;;6836			MQ_SHIFT	;high result to MQ.
						;;6837		AR_MQ, ARX_SHIFT	;put integer bits into ARX.
						;;6838		BR_AR LONG, AR_ARX (AD),;now compute fraction.
						;;6839			CLR ARX		;low integer to AR, pad with zeros in ARX.
						;;6840	ET4A:	AR_BR LONG, MQ_SHIFT,	;restore integer to AR!ARX, fraction to MQ.
						;;6841			SC_#, #/35.,	;low word must have bit 0 same as high.
						;;6842			SKP AD0, RET[4]	;  and return on sign of integer.
						;;6843	=01
						;;6844	ET5:	FM[T0]_AR, AR_ARX (AD),	;sign is high 36 bit result. Save in T0.
						;;6845			ARX/MQ,		;high 36 bits of frac to AR, low 23 to ARX.
						;;6846			MQ_SHIFT,	;low integer result to MQ.
						;;6847			CALL [SHIFT]	;high half of fraction to AR.
						;;6848	
						;;6849	;Now we have the high 36 bits of mantissa in AR, the low 23 bits if mantissa
						;;6850	;in the ARX, the high 36 bit result (the sign bits) in T0 and the low 36 bit
						;;6851	;result in the MQ. Now we compute the fraction to store.
						;;6852		BR/AR, AR_ARX, CLR ARX	;high frac to BR. Now gen low fraction bits.
						;;6853		ARX_SHIFT,		;low fraction bits to ARX.
						;;6854			SC_#, #/35.	;low word must have same sign as high.
						;;6855		GEN ARX*BR, AD/OR,	;gen composite OR of fraction into 1 word.
						;;6856			MQ_AD,		;put this funny fraction in the MQ.
						;;6857			ARX/MQ		;low integer result to ARX.
						;;6858		[AR]_FM[T0], SKP AD0,	;get high result (Sign) back in AR.
						;;6859			RET[4]		;and return to caller.
						; 6860	.ENDIF/GFTCNV		;[273]
						; 6861	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT TO INTEGER CONVERSION				

						; 6862	
						; 6863	;ISOEXP will isolate the exponent in an extended exponent data word.
						; 6864	;It will return the positive representation of the exponent.
						; 6865	;Call with AR containing high order word with "SKP AR0" to do
						; 6866	;correct things with one's complemented exponent in negative numbers.
						; 6867	;It returns 1 with the positive exponent in the AR.
						; 6868	=0
U 2212, 0001,3500,2007,4000,0020,0003,0164	; 6869	ISOEXP:	[AR]_[AR]*FM[EXPMSK],AD/ANDCB,RET[1] ;isolate pos exp in AR1-11.
U 2213, 0001,2400,2007,4000,0020,0003,0164	; 6870		[AR]_[AR]*FM[EXPMSK],AD/NOR,RET[1]   ;isolate neg exp in AR1-11.
						; 6871	
						; 6872	;SGNEXT will extend the sign bit of the AR into AR1-11. Call with
						; 6873	;SKP AR0 so the correct actions are taken for negative numbers.
						; 6874	;It will do a return 1 with either ones or zeroes in AR1-11.
						; 6875	=0
U 2250, 0001,3600,2007,4000,0020,0003,0164	; 6876	SGNEXT:	[AR]_[AR]*FM[EXPMSK], AD/AND, RET[1]  ;extend 0s into AR1-11.
U 2251, 0001,2700,2007,4000,0020,0003,0164	; 6877		[AR]_[AR]*FM[EXPMSK], AD/ORCB, RET[1] ;extend ones into AR1-11.
						; 6878	
						; 6879	;OVTEST will determine if the high order word of a double integer,
						; 6880	;as stored in the AR is all sign bits, ie either it is all zeroes
						; 6881	;or all ones. The call is via "GEN AR, SKP AD NE, J/OVTEST".
						; 6882	;It assumes that the double integer is in the AR/ARX and the SC
						; 6883	;contains 35 decimal.
						; 6884	;OVTEST will store the ARX*.5 and exit if the AR is all sign bits. 
						; 6885	;It will set AROV and jump to NOP if it finds some data bits.
U 3230, 2254,3721,2000,0000,0020,5610,0000	; 6886	OVTST1:	AR_MQ, SKP AD NE		;get the sign bits from the MQ.
						; 6887	=0
U 2254, 3231,4001,4000,0000,0217,0010,0000	; 6888	OVTEST:	AR_SHIFT, I FETCH, J/OVTST2	;the high word is all zeros - ok.
U 2255, 2260,4003,0000,0000,0040,5610,0000	; 6889		GEN AR+1, SKP AD NE		;check to see if it is all ones.
U 2260, 3231,4001,4000,0000,0217,0010,0000	; 6890	=0	AR_SHIFT, I FETCH, J/OVTST2 	;this is simply a negative number.
U 2261, 0217,4001,0000,0000,0217,1110,0420	; 6891		SET AROV, I FETCH, J/NOP	;sorry, we found some data bits.
U 3231, 0217,4001,0000,0000,0000,1010,0000	; 6892	OVTST2:	AC0_AR, J/NOP			;finish the store.
						; 6893	.ENDIF/EXTEXP
						; 6894	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DATA CONVERSION INSTRUCTIONS			

						; 6895	.TOC	"GFLT DATA CONVERSION INSTRUCTIONS"
						; 6896	
						; 6897	.IF/EXTEXP
						; 6898		.DCODE
D 0021, 2501,1116				; 6899	021:	EA,	AC,	J/L-GTPI	;GSNGL--Must mate with EXTEND
D 0022, 2000,1114				; 6900		EA,		J/L-SFTE	;GDBLE
						;;6901	.IF/GFTCNV
						;;6902		EA,	B/0,	J/L-GTDI	;GDFIX
						;;6903	024:	EA,	B/2,	J/L-GTSI	;GFIX
						;;6904		EA,	B/4,	J/L-GTDR	;GDFIXR
						;;6905		EA,	B/6,	J/L-GTSR	;GFIXR
						; 6906	.ENDIF/GFTCNV
D 0027, 2000,1111				; 6907	027:	EA,		J/L-DITE	;DGFLTR
D 0030, 2000,1112				; 6908	030:	EA,		J/L-SITE	;GFLTR
D 0031, 2001,1113				; 6909		EA,		J/L-EFSC	;GFSC
						; 6910		.UCODE
						; 6911	
U 3116, 3232,3240,2007,0000,0020,0010,0165	; 6912	3116:	[AR]_FM[E1], J/L-GTSP	; -21-	GSNGL
U 3114, 3247,3260,2007,0000,0020,0010,0165	; 6913	3114:	[AR]_FM[E1], J/L-EDBL	; -22-	GDBLE
						;;6914	.IF/GFTCNV		;[273]
						;;6915	3105:	[AR]_FM[E1], J/L-GTIN	; -23-	DGFIX
						;;6916	3106:	[AR]_FM[E1], J/L-GTIN	; -24-	GFIX
						;;6917	3107:	[AR]_FM[E1], J/L-GTIN	; -25-	DGFIXR
						;;6918	3110:	[AR]_FM[E1], J/L-GTIN	; -26-	GFIXR
						; 6919	.ENDIF/GFTCNV		;[273]
U 3111, 3264,3240,2007,0000,0020,0010,0165	; 6920	3111:	[AR]_FM[E1], J/L-FLTR	; -27-	DGFLTR
U 3112, 3273,3240,2007,0000,0020,0010,0165	; 6921	3112:	[AR]_FM[E1], J/L-DFLT	; -30-	GFLTR
U 3113, 2304,3240,2007,0000,0020,0010,0165	; 6922	3113:	[AR]_FM[E1], J/L-DFSC	; -31-	GFSC
						; 6923	
						; 6924	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DATA CONVERSION INSTRUCTIONS			

U 3232, 3233,3703,0000,0000,0312,0010,0000	; 6925	L-GTSP:	VMA_AR, LOAD AR		;-21- GSNGL EDPFP TO SPFP
U 3233, 3234,3240,0003,0000,0022,0022,0100	; 6926		AR_MEM, MQ_0.S		;load high word into AR.
U 3234, 2262,3701,0000,0000,0020,5610,0000	; 6927		GEN AR, SKP AD NE	;check for zeroes.
U 2262, 0216,0001,0000,0000,0217,0010,0000	; 6928	=0	I FETCH, J/STORAC	;high word zero, store it.
U 2263, 2264,0001,0000,0000,0000,3610,0000	; 6929		VMA_VMA+1		;point to mem low word.
						; 6930	=0	ARX_AR, SKP AR0,	;save high word in ARX.
U 2264, 2212,4001,0400,0000,1000,4550,0000	; 6931			CALL [ISOEXP]	;get the excess-2000 exponent.
U 2265, 3235,0001,0040,0000,0000,0410,0000	; 6932		CLR AR, BR/AR		;exp to BR.
U 3235, 3236,4001,0000,0000,0000,0110,0220	; 6933		AR0-8_#, #/220		;largest exponent allowed is 2200.
U 3236, 2270,1102,0004,0000,0040,5510,0000	; 6934		GEN AR-BR-1, SKP AD0	;range check exponent.
U 2270, 3237,0001,0000,0000,0000,0110,0157	; 6935	=0	AR0-8_#, #/157, J/L-GTS1;do lower range check now.(actually too low)
U 2271, 0217,0001,0000,0000,0217,1110,0620	; 6936		SET FLOV, I FETCH, J/NOP;tough
U 3237, 3240,3202,2040,0000,0000,0010,0000	; 6937	L-GTS1:	BR/AR, AR_BR		;swap values around for next subtract.
U 3240, 2274,5102,0004,0000,0040,5510,0000	; 6938		GEN AR-BR, SKP AD0	;do lower range check.
U 2274, 3241,0001,0040,0000,0000,0410,0000	; 6939	=0	BR/AR, CLR AR, J/L-GTS6	;passed. 10 bit path to do last checks.
U 2275, 0217,4001,0000,0000,0217,1110,0630	; 6940		SET FXU, I FETCH, J/NOP	;too low.
U 3241, 3242,0001,0000,0000,0000,0110,0160	; 6941	L-GTS6:	AR0-8_#, #/160		;subtract 1600 to get excess 200 exp.
U 3242, 3243,3202,2040,0000,0000,0010,0000	; 6942		AR_BR, BR/AR		;swap around to do subtract.
U 3243, 3244,5102,2004,0000,0020,0010,0000	; 6943		AR_AR-BR		;got it.
U 3244, 3245,3243,5000,0000,0000,0010,0000	; 6944		AR_AR*8			;move excess-200 exponent over.
						; 6945		FE_AR0-8, AR_ARX,	;put some exponent in FE. High word to AR.
U 3245, 3246,0001,4000,2421,2013,0010,0000	; 6946			LOAD ARX	;low word to ARX.
						; 6947	;This next test determines the relative size of the exponent. If the expo-
						; 6948	;nent is less than 401 then it is a positive exponent and all will be well.
						; 6949	;If the exponent is greater than 400 (actually 700), then the exponent is
						; 6950	;really negative but bit 0 of the FE is off. To correct the sign of the
						; 6951	;exponent and to prevent undeserved FXU later because of the incorrect sign
						; 6952	;bit, we must examine the value of the exponent so as to always get the
						; 6953	;correct sign during normalization.
						; 6954		ARX_MEM, GEN FE-#,	;undeserved FXU happens when FE00 should be
U 3246, 2300,3240,0003,5030,0042,5110,0500	; 6955			#/500, SKP SCAD0;set from previous subtract of 1600.
						; 6956	=0	FE_FE+#, #/777,		;set FE00. Later add will clear it.
						; 6957			ARX_ARX*2,	;low word * 2.
U 2300, 2302,3721,0500,2031,0000,0010,0777	; 6958			J/L-GTS7	;continue.
U 2301, 2302,3721,0500,3001,0000,0010,0000	; 6959		FE_FE-1, ARX_ARX*2	;adjust FE so later add gets right exp.
						; 6960	=0
U 2302, 2250,4001,0000,0000,0000,4550,0000	; 6961	L-GTS7:	SKP AR0, CALL [SGNEXT]	;sign extend high word.
						; 6962		AR_AR*.25 LONG,		;prepare for normalization
						; 6963			FE_FE+#, #/6,	;adjust exponent.
U 2303, 1420,3703,7700,2031,0040,0035,0006	; 6964			NORM, J/SNORM	;finish up.
						; 6965	
U 3247, 3250,3703,0000,0000,0312,0010,0000	; 6966	L-EDBL:	VMA_AR, LOAD AR		;-22- GDBLE SPFP to EXTENDED EXPONENT
U 3250, 3251,3200,0003,0000,0022,0710,0003	; 6967		AR_MEM, CLR MQ
U 3251, 3252,0001,0400,0202,1000,0410,0000	; 6968		SC_EXP, ARX_AR, CLR AR	;correct the expoent, save a copy in the ARX
U 3252, 3253,4001,0007,0000,0000,1010,0165	; 6969		FM[E1]_AR		;no sticky bits here.
U 3253, 3254,0001,0000,2400,2001,0010,0200	; 6970		EXP_SC			;put the "positive" exponent back IN THE AR.
U 3254, 3255,0301,7000,0000,0020,0010,0000	; 6971		AR_AR*.5		;must move exponent into AR4-11
U 3255, 3256,3701,7000,0000,0000,0010,0000	; 6972		AR_AR*.25		;  done.
U 3256, 3257,4001,0040,0000,0000,0410,0000	; 6973		BR/AR, CLR AR		;exp to BR.
U 3257, 3260,0001,0000,0000,0000,0110,0160	; 6974		AR0-8_#, #/160		;put 1600 in the AR for exp conversion
U 3260, 3261,0602,2000,0301,0020,0010,0775	; 6975		AR_AR+BR, FE_#, #/-3	;convert exp, set initial exp correction
U 3261, 3262,0001,4007,0000,2000,1010,0172	; 6976		FM[T2]_AR, AR_ARX	;save exp for ENORM, frac to AR
U 3262, 3263,0001,0000,0000,1001,0610,0240	; 6977		EXP_SIGN.C, ARX_0.M	;get rid of exp, clear low word
						; 6978		GEN AR, SC_#, #/3, NORM,;normalize an extended exponent number
U 3263, 1740,3701,0000,0302,0040,0035,0003	; 6979			J/ENORM; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DATA CONVERSION INSTRUCTIONS			

						; 6980	
						;;6981	.IF/GFTCNV		;[273]
						;;6982	L-GTIN:	VMA_AR, LOAD AR		;23-26. fetch high word.
						;;6983		AR_MEM, MQ_0.S,		;word in AR, init MQ.
						;;6984			VMA_VMA+1	;prepare to fetch low word.
						;;6985		GEN AR, SKP AD NE	;is high word all zeroes ?
						;;6986	=0	CLR ARX, EXIT DBL	;high word zero, store zeroes.
						;;6987		LOAD ARX, B DISP	;fetch low word, call appropriate routine.
						;;6988	
						;;6989	=000	ARX_MEM, J/L-G23	;do GDP to DP integer, truncate.
						;;6990	=010	ARX_MEM, J/L-G24	;do GDP to SP integer, truncate.
						;;6991	=100	ARX_MEM, J/L-G25	;do GDP to DP integer, rounded.
						;;6992	=110	ARX_MEM, J/L-G26	;do GDP to SP integer, rounded.
						;;6993	=				;terminate this dispatch block.
						;;6994	
						;;6995	;DGFIX needs the sticky bit fix.
						;;6996	=0010
						;;6997	L-G23:	FE_#, #/70.,		;-23- DGFIX GDP to double integer, truncate.
						;;6998			CALL [ETXIX]	;do the conversion
						;;6999	=0110	EXIT DBL		;store results.
						;;7000	=0111	BR_AR LONG, AR_ARX,	;save high 2 words in BR!BRX, MSB of
						;;7001			ARX/MQ,		;fraction to AR35. Rest of fraction to ARX.
						;;7002			SC_#, #/35.,	;get fraction all together.
						;;7003			CALL [EF12A]
						;;7004	=1111	GEN AR, SKP AD NE,	;any fraction bits on ?
						;;7005			MQ_0.S		;[240]CLEAR MQ00 FOR ARX_2 MACRO.
						;;7006	=0	AR_BR LONG, J/ST2AC	;no, leave answer alone.
						;;7007		CLR AR, ARX_2		;yes, add 1 to integer part.
						;;7008		AR_AR+BR LONG, J/ST2AC	;store result.
						;;7009	
						;;7010	;GFIX needs the sticky bit fix.
						;;7011	=0010
						;;7012	L-G24:	FE_#, #/35.,		;-24- GFIX GDP to single integer, truncate.
						;;7013			CALL [ETXIX]	;do the conversion
						;;7014	=0110
						;;7015	L-GTS2:	SKP AR NE, J/OVTEST	;test for sign bits in AR and store.
						;;7016	=0111	BR_AR LONG, AR_ARX,	;save in BR!BRX.
						;;7017			ARX/MQ,		;add one to integer part of negative number
						;;7018			SC_#, #/35.,	;if fraction is not zero.
						;;7019			CALL [EF12A]
						;;7020	=1111	GEN AR, SKP AD NE,	;is fraction zero ?
						;;7021			MQ_0.S		;[240]CLEAR MQ00 FOR ARX_2 MACRO.
						;;7022	=0	AR_BR LONG, SKP AD NE,	;yes, try to store the result.
						;;7023			J/OVTEST
						;;7024		CLR AR, ARX_2		;no, add one to integer part.
						;;7025		AR_AR+BR LONG, SKP AD NE,; do the add and test that the high
						;;7026			J/OVTEST	;word is all sign bits.
						;;7027	
						;;7028	=011
						;;7029	L-G25:	FE_#, #/70.,		;-25- DGFIXR GDP to double integer, rounded.
						;;7030			CALL [ETXIX]	;do the conversion
						;;7031	=111	BR_AR LONG, CLR AR,	;save in BR!BRX, round by adding one half
						;;7032			ARX_1,		;to result. Remember that the MSB of the
						;;7033			SC_#, #/35.	;store routine expects this.
						;;7034		AR_AR+BR LONG, AD FLAGS	;fraction is on ARX35.  Do the rounding and
						;;7035	;=0	; replace SKP CRY0 with AD FLAGS. Eliminates extra word.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9-1
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DATA CONVERSION INSTRUCTIONS			

						;;7036		EXIT DBL		;  store the double result.
						;;7037	;	SET AROV, I FETCH, J/NOP;rounding caused an overflow - too bad!
						;;7038	
						;;7039	=011
						;;7040	L-G26:	FE_#, #/35.,		;-26- GFIXR GDP to single integer, rounded.
						;;7041			CALL [ETXIX]	;do the conversion.
						;;7042	=111	BR_AR LONG, CLR AR,	;save in CR!BRX, round by adding one half
						;;7043			ARX_1,		;to result. MSB of the fraction is in ARX35.
						;;7044			SC_#, #/35.	;store routine expects this.
						;;7045		AR_AR+BR LONG, SKP AD NE,;do the rounding.
						;;7046			J/OVTEST	;figure out what, if any, to store.
						; 7047	.ENDIF/GFTCNV		;[273]
						; 7048	L-FLTR:	VMA_AR, LOAD AR,	;-27- DGFLTR DP INTEGER to EDPFP
U 3264, 3265,3701,0000,0301,0312,0010,0137	; 7049			FE_#, #/137	;inital fugde factor for exp
U 3265, 1524,3200,0003,0000,0022,0022,0100	; 7050		AR_MEM, MQ_0.S		;get high word into the AR.
						; 7051	=0*	VMA_VMA+1, LOAD ARX,	;get the low word into the ARX,
U 1524, 1045,0001,0040,0000,0013,3650,0000	; 7052			BR/AR, CALL [XFERW]; and save the high word in the BR.
U 1526, 3266,3701,0500,0000,0000,0410,0000	; 7053	=1*	ARX_ARX*2, CLR AR	;ignore the sign copy.
U 3266, 3267,0001,0007,0000,0000,1010,0165	; 7054		FM[E1]_AR		;no sticky bits here.
U 3267, 3270,4001,0000,0000,0000,0110,0200	; 7055		AR0-8_#, #/200		;ENORM expects the exponent in T2.
						; 7056		FM[T2]_AR, AR_BR,	;and save it in T2.
U 3270, 3271,3202,2217,0000,2000,1010,0172	; 7057			ARX/AD, MQ_ARX	;sign to AR, high to ARX, low to MQ.
U 3271, 3272,5441,2000,0000,0020,0016,0000	; 7058		AR_SIGN 		;
U 3272, 1740,3703,0000,0000,0040,0035,0000	; 7059		GEN AR, NORM, J/ENORM	;restore high word and normalize.
						; 7060	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; EXTEXP.MIC[4,24]	15:33 8-Feb-86			GFLT DATA CONVERSION INSTRUCTIONS			

						; 7061	
						; 7062	L-DFLT:	VMA_AR, LOAD AR,	;-30- GFLTRSP INTEGER to EDPFP
U 3273, 3274,3703,0000,0301,0312,0010,0004	; 7063			FE_#, #/4	;initial fudge factor for exp.
U 3274, 3275,3200,0003,0000,0022,0710,0003	; 7064		AR_MEM, CLR MQ		;get the single precision op.
U 3275, 3276,5401,2400,0000,1020,0016,0000	; 7065		AR_SIGN, ARX_AR		;build a dummy high word of all sign.
U 3276, 3277,0001,0040,0000,0000,0410,0000	; 7066		BR/AR, CLR AR		;save sign, prepare for exponent.
U 3277, 3300,0001,0007,0000,0000,1010,0165	; 7067		FM[E1]_AR		;no sticky bits here.
U 3300, 3301,4001,0000,0000,0000,0110,0207	; 7068		AR0-8_#, #/207		;build an initial exp of 207 for ENORM
						; 7069		FM[T2]_AR, AR_BR,	;save exp for ENORM, restore sign word.
U 3301, 1740,3242,2007,0000,0040,1035,0172	; 7070			NORM, J/ENORM	;and normalize it.
						; 7071	
						; 7072	=0
						; 7073	L-DFSC:	AR_AC0, BR/AR, SKP AD0,	;-31- GFSC EDPFP SCALE
U 2304, 2212,3240,2040,0000,0020,5550,0000	; 7074			CALL [ISOEXP]	;get the exponent into the AR.
U 2305, 3302,3242,2040,0000,0000,0010,0000	; 7075		BR/AR, AR_BR		;put exp in BR, scale factor to AR.
						; 7076		AR_AR SWAP, GEN AC0,	;put scale in left half of AR.
U 3302, 2310,3240,4000,0000,3020,5610,0000	; 7077			SKP AD NE	;is high word zero ?
U 2310, 0011,0001,0000,0000,0001,0010,0170	; 7078	=0	AR+ARX+MQ_0.M, J/ST2AC	;yes, store zero as double result.
						; 7079		AR_SIGN, ARX_AR, SC_#,	;no, move sign and scale factor together.
U 2311, 3303,5441,2400,0302,1020,0016,0042	; 7080			#/34.
U 3303, 3304,4001,4000,0401,0000,0010,0000	; 7081		AR_SHIFT, CLR FE	;sign now in AR00, scale in AR 9-19.
U 3304, 3305,0001,0000,0000,1001,0010,0200	; 7082		EXP_SIGN		;scale sign is in AR00; extend it.
U 3305, 3306,4001,0000,0302,0000,0010,0010	; 7083		SC_#, #/8.		;move scale factor into AR 1-11 and
U 3306, 3307,3200,4201,0000,0020,0010,0000	; 7084		AR_SHIFT, ARX_AC1	; put the sign to left of scale factor.
U 3307, 3310,0602,2004,0000,0020,0710,0003	; 7085		AR_AR+BR, CLR MQ	;add exponent and scale factor.
U 3310, 1023,4001,0000,0000,1020,0007,0000	; 7086		SH/AR, DISP/SH0-3	;check for over and under flovs.
						; 7087	=0011
						; 7088	L-FSC2:	[AR]_[AR]*FM[EXPMSK],	;clear out non-exponent bits.
						; 7089			AD/ANDCB,	;and AR00 in the over or under flow case.
U 1023, 3311,3500,2007,4000,0020,0010,0164	; 7090			J/L-FSC3	; and continue
						; 7091	=0111	[AR]_[AR]*FM[EXPMSK],	;clear out non-exponent bits.
						; 7092			AD/ANDCB,	;
U 1027, 3311,3500,2007,4000,0020,0010,0164	; 7093			J/L-FSC3	; and continue
U 1033, 1023,0001,0000,0000,0000,1110,0620	; 7094	=1011	SET FLOV, J/L-FSC2	;you lose
U 1037, 1023,4001,0000,0000,0000,1110,0630	; 7095	=1111	SET FXU, J/L-FSC2	;ditto
						; 7096	
U 3311, 2314,3701,0507,0000,0000,1010,0172	; 7097	L-FSC3:	FM[T2]_AR, ARX_ARX*2	;save new exponent fofr ENORM.
						; 7098	=0	AR_AC0, SKP AD0,	;get the high word.
						; 7099			SC_#, #/3,	;for ENORM.
U 2314, 2250,3240,2000,0302,0020,5550,0003	; 7100			CALL [SGNEXT]	;and sign extend it for ENORM as well.
U 2315, 1740,3703,0000,0000,0040,0035,0000	; 7101		GEN AR, NORM, J/ENORM	;put the result back together.
						; 7102	
						; 7103	.ENDIF/EXTEXP
						; 7104	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; BLT.MIC[4,24]	16:34 23-May-86				BLT - Neatly Optimized					

						; 7105	.TOC	"BLT - Neatly Optimized"
						; 7106	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7107	;									;
						; 7108	;	This implementation of BLT is a complete rewrite of the		;
						; 7109	;	instruction.  BLT has been substantially optimized by splitting	;
						; 7110	;	the copy loops into three separate loops:  one for PXCT (which	;
						; 7111	;	supports only in section copying, and is clean only in section	;
						; 7112	;	zero or one), one for spraying a single word through a block	;
						; 7113	;	of memory, and one for all other cases (usually normal block	;
						; 7114	;	copies).  In all of these cases we attempt to keep the Mbox	;
						; 7115	;	busy as much as possible by starting each load on the same	;
						; 7116	;	microinstruction that completes the previous store, thus	;
						; 7117	;	eliminating a fair amount of Mbox dead time.  (Stores cannot	;
						; 7118	;	be similarly overlapped, due to the need to wait for the 	;
						; 7119	;	parity check on each load.)  We also avoid the overhead of	;
						; 7120	;	needless state register switching in the non PXCT cases.	;
						; 7121	;									;
						; 7122	;	This code gives up on the backwards BLT idea entirely, since	;
						; 7123	;	that broke many useful programs.				;
						; 7124	;									;
						; 7125	;						--QQSV			;
						; 7126	;									;
						; 7127	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7128	
						; 7129		.DCODE
D 0251, 2000,0311				; 7130	251:	EA,		J/BLT		;BLT--adjoins EXCH
						; 7131		.UCODE
						; 7132	
						; 7133	311:					;[440] Near EXCH
U 0311, 2340,3240,2240,0000,0020,0610,0000	; 7134	BLT:	BR/AR,ARL_ARL,ARR_AC0,ARX/AD	;Generate initial dest address
						; 7135	=0	BRX/ARX,MQ_AR,AR_AR-BR,ARX/AD,	;MQ_current dest address.  Get copy
U 2340, 3315,5102,2234,0302,1020,0050,0022	; 7136		    SC_#,#/18.,CALL [REPLIC]	; count-1; set up for source grab
						; 7137		BR/AR,AR_SHIFT,ARX_ARX+1 (AD),	;Get source address half, and
U 2341, 3312,4013,4240,0000,0020,1617,0407	; 7138		    GEN CRY18,SR_BLT(AC)	; bump both AC halves
U 3312, 3313,5112,2404,0000,1020,0017,0000	; 7139		AR_ARX-BR,GEN CRY18,ARX_AR	;Finish AC; keep initial source
U 3313, 3314,3721,4000,0000,2001,1010,0002	; 7140		AC0_AR,ARL_MQL,ARR_ARX		;Set AC; gen complete source addr
U 3314, 0650,0001,0040,0000,0000,7410,0000	; 7141		BR/AR,SKP P!S XCT		;MQ_source addr; test PXCT
						; 7142	;
						; 7143	;	We are now just about set up.  BR will contain the current source
						; 7144	;	address; MQ will contain the current destination address.  ARX will
						; 7145	;	have -(count-1) of words remaining to be copied; it is incremented
						; 7146	;	each time a word is copied.  Thus, the copy terminates one word
						; 7147	;	AFTER ARX becomes positive (this makes sure that we always copy at
						; 7148	;	least one word).  BRX will contain a copy of ARX that is used only
						; 7149	;	if the instruction must quit prematurely; it is updated each time
						; 7150	;	a store completes.
						; 7151	;
						; 7152	;	Now figure out which loop to use.  If the destination address -
						; 7153	;	the source address = 1, we are spraying a word through memory.
						; 7154	;
						; 7155	=00	GEN BR,LOAD VMA(EA),ARX_BRX,	;Not PXCT. Read the first word
U 0650, 1045,3202,0600,0000,0111,0050,0402	; 7156		    CALL [XFERW]
U 0651, 1701,1723,2000,0000,0020,1610,0307	; 7157		AR_MQ-1,SR_BLT(PXCT SRC),J/BLTPX;PXCT. Back up dest vma, set up SR
U 0652, 2344,1122,0000,0000,0040,5610,0000	; 7158		GEN MQ-BR-1,SKP AD NZ		;Got word. Are we spraying memory?
						; 7159	=
U 2344, 3316,3723,0000,0000,0111,0010,0042	; 7160	=0	GEN MQ,STORE VMA(EA),J/SPRAY	;Yes. Start first store; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; BLT.MIC[4,24]	16:34 23-May-86				BLT - Neatly Optimized					

U 2345, 2360,3723,0000,0000,0131,7610,0042	; 7161		GEN MQ,STORE VMA(EA),SKP AC REF	;No. Test for AC reference
U 2360, 2364,4001,0000,0000,0020,7610,0000	; 7162	=0	SKP AC REF,J/BLTLOD		;Load from memory. Test store
						; 7163		ARX_ARX+1,SIGNS DISP,TIME/3T,	;Load from ACs. Leave SR alone and
U 2361, 1047,4001,0600,0000,0020,0032,0000	; 7164		    J/BLTLUP			; test for completion
						; 7165	;
						; 7166	=0
						; 7167	BLTLOD:	SR_BLT,ARX_ARX+1,SIGNS DISP,	;All references are to memory. Allow
U 2364, 1047,4001,0600,0000,0020,1632,0007	; 7168		    TIME/3T,J/BLTLUP		; shadow AC reference. Test if done
						; 7169	XBLTGO:	ARX_ARX+1,SIGNS DISP,TIME/3T,	;Store to ACs. Leave SR alone. Test
U 2365, 1047,4001,0600,0000,0020,0032,0000	; 7170		    J/BLTLUP			; completion
						; 7171	;
						; 7172	;	REPLIC--Subroutine to swap the ARX and the BRX, and to replicate
						; 7173	;	ARR in ARL.  It just happened to be useful.  Return 1.
						; 7174	;
						; 7175	REPLIC:	BRX/ARX,ARX_BRX,ARL_ARR,ARR_ARR,;Swap and replicate
U 3315, 0001,3202,0620,0000,3000,0603,0004	; 7176		    RETURN [1]
						; 7177	;
						; 7178	;	The main copy loop.  The cache is as overlapped with the Ebox
						; 7179	;	as possible.  (Recall that we cannot immediately store data fresh
						; 7180	;	from memory; the AR_MEM forces an extra cycle of delay for parity
						; 7181	;	checking.)  The SR has been used to set up the proper VMA context
						; 7182	;	for shadow AC reference, so we can use the same loop even if ACs
						; 7183	;	are involved.
						; 7184	;
						; 7185	=0
						; 7186	BLSTOR:	MQ_MQ+1,VMA/AD,STORE,ARX_ARX+1,	;Increment dest VMA and count,
U 2370, 1047,4021,0610,0000,0336,0732,0003	; 7187		    SIGNS DISP,TIME/3T		; start store. Are we done?
						; 7188	=0111
U 1047, 0217,4001,0003,0000,0217,0010,0000	; 7189	BLTLUP:	FIN STORE,I FETCH,J/NOP		;Count expired. All done
						; 7190		FIN STORE,BRX/ARX,AR_BR+1,	;More to do. Increment source VMA,
U 1057, 2374,4642,2023,0000,0332,7010,0000	; 7191		    VMA/AD,LOAD AR,SKP INTRPT	; start the fetch, and test int
U 2374, 2370,3200,0043,0000,0022,0010,0000	; 7192	=0	BR/AR,AR_MEM,J/BLSTOR		;No int. Wait for load and loop
U 2375, 0340,3240,0003,0000,0022,0005,0000	; 7193		AR_MEM,SR DISP,J/CLEAN		;Interrupted. Freeze BLT or XBLT
						; 7194	;
						; 7195	;	If we are spraying memory, we can use VMA_VMA+1 which preserves
						; 7196	;	globality.  Thus it does not matter whether ACs are in use here.
						; 7197	;	Indeed, once it gets started, PXCT can use this loop too.
						; 7198	;
U 3316, 1067,4021,0600,0000,0020,0032,0000	; 7199	SPRAY:	ARX_ARX+1,SIGNS DISP,TIME/3T	;Copying only one word?
						; 7200	=0111
U 1067, 0217,4001,0003,0000,0217,0010,0000	; 7201	SPRAYL:	FIN STORE,I FETCH,J/NOP		;Could be. Spray done
U 1077, 2400,0001,0000,0000,0000,7010,0000	; 7202		SKP INTRPT			;More to do. Test interrupt
						; 7203	=0	FIN STORE,BRX/ARX,VMA_VMA+1,	;No interrupt. Proceed to next
						; 7204		    STORE,ARX_ARX+1,SIGNS DISP,	; store, increment count, and
U 2400, 1067,4021,0623,0000,0036,3632,0000	; 7205		    TIME/3T,J/SPRAYL		; test completion
U 2401, 0340,4001,0023,0000,0002,0005,0000	; 7206		MEM_AR,BRX/ARX,SR DISP,J/CLEAN	;Interrupted. Freeze and clean up
						; 7207	;
						; 7208	;	Finally, the PXCT case.  We will optimize spraying memory (at
						; 7209	;	this writing, TOPS-10 still uses BLT to do that in some cases).
						; 7210	;	Note that this can be used only for copying within a section
						; 7211	;	(usually zero).  The state register must be swapped at each
						; 7212	;	operation (unless we are spraying memory) to activate the proper
						; 7213	;	PXCT bits.  SR bit 0 is off in order to force AC context.
						; 7214	;
						; 7215	=0*
						; 7216	BLTPX:	MQ_AR,VMA_BR,LOAD AR,ARX_BRX,	;Set up dest addr and count, do; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-2
; BLT.MIC[4,24]	16:34 23-May-86				BLT - Neatly Optimized					

U 1701, 1045,3202,0610,0000,1312,1650,0107	; 7217		    SR_BLT(PXCT DST),CALL [XFERW];first load, and shuffle SR
U 1703, 2402,5122,0004,0000,0040,5610,0000	; 7218		GEN MQ-BR,SKP AD NZ		;Is this a single word spray?
						; 7219	=0	VMA_MQ+1,STORE,ARX_ARX+1,	;Yes. Store first word here
U 2402, 1067,4023,0600,0000,0336,0032,0000	; 7220		    SIGNS DISP,TIME/3T,J/SPRAYL	; and use standard loop
						; 7221	PXPUT:	MQ_MQ+1,VMA/AD,STORE,ARX_ARX+1,	;Bump dest VMA and count, start
U 2403, 1127,4023,0610,0000,0336,0732,0003	; 7222		    SIGNS DISP,TIME/3T		; store, and test completion
U 1127, 0217,4001,0003,0000,0217,0010,0000	; 7223	=0111	FIN STORE,I FETCH,J/NOP		;All done. Blow out of here
U 1137, 3317,4001,0000,0000,0000,1610,0307	; 7224		SR_BLT(PXCT SRC)		;More to do. Do the SR shuffle
						; 7225		FIN STORE,BRX/ARX,AR_BR+1,	;Terminate store, freeze count, tick
						; 7226		    INH CRY18,VMA/AD,LOAD AR,	; source VMA, start load,
U 3317, 2410,4662,2023,0000,0332,7011,0000	; 7227		    SKP INTRPT			; and test for interrupt
						; 7228	=0	BR/AR,AR_MEM,SR_BLT(PXCT DST),	;No interrupt. Wait for load and
U 2410, 2403,3240,0043,0000,0022,1610,0107	; 7229		    J/PXPUT			; swap state register
U 2411, 0347,3240,0003,0000,0022,0010,0000	; 7230		AR_MEM,J/BLTFIX			;Interrupt. Common fixup code
						; 7231	;
						; 7232	;	If we get a page fault or an interrupt in the middle of this we
						; 7233	;	end up here.  The BRX keeps an accurate count of the actual
						; 7234	;	transfers complete.  We must subtract one from it, and add the
						; 7235	;	result to both halves of AC0.  ARX is set to -1 on the way in.
						; 7236	;
						; 7237	=0
U 2412, 3315,0602,6000,0000,0020,0050,0000	; 7238	BLTPGF:	AR_ARX+BRX,CALL [REPLIC]	;Set up both halves of AR
U 2413, 3775,0600,2000,4000,0020,0011,0000	; 7239		AR_AR+FM[AC0],INH CRY18,J/PGFAC0;Update AC0, then fault or int
						; 7240	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; BLT.MIC[4,24]	16:34 23-May-86				XBLT--Also Neatly Modernized				

						; 7241	.TOC	"XBLT--Also Neatly Modernized"
						; 7242	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7243	;									;
						; 7244	;	This XBLT rewrite makes use of what we learned when we up-	;
						; 7245	;	graded BLT--indeed, it shares code in a couple of cases.	;
						; 7246	;	Once again, we split this into separate loops, depending	;
						; 7247	;	upon the copy circumstances.  If we are copying forward (the	;
						; 7248	;	most common case), we distinguish the core clearing case, the	;
						; 7249	;	normal copy, and PXCT that does not clear core, using the BLT	;
						; 7250	;	code for the first two cases.  If we are copying backward, we	;
						; 7251	;	do not attempt to optimize clearing core, and there is no need	;
						; 7252	;	for a separate PXCT loop.  As a result, we have only one loop	;
						; 7253	;	for that case.							;
						; 7254	;									;
						; 7255	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7256	
						; 7257	;
						; 7258	;	The dispatch for this instruction has been rewritten so that
						; 7259	;	EXTEND special cases op code 20.  As a result, we no longer
						; 7260	;	need special DRAM for this instruction.  We get here with
						; 7261	;
						; 7262	;	ARX_AC0,SKP AD NZ		;Get copy count. Anything to do?
						; 7263	;
						; 7264	=0
U 2430, 0217,4001,0000,0000,0217,0010,0000	; 7265	XBLT:	I FETCH,J/NOP			;No. This is easy
U 2431, 3320,3200,2021,0000,0020,0010,0000	; 7266		BRX/ARX,AR_AC1			;Yes. Fetch source address
U 3320, 3321,3200,4244,0000,2020,1610,0216	; 7267		BR/AR,AR_ARX,ARX_AC2,SR_XBLT(SRC);Save it, grab destination address
U 3321, 3322,0602,2007,0000,0020,1010,0166	; 7268		FM[T0]_AR,AR_AR+BR		;Save initial count; gen and save
U 3322, 3323,0622,6011,0000,2020,1010,0000	; 7269		AC1_AR,AR_ARX+BRX,MQ_ARX	;final source addr; MQ_dest address
U 3323, 3324,5162,0604,0000,0021,1010,0030	; 7270		AC2_AR,AR_0.M,ARX_-BRX		;Save final dest addr
						; 7271		AC0_AR,ARX_ARX+1,SIGNS DISP,	;Clear final count; set up internal
U 3324, 1147,4001,0600,0000,0020,1032,0000	; 7272		    TIME/3T			; count. Going forward or backward?
U 1147, 1227,2502,0600,0000,0000,0010,0000	; 7273	=0111	ARX_BRX COMP,J/BLBACK		;Backward. Fix internal count first
						; 7274		BRX/ARX,VMA_BR,LOAD AR,		;Forward. Get first word
U 1157, 3325,3202,0020,0000,0312,1610,0316	; 7275		    SR_XBLT(DST)		; and switch SR
U 3325, 2432,1122,0000,0000,0040,5610,0000	; 7276		GEN MQ-BR-1,SKP AD NZ		;Are we spraying memory?
U 2432, 3330,3240,0003,0000,0022,0010,0000	; 7277	=0	AR_MEM,J/XSPRAY			;Yes. Wait; then start spray
U 2433, 2434,3200,0003,0000,0022,7410,0000	; 7278		AR_MEM,SKP P!S XCT		;No. Is this PXCT?
U 2434, 2365,3723,0000,0000,0316,0010,0000	; 7279	=0	VMA_MQ,STORE,STORE,J/XBLTGO	;No. Store first word and loop
U 2435, 3326,1721,0010,0000,0020,0710,0003	; 7280		MQ_MQ-1				;Yes. Back up to get correct start
						; 7281	;
						; 7282	;	The PXCT case.  As with BLT, this requires state register swapping.
						; 7283	;
						; 7284	XBLPX:	MQ_MQ+1,VMA/AD,STORE,ARX_ARX+1,	;Increment dest pointer, store word
U 3326, 1167,4021,0610,0000,0336,0732,0003	; 7285		    SIGNS DISP,TIME/3T		;Are we done yet?
U 1167, 0217,4001,0003,0000,0217,0010,0000	; 7286	=0111	FIN STORE,I FETCH,J/NOP		;Yes. Leave now
U 1177, 3327,0001,0000,0000,0000,1610,0216	; 7287		SR_XBLT(SRC)			;No. Switch state register
						; 7288		FIN STORE,BRX/ARX,AR_BR+1,	;Terminate store, tick source, and
U 3327, 2442,4662,2023,0000,0332,7010,0000	; 7289		    VMA/AD,LOAD AR,SKP INTRPT	; start next load. Interrupted?
						; 7290	=0	BR/AR,AR_MEM,SR_XBLT(DST),	;No. Wait for load and swap SR
U 2442, 3326,3240,0043,0000,0022,1610,0316	; 7291		    J/XBLPX
U 2443, 0356,3240,0003,0000,0022,0010,0000	; 7292		AR_MEM,J/XBLFIX			;Yes. Clean up, then interrupt
						; 7293	;
						; 7294	;	Spray a word through memory.  Get it started.
						; 7295	;
U 3330, 3316,3723,0000,0000,0316,0010,0000	; 7296	XSPRAY:	VMA_MQ,STORE,J/SPRAY		;Start spray properly for XBLT; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-1
; BLT.MIC[4,24]	16:34 23-May-86				XBLT--Also Neatly Modernized				

						; 7297	;
						; 7298	;	Copy is going backwards.  We must spend a cycle to generate a -1
						; 7299	;	(since there is no way to generate something like AR_BR-1); the
						; 7300	;	extra cycle gives us time to swap the state register.  Thus there
						; 7301	;	is no need for special PXCT code.  We could make a backwards
						; 7302	;	memory spray run faster, but it doesn't seem worth the bother.
						; 7303	;	Note that both source and destination start out at one greater
						; 7304	;	than the actual first address used, so we do not need special
						; 7305	;	startup code.
						; 7306	;
						; 7307	BACLUP:	MQ_MQ-1,VMA/AD,STORE,		;Decrement destination pointer and
U 3331, 1227,1721,0010,0000,0336,0732,0003	; 7308		    SIGNS DISP,TIME/3T		; store word. Are we done?
						; 7309	=0111
						; 7310	BLBACK:	MEM_AR,BRX/ARX,ARX_1S,		;More to do. Keep count and set to
U 1227, 3332,2341,0223,0000,0002,1610,0216	; 7311		    SR_XBLT(SRC),J/BACKLD	; decrement count and source addr
U 1237, 0217,4001,0003,0000,0217,0010,0000	; 7312		FIN STORE, I FETCH,J/NOP	;All done. Get out
						; 7313	;
						; 7314	BACKLD:	AR_ARX+BR,VMA/AD,LOAD AR,	;Decrement pointer, fetch next word
U 3332, 2450,0612,2604,0000,0332,7010,0000	; 7315		    ARX_ARX+BRX,TIME/3T,SKP INTRPT; Decrement count. Interrupted?
U 2450, 3331,3240,0043,0000,0022,1610,0316	; 7316	=0	BR/AR,AR_MEM,SR_XBLT(DST),J/BACLUP;No. Wait; then loop on
U 2451, 0356,3240,0003,0000,0022,0010,0000	; 7317		AR_MEM,J/XBLFIX			;Yes. Clean up before taking it
						; 7318	;
						; 7319	;	XBLT freeze comes here.  If we have an interrupt, we must restore
						; 7320	;	all three ACs to a usable state.  T0 tells which direction we are
						; 7321	;	going (the restore count must be generated differently for each
						; 7322	;	direction).
						; 7323	;
						; 7324	;	GEN FM[T0],SKP AD0		;Which way are we copying?
						; 7325	=0
U 2452, 3337,2301,0200,0000,0000,0010,0000	; 7326	XBLFRZ:	ARX_1S,J/FORSUB			;Forward. Must subtract one
U 2453, 3333,4642,6000,0000,0020,0010,0000	; 7327		AR_BRX+1			;Backward. Add one to count
U 3333, 3334,0600,2441,0000,1020,1610,0000	; 7328	FORFRZ:	BR/AR,ARX_AR,AR_AR+FM[AC1],SR_0	;Keep count. Adjust source addr
U 3334, 3335,0610,0204,0000,0020,0010,0000	; 7329		ARX_ARX+FM[AC2]			;Adjust dest addr
U 3335, 3336,4001,4001,0000,2000,1010,0000	; 7330		AC1_AR,AR_ARX			;Restore source
U 3336, 3775,5162,2004,0000,0020,1010,0000	; 7331		AC2_AR,AR_-BR,J/PGFAC0		;Restore dest and count. Done
						; 7332	;
U 3337, 3333,0602,6000,0000,0020,0010,0000	; 7333	FORSUB:	AR_ARX+BRX,J/FORFRZ		;Subtract one and rejoin; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  ILDB, LDB			

						; 7334		.TOC	"Single Byte Instructions:  ILDB, LDB"
						; 7335	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7336	;									;
						; 7337	;	The following code represents a complete overhauling of the	;
						; 7338	;	byte oriented PDP-10 instructions.  These instructions have	;
						; 7339	;	been reworked with one and two word global byte pointers in	;
						; 7340	;	mind.  Special emphasis has been placed on high speed oper-	;
						; 7341	;	ation of the one word byte pointers, even where	that has meant	;
						; 7342	;	spending a substantial amount of CRAM; TWGs, by contrast,	;
						; 7343	;	have just been made to work.					;
						; 7344	;									;
						; 7345	;	The approach used for OWLs has been to minimize the amount	;
						; 7346	;	of computation that is not overlapped with memory reference.	;
						; 7347	;	This has been done by carefully initializing the SC and FE	;
						; 7348	;	in such a manner that the next shift count can be computed	;
						; 7349	;	while the current shift is taking place.  The OWG code dis-	;
						; 7350	;	patches into CRAM tables which set up these counts.  This	;
						; 7351	;	requires a lot of CRAM (one word for each possible OWG for	;
						; 7352	;	both loading and depositing bytes), but it eliminates the	;
						; 7353	;	requirement for a memory access to look up that information	;
						; 7354	;	in the EPT.							;
						; 7355	;									;
						; 7356	;						--QQSV			;
						; 7357	;									;
						; 7358	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7359	
						; 7360	;
						; 7361	;	ILDB--Increment a byte pointer (in memory), then load the byte
						; 7362	;	it specifies into AC.
						; 7363	;	LDB--Load byte specified by byte pointer into AC.
						; 7364	;	The constraints immediately below are necessary to make IBP
						; 7365	;	work.
						; 7366	;
						; 7367		.DCODE
D 0134, 6500,0400				; 7368	134:	RW,	AC,	J/ILDB		;ILDB
D 0135, 4500,0404				; 7369		R,	AC,	J/LDB		;LDB--No write test
						; 7370		.UCODE
						; 7371	
						; 7372	=0****000000
						; 7373	ILDB:	ARX_AR,SC_P-#,#/37.,		;Save word for later dispatch
U 0400, 2042,4001,0400,5132,1020,0074,0045	; 7374			BYTE DISP,CALL [INCRBP]	; Test for OWG, increment BP
						; 7375	=100
						; 7376	LDB:	MEM_AR,ARX_AR,			;Await possible pointer store
U 0404, 0414,4001,0403,5132,1022,0034,0045	; 7377			SC_P-#,#/37.,BYTE DISP	; Save P; split OWL, OWG, TWG
						; 7378	=1100	GEN AR,EXT BYTE READ,SC_FE#,	;An OWG. Start reading the word
U 0414, 1253,3703,0000,0002,1131,0007,0620	; 7379			AR0-3 DISP,J/OWGLDB	; Split the low and high OWGs
						; 7380		MEM_AR,SET FPD,FE_S,		;A simple OWL. Save S and unwind
U 0415, 1474,2301,0003,2411,0002,1176,0100	; 7381			EA MOD DISP,CALL [LDEA]	; byte pointer EA
						; 7382		GEN AR,EXT BYTE READ,SC_FE#,	;An OWG (bit 12 is irrelevant)
U 0416, 1253,3703,0000,0002,1131,0007,0620	; 7383			AR0-3 DISP,J/OWGLDB	; Split the low and high OWGs
U 0417, 0434,4001,0003,0000,0002,7510,0000	; 7384		MEM_AR,SKP -VMA SEC0		;A TWG, maybe. Not in section 0
						; 7385	=11100	FE_S,SET FPD,
U 0434, 1474,2341,0000,2411,0000,1176,0100	; 7386			EA MOD DISP,CALL [LDEA]	;No TWGs in section 0 (treat as OWL)
U 0435, 0436,4001,0000,2411,0011,3610,0610	; 7387		FE_S,READ BP2			;Real TWG. Treat as global indirect
U 0436, 0720,4001,0000,0000,0000,1150,0100	; 7388		SET FPD,CALL [LEAIND]
						; 7389	=11111	FIN LOAD,SC_#-SC,#/-1,		;Wait for byte word. SC = 36-(P+S); KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  ILDB, LDB			

U 0437, 2454,3200,0003,5302,0237,4710,0777	; 7390			SKP SC0,I FETCH		; Does byte go off the top?
						; 7391	=
						; 7392	=0	CLR FPD,ARX_AR,AR_0S,SC_FE+SC,	;Yes. Byte is at top of word; try
U 2454, 0014,3401,2400,2002,1020,5114,0000	; 7393			SKP SCAD0,J/SHFLOD	; to truncate it. Test if P off top
						; 7394	OWGLOD:	CLR FPD,ARX_SHIFT,AR_0S,	;Normal byte. Put at top of ARX;
U 2455, 0014,3401,2400,0002,0000,0014,0000	; 7395			SC_FE#,J/SHFLOD		; SC = S. Set for final shift
						; 7396	;
						; 7397	;	Load byte from an OWG.  Split P&S in range 45-57 octal (in which
						; 7398	;	case we optimize for a byte size of 6) or 60-77 (optimize for size
						; 7399	;	7.)  (Unfortunately, we can't reasonably optimize for size 8 bytes,
						; 7400	;	as they run from 54 to 60, thus including both ranges.)  The idea
						; 7401	;	here is to set up the shift counts that will be required for the
						; 7402	;	actual byte.  Thus, SC_36.-(P+S) and FE_S.
						; 7403	;
						; 7404	=1011
U 1253, 0040,0001,0000,0301,0020,0007,0006	; 7405	OWGLDB:	FE_#,#/6,SH DISP,J/OWGLOW	;Range is 45-57. Assume size 6
U 1257, 0060,0001,0000,0301,0020,0007,0007	; 7406		FE_#,#/7,SH DISP,J/OWGHIG	;Range is 60-77. Assume size 7
						; 7407	;
						; 7408	=00000
						; 7409	OWGLOW:					;Dummy label (40-44 are OWLs)
						; 7410	=00101	FIN LOAD,I FETCH,CLR FPD,	;45 S=6, P=36 (bad). Clear the AC
U 0045, 0014,3240,0003,0302,0237,0514,0044	; 7411		    CLR ARX,SC_#,#/36.,J/SHFLOD	;
U 0046, 2455,3240,0003,0402,0237,0010,0000	; 7412		FIN LOAD,I FETCH,CLR SC,J/OWGLOD;46 S=6, P=30
U 0047, 2455,3200,0003,0302,0237,0010,0006	; 7413		FIN LOAD,I FETCH,SC_#,#/6,J/OWGLOD;47 S=6, P=24
U 0050, 2455,3200,0003,0302,0237,0010,0014	; 7414		FIN LOAD,I FETCH,SC_#,#/12.,J/OWGLOD;50 S=6, P=18
U 0051, 2455,3200,0003,0302,0237,0010,0022	; 7415		FIN LOAD,I FETCH,SC_#,#/18.,J/OWGLOD;51 S=6, P=12
U 0052, 2455,3200,0003,0302,0237,0010,0030	; 7416		FIN LOAD,I FETCH,SC_#,#/24.,J/OWGLOD;52 S=6, P=6
U 0053, 2455,3200,0003,0302,0237,0010,0036	; 7417		FIN LOAD,I FETCH,SC_#,#/30.,J/OWGLOD;53 S=6, P=0
						; 7418	
						; 7419		FIN LOAD,I FETCH,CLR FPD,	;54 S=8, P=36 (bad). Clear AC with
U 0054, 0014,3240,0003,0302,0237,0514,0044	; 7420		    CLR ARX,SC_#,#/36.,J/SHFLOD	; later shift
U 0055, 3340,0001,0000,0402,0000,0010,0000	; 7421		CLR SC,J/SIZE8L			;55 S=8, P=28. Correct the size
U 0056, 3340,0001,0000,0302,0000,0010,0010	; 7422		SC_#,#/8,J/SIZE8L		;56 S=8, P=20
U 0057, 3340,0001,0000,0302,0000,0010,0020	; 7423		SC_#,#/16.,J/SIZE8L		;57 S=8, P=12
U 0060, 3340,4001,0000,0302,0000,0010,0030	; 7424	OWGHIG:	SC_#,#/24.,J/SIZE8L		;60 S=8, P=4
						; 7425	
						; 7426		FIN LOAD,I FETCH,CLR FPD,	;61 S=7, P=36 (bad). Clear AC with
U 0061, 0014,3240,0003,0302,0237,0514,0044	; 7427		    CLR ARX,SC_#,#/36.,J/SHFLOD	; later shift
U 0062, 2455,3240,0003,0402,0237,0010,0000	; 7428		FIN LOAD,I FETCH,CLR SC,J/OWGLOD;62 S=7, P=29. Ready to shift
U 0063, 2455,3240,0003,0302,0237,0010,0007	; 7429		FIN LOAD,I FETCH,SC_#,#/7,J/OWGLOD;63 S=7, P=22
U 0064, 2455,3240,0003,0302,0237,0010,0016	; 7430		FIN LOAD,I FETCH,SC_#,#/14.,J/OWGLOD;64 S=7, P=15
U 0065, 2455,3240,0003,0302,0237,0010,0025	; 7431		FIN LOAD,I FETCH,SC_#,#/21.,J/OWGLOD;65 S=7, P=8
U 0066, 2455,3240,0003,0302,0237,0010,0034	; 7432		FIN LOAD,I FETCH,SC_#,#/28.,J/OWGLOD;66 S=7, P=1
						; 7433	
						; 7434		FIN LOAD,I FETCH,CLR FPD,	;67 S=9, P=36 (bad). Clear the AC
U 0067, 0014,3240,0003,0302,0237,0514,0044	; 7435		    CLR ARX,SC_#,#/36.,J/SHFLOD
U 0070, 3341,4001,0000,0402,0000,0010,0000	; 7436		CLR SC,J/SIZE9L			;70 S=9, P=27. Correct size
U 0071, 3341,0001,0000,0302,0000,0010,0011	; 7437		SC_#,#/9,J/SIZE9L		;71 S=9, P=18
U 0072, 3341,0001,0000,0302,0000,0010,0022	; 7438		SC_#,#/18.,J/SIZE9L		;72 S=9, P=9
U 0073, 3341,0001,0000,0302,0000,0010,0033	; 7439		SC_#,#/27.,J/SIZE9L		;73 S=9, P=0
						; 7440	
						; 7441		FIN LOAD,I FETCH,CLR FPD,	;74 S=18, P=36 (bad). Clear AC
U 0074, 0014,3240,0003,0302,0237,0514,0044	; 7442		    CLR ARX,SC_#,#/36.,J/SHFLOD
U 0075, 0502,3240,0003,0000,0237,0014,0000	; 7443		FIN LOAD,I FETCH,CLR FPD,J/HLRZ	;75 S=18, P=18. This is HLRZ, folks
U 0076, 0200,3240,0003,0000,0237,0014,0000	; 7444		FIN LOAD,I FETCH,CLR FPD,J/HRRZ	;76 S=18, P=0. Same as HRRZ
						; 7445	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-2
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  ILDB, LDB			

U 0077, 1457,3200,0003,0000,0022,0010,0000	; 7446		AR_MEM,J/ILLOWG			;77 Illegal. Force UUO
						; 7447	;
U 3340, 2455,3240,0003,0301,0237,0010,0010	; 7448	SIZE8L:	FIN LOAD,I FETCH,FE_#,#/8,J/OWGLOD;Fix up all size 8 bytes
U 3341, 2455,3200,0003,0301,0237,0010,0011	; 7449	SIZE9L:	FIN LOAD,I FETCH,FE_#,#/9,J/OWGLOD;Do the same for size 9
						; 7450	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  DPB, IDPB			

						; 7451		.TOC	"Single Byte Instructions:  DPB, IDPB"
						; 7452	;
						; 7453	;	IDPB--Increment a byte pointer (in memory), then store the rightmost
						; 7454	;	bits of the AC into the byte it specifies.
						; 7455	;	DPB--Store the rightmost bits of the AC into byte specified by
						; 7456	;	pointer.
						; 7457	;	The constraints immediately below are necessary to make IBP
						; 7458	;	work.
						; 7459	;
						; 7460		.DCODE
D 0136, 6601,0500				; 7461	136:	RW,	M,	J/IDPB		;IDPB
D 0137, 4601,0504				; 7462		R,	M,	J/DPB		;DPB--No write test if no increment
						; 7463		.UCODE
						; 7464	
						; 7465	=0****000000
						; 7466	IDPB:	ARX_AR,SC_P-#,#/37.,BYTE DISP,	;Save for dispatch later, test
U 0500, 2042,4001,0400,5132,1020,0074,0045	; 7467			CALL [INCRBP]		; for OWG, increment pointer
						; 7468	=100
						; 7469	DPB:	MEM_AR,ARX_AR,SC_P-#,#/37.,	;Await possible pointer store
U 0504, 0514,0001,0403,5132,1022,0034,0045	; 7470			BYTE DISP		; Save P; test OWL, OWG, TWG
						; 7471	=1100	GEN AR,EXT BYTE RPW,SC_FE#,	;An OWG. Start byte read; SC_2
U 0514, 1273,3701,0000,0002,1131,0007,0760	; 7472			AR0-3 DISP,J/OWGDPB	; Split into OWG groups
						; 7473		MEM_AR,SET FPD,FE_-S,		;An OWL. Save byte size
U 0515, 1554,2301,0003,5411,0002,1136,0100	; 7474			EA MOD DISP,J/DPEA	; and compute byte word address
						; 7475		GEN AR,EXT BYTE RPW,SC_FE#,	;An OWG. See above
U 0516, 1273,3701,0000,0002,1131,0007,0760	; 7476			AR0-3 DISP,J/OWGDPB	; (Bit 12 is irrelevant here)
U 0517, 2460,4001,0003,0000,0002,7510,0000	; 7477		MEM_AR,SKP -VMA SEC0,J/DEPTWG	;Maybe a TWG. Never in section 0
						; 7478	336:					;Constrain for parity (see DPEA)
U 0336, 3343,3200,0010,1401,0020,0713,0003	; 7479	GUDSIZ:	FE_-SC-1,SC_FE,MQ_FM[AC0],J/DEPBYT;Copy byte to MQ; FE_36-P, SC_-S
U 0337, 0336,0001,0000,2301,0000,0010,0001	; 7480	337:	FE_#+SC,#/1,J/GUDSIZ		;Size too large. Force to 36-P
						; 7481	=
						; 7482	=0
						; 7483	DEPTWG:	MEM_AR,SET FPD,FE_-S,		;No TWGs allowed in section 0
U 2460, 1554,2301,0003,5411,0002,1136,0100	; 7484			EA MOD DISP,J/DPEA
U 2461, 3342,4001,0000,5411,0011,3610,0610	; 7485		FE_-S,READ BP2			;A TWG. Start reading second word
U 3342, 0770,0001,0000,0000,0000,1110,0100	; 7486		SET FPD,J/DEAIND		;And dive into indirection loop
						; 7487	;
						; 7488	;	At this point, we have FE = 36-P and SC = -S with memory being
						; 7489	;	loaded into both AR and ARX.  Also, both S and P have been forced
						; 7490	;	into the range 0:36.  The deposit is done with three shifts:
						; 7491	;
						; 7492	;	Shift 1:  AR and ARX have memory; shift count = 36-P
						; 7493	;	Shift 2:  AR has byte to deposit, ARX has previous shift;
						; 7494	;		  shift count = 36-S
						; 7495	;	Shift 3:  AR and ARX have previous shift; shift count = P+S
						; 7496	;
						; 7497	DEPBYT:	AR_MEM,ARX_MEM,TIME/3T,		;Wait for memory load
U 3343, 3344,3240,0003,2301,0022,0013,0044	; 7498			SC_FE,FE_#+SC,#/36.	;SC_36-P, FE_36-S
						; 7499	DEPOWG:	AR_MQ,ARX_SHIFT,		;Fetch byte, do first shift
U 3344, 3345,3723,2400,5301,0000,0013,0110	; 7500			SC_FE,FE_#-SC,#/72.	;SC_36-S, FE_72-(36-P) = 36+P
U 3345, 3346,4001,4400,5002,0000,0010,0000	; 7501		AR_SHIFT,ARX_SHIFT,SC_FE-SC	;Next shift; SC_(36+P)-(36-S) = P+S
						; 7502	RELMEM: AR_SHIFT,STORE,CLR FPD,
U 3346, 0016,0001,4000,0000,0016,1614,0000	; 7503			SR_0,J/STMEM		;Last shift; store and clear FPD
						; 7504	;
						; 7505	;	Deposit byte with an OWG.  Once again, P&S gets split into the
						; 7506	;	ranges 45-57 octal (optimized for size 6) and 60-77 (optimized; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-1
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  DPB, IDPB			

						; 7507	;	for size 7).  In addition to setting SC to 36-P and FE to 36-S,
						; 7508	;	this code also copies AC to MQ.  Since MQ_FM[] uses the # field,
						; 7509	;	this is accomplished by reading the AC into ARX and then copying
						; 7510	;	it to the MQ just before the cache can step on ARX with the
						; 7511	;	byte data.  The timing for this is a tad hairy, but it seems to
						; 7512	;	work.
						; 7513	;
						; 7514	=1011
						; 7515	OWGDPB:	ARX_FM[AC0],FE_#,#/30.,SH DISP,	;Low range OWG. Assume size 6
U 1273, 0240,3200,0200,0301,0020,0007,0036	; 7516			TIME/3T,J/ODLOW		;Fetch byte to store
						; 7517		ARX_FM[AC0],FE_#,#/29.,SH DISP,	;High range. Assume size 7
U 1277, 0260,3240,0200,0301,0020,0007,0035	; 7518			TIME/3T,J/ODHIGH
						; 7519	;
						; 7520	=00000
						; 7521	ODLOW:					;Another dummy (40-44 are OWLs)
U 0245, 3346,3200,0003,0402,0022,0010,0000	; 7522	=00101	AR_MEM,CLR SC,J/RELMEM		;45 S=6, P=36 (bad). Release memory
U 0246, 3344,3200,0013,0302,2022,0010,0006	; 7523		MQ_ARX,AR_MEM,SC_#,#/6,J/DEPOWG	;46 S=6, P=30. Copy byte to MQ
U 0247, 3344,3200,0013,0302,2022,0010,0014	; 7524		MQ_ARX,AR_MEM,SC_#,#/12.,J/DEPOWG;47 S=6, P=24
U 0250, 3344,3200,0013,0302,2022,0010,0022	; 7525		MQ_ARX,AR_MEM,SC_#,#/18.,J/DEPOWG;50 S=6, P=18
U 0251, 3344,3200,0013,0302,2022,0010,0030	; 7526		MQ_ARX,AR_MEM,SC_#,#/24.,J/DEPOWG;51 S=6, P=12
U 0252, 3344,3200,0013,0302,2022,0010,0036	; 7527		MQ_ARX,AR_MEM,SC_#,#/30.,J/DEPOWG;52 S=6, P=6
U 0253, 3344,3200,0013,0302,2022,0010,0044	; 7528		MQ_ARX,AR_MEM,SC_#,#/36.,J/DEPOWG;53 S=6, P=0
						; 7529	
U 0254, 3346,3200,0003,0402,0022,0010,0000	; 7530		AR_MEM,CLR SC,J/RELMEM		;54 S=8, P=36. Just release memory
U 0255, 3347,4001,0010,0302,2000,0010,0010	; 7531		MQ_ARX,SC_#,#/8,J/SIZE8D	;55 S=8, P=28. Copy byte, fix size
U 0256, 3347,4001,0010,0302,2000,0010,0020	; 7532		MQ_ARX,SC_#,#/16.,J/SIZE8D	;56 S=8, P=20
U 0257, 3347,0001,0010,0302,2000,0010,0030	; 7533		MQ_ARX,SC_#,#/24.,J/SIZE8D	;57 S=8, P=12
U 0260, 3347,4001,0010,0302,2000,0010,0040	; 7534	ODHIGH:	MQ_ARX,SC_#,#/32.,J/SIZE8D	;60 S=8, P=4
						; 7535	
U 0261, 3346,3200,0003,0402,0022,0010,0000	; 7536		AR_MEM,CLR SC,J/RELMEM		;61 S=7, P=36 (bad). Release memory
U 0262, 3344,3240,0013,0302,2022,0010,0007	; 7537		MQ_ARX,AR_MEM,SC_#,#/7,J/DEPOWG	;62 S=7, P=29. Copy byte to MQ
U 0263, 3344,3240,0013,0302,2022,0010,0016	; 7538		MQ_ARX,AR_MEM,SC_#,#/14.,J/DEPOWG;63 S=7, P=22
U 0264, 3344,3240,0013,0302,2022,0010,0025	; 7539		MQ_ARX,AR_MEM,SC_#,#/21.,J/DEPOWG;64 S=7, P=15
U 0265, 3344,3240,0013,0302,2022,0010,0034	; 7540		MQ_ARX,AR_MEM,SC_#,#/28.,J/DEPOWG;65 S=7, P=8
U 0266, 3344,3240,0013,0302,2022,0010,0043	; 7541		MQ_ARX,AR_MEM,SC_#,#/35.,J/DEPOWG;66 S=7, P=1
						; 7542	
U 0267, 3346,3200,0003,0402,0022,0010,0000	; 7543		AR_MEM,CLR SC,J/RELMEM		;67 S=9, P=36, no good. Let go!
U 0270, 3350,0001,0010,0302,2000,0010,0011	; 7544		MQ_ARX,SC_#,#/9,J/SIZE9D	;70 S=9, P=27. Copy byte, fix size
U 0271, 3350,0001,0010,0302,2000,0010,0022	; 7545		MQ_ARX,SC_#,#/18.,J/SIZE9D	;71 S=9, P=18
U 0272, 3350,0001,0010,0302,2000,0010,0033	; 7546		MQ_ARX,SC_#,#/27.,J/SIZE9D	;72 S=9, P=9
U 0273, 3350,0001,0010,0302,2000,0010,0044	; 7547		MQ_ARX,SC_#,#/36.,J/SIZE9D	;73 S=9, P=0
						; 7548	
U 0274, 3346,3200,0003,0402,0022,0010,0000	; 7549		AR_MEM,CLR SC,J/RELMEM		;74 S=18, P=36. Just unpause memory
U 0275, 0114,3240,0003,0000,0022,0014,0000	; 7550		AR_MEM,CLR FPD,J/HRLM		;75 S=18, P=18. Treat as HRLM
U 0276, 0316,3240,0003,0000,0022,0014,0000	; 7551		AR_MEM,CLR FPD,J/HLL		;76 S=18, P=0. Treat as HRRM
						; 7552	
U 0277, 1457,3200,0003,0000,0036,0010,0000	; 7553		FIN LOAD,STORE,J/ILLOWG		;77 Illegal byte pointer. UUO it
						; 7554	;
U 3347, 3344,3240,0003,0301,0022,0010,0034	; 7555	SIZE8D:	AR_MEM,FE_#,#/28.,J/DEPOWG	;Fix FE for size 8 bytes
U 3350, 3344,3200,0003,0301,0022,0010,0033	; 7556	SIZE9D:	AR_MEM,FE_#,#/27.,J/DEPOWG	;Same for size 9
						; 7557	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  IBP, ADJBP			

						; 7558		.TOC	"Single Byte Instructions:  IBP, ADJBP"
						; 7559	;
						; 7560	;	IBP--Increment a byte pointer (in memory).
						; 7561	;	ADJBP--Adjust a one or two word byte pointer from memory by an
						; 7562	;	amount specified by the (non zero) AC.
						; 7563	;	Both of these instructions key off of the same op code (133);
						; 7564	;	they are distinguished by ADJBP having a non zero AC field.
						; 7565	;
						; 7566	;	The IBP case is rather simple.
						; 7567	;
						; 7568		.DCODE
D 0133, 4001,1503				; 7569	133:	R,	B/0,	J/IBP		;IBP and ADJBP--must adjoin FSC
						; 7570		.UCODE
						; 7571	
						; 7572	1503:					;[345] In same block of 8 as FSC
U 1503, 0032,3401,0200,5132,0000,4610,0045	; 7573	IBP:	SC_P-#,#/37.,ARX_0S,SKP AC EQ 0	;[407] Test for OWG. IBP or ADJBP?
						; 7574	=11010
U 0032, 2462,4001,0010,5412,2000,4710,0000	; 7575	IBPTST:	SC_-S,MQ_ARX,SKP SC0,J/ADJBP	;[407] ADJBP. Clear MQ0. OWG?
U 0033, 2042,4001,0000,0000,0000,4750,0000	; 7576		SKP SC0,CALL [INCRBP]		;IBP. Test for OWG and do it
U 0037, 0217,0001,0003,0000,0217,0014,0000	; 7577	=11111	FIN STORE,CLR FPD,I FETCH,J/NOP	;Tidy up and leave
						; 7578	;
						; 7579	;	ADJBP is handled separately for OWGs and OWL/TWGs.  We consider
						; 7580	;	the latter case first.
						; 7581	;	Step 1:  figure out the byte capacity of a word.  This is broken
						; 7582	;	into the capacity to the left of the current byte (including the
						; 7583	;	byte itself) and the capacity to the right of the byte.  If these
						; 7584	;	add up to zero, then the byte can't fit in a word, and we return
						; 7585	;	to the user with no divide set.  If the byte size is zero, we
						; 7586	;	return with the pointer as is.
						; 7587	;	For this version, we compute the capacities by using repeated
						; 7588	;	subtraction.  Since the numbers involved are typically no greater
						; 7589	;	than five or six (and are never bigger than 36) this will be faster
						; 7590	;	than division.
						; 7591	=0
U 2462, 1370,4061,0500,0101,1020,0007,0000	; 7592	ADJBP:	FE_P,ARX_2+MQ0,AR0-3 DISP,J/ADJOWG;[407] OWG. Split on range
U 2463, 2470,3441,0200,0103,0000,4710,0000	; 7593		FE_P,SC/SCAD,ARX_0S,SKP SC0	;OWL/TWG. Is the size zero?
U 2470, 2512,4001,0000,0000,0000,7510,0000	; 7594	=0	SKP -VMA SEC0,J/OWLCPY		;Yes. Test for possible TWG
U 2471, 2472,4001,0010,5011,2020,5110,0000	; 7595		MQ_ARX,FE_FE-S,SKP SCAD0	;No. Clear MQ. Bytes to the right?
						; 7596	=0
						; 7597	CAPLOW:	ARX_ARX+1,FE_FE-S,SKP SCAD0,	;Yes. Count the byte and look
U 2472, 2472,4001,0600,5011,0020,5110,0000	; 7598			J/CAPLOW		; for another one
						; 7599		BR/AR,BRX/ARX,ARX_-2+MQ0,	;No more. Save count and pointer
U 2473, 2500,2301,0560,5300,3000,0110,0044	; 7600			P_#-SC,#/36.		; and set up next count
						; 7601	=0
						; 7602	CAPHGH:	P_P-S,ARX_ARX+1,SKP SCAD0,	;Count possible byte on left,
U 2500, 2500,4021,0600,5110,3021,5110,0200	; 7603			J/CAPHGH		; saving alignment info
U 2501, 3351,4622,6007,0000,0020,1010,0166	; 7604		T0_AR,AR_ARX+BRX+1		;All counted. Get total capacity
U 3351, 2502,3703,0000,0000,0020,5610,0000	; 7605		SKP AR NZ			;Will any bytes fit into word?
U 2502, 0217,0001,0000,0000,0217,1110,0424	; 7606	=0	SET NO DIVIDE,I FETCH,J/NOP	;[422] No. This is pretty silly
						; 7607	;
						; 7608	;	Step 2:  generate a modified adjustment count and compute the
						; 7609	;	number of words to move and the relative byte position within
						; 7610	;	the word.  All adjustments are done relative to the first byte
						; 7611	;	in the word, so that the resulting quotient is the actual
						; 7612	;	number of words to add to the base address.  If the adjustment
						; 7613	;	is negative, however, we must back up the quotient by one and; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3-1
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  IBP, ADJBP			

						; 7614	;	offset the remainder by the capacity if it is non zero.
						; 7615	;
						; 7616	;	In order to speed up the division, the absolute value of the
						; 7617	;	modified adjustment is broken into ranges of up to 63, 64 to
						; 7618	;	2**18-1, and 2**18 or greater.  This lets us use step counts of
						; 7619	;	7, 19, and 36, respectively, saving a lot of time for the most
						; 7620	;	common cases.
						; 7621	;
						; 7622	;	For this portion of the work, OWGs and OWLs are identical.
						; 7623	;
U 2503, 3352,0610,0200,0322,0020,0010,0036	; 7624	ADJOIN:	ARX_ARX+FM[AC0],SC_#,#/30.	;Compute modified adjustment
						; 7625		T1_AR,BR/AR,AR_ARX,BRX/ARX,	;Divisor is capacity. Is modified
U 3352, 1327,3441,4267,0000,2000,1032,0171	; 7626			ARX_0S,SIGNS DISP,TIME/2T; adjustment negative?
						; 7627	=0111	AC0_AR,BRX/ARX,ARX_AR (AD),	;No. Clear BRX; use adjustment as
						; 7628			ARL_ARL.S,ARR+MQ_0.S,	; dividend, and look at high order
U 1327, 3353,3701,0220,0000,0000,1022,0110	; 7629			J/POSADJ		; half of dividend for speedup
						; 7630		AC0_AR,BRX/ARX,ARX_-BRX,	;Yes. Negate adjustment for both
U 1337, 3353,5162,0620,0000,0020,1022,0116	; 7631			ARL/ADX,ARR+MQ_0.S	; dividend and test
						; 7632	POSADJ:	AR_ARX (ADX),ARX_SHIFT,		;Generate high order 30 bits of
U 3353, 2504,3703,6400,0301,0020,5610,0044	; 7633			FE_#,#/36.,SKP AR NZ	; dividend. Are high 18 bits zero?
						; 7634	=0	ARX_SHIFT,SC_FE,		;Yes. Align low six bits to top of
U 2504, 2002,3713,0400,0000,0020,5613,0000	; 7635			SKP ARX NZ,J/SMALDV	; word. Is that enough?
U 2505, 2000,0301,0200,0301,0020,0413,0041	; 7636		ARX_AR*2,CLR AR,FE_#,#/33.,SC_FE;Need long division. Align dividend
						; 7637	=000
						; 7638	ADJDIV:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,	;Do first divide step
U 2000, 0442,5102,5500,3001,0020,0071,0000	; 7639			CALL [DIVS3]		; and call subroutine for the rest
						; 7640	=010
U 2002, 0374,3401,2000,0301,0000,0050,0004	; 7641	SMALDV:	AR_0S,FE_#,#/4,CALL [DIVS1]	;Very short division is adequate
						; 7642		ARX_AR SWAP,AR_0S,FE_#,#/16.,	;Medium size needed. Put significant
U 2003, 2000,3441,2400,0301,3000,0010,0020	; 7643			J/ADJDIV		; dividend bits in proper spot
						; 7644	;
						; 7645	;	Return from division is either 6 (negative dividend) or 7
						; 7646	;	(non negative dividend).  We tuck the negative offset code in
						; 7647	;	at 4 and 5 for convenience.
						; 7648	;
U 2004, 2007,5142,0600,0000,0020,0010,0000	; 7649	NEGADJ:	ARX_-BRX,J/ADJUST		;Zero remainder. Negate quotient
U 2005, 2007,0600,2007,0000,0020,0010,0171	; 7650		AR_AR+FM[T1],J/ADJUST		;Non zero. Offset by capacity
						; 7651	;
						; 7652	;	On exit from division, AR has the signed remainder and ARX and
						; 7653	;	BRX have the positive quotient.  If the dividend was negative,
						; 7654	;	we must either negate the quotient or negate and subtract one
						; 7655	;	(thus one's complementing it) depending upon whether there was
						; 7656	;	a non zero remainder.
						; 7657	;
U 2006, 2004,2542,0600,0000,0000,4510,0000	; 7658		ARX_BRX COMP,SKP AR0,J/NEGADJ	;Negative dividend. Complement
						; 7659						; quotient and test remainder
						; 7660	;
						; 7661	;	Step 3:  add the final quotient to the address, and offset the
						; 7662	;	byte into the word by the adjusted remainder.  To do this, we
						; 7663	;	must finally differentiate an OWL from a TWG.  (Recall that we
						; 7664	;	saved most of the original byte pointer (including bit 12) in
						; 7665	;	T0 before we did the division.)  In any event, for an OWL we
						; 7666	;	add the quotient to the right half of the byte pointer; for a
						; 7667	;	TWG we fetch the second word and then add the quotient to bits
						; 7668	;	6-35 if it's global, to bits 18-35 if it's local.
						; 7669	;; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3-2
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  IBP, ADJBP			

						; 7670	;	After this, we subtract the byte pointer S field from (36 - the
						; 7671	;	alignment information left in the P field) precisely remainder
						; 7672	;	times (recall that division copied SC, preloaded with 36, into
						; 7673	;	FE when it finished).  That's about it.
						; 7674	;
						; 7675	;	OWGs split off their separate way.
						; 7676	;
U 2007, 1356,3240,2017,0000,1020,0005,0166	; 7677	ADJUST:	MQ_AR,AR_T0,SR DISP		;Remainder to MQ. OWG or OWL/TWG?
						; 7678	=1110	BR/AR,SC_P+S,MQ_MQ-1,		;OWL/TWG. Copy pointer, add first
U 1356, 1465,1721,0050,2112,0020,0734,0003	; 7679			BYTE DISP,J/ADJTWG	; S, generate count. Perhaps TWG?
						; 7680		FE_P+1,BR/AR,AR_MQ,		;An OWG. Grab initial P&S and
U 1357, 3360,3723,2040,4101,0217,0010,0000	; 7681			I FETCH,J/SNATCH	; set up quotient addition
						; 7682	;
						; 7683	=101
						; 7684	ADJTWG:	FE_FE-SC,ARL_ARL,ARR_ARX+BR,	;OWL. Adjust address; initialize P
U 1465, 2024,0612,2300,5001,0020,0610,0000	; 7685			ARX/MQ,J/ADJP
						; 7686		FE_FE-SC,AR_ARX,ARX_AR (AD),	;Perhaps TWG. Move quotient to AR
U 1467, 0670,3701,4200,5001,2000,7510,0000	; 7687			SKP -VMA SEC0		; No TWGs allowed in section 0
						; 7688	=00	ARL_ARXL,ARR_AR+BR,		;Section 0. An OWL in TWG's clothing
U 0670, 2024,0602,2304,0000,2020,0022,0004	; 7689			ARX/MQ,J/ADJP
						; 7690		BR/AR,BRX/ARX,VMA_VMA+1,LOAD AR,;A real TWG. Keep quotient and
U 0671, 1045,0001,0060,0000,0012,3650,0000	; 7691			CALL [XFERW]		; remainder while fetching address
U 0673, 2510,0602,2400,0102,1020,4510,0000	; 7692	=11	SC_P,AR_AR+BR,ARX_AR,SKP AR0	;Assume global address. Is it?
U 2510, 3354,0001,0000,2400,3001,0010,0200	; 7693	=0	P_SC,J/TWGDUN			;Yes. Use 30 bit addition
U 2511, 3354,0612,2000,0000,2020,0022,0004	; 7694		ARL_ARXL,ARR_ARX+BR		;No. Foolish, but 18 bits is enough
U 3354, 2024,3242,6301,0000,0000,1010,0000	; 7695	TWGDUN:	AC1_AR,AR_BRX,ARX/MQ		;Store address; restore first word
						; 7696	;
						; 7697	;	Address has been adjusted.  Adjust P by remainder bytes.
						; 7698	;
						; 7699	=100
						; 7700	ADJP:	FE_FE-S,P_SCAD,ARX_ARX-1,	;Step to next byte and count
U 2024, 2024,1701,0600,5011,3021,4310,0200	; 7701			SKP ARX0,J/ADJP		; down the remainder
U 2025, 0216,0001,0000,0000,0217,0010,0000	; 7702	TWGCPY:	I FETCH,J/STORAC		;Adjustment done. Load AC0
						; 7703	;
						; 7704	;	If the byte size is zero, we just load AC0, or ACs 0 and 1 if it's
						; 7705	;	a TWG.
						; 7706	;
U 2027, 0147,0001,0000,0000,0012,3610,0000	; 7707	=111	VMA_VMA+1,LOAD AR,J/TWJUNK	;[413][424] A TWG. Use DMOVE code
						; 7708	;
						; 7709	=0
U 2512, 0216,0001,0000,0000,0217,0010,0000	; 7710	OWLCPY:	I FETCH,J/STORAC		;Section 0, an OWL. Just load AC0
U 2513, 2025,4001,0000,0000,0000,0034,0000	; 7711		BYTE DISP,TIME/2T,J/TWGCPY	;Not section 0. Test AR12 for TWG
						; 7712	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; BYTE.MIC[4,24]	17:29 14-Mar-86			Single Byte Instructions:  IBP, ADJBP			

						; 7713	;
						; 7714	;	OWGs use the same basic algorithm as OWLs and TWGs, but the
						; 7715	;	actual implementation of steps 1 and 3 is quite different.
						; 7716	;	Step 1:  get the byte capacity of the word and current offset
						; 7717	;	of the OWG within the word.  Note that OWGs may be split into
						; 7718	;	ranges of sizes, with the capacity identical for each OWG within
						; 7719	;	a range.  The current offset within the word can be computed by
						; 7720	;	subtracting the range base + 1 from the P&S field.  The range base
						; 7721	;	is saved in the OWG for later final adjustment.  The capacity is
						; 7722	;	computed in a rather wry way:  ARX is initially loaded with the
						; 7723	;	capacity - 4; later, AR is set to -1.  When AR*4 is subtracted
						; 7724	;	from ARX, AR*4 will be -4 as long as ARX was positive.  The only
						; 7725	;	negative case is for a capacity of 2 (for 18 bit bytes); that one
						; 7726	;	is special cased.
						; 7727	;
						; 7728	=1000
						; 7729	ADJOWG:					;40:43. No OWGs here
						; 7730	=1001	ARX_2+MQ0,TIME/2T,P_#,#/45,	;44:47. Size 6: capacity 6, base 45
U 1371, 3355,4041,0500,0302,3000,0110,0045	; 7731			SC/SCAD,J/OWGCOM	;[407]
						; 7732		ARX_2+MQ0,TIME/2T,P_#,#/45,	;50:53. More size 6
U 1372, 3355,4041,0500,0302,3000,0110,0045	; 7733			SC/SCAD,J/OWGCOM	;[407]
U 1373, 3355,3401,0200,0302,3000,0110,0054	; 7734		ARX_0S,P_#,#/54,SC/SCAD,J/OWGCOM;54:57. Size 8: capacity 4, base 54
U 1374, 2522,4001,0000,5030,0020,5110,0061	; 7735		GEN FE-#,#/61,SKP SCAD0,J/EIGHT7;60:63. Either size 8 or size 7
U 1375, 2524,4001,0000,5030,0020,5110,0067	; 7736		GEN FE-#,#/67,SKP SCAD0,J/SEVEN9;64:67. Either size 7 or size 9
U 1376, 3355,3401,0200,0302,3000,0110,0067	; 7737		ARX_0S,P_#,#/67,SC/SCAD,J/OWGCOM;70:73. Size 9: capacity 4, base 67
U 1377, 2520,4001,0000,5030,0020,5110,0077	; 7738		GEN FE-#,#/77,SKP SCAD0		;74:77. Is this an illegal pointer?
U 2520, 1002,3242,2000,0000,0000,0010,0000	; 7739	=0	AR_BR,J/UUO			;77 is no good. UUO it
						; 7740		BRX/ARX,ARX_1S,P_#,#/74,SC/SCAD,;74:76. Size 18: capacity 2, base 74
U 2521, 3355,2301,0220,0302,3000,0110,0074	; 7741			J/OWGCOM		; Save size; force ARX negative
						; 7742	;
						; 7743	=0
						; 7744	EIGHT7:	ARX_1,TIME/2T,P_#,#/61,SC/SCAD,	;61:63. Size 7: capacity 5, base 61
U 2522, 3355,4061,0200,0302,3000,0110,0061	; 7745			J/OWGCOM
U 2523, 3355,3401,0200,0302,3000,0110,0054	; 7746		ARX_0S,P_#,#/54,SC/SCAD,J/OWGCOM;60 is the last size 8 byte
						; 7747	;
						; 7748	=0
U 2524, 3355,3401,0200,0302,3000,0110,0067	; 7749	SEVEN9:	ARX_0S,P_#,#/67,SC/SCAD,J/OWGCOM;67 is the first size 9 byte
U 2525, 3355,4061,0200,0302,3000,0110,0061	; 7750		ARX_1,TIME/2T,P_#,#/61,SC/SCAD	;64:66 are the last size 7 bytes
U 3355, 3356,2301,2007,1001,0000,1010,0166	; 7751	OWGCOM:	T0_AR,AR_1S,FE_FE-SC-1		;Save pointer; find initial offset
U 3356, 2530,5113,0200,0000,3021,4310,0200	; 7752		P_FE,ARX_ARX-AR*4,SKP ARX0	;Try to get capacity. Is it 2?
						; 7753	=0	BRX/ARX,ARX_AR,AR_SIGN,		;No. Save it; set up offset and
U 2530, 3357,5441,2420,0302,1020,0016,0006	; 7754			SC_#,#/6,J/OFSHFT	; shift count for offset generation
U 2531, 3357,5401,2400,0302,1020,0016,0006	; 7755		ARX_AR,AR_SIGN,SC_#,#/6		;Yes. Size was loaded above
U 3357, 2503,3202,6400,0000,0000,1610,0001	; 7756	OFSHFT:	AR_BRX,ARX_SHIFT,SR_1,J/ADJOIN	;Mark OWG and rejoin for step 2
						; 7757	;
						; 7758	;	Step 3: add the final quotient to the address, and offset the OWG
						; 7759	;	into the word by remainder bytes.  Since this becomes a simple
						; 7760	;	integer add, this portion is rather trivial.
						; 7761	;
U 3360, 3361,0612,2004,0002,0020,1613,0000	; 7762	SNATCH:	SC_EA,AR_ARX+BR,SR_0		;Grab offset; adjust address
U 3361, 0015,0001,0000,2000,3000,0110,0000	; 7763		P_FE+SC,J/STAC			;Add proper offset to P&S. Done!
						; 7764	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; BYTE.MIC[4,24]	17:29 14-Mar-86			Subroutines for Single Byte Instructions		

						; 7765	.TOC	"Subroutines for Single Byte Instructions"
						; 7766	;
						; 7767	;	INCRBP--Subroutine to increment a byte pointer.  The first (or
						; 7768	;	only) word of the relevant pointer is in AR.  Call with SC_P-#,
						; 7769	;	#/37.,BYTE DISP, thus testing for OWG and first part done
						; 7770	;	simultaneously.  If FPD is set, this routine returns 4 without
						; 7771	;	doing anything; otherwise, the pointer will be incremented and
						; 7772	;	the store will have been started.  Return 4 if an OWL or TWG
						; 7773	;	must recompute SC or on any OWG, 15 if an OWL and SC is OK, and
						; 7774	;	17 if possibly a TWG with SC OK.  Note that ARX must have the
						; 7775	;	first byte pointer word on exit if this is an OWL or TWG.
						; 7776	;
						; 7777	=010					;Test FPD and OWG
U 2042, 1430,4001,0000,0002,1020,1107,0100	; 7778	INCRBP:	SC_FE#,SET FPD,AR0-3 DISP,J/OWGINC;OWG, no FPD. SC_2; test edges
U 2043, 2064,0001,0000,5110,3021,0034,0200	; 7779		P_P-S,BYTE DISP,J/BYTINC	;No OWG, no FPD. Check for overflow
U 2046, 0004,4001,0000,0000,0000,0003,0000	; 7780		RETURN4				;OWG, FPD. Forget it
U 2047, 0004,4001,0000,0000,0000,0003,0000	; 7781		RETURN4				;No OWG, FPD. No increment needed
						; 7782	;
						; 7783	;	Either OWL or TWG.  Check which; if no overflow, it doesn't really
						; 7784	;	matter.
						; 7785	;
						; 7786	=100
U 2064, 0015,4001,0000,5132,0016,0003,0045	; 7787	BYTINC:	SC_P-#,#/37.,STORE,RETURN15	;OWL, no overflow. Store and leave
						; 7788		FE_#,#/36.,GEN AR+1,		;OWL, overflow. Compute new P and
U 2065, 2532,4003,0000,0301,0000,0010,0044	; 7789			TIME/2T,J/OWLINC	; set up new address portion
U 2066, 0017,0001,0000,5132,0016,0003,0045	; 7790		SC_P-#,#/37.,STORE,RETURN17	;TWG, no overflow. Just like OWL
						; 7791		FE_#,#/36.,GEN AR+1,TIME/2T,	;TWG, overflow. Compute new P and
U 2067, 2532,4001,0000,0301,0000,7510,0044	; 7792			SKP -VMA SEC0		; test for valid TWG
						; 7793	=0
						; 7794	OWLINC:	P_FE-S,ARR_AR+1,TIME/2T,STORE,	;OWL. Increment address, set new P
U 2532, 3365,4001,2000,5010,3016,0610,0200	; 7795			J/SNARFP
U 2533, 3362,4001,0000,5010,3012,3622,0200	; 7796		P_FE-S.S,VMA_VMA+1,LOAD AR	;TWG. Set new P, fetch second word
U 3362, 3363,3240,0403,0000,1022,0010,0000	; 7797		ARX_AR,AR_MEM			;Save first word, await second
U 3363, 2534,4001,2040,0102,0020,4510,0000	; 7798		SC_P,BR/AR,SKP AR0,AR_AR+1	;Increment address, check I/EFIW
U 2534, 3364,0001,0000,2400,3016,0110,0000	; 7799	=0	P_SC#,STORE,J/STORTG		;EFIW. Do full global increment
U 2535, 3364,3242,0000,0000,0016,0610,0002	; 7800		ARL_BRL,STORE			;IFIW. Just increment right half
						; 7801	STORTG:	FIN STORE,AR_ARX,VMA_VMA-1,	;Finish second word
U 3364, 0004,0001,4003,0000,2016,3503,0000	; 7802			STORE,RETURN4		; and store first
						; 7803	;
U 3365, 0015,0001,0400,5132,1000,0003,0045	; 7804	SNARFP:	ARX_AR,SC_P-#,#/37.,RETURN15	;[351] Save offset P, new pointer
						; 7805	;
						; 7806	;	An OWG.  53, 60, 66, 73, 76, and 77 need special handling.
						; 7807	;	All others just tick the P&S field.
						; 7808	;
						; 7809	=1000
						; 7810	OWGINC:					;40:43. No OWGs here
U 1431, 0004,4001,0000,4100,3016,0103,0000	; 7811	=1001	P_P+1,STORE,RETURN4		;44:47. No special handling
						; 7812		GEN AR+1,GEN P-#,#/53,		;50:53. 53 becomes 46
U 1432, 2550,4001,0000,5130,0020,5210,0053	; 7813			SKP SCAD NE,J/OVER6
U 1433, 0004,4001,0000,4100,3016,0103,0000	; 7814		P_P+1,STORE,RETURN4		;54:57. No special handling
						; 7815		GEN AR+1,GEN P-#,#/60,		;60:63. 60 becomes 55
U 1434, 2554,4003,0000,5130,0020,5210,0060	; 7816			SKP SCAD NE,J/OVER8
						; 7817		GEN AR+1,GEN P-#,#/66,		;64:67. 66 becomes 62
U 1435, 2552,4003,0000,5130,0020,5210,0066	; 7818			SKP SCAD NE,J/OVER7
						; 7819		GEN AR+1,GEN P-#,#/73,		;70:73. 73 becomes 70
U 1436, 2560,4003,0000,5130,0020,5210,0073	; 7820			SKP SCAD NE,J/OVER9; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5-1
; BYTE.MIC[4,24]	17:29 14-Mar-86			Subroutines for Single Byte Instructions		

U 1437, 1454,4003,0000,4102,0020,0007,0000	; 7821		GEN AR+1,SC_P+1,SH DISP		;74:77. Test low P&S bits
U 1454, 0004,4001,0000,2400,3016,0103,0000	; 7822	=1100	P_SC#,STORE,RETURN4		;74 becomes 75. Store and leave
U 1455, 0004,4001,0000,2400,3016,0103,0000	; 7823	NXTOWG:	P_SC#,STORE,RETURN4		;75 becomes 76. Store and leave
						; 7824		AR_AR+1,TIME/2T,SC_#,#/75,	;76 becomes 75. Increment address
U 1456, 1455,4003,2000,0302,0000,0010,0075	; 7825			J/NXTOWG		; first
U 1457, 3002,0001,0003,0000,0002,0014,0000	; 7826	ILLOWG:	MEM_AR,CLR FPD,J/IOCHK		;[414] 77 Illegal byte pointer
						; 7827	;
						; 7828	=0
U 2550, 1455,4003,2000,0302,0000,0010,0046	; 7829	OVER6:	AR_AR+1,TIME/2T,SC_#,#/46,J/NXTOWG;53. Increment address first
U 2551, 0004,4001,0000,4100,3016,0103,0000	; 7830		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 7831	;
						; 7832	=0
U 2552, 1455,4003,2000,0302,0000,0010,0062	; 7833	OVER7:	AR_AR+1,TIME/2T,SC_#,#/62,J/NXTOWG;66. Increment address first
U 2553, 0004,4001,0000,4100,3016,0103,0000	; 7834		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 7835	;
						; 7836	=0
U 2554, 1455,4001,2000,0302,0000,0010,0055	; 7837	OVER8:	AR_AR+1,TIME/2T,SC_#,#/55,J/NXTOWG;60. Increment address first
U 2555, 0004,4001,0000,4100,3016,0103,0000	; 7838		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 7839	;
						; 7840	=0
U 2560, 1455,4003,2000,0302,0000,0010,0070	; 7841	OVER9:	AR_AR+1,TIME/2T,SC_#,#/70,J/NXTOWG;73. Increment address first
U 2561, 0004,4001,0000,4100,3016,0103,0000	; 7842		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 7843	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; BYTE.MIC[4,24]	17:29 14-Mar-86			Subroutines for Single Byte Instructions		

						; 7844	;
						; 7845	;	LDEA--Subroutine to compute the effective address of a byte from
						; 7846	;	a one word local byte pointer.  Called with the byte pointer in ARX
						; 7847	;	and EA MOD DISP on it.
						; 7848	;	LEAIND--Entry point for two word global EA calculation on the
						; 7849	;	second pointer word.  Called with READ BP2 on the second pointer
						; 7850	;	word.
						; 7851	;	Both entries return 37 with the byte being loaded into AR, and
						; 7852	;	with the FE added to SC.
						; 7853	;	Warning:  two of the words below (LDEA+1, LEAIND+5) cannot have
						; 7854	;	their parity generated directly by the assembler.  The SKP AR0 macro
						; 7855	;	can be used to force correct parity.  It will be ignored, since
						; 7856	;	J/37.
						; 7857	;
						; 7858	=1100
						; 7859	LDEA:	GEN AR,BYTE LOAD,		;No index, no indirect. Load byte
U 1474, 0037,3703,0000,2002,0111,0003,0420	; 7860			SC_FE+SC,RETURN37	; word
						; 7861		GEN AR+XR,INDEXED,BYTE LOAD,	;Index, no indirect. Add index
U 1475, 0037,0600,0002,2002,2131,0003,0420	; 7862			SC_FE+SC,RETURN37	; register to generate byte address
						; 7863		GEN AR,BYTE INDRCT,		;No index, indirect. Test for
U 1476, 0720,3703,0000,0000,0111,7010,0610	; 7864			SKP INTRPT,J/LEAIND	; interrupt
						; 7865		GEN AR+XR,INDEXED,BYTE INDRCT,	;[350] Index, indirect. Add index
U 1477, 0720,0600,0002,0000,2131,7010,0610	; 7866			SKP INTRPT		; register and test for interrupt
						; 7867	=00
U 0720, 3366,3240,0003,0000,0022,2550,0000	; 7868	LEAIND:	ARX_MEM,LONG EN,CALL [BYTIND]	;No interrupt. Unwind indirection
U 0721, 0144,3200,0003,0000,0022,7710,0000	; 7869		ARX_MEM,TAKE INTRPT		;Interrupted. Blow this place
U 0722, 1474,2341,0002,0000,0020,0036,0000	; 7870		XR,EA MOD DISP,TIME/3T,J/LDEA	;Local word at end. Untangle it
U 0723, 1536,2301,0002,0000,0020,0036,0000	; 7871		XR,EA MOD DISP,TIME/3T		;Global word at end. Indexed?
						; 7872	=1110	GEN ARX,GLOBAL,BYTE LOAD,	;No indexing. Read global word
U 1536, 0037,3713,0000,2002,1111,0003,0420	; 7873			SC_FE+SC,RETURN37	; and add FE to SC
						; 7874		GEN ARX+XR,GLOBAL,BYTE LOAD,	;Indexing. Add index to address
						; 7875			SC_FE+SC,RETURN37,	; and do otherwise the same
U 1537, 0037,0610,0002,2002,1131,4503,0420	; 7876			SKP AR0			; (This forces odd parity)
						; 7877	;
						; 7878	;	DPEA--Routine to compute the effective address of a byte from
						; 7879	;	a one word local byte pointer to be used in a deposit operation.
						; 7880	;	Entered with the byte pointer in ARX and EA MOD DISP on it.
						; 7881	;	DEAIND--Entry point for two word global EA calculation on the
						; 7882	;	second pointer word.  Entered with READ BP2 on the second pointer
						; 7883	;	word.
						; 7884	;	Both entries return to GUDSIZ testing the sign of FE-SC-1.
						; 7885	;	[340] This code has been desubroutinized for now, since it
						; 7886	;	must be called from an odd address.
						; 7887	;	WARNING:  two of the words below (DPEA+1, DEAIND+5) cannot
						; 7888	;	have their parity generated by the assembler.  Since the SKIP
						; 7889	;	field is already busy, we use the MQ field and set MQ/SH when
						; 7890	;	we need to generate parity.  GUDSIZ is constrained to be at an
						; 7891	;	address with even parity, so we don't have to worry about things
						; 7892	;	moving around. [412]
						; 7893	;
						; 7894	=1100
						; 7895	DPEA:	GEN AR,BYTE RPW,GEN FE-SC-1,	;No index, no indirect. Load byte
U 1554, 0336,3703,0000,1000,0131,5110,0760	; 7896			SKP SCAD0,J/GUDSIZ	; word, test word underflow
						; 7897		GEN AR+XR,INDEXED,BYTE RPW,	;Index, no indirect. Add index
						; 7898			GEN FE-SC-1,SKP SCAD0,	; register, load byte, test word
U 1555, 0336,0600,0012,1000,2151,5110,0760	; 7899			MQ/SH,J/GUDSIZ		; underflow, and force odd parity; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6-1
; BYTE.MIC[4,24]	17:29 14-Mar-86			Subroutines for Single Byte Instructions		

						; 7900		GEN AR,BYTE INDRCT,		;No index, indirect. Start read
U 1556, 0770,3703,0000,0000,0111,7010,0610	; 7901			SKP INTRPT,J/DEAIND	; and test for interrupt
						; 7902		GEN AR+XR,INDEXED,BYTE INDRCT,	;Index, indirect. Add index
U 1557, 0770,0600,0002,0000,2131,7010,0610	; 7903			SKP INTRPT		; register, read, test interrupt
						; 7904	=00
U 0770, 3366,3240,0003,0000,0022,2550,0000	; 7905	DEAIND:	ARX_MEM,LONG EN,CALL [BYTIND]	;No interrupt. Unwind indirection
U 0771, 0144,3200,0003,0000,0022,7710,0000	; 7906		ARX_MEM,TAKE INTRPT		;Interrupted. Blast out sideways
U 0772, 1554,2341,0002,0000,0020,0036,0000	; 7907		XR,EA MOD DISP,TIME/3T,J/DPEA	;Local word at end. Decode further
U 0773, 1576,2341,0002,0000,0020,0036,0000	; 7908		XR,EA MOD DISP,TIME/3T		;Global end word. Indexed?
						; 7909	=1110	GEN ARX,GLOBAL,BYTE RPW,	;No index. Read byte word
						; 7910			GEN FE-SC-1,SKP SCAD0,	; and test byte underflow
U 1576, 0336,3713,0000,1000,1131,5110,0760	; 7911			J/GUDSIZ
						; 7912		GEN ARX+XR,GLOBAL,BYTE RPW,	;Index. Add it in, read byte word,
						; 7913			GEN FE-SC-1,SKP SCAD0,	; and test byte underflow
U 1577, 0336,0610,0002,1000,1151,5110,0760	; 7914			J/GUDSIZ		;Can force odd parity here
						; 7915	;
						; 7916	;	BYTIND--Subroutine to unwind some indirection for an OWL or
						; 7917	;	a TWG.  Call with current indirect word in ARX.  Return 2 if
						; 7918	;	final word is local (possibly indirected), 3 if it is global.
						; 7919	;	Return 0 if final word is global indirect, in which case we
						; 7920	;	will dive back in again if no interrupt is pending.
						; 7921	;
U 3366, 1623,2341,4002,0000,2020,0036,0000	; 7922	BYTIND:	AR_ARX,XR,EA MOD DISP,TIME/3T	;Dispatch on global indirection
U 1623, 1656,2301,0002,0000,0020,0036,0000	; 7923	=0011	XR,EA MOD DISP,TIME/3T,J/GLBIND	;Global indirect. Test indexing
U 1627, 0003,0001,0000,0000,0000,0003,0000	; 7924		RETURN3				;Global, no indirect. Done for now
U 1633, 0170,0001,0000,0301,0000,0010,0024	; 7925		FE_#,#/24,J/PF24		;Both bits 0 and 1 set. No good
U 1637, 0002,4001,0000,0000,0000,0003,0000	; 7926		RETURN2				;Local word. Let main line handle it
						; 7927	;
						; 7928	=1110
						; 7929	GLBIND:	GEN ARX,GLOBAL,BYTE INDRCT,	;No indexing. Fetch next word in
U 1656, 0000,3711,0000,0000,1111,7003,0610	; 7930			SKP INTRPT,RETURN0	; loop, testing for interrupt
						; 7931		GEN ARX+XR,GLOBAL,BYTE INDRCT,	;Indexing. Add in index and do
U 1657, 0000,0610,0002,0000,1131,7003,0610	; 7932			SKP INTRPT,RETURN0	; similarly
						; 7933	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; BYTSUB.MIC[4,24]	14:19 22-May-86			BYTE GROUP -- Some Old Style Subroutines		

						; 7934	.TOC	"BYTE GROUP -- Some Old Style Subroutines"
						; 7935	;
						; 7936	;	This file once included all of the byte instruction code.
						; 7937	;	With the coming of the new version of the byte instructions,
						; 7938	;	however, much of this stuff became unnecessary and has
						; 7939	;	been eliminated as a result.  It is hoped to be able to
						; 7940	;	eliminate more of this once we rewrite the string instructions.
						; 7941	;	[345]
						; 7942	;
						; 7943	.TOC	"INCREMENT BYTE POINTER SUBROUTINE"
						; 7944	
						; 7945	;
						; 7946	;	This subroutine is now called only by the 10/11 interface handler.
						; 7947	;	Call testing sign of P-S.  [Time=2+2(BP OVFLO)]
						; 7948	;	CALL WITH BP IN AR, P_P-S,SKP SCAD0.
						; 7949	;
						; 7950	=0
U 2562, 0004,0001,0000,0000,0016,0003,0000	; 7951	IBPS:	STORE,RETURN4			;SIMPLE CASE
U 2563, 3367,4001,0000,0301,0000,0010,0044	; 7952		FE_#,#/36.,GEN AR+1,TIME/2T	;[424] POINTER OVERFLOW, B12=0
						; 7953		P_FE-S,AR_AR+1,TIME/2T,		;[424] SINGLE WORD BP
U 3367, 0004,4001,2000,5010,3016,0603,0200	; 7954		    STORE,RETURN4
						; 7955	
						; 7956	
						; 7957	.TOC	"BYTE EFFECTIVE ADDRESS EVALUATOR - XADDR"
						; 7958	;
						; 7959	;	This code is no longer used by the single byte instructions.
						; 7960	;	The string instructions get the second part of a two word pointer
						; 7961	;	from an AC, and they do not set FPD.  Thus, they enter at BFETCH
						; 7962	;	(for single word pointers) or BYTEI (long word pointers).  The
						; 7963	;	DTE interface can only use single word pointers, and thus no longer
						; 7964	;	requires the two word pointer test.  [424]
						; 7965	;	In the interest of saving space, we are now using the same indirec-
						; 7966	;	tion evaluation technique as the single byte instructions.  [427]
						; 7967	;
U 3370, 1674,2301,0003,2411,0002,1136,0100	; 7968	BYTEA:	MEM_AR,FE_S,SET FPD,EA MOD DISP	;[424]
						; 7969	=1100					;[427]
U 1674, 0001,3701,0000,0000,0111,0003,0620	; 7970	BFETCH:	GEN AR,BYTE READ,RETURN1
U 1675, 0001,0600,0002,4000,2131,0003,0620	; 7971		GEN AR+XR,INDEXED,BYTE READ,RETURN1
U 1676, 1040,3703,0000,0000,0111,7010,0610	; 7972		GEN AR,BYTE INDRCT,SKP INTRPT,J/BYTEI
						; 7973		GEN AR+XR,INDEXED,BYTE INDRCT,
U 1677, 1040,0600,0002,0000,2131,7010,0610	; 7974			SKP INTRPT
						; 7975	=00					;[427]
U 1040, 3366,3240,0003,0000,0022,2550,0000	; 7976	BYTEI:	ARX_MEM,LONG EN,CALL [BYTIND]	;[427] Unwind indirection chain
U 1041, 0340,3240,0003,0000,0022,0005,0000	; 7977		ARX_MEM,SR DISP,J/CLEAN		;[427] Interrupted. Clean up first
U 1042, 1674,2301,0002,0000,0020,0036,0000	; 7978		XR,EA MOD DISP,TIME/3T,J/BFETCH	;[427] Local at end. Untangle above
U 1043, 1716,2301,0002,0000,0020,0036,0000	; 7979		XR,EA MOD DISP,TIME/3T		;[427] Global at end. Indexed?
U 1716, 0001,3711,0000,0000,1111,0003,0620	; 7980	=1110	GEN ARX,GLOBAL,BYTE READ,RETURN1;No. Read global word
						; 7981		GEN ARX+XR,GLOBAL,BYTE READ,	;Yes. Add index and do likewise
U 1717, 0001,0610,0002,0000,1131,0003,0620	; 7982		    RETURN1
						; 7983	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; BYTSUB.MIC[4,24]	14:19 22-May-86			Load and Deposit Byte Subroutines			

						; 7984	.TOC	"Load and Deposit Byte Subroutines"
						; 7985	;
						; 7986	;	Load byte subroutine.  Enter with S in FE, P+S in SC, and
						; 7987	;	AR load in progress.  SKP INTRPT at entry is optional.
						; 7988	;	RETURN2 WITH BYTE RIGHT JUSTIFIED IN AR. [TIME=7]
						; 7989	;
						; 7990	=0
						; 7991	LDB1:	AR_MEM,SC_#-SC,#/36.,SKP SCAD0,	;36-(P+S)
U 2570, 2572,3200,0003,5302,0022,5110,0044	; 7992			TIME/3T,J/LDB2
U 2571, 0340,3240,0003,0000,0022,0005,0000	; 7993		AR_MEM,SR DISP,J/CLEAN		;HERE IF INTERRUPT PENDING
						; 7994	
						; 7995	=0
U 2572, 2604,3401,2400,0000,0000,0013,0000	; 7996	LDB2:	ARX_SHIFT,AR_0S,SC_FE,J/SHIFT	;BYTE IN ARX HI, READY TO SHIFT
						; 7997		ARX_AR,AR_0S,			;P+S > 36, PUT BYTE IN ARX HI
U 2573, 2604,3441,2400,2002,1020,5110,0000	; 7998			SC_FE+SC,SKP SCAD0	;ADJUST S AND SHIFT BYTE
						; 7999	
						; 8000	;PUT BYTE INTO AR RIGHT-JUSTIFIED
						; 8001	; THIS INSTRUCTION ALSO CALLED ALONE AS A SUBROUTINE
						; 8002	
						; 8003	=0
U 2604, 0002,0001,4000,0000,0000,0003,0000	; 8004	SHIFT:	AR_SHIFT,RETURN2		;RETURN WITH BYTE IN AR
U 2605, 0002,4001,0000,0000,0000,0003,0000	; 8005		RETURN2				;BYTE WAS OFF THE END, RETURN AR=0
						; 8006	;
						; 8007	;	Deposit byte subroutine.  Enter with byte right justified in AR,
						; 8008	;	pointer in BR, S in FE, 36-P in SC, and LOAD AR-ARX in progress.
						; 8009	;	Skip if P > 36.  Return3 with final store going.  [TIME=11]
						; 8010	;
						; 8011	=0
						; 8012	DPB1:	MQ_AR,AR_MEM,ARX_MEM,GEN FE-SC-1,;[303] Keep byte, get data word.
U 2610, 2612,3200,0013,1000,1022,5110,0000	; 8013		    SKP SCAD0,TIME/3T,J/DPB2	; Is P+S <= 36?
U 2611, 0003,3200,0003,0000,0022,0003,0000	; 8014		AR_MEM,RETURN3			;[226]P>36, STORE NOTHING
						; 8015	;
						; 8016	=0
U 2612, 2613,0001,0000,2401,0000,0010,0000	; 8017	DPB2:	FE_SC				;P+S>36, S_36-P
						; 8018		ARX_SHIFT,AR_MQ,SC_FE,		;ARX HAS P,X,S
U 2613, 3371,3721,2400,5301,0000,0013,0110	; 8019			FE_#-SC,#/72.		;SC_S, FE_72-(36-P)=36+P
U 3371, 3372,0001,0000,5302,0000,0010,0044	; 8020		SC_#-SC,#/36.			;SC_36-S (KNOWN .LE. P)
						; 8021		AR_SHIFT,ARX_SHIFT,		;S,P,X
U 3372, 3373,4001,4400,5002,0000,0010,0000	; 8022			SC_FE-SC		;SC_(36+P)-(36-S)=P+S
U 3373, 0003,0001,4000,0000,0016,0003,0000	; 8023		AR_SHIFT,STORE,RETURN3		;[335][345] DONE, STORE IT BACK
						; 8024	
						; 8025	;SUBROUTINE TO GET CONTENTS OF SC RIGHT ALIGNED IN AR
						; 8026	;[TIME=6]
						; 8027	
U 3374, 3375,4001,0000,2400,2000,0022,0200	; 8028	GETSC:	AR0-8_SC			;PUT SC INTO AR
U 3375, 2604,0001,0400,0302,1000,0010,0011	; 8029		ARX_AR,SC_#,#/9.,J/SHIFT	;HERE WITH DATA IN AR0-8
						; 8030	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; EIS.MIC[4,24]	10:37 27-May-86				EXTENDED INSTRUCTION SET DECODING			

						; 8031	.TOC	"EXTENDED INSTRUCTION SET DECODING"
						; 8032	
						; 8033	;GET HERE WITH E0 IN BR, (E0) IN AR
						; 8034	;	(E0) IS THE OPERATION WORD, AND HAS THE NORMAL -10 INSTRUCTION
						; 8035	;	FORMAT -- BITS 0-8 ARE OPCODE, 9-12 IGNORED, 13 @, 14-17 XR,
						; 8036	;	AND 18-35 Y.  THE AC USED COMES FROM THE EXTEND INSTRUCTION.
						; 8037	; COMPUTE E1 FROM 13-35
						; 8038	
						; 8039	;HERE FOR EXTENDED INSTRUCTION SET DECODING UNDER XADDR
						; 8040	
						; 8041		.DCODE
D 0123, 4000,1411				; 8042	123:	R,		J/EXTEND	;Adjacent to FIX
						; 8043		.UCODE
						; 8044	
						; 8045	1411:					;Must be near FIX
U 1411, 2620,0001,0000,2320,0020,5210,0760	; 8046	EXTEND:	GEN #+AR0-8,#/-20,SKP SCAD NZ	;[427] Dispatch XBLT quickly
U 2620, 2430,3240,0200,0000,0020,5610,0000	; 8047	=0	ARX_AC0,SKP AD NZ,J/XBLT	;[427] XBLT. Is count null?
						; 8048	.IF/EXTEXP
						; 8049		SC_#+AR0-8,#/-32,SKP SCAD0,	;[427] VALID EXTENDED OPERATION?
U 2621, 2622,3242,2400,2322,1020,5110,0746	; 8050			ARX_AR,AR_BR		; OPR TO ARX, AC TO AR
						;;8051	.IFNOT/EXTEXP				;Don't allow G floating exponents
						;;8052		SC_#+AR0-8,#/-21,SKP SCAD0,	;[427] VALID EXTENDED OPERATION?
						;;8053			ARX_AR,AR_BR		; OPR TO ARX, AC TO AR
						; 8054	.ENDIF/EXTEXP
U 2622, 1002,3242,2000,0000,0000,0010,0000	; 8055	=0	AR_BR,J/UUO			;Opcode is too large.
U 2623, 3376,3242,6017,0000,1000,1010,0176	; 8056		E0_AR,MQ_AR,AR_BRX		;SAVE E0.  GET AC FROM EXTEND
						; 8057	.IF/EXTEXP
U 3376, 3377,0001,0000,2302,2000,0110,0032	; 8058		AR0-8_#+SC,#/32,SC/SCAD		;COMBINE EXT OP <32 WITH AC
						;;8059	.IFNOT/EXTEXP
						;;8060		AR0-8_#+SC,#/21,SC/SCAD		;COMBINE EXT OP <21 WITH AC
						; 8061	.ENDIF/EXTEXP
U 3377, 2640,4001,0000,2400,0020,5210,0000	; 8062		GEN SC,SKP SCAD NE		;TEST OP CODE
U 2640, 1002,3242,2000,0000,0000,0010,0000	; 8063	=0	AR_BR,J/UUO			;OP CODE = 0 (UUO) [217][251]
U 2641, 3400,3703,0000,0000,0000,1410,0000	; 8064		GEN AR,LOAD IR			;MAP THIS OVER THE LUUO SPACE
U 3400, 1734,2301,4000,0000,2000,0036,0000	; 8065	EXTF2:	AR_ARX,EA MOD DISP		;[427] GO EVALUATE E1
						; 8066	;
						; 8067	;	[325]
						; 8068	;	The effective address dispatch logic is quite arcane.  It appears
						; 8069	;	that MEM/A RD,DISP/DRAM A RD, and SH/2 interact to get the section
						; 8070	;	number from either AD (if the AC > 777777) or from VMA section, but
						; 8071	;	in order for that to work, we must do something with the VMA, even
						; 8072	;	though we don't actually use it here if the address computation
						; 8073	;	is complete.  Thus the VMA/LOAD has been added for the index case.
						; 8074	;
						; 8075	=1100					;[427]
U 1734, 2102,3701,0300,0000,0004,0033,0400	; 8076	EXTLA:	GEN AR,EXT ADDR,ARX/MQ,J/EXT2
U 1735, 3177,0600,0302,4000,2124,0002,0400	; 8077		GEN AR+XR,INDEXED,EXT INDEX,ARX/MQ,VMA/LOAD,J/BEXT2;[325][414]
U 1736, 1050,3701,0000,0000,0111,7010,0230	; 8078		GEN AR,EXT INDRCT,SKP INTRPT,J/EXTI
U 1737, 1050,0600,0002,4000,2131,7010,0230	; 8079		GEN AR+XR,INDEXED,EXT INDRCT,SKP INTRPT
						; 8080	=00					;[427]
U 1050, 3366,3240,0003,0000,0022,2550,0000	; 8081	EXTI:	ARX_MEM,LONG EN,CALL [BYTIND]	;[427] Unwind indirection
U 1051, 0144,3200,0003,0000,0022,7710,0000	; 8082		ARX_MEM,TAKE INTRPT		;Interrupted. Bust out of here
U 1052, 1734,2301,0002,0000,0020,0036,0000	; 8083		XR,EA MOD DISP,TIME/3T,J/EXTLA	;[427] Local word at end. Decode it
U 1053, 1756,2341,0002,0000,0020,0036,0000	; 8084		XR,EA MOD DISP,TIME/3T		;[427] Global word. Is it indexed?
						; 8085	=1110	GEN ARX,GLOBAL,EXT INDEX,ARX/MQ,;[414] No. Generate final address
U 1756, 3177,3713,0300,0000,1004,0002,0400	; 8086		    J/BEXT2; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1-1
; EIS.MIC[4,24]	10:37 27-May-86				EXTENDED INSTRUCTION SET DECODING			

						; 8087		GEN ARX+XR,GLOBAL,EXT INDEX,	;[414][427] Yes. Add index to
U 1757, 3177,0610,0302,4000,1024,0002,0400	; 8088		    ARX/MQ			; final word
						; 8089	;
						; 8090	;	As first written, locations 3044, 3045, 3046, 3050, 3051, 3052,
						; 8091	;	3144, 3145, 3146, 3147, 3150, 3151, 3152, 3153, and 3154 all were
						; 8092	;	B DISP,J/EXT2.  The comment:  these are index cases because index
						; 8093	;	must do AREAD with the DISP function in order to get the correct
						; 8094	;	index value for E1. 3077 is no longer the same, either. [414]
						; 8095	;
						; 8096	3177:
U 3177, 2102,4001,0000,0000,0000,0033,0000	; 8097	BEXT2:	B DISP				;[251][427] Test for offset mode
						; 8098	=010
						; 8099	EXT2:	E1_AR,AR_ARX,VMA_ARX+1,		;ESTABLISH E1
U 2102, 2000,4011,4007,0000,2320,1001,0165	; 8100			IR DISP,J/2000		;GO TO SPECIFIC HANDLER
U 2103, 2642,0001,0000,0000,0001,4410,0020	; 8101		ARL_0.M,SKP AR18,J/EXT3		;OFFSET MODE.  EXTEND E1
						; 8102		E1_AR,AR_ARX,VMA_ARX+1,		;[301] Duplicate these to
U 2106, 2000,4011,4007,0000,2320,1001,0165	; 8103			IR DISP,J/2000		; distinguish GSNGL (B=5) from
						; 8104		E1_AR,AR_ARX,VMA_ARX+1,		; offset instructions (B=1)
U 2107, 2000,4011,4007,0000,2320,1001,0165	; 8105			IR DISP,J/2000
						; 8106	=0
						; 8107	EXT3:	E1_AR,AR_ARX,VMA_ARX+1,		;ESTABLISH E1
U 2642, 2000,4011,4007,0000,2320,1001,0165	; 8108			IR DISP,J/2000		;GO TO SPECIFIC HANDLER
U 2643, 2642,2301,0000,0000,0000,0610,0002	; 8109		ARL_1S,J/EXT3			;NEGATIVE OFFSET
						; 8110	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; EIS.MIC[4,24]	10:37 27-May-86				EXTENDED INSTRUCTION SET DECODING			

						; 8111	;	By using "IR DISP,J/2000" we can use the same DRAM for LUUOs as
						; 8112	;	for the EXTEND instructions with like opcodes.  The LUUOs dispatch
						; 8113	;	to addresses in the range 1000-1017; by dispatching with J/2000,
						; 8114	;	the EXTEND ops go to 3000-3017 (model B) or 2000-20017 (model A).
						; 8115	
						; 8116		.DCODE
D 0001, 2200,1005				; 8117	001:	EA,	SJCL,	J/L-CMS		;CMSX HIDDEN BENEATH LUUO
D 0002, 2100,1005				; 8118		EA,	SJCE,	J/L-CMS
D 0003, 2001,1005				; 8119		EA,	SJCLE,	J/L-CMS
						; 8120	
D 0004, 2200,1006				; 8121	004:	EA,	B/2,	J/L-EDIT	;EDIT
D 0005, 2601,1005				; 8122		EA,	SJCGE,	J/L-CMS
D 0006, 2501,1005				; 8123		EA,	SJCN,	J/L-CMS
D 0007, 2400,1005				; 8124		EA,	SJCG,	J/L-CMS
						; 8125	
D 0010, 2101,1010				; 8126	010:	EA,	B/1,	J/L-DBIN	;CVTDBO
D 0011, 2401,1010				; 8127		EA,	B/4,	J/L-DBIN	;CVTDBT
D 0012, 2100,1011				; 8128		EA,	B/1,	J/L-BDEC	;CVTBDO
D 0013, 2001,1011				; 8129		EA,	B/0,	J/L-BDEC	;CVTBDT
						; 8130	
D 0014, 2100,1012				; 8131	014:	EA,	B/1,	J/L-MVS		;MOVSO
D 0015, 2001,1012				; 8132		EA,	B/0,	J/L-MVS		;MOVST
D 0016, 2200,1012				; 8133		EA,	B/2,	J/L-MVS		;MOVSLJ
D 0017, 2301,1012				; 8134		EA,	B/3,	J/L-MVS		;MOVSRJ
						; 8135		.UCODE
						; 8136	
						;;8137	.IFNOT/OWGBP				;[265]
						;;8138	3005:	AR_AC3,J/CMPS			;HIDDEN BEHIND L-CMS
						;;8139	3006:	CLR AR,ARX_1S,SC_#,#/15.,J/EDIT	;HIDDEN BEHIND L-EDIT
						;;8140	3010:	AR_AC0 COMP,J/DBIN		;HIDDEN BEHIND L-DBIN
						;;8141	3011:	AR_AC1,ARL/AD,SC_1,ARX+MQ_0.M,
						;;8142			BYTE DISP,J/BDEC	;HIDDEN BEHIND L-BDEC
						;;8143	3012:	AR_AC3,LOAD AR,J/MVST		;HIDDEN BEHIND L-MVS
						; 8144	.IF/OWGBP				;[265]
						; 8145	;
						; 8146	;	[347] CMPS dispatch rewritten to test bad high length bits first.
						; 8147	;
U 3005, 3401,3200,2000,0301,0020,0050,0777	; 8148	3005:	AR_AC0,FE_#,#/777,CALL [FLGTST]	;[347] Any illegal high bits in len?
U 3025, 2120,4001,0007,0000,0000,1050,0173	; 8149	3025:	FILL_AR,CALL [EXT2WD]		;[310][347] Save fill VMA, test OWG
U 3035, 3436,3200,2015,0000,2020,0010,0000	; 8150	3035:	AR_AC3,MQ_ARX,J/CMPS		;[310][347] Get dest length and go
						; 8151	;
U 3006, 0626,0001,0000,0000,0000,0010,0000	; 8152	3006:	J/EDIT				;HIDDEN BEHIND L-EDIT
U 3010, 1120,0001,0000,0000,0000,0010,0000	; 8153	3010:	J/DBIN				;HIDDEN BEHIND L-DBIN
U 3011, 0221,4011,2000,0000,0020,0010,0000	; 8154	3011:	AR_ARX+1 (AD),J/BDEC		;[344] HIDDEN BEHIND L-BDEC
						; 8155	;
U 3012, 3401,3240,2000,0301,0020,0050,0077	; 8156	3012:	AR_AC0,FE_#,#/77,CALL [FLGTST]	;[347] MVST. Watch out for illegal
U 3032, 0621,4001,0000,0000,0012,0010,0000	; 8157	3032:	LOAD AR,J/MVST			; flags first. 
						; 8158	;
						; 8159	;	Subroutine to check for bits set that are not allowed to be.
						; 8160	;	Enter with AR containing AC0 and FE with relevant bit mask.
						; 8161	;	Return 20 if none set; sideways exit to UUO if any are.  Note
						; 8162	;	that BRX must still contain the EXTEND for this to work.
						; 8163	;
U 3401, 3402,3200,2005,7021,0020,0010,0000	; 8164	FLGTST:	AR_AC3,FE_FE AND AR0-8		;[347] Get dest length
U 3402, 2660,4001,0000,6020,0020,5210,0000	; 8165		GEN FE OR AR0-8,SKP SCAD NZ	;[347] Are any high bits set?
U 2660, 0020,4013,2000,0000,0020,0003,0000	; 8166	=0	AR_ARX+1 (AD),RETURN20		;[347] No. Start saving fill VMA; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2-1
; EIS.MIC[4,24]	10:37 27-May-86				EXTENDED INSTRUCTION SET DECODING			

U 2661, 1002,3242,2000,0000,0000,0010,0000	; 8167		AR_BR,J/UUO			;[347] Yes. Blow out of the water
						; 8168	.ENDIF/OWGBP				;[265]
						; 8169	;3042:	AR_BR,J/UUO		;[217] INDEXING ON ILL. EXTEND OP.
						; 8170	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; EIS.MIC[4,24]	10:37 27-May-86				ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND	

						; 8171	.TOC	"ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND"
						; 8172	;
						; 8173	; HERE FOR MVST, EDIT AND CMPS INSTRUCTIONS
						; 8174	; MUST CHECK BOTH AC1 AND AC4 FOR OWGBP
						; 8175	; AND CONVERT TO TWO WORD GLOBAL POINTERS.
						; 8176	; There is also a hack in here for the CMPSx instructions.  In
						; 8177	; order to find their fill characters in the right place, we must
						; 8178	; fetch FILL (saved as E0+1) into ARX.  [310]
						; 8179	; BDEC ENTERS AT EXT01 FOR AC4 ONLY
						; 8180	.IF/OWGBP				;[265]
						; 8181	=000
U 2120, 3407,3240,2001,0000,0020,0050,0000	; 8182	EXT2WD:	AR_AC1,CALL [TST2WD]		;AC1 OWGBP ?
U 2121, 3415,0001,0000,0000,0000,0050,0000	; 8183		CALL [STR2WD]			;YES, CONVERT DONE, STORE
U 2122, 2124,4001,0000,0000,0000,0010,0000	; 8184		J/EXT01				;NO, TRY AC4
U 2123, 3406,3312,2004,0000,0000,1010,0000	; 8185		AC2_AR,AR_BR OR ARX,J/EXT02	;ADDRESS STORE
U 2124, 3407,3240,2006,0000,0020,0050,0144	; 8186	EXT01:	AR_AC4,CALL [TST2WD]		;AC4 OWGBP ?
U 2125, 3415,3240,0207,0000,0020,0050,0173	; 8187		ARX_FILL,CALL [STR2WD]		;[310] YES, CONVERT DONE, STORE
U 2126, 0010,3200,0207,0000,0020,0003,0173	; 8188		ARX_FILL,RETURN10		;[310][347] NO, CAN'T DO NO MORE
U 2127, 3403,0001,0000,0000,0000,0010,0145	; 8189		SEL DSTP2			;[310] DON'T GLITCH ON AC5 STORE
U 3403, 3404,3312,2006,0000,0000,1010,0145	; 8190		AC5_AR,AR_BR OR ARX		; (See second edit #210)
U 3404, 3405,3202,0600,0000,0000,0010,0144	; 8191		ARX_BRX,SEL AC4			;[310] RESTORE ARX AND SELECT AC4
U 3405, 0010,4001,0006,0000,0000,1003,0144	; 8192		AC4_AR,RETURN10			;[347] P,S,BIT 12 = 1 TO AC4
						; 8193	
U 3406, 2124,4001,0001,0000,0000,1010,0000	; 8194	EXT02:	AC1_AR,J/EXT01			;P,S,BIT 12 = 1 TO AC1
						; 8195	
						; 8196	;
						; 8197	;	TST2WD--TEST FOR OWG IN THE AR AND TO CONVERT IT IF IT'S OK.
						; 8198	;	Return 1 if converted OK, 2 if not OWG.  [437]
						; 8199	;
U 3407, 2146,0001,0000,5132,0020,5034,0045	; 8200	TST2WD:	SC_P-#,#/45,BYTE DISP,SKP PC SEC0;[437] Is this, in fact, an OWG?
U 2146, 3410,3731,0240,0000,0000,1710,0000	; 8201	=110	BR/AR,ARX_VMA HELD,J/CNV2WD	;[437] Yes. Convert to TWG
U 2147, 0002,4001,0000,0000,0000,0003,0000	; 8202	RET2:	RETURN2				;No. Just leave
						; 8203	;
						; 8204	;CNV2WD -- ROUTINE TO CALCULATE NEW P FIELD OF ONE WORD GLOBAL BYTE
						; 8205	;POINTER AND STORE NEW POINTER. A TABLE IS IN THE EPT STARTING AT 700
						; 8206	;AND THIS IS USED TO CONVERT THE OWGBP TO A TWO WORD GLOBAL POINTER
						; 8207	;AND TO CALCULATE THE NEW P FOR THE STORE.
						; 8208	;
U 3410, 1724,0001,0010,2401,2000,0022,0200	; 8209	CNV2WD:	MQ_ARX,AR0-8_SC,FE/SCAD		;[437] SAVE VMA. Set P-45
						; 8210	=0*	AR_ARX (AD),ARX_AR,SC_#,#/8,	;[437] Divide by two and right
U 1724, 2604,3713,2400,0302,1000,0050,0010	; 8211		    CALL [SHIFT]		; align in AR
U 1726, 3411,0001,0000,0000,0100,3310,0700	; 8212		VMA_#+AR32-35,#/700		;POINT TO RIGHT WORD
U 3411, 3412,4001,0000,0000,0012,0026,0111	; 8213		LOAD AR,EPT REF CACHE		;GET AND CACHE DATA FROM EPT [260]
						; 8214		MB WAIT,GEN FE AND #,#/1,	;[437] Wait for EPT word. Is this
U 3412, 2662,0001,0000,7030,0022,5210,0001	; 8215		    SKP SCAD NZ			; an odd offset?
						; 8216	=0
U 2662, 3413,4001,0400,2411,1000,0010,0000	; 8217	CNV01:	FE_S,ARX_AR,J/CNV02		;SKIP SWAP
U 2663, 2662,0001,4000,0000,3000,0010,0000	; 8218		AR_AR SWAP,J/CNV01		;SWAP HALVES FOR ODD
U 3413, 2700,3701,0020,0000,0020,5610,0000	; 8219	CNV02:	BRX/ARX,GEN AR,SKP AD NE	;DID WE GET 0 DATA ?
U 2700, 1002,0001,0040,0000,0000,0010,0000	; 8220	=0	BR/AR,J/UUO			;P=77 OR EPT NOT SET UP
U 2701, 3414,3723,0000,0000,0103,0010,0000	; 8221		RSTR VMA_MQ			;[307][326][347] NO, RESTORE VMA
U 3414, 0001,3202,0010,0000,0000,0703,0003	; 8222		MQ_BR,RETURN1			;GET OUT
						; 8223	
						; 8224	; HERE TO GET P,S,BIT 12 = 1 AND A GOOD ADDRESS
						; 8225	; SOME VERY TRICKY STUFF GOING ON HERE
						; 8226	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3-1
; EIS.MIC[4,24]	10:37 27-May-86				ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND	

U 3415, 3416,3260,2067,0000,0020,0010,0164	; 8227	STR2WD:	[AR]_FM[EXPMSK], BR/AR,BRX/ARX	;[310] P,S,JUNK TO BR, SAVE ARX
U 3416, 3417,3002,0204,0000,0000,0010,0000	; 8228		ARX_AR ANDCA BR			;P,S,0 TO ARX
U 3417, 3420,5100,2007,0000,0020,0010,0175	; 8229		[AR]_[AR]-FM[ADMSK]		;BIT 12 = 1 TO AR
						; 8230		AR_[MQ] AND FM[ADMSK],		;0,ADDRESS TO AR
U 3420, 0002,3620,2047,4000,0020,0003,0175	; 8231			BR/AR,RETURN2		;BIT 12=1 TO BR
						; 8232	.ENDIF/OWGBP				;[265]
						; 8233	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- STRING MOVE					

						; 8234	.TOC	"EIS -- STRING MOVE"
						; 8235	
						; 8236	; HERE FOR MOVE STRING, CHECK FOR OWGBP FIRST
						; 8237	;SLEN IS THE COMPLEMENT OF THE SHORTER STRING LENGTH
						; 8238	;DLEN IS <SRC LEN>-<DST LEN>
						; 8239	
						; 8240	.IF/OWGBP
						; 8241	=0*0*					;[347]
U 0621, 2147,3200,0003,0000,0022,0050,0000	; 8242	MVST:	AR_MEM,CALL [RET2]		;[260]GET FILL, WAIT FOR PARITY
U 0623, 2120,4001,0007,0000,0000,1050,0173	; 8243		FILL_AR,CALL [EXT2WD]		;SAVE FILL, CHECK FOR OWGBP
U 0633, 3421,3200,2005,0000,0020,0010,0000	; 8244	=1*1*	AR_AC3				;[347] GET DLEN
U 3421, 3422,3240,2040,0000,0020,0010,0000	; 8245		BR/AR,AR_AC0			;[347] Copy for length compare
						;;8246	.IFNOT/OWGBP
						;;8247	MVST:	BR/AR,AR_MEM,			;HOLD AC3, WAIT FOR FILLER
						;;8248			FE_AR0-8,SKP SCAD NE	;CHECK FOR FLAGS IN DEST LEN
						;;8249	=0	ARX_AC0,J/MVST1			;GET SRC LEN, FLAGS
						;;8250	NOLENS:	AR_E0,J/UUO			;NO FLAGS ALLOWED IN DST LEN
						;;8251	MVST1:	FILL_AR,AR_ARX			;SAVE FILL CHAR
						; 8252	.ENDIF/OWGBP
U 3422, 3423,0001,0000,2421,0000,0110,0000	; 8253		FE_AR0-8,AR0-8_#,#/0		;SEPARATE FLAGS OFF
U 3423, 2702,5102,2404,0000,1040,5510,0000	; 8254		ARX_AR,AR_AR-BR,SKP AD0		;COMPUTE SRC-DST LEN
U 2702, 3424,2542,2005,0000,0000,1010,0000	; 8255	=0	DLEN_AR,AR_BR COMP,J/MVST2	;SRC LONGER
U 2703, 3424,2001,6005,0000,0000,1010,0000	; 8256		DLEN_AR,AR_ARX COMP		;DST LONGER
U 3424, 3425,3401,2417,0000,1000,1010,0170	; 8257	MVST2:	SLEN_AR,ARX_AR,MQ_AR,AR_0S	;-SHORT LEN -1 TO MQ
U 3425, 3426,4001,0020,0000,2000,0022,0200	; 8258		AR0-8_FE,BRX/ARX		; AND BRX
U 3426, 2164,0001,0000,0000,0000,1033,0000	; 8259		SFLGS_AR,B DISP
U 2164, 3430,2301,0200,0302,0000,0410,0014	; 8260	=100	CLR AR,ARX_1S,SC_#,#/12.,J/MOVS2;[220]TRANSLATE, BUILD MASK
U 2165, 3427,3240,2006,0000,0020,0010,0144	; 8261		AR_DSTP,J/MVSO3			;OFFSET, MASK DEPENDS ON S
U 2166, 2240,4662,6600,0000,0020,1610,0111	; 8262		ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1	;LEFT JUSTIFY
U 2167, 1060,3200,2005,0000,0020,5510,0000	; 8263		AR_DLEN,SKP AD0,J/MOVRJ		;RIGHT JUSTIFY
						; 8264	
U 3427, 3430,2301,2000,2412,0000,0510,0000	; 8265	MVSO3:	SC_S,CLR ARX,AR_1S		;PREPARE TO BUILD MASK
U 3430, 3431,0001,4000,0000,0000,1610,0111	; 8266	MOVS2:	AR_SHIFT,SR_SRC
U 3431, 2200,4001,0007,0000,0000,1010,0167	; 8267		MSK_AR
						; 8268	=000
U 2200, 2600,4660,2007,0000,0020,0050,0170	; 8269	MOVELP:	AR_SLEN+1,CALL,J/SRCMOD		;PICK UP SOURCE BYTE
U 2201, 2241,3240,2005,0000,0020,0010,0000	; 8270		AR_DLEN,J/MOVSTX		;(1) LENGTH EXHAUSTED
						; 8271	=100
U 2204, 1160,4001,0000,0000,0000,1650,0213	; 8272	MOVPUT:	SR_SRC+DST,CALL,J/PUTDST	;(4) NORMAL, STORE DST BYTE
U 2205, 3432,3240,2005,0000,0020,0010,0000	; 8273		AR_DLEN,J/MVABT			;(5) ABORT [437]
U 2206, 2200,4001,0000,0000,0000,1610,0111	; 8274	=110	SR_SRC,J/MOVELP			;(6) DPB DONE
						; 8275	=
						; 8276	;HERE TO ABORT A STRING MOVE DUE TO TRANSLATE OR OFFSET FAILURE
						; 8277	
U 3432, 2710,5140,2047,0000,0020,4510,0170	; 8278	MVABT:	BR/AR,AR_-SLEN,SKP AR0		;WHICH STRING LONGER?
						; 8279	=0
U 2710, 3433,4001,0005,0000,0000,1010,0000	; 8280	MVABT1:	AC3_AR,J/MVABT2			;[437] PUT AWAY DEST LEN
U 2711, 2710,5102,2004,0000,0020,0010,0000	; 8281		AR_AR-BR,J/MVABT1		;DEST LEN WAS GREATER
						; 8282	
U 3433, 2712,2540,2007,0000,0237,4210,0170	; 8283	MVABT2:	AR_SLEN COMP,SKP BR0,I FETCH	;[437] GET UNDECREMENTED SLEN
U 2712, 2713,0602,2000,0000,0020,0010,0000	; 8284	=0	AR_AR+BR			;SRC LONGER BY (DLEN)
U 2713, 0015,3300,2000,0000,0020,1610,0000	; 8285	MVEND:	AR_AR*SFLGS,AD/OR,SR_0,J/STAC	;PUT BACK REMAINING LEN, don't skip
						; 8286	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- STRING MOVE					

						; 8287	;HERE TO BEGIN RIGHT-JUSTIFIED MOVE
						; 8288	
						; 8289	=00
U 1060, 2730,3200,2401,0000,1020,1610,0111	; 8290	MOVRJ:	ARX_AR,AR_SRCP,SR_SRC,J/MVSKP	;SRC LONGER, SKIP OVER SOME
U 1061, 0742,4001,0000,0000,0000,1650,0214	; 8291		SR_DSTF,CALL,J/MOVF1		;DST LONGER, FILL IT
U 1063, 2240,4662,6600,0000,0020,1610,0111	; 8292	=11	ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1	;DONE FILLING
						; 8293	
						; 8294	=0
						; 8295	MVSKP:	ARX_ARX-1 (AD),FE_#,#/36.,
U 2730, 2016,1711,0200,0301,0040,7032,0044	; 8296			SIGNS DISP,SKP INTRPT,J/MVSK1
U 2731, 2730,4001,2000,5010,3020,0610,0200	; 8297		P_FE-S,AR_AR+1,J/MVSKP
						; 8298	=1110
U 2016, 2730,4001,0000,5110,3021,5110,0200	; 8299	MVSK1:	P_P-S,SKP SCAD0,J/MVSKP		;BUMP POINTER
U 2017, 2036,3711,0001,0000,0021,1032,0030	; 8300		SRCP_AR,GEN ARX,SIGNS DISP,AR_0.M
U 2036, 3434,2500,2227,0000,0020,0010,0170	; 8301	=1110	BRX/ARX,AR_SLEN COMP,ARX/AD,J/MVSK3	;INTERRUPTED
U 2037, 2246,4001,0005,0000,0000,1010,0000	; 8302		DLEN_AR,J/MVSK4			;DONE FILLING
						; 8303	
U 3434, 3435,4602,6005,4000,0020,1010,0000	; 8304	MVSK3:	AC3_AR,AR_ARX*BRX,AD/A+B+1	;DEST HAS SHORT LEN
U 3435, 3774,4001,0000,0000,0000,1610,0000	; 8305		SR_0,J/STRPF2			;FIX UP AC0, SERVE INTRPT
						; 8306	
						; 8307	;HERE FOR NO-MODIFICATION STRING MOVES
						; 8308	
						; 8309	;[266]	Remove edit 244
						; 8310	;;[244]	THIS ADDRESS MUST REMAIN SET FOR THE PROBLEM
						; 8311	;;	OF THE S FIELD OF THE SOURCE POINTER BEING > 36.
						; 8312	;;
						; 8313	;.IF/MODEL.B
						; 8314	;1300:					;[244]
						; 8315	;MOVST1:	SLEN_AR,BRX/ARX,		;PUT UPDATED LEN AWAY
						; 8316	;		AR+ARX+MQ_0.M,CALL.M,
						; 8317	;		SIGNS DISP,J/GSRC
						; 8318	;1301:
						; 8319	;MOVSTX:	SKP AR0,ARX_AR,AR_0S,J/MOVST2	;SHORT LEN EXHAUSTED
						; 8320	;1302:	SR_SRC+DST,CALL,J/PUTDST
						; 8321	;1306:
						; 8322	;MVSK4:	ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1
						; 8323	;.IFNOT/MODEL.B		;[244][266]
						; 8324	=000
						; 8325	MOVST1:	SLEN_AR,BRX/ARX,		;PUT UPDATED LEN AWAY
						; 8326			AR+ARX+MQ_0.M,CALL.M,
U 2240, 2313,4001,0027,0000,0021,1072,0170	; 8327			SIGNS DISP,J/GSRC
U 2241, 1070,3401,2400,0000,1000,4510,0000	; 8328	MOVSTX:	SKP AR0,ARX_AR,AR_0S,J/MOVST2	;SHORT LEN EXHAUSTED
U 2242, 1160,4001,0000,0000,0000,1650,0213	; 8329	=010	SR_SRC+DST,CALL,J/PUTDST
						; 8330	=110
U 2246, 2240,4662,6600,0000,0020,1610,0111	; 8331	MVSK4:	ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1
						; 8332	=
						; 8333	;.ENDIF/MODEL.B		;[244][266]
						; 8334	=00
						; 8335	MOVST2:	TEST ARX,TEST FETCH,		;SKIP IF BOTH LENGTHS =0
U 1070, 2713,3711,4005,0000,2246,1010,0203	; 8336			AC3_AR,AR_ARX,J/MVEND	;CLEAR DEST LEN, REBUILD SRC
U 1071, 0742,4001,0000,0000,0000,1650,0212	; 8337		SR_DST,CALL,J/MOVF1		;SOURCE GONE, FILL OUT DST
U 1073, 3532,3240,2000,0000,0220,0010,0000	; 8338	=11	AR_SFLGS,VMA_PC+1,J/SFET1	;DONE FILLING
						; 8339	
						; 8340	;NOTE -- IT AIN'T AS EASY AS IT LOOKS TO BUM A CYCLE OUT OF THIS
						; 8341	; ROUTINE, BECAUSE AN INTERRUPT, IF ANY, HAS TO BE TAKEN AFTER THE
						; 8342	; POINTER UPDATE AND BEFORE THE LENGTH UPDATE.  GOOD HUNTING!; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5-1
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- STRING MOVE					

						; 8343	=01*
U 0742, 1160,3240,2007,0000,0020,0050,0173	; 8344	MOVF1:	AR_FILL,CALL,J/PUTDST
U 0746, 2732,4640,2005,0000,0020,7010,0000	; 8345		AR_DLEN+1,SKP INTRPT,J/MOVF2
						; 8346	=0
U 2732, 2053,0001,0005,0000,0020,1032,0000	; 8347	MOVF2:	DLEN_AR,SIGNS DISP,J/MOVF3	;DONE?
U 2733, 0340,4001,0000,0000,0000,0005,0000	; 8348		SR DISP,J/CLEAN			;BREAK OUT FOR INTERRUPT
						; 8349	=1011
U 2053, 0002,4001,0000,0000,0000,0003,0000	; 8350	MOVF3:	RETURN2				;YES, DONE
U 2057, 0742,0001,0000,0000,0000,0010,0000	; 8351		J/MOVF1				;NO, DO ANOTHER
						; 8352	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- STRING COMPARE					

						; 8353	.TOC	"EIS -- STRING COMPARE"
						; 8354	
						; 8355	;HERE FOR CMPS, CHECK FOR OWGBP FIRST
						; 8356	; [310] E0+1 will be saved in FILL during OWG checking.  We restore it
						; 8357	; from ARX to MQ here.  This keeps us from fetching bogus fill characters.
						; 8358	
						; 8359	.IF/OWGBP
U 3436, 3437,3200,2440,0000,1020,0010,0000	; 8360	CMPS:	BR/AR,ARX_AR,AR_AC0		;[347]DEST LEN TO BR, GET SRC LEN
U 3437, 2740,3102,0004,0000,0020,5410,0000	; 8361		SKP AR GT BR			;[347] Which string is longer?
U 2740, 3440,3723,0000,0000,0300,0010,0000	; 8362	=0	VMA_MQ,J/CMPS1			;[310] Source shorter
U 2741, 3440,4021,0000,0000,0320,0010,0000	; 8363		VMA_MQ+1			;[310] SRC LONGER, GET DST FILLER
U 3440, 3441,1701,6200,0000,0032,0010,0000	; 8364	CMPS1:	LOAD AR,AR_ARX-1,ARX_AR-1,TIME/3T;[347] Decrement lengths, get fill
U 3441, 2743,3200,0063,0000,0022,0010,0000	; 8365		AR_MEM,BR/AR,BRX/ARX,J/CMPS4	;DECREMENTED LEN'S TO BR'S
						; 8366	
						;;8367	.IFNOT/OWGBP				;[347]
						;;8368	CMPS:	BR/AR,ARX_AR,FE_AR0-8,AR_AC0	;DEST LEN TO BR, GET SRC LEN
						;;8369		FE_FE OR AR0-8,			;GATHER HIGH BITS OF LEN'S
						;;8370			SKP AR GT BR		;WHICH STRING LONGER?
						;;8371	=0					;[347]
						;;8372	CMPS1:	LOAD AR,AR_ARX-1,ARX_AR-1,	;SRC SHORTER
						;;8373			GEN FE,SKP SCAD NE,J/CMPS2 ;CHECK LEN'S PURE
						;;8374		VMA_VMA+1,J/CMPS1		;SRC LONGER, GET DST FILLER
						;;8375	=0
						;;8376	CMPS2:	AR_MEM,BR/AR,BRX/ARX,J/CMPS4	;DECREMENTED LEN'S TO BR'S
						;;8377		AR_MEM,J/NOLENS			;[275] ILLEGAL BITS IN LEN'S
						; 8378	.ENDIF/OWGBP
						; 8379	
						; 8380	;HERE IS THE COMPARE LOOP.
						; 8381	; MQ CONTAINS THE FILL CHARACTER FOR THE SHORTER STRING,
						; 8382	; BR CONTAINS THE REMAINING DESTINATION LENGTH,
						; 8383	; BRX CONTAINS THE REMAINING SOURCE LENGTH
						; 8384	=0
U 2742, 2076,4323,0700,0000,0020,0010,0000	; 8385	CMPS3:	ARX0_MQ35,J/CMPSX		;WE GOT INEQUALITY.  GET SIGN
						; 8386	CMPS4:	MQ_AR,ARX_AR,FE_#,#/36.,	;FILL TO MQ & ARX
U 2743, 2070,3202,2410,0301,1000,4310,0044	; 8387			AR_BR,SKP ARX0		;MORE CHARS IN SRC STRING?
						; 8388	=1000	AR_SRCP,ARX_SRCP,		;READY WITH SRC POINTER
U 2070, 3036,3240,2201,0000,0040,1650,0101	; 8389			SR_ED(S),CALL,J/GSRC1	;GO GET SRC BYTE
U 2071, 2072,3401,4200,0000,2020,1632,0000	; 8390		AR_ARX,ARX_0S,SR_0,SIGNS DISP	;SRC DONE.  TEST DEST LEN
U 2072, 2075,3721,2007,0000,0020,1032,0166	; 8391	=1010	T0_AR,AR_MQ,SIGNS DISP,J/CMPS5	;SRC (OR SRC FILL) TO T0,
						; 8392	=1110					;TEST FOR END OF DEST STRING
U 2076, 0217,3711,0000,0000,0246,0010,0202	; 8393	CMPSX:	GEN ARX,CMS FETCH,J/NOP		;QUIT WITH COMPARE COND IN ARX
						; 8394	=
						; 8395	;HERE TO GET DESTINATION BYTE.  SRC IS IN T0, FILL CHAR IN AR
						; 8396	;HERE WITH SIGNS DISP, TO AVOID CALL ON CMPDST IF DST LEN EXHAUSTED
						; 8397	
						; 8398	=1101
U 2075, 2760,0001,0000,0000,0000,1650,0224	; 8399	CMPS5:	SR_ED(+D),CALL,J/CMPDST		;GO FOR DESTINATION BYTE
						; 8400		AR_AR*T0,AD/XOR,		;AR ZERO IF EQUAL
U 2077, 3442,3100,2307,4000,0020,0012,0166	; 8401			ARX/MQ,MQ_MQ*2		;FILL TO ARX, CRY TO MQ35
						; 8402		BR/AR,BRX/ARX,			;EQUALITY TO BR, FILL TO BRX
U 3442, 2754,3202,2660,0000,0000,4210,0000	; 8403			AR_BR,ARX_BRX,SKP BR0	;LENGTHS TO AR, ARX
						; 8404	=0	AC3_AR,ARX_AR,AR_ARX (AD),	;UPDATE DEST LEN IN AC3
U 2754, 2116,3713,2405,0000,1020,1032,0000	; 8405			SIGNS DISP,J/CMPS6	;TEST SRC LEN
U 2755, 2116,3713,2400,0000,1000,0010,0000	; 8406		ARX_AR,AR_ARX (AD)		;DEST LEN EXHAUSTED
						; 8407	=1110
U 2116, 3443,1701,6200,0000,0040,1010,0000	; 8408	CMPS6:	AC0_AR,AR_ARX-1,ARX_AR-1,J/CMPS7	;UPDATE SRC LEN IN AC0; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6-1
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- STRING COMPARE					

U 2117, 3443,1703,6200,0000,0040,0010,0000	; 8409		AR_ARX-1,ARX_AR-1		;SRC EXHAUSTED PREVIOUSLY
						; 8410	
						; 8411	CMPS7:	BR/AR,BRX/ARX,			;LENGTHS TO BR'S
U 3443, 2742,7142,6060,0000,0040,5410,0000	; 8412			SKP BR EQ,AR/ADX,J/CMPS3	;CHECK FOR EQUALITY
						; 8413	
						; 8414	=0
						; 8415	CMPDST:	AR_DSTP,ARX_DSTP,		;GET DEST BYTE FOR COMPARE
U 2760, 3515,3240,2206,0000,0040,0050,0144	; 8416			CALL,J/IDST		;UPDATE DEST POINTER
U 2761, 2570,0001,0000,2002,0000,7010,0000	; 8417		SC_FE+SC,SKP INTRPT,J/LDB1	;GET DEST BYTE
						; 8418	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- DECIMAL TO BINARY CONVERSION			

						; 8419	.TOC	"EIS -- DECIMAL TO BINARY CONVERSION"
						; 8420	
						; 8421	; HERE WITH AC0 (SRC LEN) IN AR COMPLEMENTED
						; 8422	; IN THE LOOP, AC3 CONTAINS 10 (DECIMAL), BR'BRX HAS ACCUMULATED BINARY
						; 8423	; First take care of OWG conversion. [441]
						; 8424	
						; 8425	.IF/OWGBP
						; 8426	=00
U 1120, 3407,3240,2001,0000,0020,0050,0000	; 8427	DBIN:	AR_AC1,CALL [TST2WD]		;[441] AC1 OWGBP ?
U 1121, 3415,0001,0000,0000,0000,0050,0000	; 8428		CALL [STR2WD]			;YES, CONVRT DONE, STORE
U 1122, 3445,2540,2000,0000,0020,0010,0000	; 8429	DBFLGS:	AR_AC0 COMP,J/DBINGO		;[441] Maybe no. FLAGS TO AR
U 1123, 3444,3312,2004,0000,0000,1010,0000	; 8430		AC2_AR,AR_BR OR ARX		;[407] Address to AC2
U 3444, 1122,4001,0001,0000,0000,1010,0000	; 8431		AC1_AR,J/DBFLGS			;[407] P,S,bit 12 = 1 to AC1
						; 8432	;
U 3445, 3446,0001,0040,1421,0000,0110,0777	; 8433	DBINGO:	BR/AR,FE_AR0-8 COMP,AR0-8_#,#/-1;[441] FORCE OUT FLAGS
						;;8434	.IFNOT/OWGBP
						;;8435	DBIN:	BR/AR,FE_AR0-8 COMP,AR0-8_#,#/-1	;FORCE OUT FLAGS
						; 8436	.ENDIF/OWGBP
U 3446, 2135,3441,2207,0000,0020,1032,0170	; 8437		SLEN_AR,AR_0S,ARX_0S,SIGNS DISP
U 2135, 3450,3200,0216,0000,2020,0110,0144	; 8438	=1101	AR0-8_FE#,MQ_ARX,ARX_AC4,J/DBS1	;BUILD SFLGS
U 2137, 2266,0001,0000,0000,0000,0033,0000	; 8439		B DISP				;OFFSET OR TRANSLATE?
U 2266, 3447,4001,0000,0000,2000,0022,0200	; 8440	=110	AR0-8_FE,J/DBST			;TRANSLATE, LET S FLAG SET LATER
U 2267, 3447,4001,0000,6030,2000,0110,0400	; 8441		AR0-8_FE OR #,#/400		;OFFSET, SET S FLAG
U 3447, 3452,3441,2200,0000,0000,1010,0000	; 8442	DBST:	SFLGS_AR,AR_0S,ARX_0S,J/DBS2	;CLEAR BINARY
						; 8443	
U 3450, 3451,3721,0500,0000,0000,1010,0000	; 8444	DBS1:	SFLGS_AR,ARX_ARX*2		;HERE WHEN SIG ALREADY ON
U 3451, 3452,3240,2005,0000,0020,0010,0000	; 8445		AR_AC3				;ACCUMULATED BINARY IN AR
U 3452, 3453,4041,2060,0000,0020,0510,0000	; 8446	DBS2:	BR_AR LONG,AR_1,CLR ARX
U 3453, 2306,0603,5004,0302,0020,0033,0004	; 8447		AR_AR*10,B DISP,SC_#,#/4	;GET CONSTANT 10 FOR COMPARE
U 2306, 3454,2301,4205,0000,2000,1010,0000	; 8448	=110	AC3_AR,AR_ARX,ARX_1S,J/DBS3	;PREPARE TO BUILD MASK
U 2307, 3454,2301,2005,0000,0000,1010,0000	; 8449		AC3_AR,AR_1S			;OFFSET
U 3454, 3455,0001,4000,0000,0000,1610,0102	; 8450	DBS3:	AR_SHIFT,SR_DB
U 3455, 0520,3202,2607,0000,0000,1010,0167	; 8451		MSK_AR,AR_BR LONG		;SAVE MASK, GET INITIAL INPUT
						; 8452	
						; 8453	=0*0
						; 8454	DBINLP:	BR_AR LONG,AR_SLEN+1,		;BINARY BACK TO BR, COUNT LENGTH
U 0520, 2600,4660,2067,0000,0020,0050,0170	; 8455			CALL,J/SRCMOD		;PICK UP A DIGIT
U 0521, 2764,3203,0000,0000,0220,5510,0000	; 8456		SKP AR2,VMA_PC+1,J/DBXIT	;(1) DONE, TEST M FLAG
						; 8457		ARX_AR,AR+MQ_0.M,GEN AR-AC3,	;(4) NORMAL, ADD IN DIGIT
U 0524, 2762,5100,0405,0000,1041,5410,0130	; 8458			SKP CRY0,J/DBIN2	;TEST FOR DIGIT >9
U 0525, 3460,2540,2007,0000,0020,0010,0170	; 8459		AR_SLEN COMP,J/DBABT		;(5) ABORT
						; 8460	
						; 8461	;HERE TO ADD IN A DIGIT
						; 8462	
						; 8463	=0
U 2762, 3456,3202,2660,0000,0000,0010,0000	; 8464	DBIN2:	BR_AR LONG,AR_BR LONG,J/DBIN3	;DIGIT TO BR LONG, BINARY TO AR LONG
U 2763, 3460,2540,2007,0000,0020,0010,0170	; 8465		AR_SLEN COMP,J/DBABT		;DIGIT >9, ABORT
						; 8466	
U 3456, 3457,0603,2604,0000,0020,0027,0000	; 8467	DBIN3:	AR_AR*5 LONG			;ALREADY HAVE BINARY *2
U 3457, 0520,0602,5500,0000,0020,0027,0000	; 8468		AR_2(AR+BR) LONG,J/DBINLP	;ADD IN DIGIT, SHIFT LEFT
						; 8469	
						; 8470	;HERE ON ABORT
						; 8471	
U 3460, 3461,3300,2000,0000,0020,0010,0000	; 8472	DBABT:	AR_AR*SFLGS,AD/OR		;[230][221]FLAGS +LEN REMAINING
						; 8473		AC0_AR,AR_BR LONG,SC_#,#/35.,	;PUT BACK UNUSED LENGTH
U 3461, 3462,3242,2600,0302,0200,1010,0043	; 8474			VMA_PC+1,J/STOR34	;END WITH NO SKIP; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7-1
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- DECIMAL TO BINARY CONVERSION			

						; 8475	
						; 8476	;HERE AT END
						; 8477	
						; 8478	=0
						; 8479	DBXIT:	AR_BR LONG,VMA_VMA+1,		; M FLAG=0
U 2764, 3462,3242,2600,0302,0000,3610,0043	; 8480			SC_#,#/35.,J/STOR34	;GO FOR NEXT INSTR
						; 8481		AR_-BR LONG,VMA_VMA+1,		;NEGATE
U 2765, 3462,5142,2600,0302,0020,3627,0043	; 8482			SC_#,#/35.
U 3462, 3463,5401,2005,0000,0037,1016,0000	; 8483	STOR34:	AC3_AR,AR_SIGN,FETCH		;STORE HIGH PART
U 3463, 3464,4001,4000,0000,0000,1610,0000	; 8484		AR_SHIFT,SR_0			;GET LOW READY
U 3464, 3465,4001,0000,0000,0000,0010,0144	; 8485		SEL AC4				;PRESEL NUMBER TO FIX HARDW GLITCH
U 3465, 0133,0001,0006,0000,0000,1010,0144	; 8486	STAC4:	AC4_AR,FINISH
						; 8487	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- BINARY TO DECIMAL CONVERSION			

						; 8488	.TOC	"EIS -- BINARY TO DECIMAL CONVERSION"
						; 8489	
						; 8490	;	AC0,AC1 = BINARY INTEGER INPUT
						; 8491	;	AC3 = FLAGS, MAX LENGTH OF DECIMAL STRING
						; 8492	;	AC4 = DESTINATION STRING POINTER
						; 8493	; TEMPS ARE USED AS FOLLOWS:
						; 8494	;	FILL = VMA of fill character (to preserve through OWGBP check) [344]
						; 8495	;	SLEN= # OF SIGNIFICANT DIGITS
						; 8496	;	T1,2= 10.**(SLEN) THE LOWEST POWER OF TEN LARGER THAN BINARY
						; 8497	;
						; 8498	;FPD IS SET IF THE INSTRUCTION WAS INTERRUPTED AFTER CONVERSION OF THE
						; 8499	; BINARY INTEGER TO FRACTION FORM (AFTER STORING FILL, IF NEEDED).
						; 8500	
						; 8501	.IF/OWGBP
						; 8502	=0***					;[347]
U 0221, 2124,0001,0007,0000,0000,1050,0173	; 8503	BDEC:	FILL_AR,CALL [EXT01]		;[344] Save fill VMA, check OWGBP
						; 8504		AR_AC1,ARL/AD,SC_1,ARX+MQ_0.M,
U 0231, 1663,3200,2001,4402,0041,0034,0142	; 8505			BYTE DISP		;GET BIN INTEGER
						; 8506	=011	ARX_SHIFT,AR_AC0,SKP AD0,	;BINARY INTEGER NOW IN AR LONG
U 1663, 3000,3200,2400,0302,0020,5510,0020	; 8507			SC_#,#/20,J/BD1		;IS IT NEGATIVE?
						;;8508	.IFNOT/OWGBP
						;;8509	=011
						;;8510	BDEC:	ARX_SHIFT,AR_AC0,SKP AD0,	;BINARY INTEGER NOW IN AR LONG
						;;8511			SC_#,#/20,J/BD1		;IS IT NEGATIVE?
						; 8512	.ENDIF/OWGBP
U 1667, 3466,3200,2405,0000,1020,1610,0010	; 8513	BDDR1:	ARX_AR,AR_AC3,SR_BDT		;RESUME WITH FRACTION IN AR LONG
						; 8514		BR/AR,CLR EXP,			;SEPARATE FLAGS & LENGTH
U 3466, 3467,3240,0260,0400,2021,0010,0200	; 8515			BRX/ARX,ARX_AC0		;LOW FRAC TO BRX, HI TO ARX
U 3467, 3470,3002,2044,0000,0000,0010,0000	; 8516		AR_AR*BR,AD/ANDCA,BR/AR		;JUST FLAGS TO AR, JUST LEN TO BR
U 3470, 3471,4001,4005,0000,2000,1010,0000	; 8517		AC3_AR,AR_ARX			;GET HI FRAC TO AR
						; 8518		BR/AR,VMA_PC+1,			;FRAC TO BR LONG, GET VMA READY
U 3471, 3016,5162,2040,0000,0240,5410,0000	; 8519			AR_-BR,SKP CRY0,J/BDDR4	;CHECK FOR MORE TO GO
						; 8520	
						; 8521	=0
U 3000, 1130,3703,0000,0000,0040,5427,0000	; 8522	BD1:	SKP AR NE,AD LONG,J/BD2		;TEST FOR ZERO LONG
U 3001, 1131,5143,7700,0302,0020,0027,0030	; 8523		AR_-AR LONG,SC_#,#/30,J/BD3	;MAKE POSITIVE, SET N&M FLAGS
						; 8524	=00
						; 8525	BD2:	BR_AR LONG,AR_1 LONG,		;BINARY RIGHT-ALIGNED IN BR,
U 1130, 2342,4041,7660,0303,0020,0010,0024	; 8526			SC_#,FE_#,#/20.,J/BD4	;LOOK FOR LARGER POWER OF TEN
						; 8527	BD3:	BR_AR LONG,AR_AC3,		;SAVE POS BINARY, GET AC FLAGS
U 1131, 3553,3200,2065,0000,0020,0050,0000	; 8528			CALL,J/SETFLG		; SET FLAGS AS NEEDED
U 1133, 1130,3241,7705,0000,0000,1010,0000	; 8529	=11	AC3_AR,AR_BR*.5 LONG,J/BD2	;SAVE NEW FLAGS, SHIFT BINARY RIGHT
						; 8530	
						; 8531	;HERE TO FIND THE SMALLEST POWER OF TEN LARGER THAN THE BINARY INTEGER.
						; 8532	;BINARY IS IN BR LONG, AND POSITIVE UNLESS IT WAS 1B0.  IN THIS CASE THE
						; 8533	;COMPARISON WILL NEVER FIND A LARGER POWER OF TEN, BUT THE COUNT IN FE
						; 8534	;WILL RUN OUT, AND WE WILL CORRECTLY COMPUTE 22 DIGITS REQUIRED.
						; 8535	
						; 8536	=010					;IGNORE BR SIGN
U 2342, 3472,0603,5500,3001,0020,0027,0000	; 8537	BD4:	AR_AR*10 LONG,FE_FE-1,J/BD6	;THIS POWER IS TOO SMALL
U 2343, 2134,4001,4007,1002,2000,1010,0171	; 8538		SC_FE-SC-1,T1_AR,AR_ARX,J/BD7	;THIS POWER IS BIG ENOUGH
U 2346, 2347,4001,0000,3001,0000,0010,0000	; 8539		FE_FE-1				;10.**21 IS TOO SMALL, USE 22
U 2347, 2134,4001,4007,1002,2000,1010,0171	; 8540		SC_FE-SC-1,T1_AR,AR_ARX,J/BD7	;10.**21 IS BIG ENOUGH
						; 8541	
U 3472, 2342,1102,0004,0000,0020,0031,0000	; 8542	BD6:	GEN AR-BR-1,DISP/DIV,J/BD4	;COMPARE BINARY TO 10**N
						; 8543	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- BINARY TO DECIMAL CONVERSION			

						; 8544	;HERE HAVING FOUND THE NUMBER OF DIGITS REQUIRED TO REPRESENT THE
						; 8545	; GIVEN INTEGER.  THE ONE'S COMPLEMENT OF THE NUMBER OF DIGITS IS NOW
						; 8546	; IN SC, AND T1/T2 IS GETTING A POWER OF TEN LARGER THAN THE INPUT.
						; 8547	
						; 8548	=0*
U 2134, 3374,2341,2007,0000,0000,1050,0172	; 8549	BD7:	T2_AR,AR_1S,CALL,J/GETSC	;SAVE (10**N), GET -# OF DIGITS
U 2136, 3473,2503,0207,0000,0000,1010,0170	; 8550		SLEN_AR,ARX_AR*4 COMP		;-# OF SIGNIFICANT DIGITS-1
U 3473, 3474,3200,2005,0000,0020,0010,0000	; 8551		AR_AC3				;GET FLAGS, LENGTH
U 3474, 3475,0001,0000,2421,0000,0110,0000	; 8552		FE_AR0-8,AR0-8_#,#/0		;LEN IN AR, FLAGS IN FE
						; 8553		AR_ARX*.25-AR-1,SKP CRY0,	;-# OF FILL CHARS -1
U 3475, 3014,1113,7000,5032,0040,5410,0400	; 8554			SC_FE-#,#/400		;SC0 SET IF S FLAG =0
U 3014, 3476,4003,0200,0000,0021,0010,0030	; 8555	=0	ARX_AR+1,AR_0.M,J/BD8		;ENOUGH SPACE. -FILL CNT TO ARX
U 3015, 0217,4001,0000,0000,0217,0010,0000	; 8556		I FETCH,J/NOP			;OVERFLOW
						; 8557	BD8:	AR0-8_FE.M,SKP SC0,		;FLAGS TO AR.  S FLAG =0?
U 3476, 2156,2013,0000,0000,2021,4732,0200	; 8558			GEN ARX COMP,SIGNS DISP	; OR EXACT LENGTH?
U 2156, 3477,3260,0007,0000,0332,0010,0173	; 8559	=1110	VMA_FM[FILL],LOAD AR,J/BDF1	;[344] Must fill. GET FILLER
U 2157, 3500,0001,0005,0000,0000,1010,0000	; 8560	BD9:	AC3_AR,J/BDDV1			;NO FILL.  FLAGS TO AC3
						; 8561	
U 3477, 1140,4001,0007,0000,0000,1010,0166	; 8562	BDF1:	T0_AR				;[344] Save flags in T0
U 1140, 3003,3200,0003,0000,0022,1650,0203	; 8563	=00	AR_MEM,SR_BDF,CALL,J/RET1	;GET FILLER, GO WAIT FOR PARITY
U 1141, 2732,4001,4007,0000,2000,1050,0173	; 8564		FILL_AR,AR_ARX,CALL,J/MOVF2	;FILL AS REQUIRED
U 1143, 2157,3200,2007,0000,0020,0010,0166	; 8565	=11	AR_T0,J/BD9			;GET FLAGS BACK
						; 8566	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- BINARY TO DECIMAL CONVERSION			

						; 8567	;SETUP FOR LONG DIVISION OF BINARY BY 10**N
						; 8568	;BR STILL HAS BINARY RIGHT ALIGNED (IE, LOW SIGN SQUEEZED OUT BY
						; 8569	; SHIFTING HIGH WORD RIGHT).  BR IS POSITIVE UNLESS INPUT INTEGER WAS
						; 8570	; 1B0, IN WHICH CASE BR IS -1B1.  T1,T2 HAS LARGER POWER OF TEN, UNLESS
						; 8571	; BINARY EXCEEDS 10**21, IN WHICH CASE T1,T2 CONTAINS 10**21. SINCE
						; 8572	; BINARY CANNOT BE AS LARGE AS 2 * 10**21, THE FIRST DIVIDE STEP
						; 8573	; IS GUARANTEED TO GENERATE A 1 IN THIS CASE ONLY, AND TO REDUCE THE
						; 8574	; BINARY TO LESS THAN 10**21.
						; 8575	
U 3500, 2506,3240,0207,0000,0020,0410,0172	; 8576	BDDV1:	ARX_T2,CLR AR			;FILL DONE.  GET 10**N
						; 8577	=110	AR_T1,MQ_AR,			;D'SOR SET IN AR, MQ CLR
U 2506, 2540,3200,2017,0000,1020,4250,0171	; 8578			SKP BR0,CALL,J/BDDV2	; CHK D'END SIGN
U 2507, 3501,3240,2400,0000,1020,1110,0100	; 8579		ARX_AR,AR_AC0,SET FPD		;DONE, GET FULL QUO IN AR LONG
U 3501, 3502,4001,2600,0000,0020,1627,0010	; 8580		AR_AR+1 LONG,SR_BDT,J/BDD1	;PREVENT 9'S DISEASE
						; 8581	
						; 8582	=000
						; 8583	BDDV2:	AR_BR LONG,BR_AR LONG,		;BEGIN LONG DIVISION
						; 8584			SC_#,FE_#,#/34.,	;STEP COUNTS FOR BOTH PARTS
U 2540, 0762,3202,2660,0303,0000,0050,0042	; 8585			CALL,J/DDVSUB
						; 8586		AR_-BR,ARX/ADX,BR_AR LONG,	;HERE IF BINARY WAS 1B0
						; 8587			SC_#,FE_#,#/34.,	; IT'S NOW 1B1
U 2541, 0762,5142,2660,0303,0020,0050,0042	; 8588			CALL,J/DDVSUB
						; 8589	=011	AC0_AR,AR_MQ,ARL/AD,MQ_0.M,	;HALF DONE WITH DIVISION
U 2543, 0760,3721,2000,2401,0001,1010,0102	; 8590			FE_SC,J/DDVLP		;RESUME WITH ADD STEP
						; 8591	=101	AC0_AR,AR_MQ,ARL/AD,MQ_0.M,
U 2545, 0762,3723,2000,2401,0001,1010,0102	; 8592			FE_SC,J/DDVSUB		;RESUME WITH SUBTRACT STEP
						; 8593	=
						; 8594	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- BINARY TO DECIMAL CONVERSION			

						; 8595	;HERE WITH QUOTIENT OF <INPUT INTEGER>/<10**N> IN AR LONG, WITH THE
						; 8596	; BINARY POINT BETWEEN BITS 0 AND 1 OF AR.  THUS, BIT 0 WILL BE SET
						; 8597	; IFF THE INPUT INTEGER WAS GREATER THAN OR EQUAL TO 10**21.
						; 8598	; SINCE THIS IS A TRUNCATED FRACTION, IT IS NOT GREATER THAN THE TRUE
						; 8599	; QUOTIENT, AND THE ERROR IS LESS THAN 2**-71. WE ADD 2**-71, TO
						; 8600	; GUARANTEE THAT OUR FRACTION IS GREATER THAN THE TRUE QUOTIENT,
						; 8601	; WITH AN ERROR NO GREATER THAN 2**-71.  WE WILL THEN MULTIPLY THIS
						; 8602	; FRACTION BY 10 N TIMES, REMOVING THE INTEGER PART AT EACH STEP
						; 8603	; TO EXTRACT THE N DIGITS.  SINCE N IS AT MOST 21, THIS IS A MULTIPLI-
						; 8604	; CATION BY AT MOST 10**21, SO THE ERROR IS AT MOST (2**-71)*(10**21).
						; 8605	; SINCE THIS IS LESS THAN ONE, THE ERROR DOES NOT INTRUDE INTO THE
						; 8606	; OUTPUT DIGIT STRING.
						; 8607	
						; 8608	;HERE IS LOOP TO EXTRACT DIGITS FROM FRACTION IN AC0,AC1
						; 8609	
						; 8610	BDD1:	BR_AR LONG,VMA_PC+1,		;START NEXT LOOP ITERATION
U 3502, 3016,4640,2067,0000,0240,5410,0170	; 8611			AR_SLEN+1,SKP CRY0	;ANY MORE DIGITS?
						; 8612	=0					;HERE TO RESUME AFTER INTERRUPT
						; 8613	BDDR4:	SLEN_AR,MQ_AR,SC_1,		;YES, SAVE LENGTH REMAINING
						; 8614			AR_BR LONG,		; AND GET FRACTION
U 3016, 2175,3202,2617,4402,1020,1032,0170	; 8615			SIGNS DISP,J/BDD2	;CHECK FOR 1ST DIGIT OF 10**21
						; 8616		AR_0S,ARX_0S,CLR FPD,		;NO, DONE.  CLEAR AC0 & AC1
U 3017, 3503,3441,2200,0000,0000,3614,0000	; 8617			VMA_VMA+1
U 3503, 1632,0001,0000,0000,0017,1010,0000	; 8618		AC0_AR,FETCH,J/STRAC1		;MOVE FETCH WHEN TIMING FIXED
						; 8619	=1101					;LOOK AT BR0 ONLY
U 2175, 2177,0603,7700,0302,0020,0027,0004	; 8620	BDD2:	AR_AR*1.25 LONG,SC_#,#/4	;NEXT DIGIT TO AR0-3
U 2177, 3020,3441,2400,0000,1000,7010,0000	; 8621		ARX_AR,AR_0S,SKP INTRPT		;READY TO SHIFT IN DIGIT
U 3020, 3022,4001,4000,0000,0000,0033,0000	; 8622	=0	AR_SHIFT,B DISP,J/BDD3		;STORE IT
U 3021, 3765,3242,2600,0000,0000,1610,0000	; 8623		AR_BR LONG,SR_0,J/B2DPF		;UPDATE REGS & QUIT
						; 8624	
						; 8625	;HERE TO STORE DIGIT IN AR FOR BDEC
						; 8626	=0
U 3022, 3504,0600,0007,0000,0332,0010,0165	; 8627	BDD3:	VMA_AR+E1,LOAD AR,J/BDD4	;TRANSLATE: GET TABLE ENTRY
U 3023, 2564,0600,2007,0000,0020,0010,0165	; 8628		AR_AR+E1,J/BDD7			;OFFSET AR AND STORE IT
						; 8629	
U 3504, 3026,6023,0000,0000,0021,5410,0040	; 8630	BDD4:	SKP MQ EQ -1,TIME/3T,ARX_0.M	;LAST DIGIT?
						; 8631	=0
U 3026, 3505,3240,0003,0000,0022,0010,0000	; 8632	BDD5:	AR_MEM,J/BDD6			;NO, STORE RH (POS DIGIT)
U 3027, 3026,3200,0205,0000,0020,0010,0000	; 8633		ARX_AC3,J/BDD5			;YES, LOOK AT M FLAG
U 3505, 2564,3243,0400,0000,3021,5310,0020	; 8634	BDD6:	SKP ARX2,ARX_AR SWAP,ARL_0.M
						; 8635	=100
U 2564, 1160,0001,0000,0000,0000,1650,0206	; 8636	BDD7:	SR_BDD,CALL,J/PUTDST
U 2565, 2564,4001,4000,0000,2001,0010,0020	; 8637		AR_ARX,ARL_0.M,J/BDD7		;M SET ON LAST DIGIT, USE LH
						; 8638	
						; 8639		AR_BR LONG,SR_BDT,		;GET FRACTION BACK
U 2566, 2215,3242,2600,0000,0020,1632,0010	; 8640			SIGNS DISP		;CHECK BR0 FOR INTEGER PART
						; 8641	=
U 2215, 2217,0603,5500,0000,0020,0027,0000	; 8642	=1101	AR_AR*10 LONG			;DISCARD PREVIOUS DIGIT
U 2217, 3502,0001,0000,7130,3000,0110,0037	; 8643		P_P AND #,#/37,J/BDD1		;CLEAR AR0, GO FOR NEXT
						; 8644	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE	

						; 8645	.TOC	"EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE"
						; 8646	
						; 8647	;SLEN = COMPLEMENT OF LENGTH
						; 8648	;MSK = MASK
						; 8649	;E1 = EFFECTIVE ADDRESS OF OPERATION WORD (SIGN EXTENDED IF OFFSET)
						; 8650	
						; 8651	;CALL WITH:	AR_SLEN+1,CALL,J/SRCMOD
						; 8652	;RETURNS:	1 LENGTH EXHAUSTED: FLAGS IN AR
						; 8653	;		2 (EDIT ONLY) NO SIGNIFICANCE: FLAGS IN FE
						; 8654	;		3 (EDIT ONLY) SIGNIFICANCE START: BYTE IN AR, FLAGS IN FE
						; 8655	;		4 NORMAL: BYTE IN AR
						; 8656	;		5 ABORT: OUT OF RANGE OR TRANSLATE FAILURE
						; 8657	;	BR, BRX, PRESERVED.
						; 8658	;	B=0 IF TRANSLATE, =1 IF OFFSET MODE, =2 IF EDIT, =4 IF CVTDBT
						; 8659	
						; 8660	;[266] Remove edit 244
						; 8661	;;[244]	THIS ADDRESS MUST REMAIN FOR THE PROBLEM OF THE
						; 8662	;;	S FIELD OF THE SOURCE POINTER BEING GREATER THAT 36.
						; 8663	;
						; 8664	;.IF/MODEL.B
						; 8665	;1200:					;[244]
						; 8666	;SRCMOD:	SLEN_AR,AR+ARX+MQ_0.M,CALL.M,	;PUT LENGTH AWAY, GET BYTE
						; 8667	;		SIGNS DISP,J/GSRC	;CHECK FOR LENGTH EXHAUSTION
						; 8668	;1201:	AR_SFLGS,SR_0,RETURN1		;LEN =0, DONE
						; 8669	;1202:	E1,TIME/2T,B DISP		;BYTE IN AR
						; 8670	;1206:	AR_AR*.5 LONG,E1,J/XLATE	;LOW BIT TO ARX0, BYTE/2 TO AR LOW
						; 8671	;1207:	AR_AR+E1,TIME/3T		;OFFSET, ADD OFFSET, TEST MASK
						; 8672	;	TEST AR.MSK,SKP CRY0,RETURN4	;RETURN 4 IF OK, 5 OUT OF RANGE
						; 8673	;.IFNOT/MODEL.B				;[244][266]
						; 8674	=000
						; 8675	SRCMOD:	SLEN_AR,AR+ARX+MQ_0.M,CALL.M,	;PUT LENGTH AWAY, GET BYTE
U 2600, 2313,0001,0007,0000,0021,1072,0170	; 8676			SIGNS DISP,J/GSRC	;CHECK FOR LENGTH EXHAUSTION
U 2601, 0001,3200,2000,0000,0020,1603,0000	; 8677		AR_SFLGS,SR_0,RETURN1		;LEN =0, DONE
U 2602, 2606,4001,0007,0000,0000,0033,0165	; 8678		E1,TIME/2T,B DISP		;BYTE IN AR
U 2606, 3507,0301,7707,0000,0020,0027,0165	; 8679	=110	AR_AR*.5 LONG,E1,J/XLATE	;LOW BIT TO ARX0, BYTE/2 TO AR LOW
U 2607, 3506,0600,2007,0000,0020,0010,0165	; 8680		AR_AR+E1,TIME/3T		;OFFSET, ADD OFFSET, TEST MASK
U 3506, 0004,3600,0007,4000,0040,5403,0167	; 8681		TEST AR.MSK,SKP CRY0,RETURN4	;RETURN 4 IF OK, 5 OUT OF RANGE
						; 8682	;.ENDIF/MODEL.B		;[244][266]
						; 8683	
						; 8684	;HERE ON TRANSLATE-MODE OPERATIONS, WITH THE BYTE/2 IN AR, AND
						; 8685	; THE LEAST SIGNIFICANT BIT OF THE BYTE IN ARX0.  PERFORM THE
						; 8686	; TABLE LOOKUP, AND OPERATE AS CONTROLLED BY THE HIGH THREE BITS
						; 8687	; OF THE TABLE ENTRY.
						; 8688	
U 3507, 3510,0600,0007,0000,0332,0010,0165	; 8689	XLATE:	VMA_AR+E1,LOAD AR		;GET FUNCTION FROM TABLE
						; 8690	
U 3510, 3030,3240,0003,0302,0022,4310,0022	; 8691	TRNAR:	AR_MEM,SKP ARX0,SC_#,#/18.	;WHICH HALF?
						; 8692	=0	ARX_AR,AR0-3 DISP,		;LH, MOVE TO ARX LEFT
U 3030, 2221,3240,2400,0000,1040,0007,0000	; 8693			AR_SFLGS,J/TRNFNC
						; 8694		ARX_AR SWAP,AR18-21 DISP,	;RH, MOVE THAT TO ARX LEFT
U 3031, 2221,3200,2400,0000,3040,0007,0000	; 8695			AR_SFLGS,J/TRNFNC
						; 8696	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE	

						; 8697	;HERE ON TRANSLATE OPERATION TO PERFORM FUNCTIONS REQUIRED BY
						; 8698	; THE 3 HIGH ORDER BITS OF THE TRANSLATE FUNCTION HALFWORD.
						; 8699	; WE HAVE DISPATCHED ON THOSE THREE BITS, WITH THE FUNCTION
						; 8700	; HALFWORD IN LH(ARX), AND THE FLAGS FROM AC0 IN AR.
						; 8701	
						; 8702	=0001
						; 8703	TRNFNC:	SFLGS_AR,FE_P,AR_SHIFT,		;SAVE FLAGS, GET FCN IN AR RIGHT
U 2221, 2253,0001,4000,0101,0020,1032,0000	; 8704			SIGNS DISP,J/TRNRET	;WAS S FLAG ALREADY SET?
U 2223, 0005,0001,0000,7131,0000,1003,0003	; 8705	TRNABT:	SFLGS_AR,FE_P AND #,#/3,RETURN5	;ABORT
U 2225, 2221,4001,0000,7130,3000,0110,0067	; 8706		P_P AND #,#/67,J/TRNFNC		;CLEAR M FLAG
U 2227, 2221,0001,0000,6130,3000,0110,0010	; 8707		P_P OR #,#/10,J/TRNFNC		;SET M FLAG
U 2231, 2221,0001,0000,6130,3000,0110,0020	; 8708	TRNSIG:	P_P OR #,#/20,J/TRNFNC		;SET N FLAG
U 2233, 2223,4001,0000,6130,3000,0110,0020	; 8709		P_P OR #,#/20,J/TRNABT		;SET N AND ABORT
U 2235, 2231,0001,0000,7130,3000,0110,0067	; 8710		P_P AND #,#/67,J/TRNSIG		;CLEAR M, THEN SET N
U 2237, 2221,4001,0000,6130,3000,0110,0030	; 8711		P_P OR #,#/30,J/TRNFNC		;SET N AND M
						; 8712	
						; 8713	=1011
						; 8714	TRNRET:	ARX_AR*MSK,AD/AND,		;S FLAG IS 0, GET BYTE IN AR
U 2253, 2624,3600,0207,4000,0020,4433,0167	; 8715			SKP AR18,B DISP,J/TRNSS	;IS THIS EDIT?
U 2257, 0004,3600,2007,4000,0020,0003,0167	; 8716		AR_AR*MSK,AD/AND,RETURN4	;RETURN NORMAL SINCE S FLAG SET
						; 8717	
						; 8718	=100
U 2624, 0150,3240,2005,0000,0020,0033,0000	; 8719	TRNSS:	AR_DLEN,B DISP,J/TRNNS1		;NO SIG ON MOVE OR D2B
U 2625, 3512,3240,2000,0302,0020,0010,0040	; 8720		AR_SFLGS,SC_#,#/40,J/TRNSS1	;SIG START, SET FLAG
U 2626, 0002,4640,0007,0000,0332,0003,0176	; 8721		VMA_E0+1,LOAD AR,RETURN2	;EDIT NO SIG.  GET FILL
U 2627, 0003,3200,2006,0301,0020,0003,0144	; 8722		AR_DSTP,FE_#,#/144,RETURN3	;EDIT SIG START
						; 8723	
						; 8724	=0**
U 0150, 3511,1703,2000,0000,0020,0010,0000	; 8725	TRNNS1:	AR_AR-1,J/TRNNS2		;COMPENSATE FOR IGNORING SRC
U 0154, 2600,4640,2007,0000,0020,0010,0170	; 8726		AR_SLEN+1,J/SRCMOD		;D2B HAS NO DEST LENGTH
U 3511, 2273,0001,0005,0000,0020,1032,0000	; 8727	TRNNS2:	DLEN_AR,SIGNS DISP
U 2273, 2600,3240,2007,0000,0020,0010,0170	; 8728	=1011	AR_SLEN,J/SRCMOD		;SLEN = DST LEN, DON'T CHANGE IT
U 2277, 2600,4640,2007,0000,0020,0010,0170	; 8729		AR_SLEN+1,J/SRCMOD		;SLEN REFLECTS SRC LENGTH
						; 8730						; COUNT DOWN FOR BYTE SKIPPED
U 3512, 3513,0001,0000,6100,3000,0110,0000	; 8731	TRNSS1:	P_P OR SC
U 3513, 0004,0001,4000,0000,2000,1003,0000	; 8732		SFLGS_AR,AR_ARX,RETURN4		;RETURN WITH SIG SET
						; 8733	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE	

						; 8734	;SUBROUTINE TO GET BYTE FROM SOURCE STRING
						; 8735	; CALL GSRC WITH SIGNS DISP TO CHECK FOR LENGTH EXHAUSTION
						; 8736	; [TIME = 17 + 3(BP OVERFLOW)]
						; 8737	
						; 8738	=1011
U 2313, 0001,3240,2005,0000,0020,0003,0000	; 8739	GSRC:	AR_DLEN,RETURN1			;LEN RAN OUT
U 2317, 3036,3200,2201,0301,0040,0010,0044	; 8740	GETSRC:	AR_SRCP,ARX_SRCP,FE_#,#/36.
						; 8741	;[266] Remove edit 244
						; 8742	;.IF/MODEL.B
						; 8743	;	GEN FE-S,SKP SCAD0		;[244] IS S > 36 ?
						; 8744	;=0	J/GSRC1				;[244] NO, GO BELOW
						; 8745	;	DISP/RETURN,J/501		;[244] YES, TRICKY WAY TO
						; 8746	;					;[244] GET OUT
						; 8747	;;[244]	THIS IS DONE THIS WAY SO THAT WE CAN TAKE THE ERROR
						; 8748	;;	RETURN OF THE EXTEND INSTRUCTION. THE TWO PLACES THAT
						; 8749	;;	CALL GSRC ARE SET SO THAT A RETURN WITH J FIELD OF 500
						; 8750	;;	WILL GO TO HERE.
						; 8751	;1701:	RETURN5				;[244] ERROR RETURN
						; 8752	;.ENDIF/MODEL.B		;[244][266]
						; 8753	=0
						; 8754	GSRC1:	P_P-S,SC/SCAD,VMA_PC,CALL.M,	;[352] Increment pointer, init VMA
U 3036, 2644,0001,0000,5112,3121,0074,0200	; 8755			BYTE DISP,J/GSRC2	; section, test word overflow
U 3037, 2570,0001,0000,2002,0000,7010,0000	; 8756		SC_FE+SC,SKP INTRPT,J/LDB1	;GET BYTE & RETURN TO CALLER
						; 8757	=100
						; 8758	GSRC2:	SRCP_AR,ARX_AR,FE_S,		;[352] STORE POINTER,
U 2644, 1674,2301,0401,2411,1000,1036,0000	; 8759			EA MOD DISP,J/BFETCH	; GO EVALUATE THE ADDRESS
						; 8760	GSRC3:	ARR_AR+1,ARX/AD,INH CRY18,	;[352] Update address for ARX (used
U 2645, 2644,4001,2200,5012,3020,0611,0200	; 8761			P_FE-S,SC/SCAD,J/GSRC2	; in EA MOD DISP) and set P
U 2646, 1152,3240,0204,0000,0020,5010,0000	; 8762		ARX_SRCP2,SKP PC SEC0,J/GSRC4	;GET ADDR PART OF POINTER
U 2647, 3052,3240,2404,0000,1020,5010,0000	; 8763		ARX_AR,AR_SRCP2,SKP PC SEC0
U 3052, 3060,4003,2000,0101,0020,4516,0000	; 8764	=0	FE_P,AR_AR+1-AR0,SKP AR0,J/GSRC5
U 3053, 2645,4001,4000,0000,2000,0010,0000	; 8765		AR_ARX,J/GSRC3			;OOPS, SEC 0 IS COMPATABLE
						; 8766	=0
U 3060, 1150,4001,0000,0000,3001,0010,0200	; 8767	GSRC5:	P_FE,J/GSRC6			;EFIW, INCR ALL BUT 0-5
U 3061, 1150,4003,2000,0000,0020,0011,0000	; 8768		AR_AR+1,INH CRY18		;IFIW, INCR RIGHT HALF ONLY
						; 8769	=00
						; 8770	GSRC6:	SRCP2_AR,AR_ARX,ARX_AR (AD),	;SAVE ADDR PART
U 1150, 3514,3701,4204,0000,2000,1050,0000	; 8771			CALL,J/RESETP		;GO SET P TO 36-S
						; 8772	=10
U 1152, 1040,4001,0001,2411,0000,1010,0000	; 8773	GSRC4:	SRCP_AR,FE_S,J/BYTEI		;[352] GO EVALUATE LONG POINTER
U 1153, 1674,2301,0401,2411,1000,1036,0000	; 8774		SRCP_AR,ARX_AR,FE_S,EA MOD DISP,J/BFETCH
						; 8775	
						; 8776	;SUBROUTINE TO LOAD P FROM 36-S
						; 8777	
U 3514, 0002,0001,0000,5312,3000,0103,0044	; 8778	RESETP:	P_#-S,#/36.,SC/SCAD,RETURN2	;START P BACK AT LEFT EDGE
						; 8779	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE	

						; 8780	;SUBR TO STORE AR IN DEST STRING
						; 8781	; [TIME = 24 + 3(BP OVERFLOW)]
						; 8782	
						; 8783	=00
U 1160, 3515,3240,2216,0000,1040,0050,0144	; 8784	PUTDST:	MQ_AR,AR_DSTP,ARX_DSTP,CALL,J/IDST
						; 8785		AR_MQ,SC_#-SC,#/36.,SKP SCAD0,
U 1161, 2610,3721,2000,5302,0020,5150,0044	; 8786			CALL,J/DPB1
U 1163, 0006,4001,0003,0000,0002,0003,0000	; 8787	=11	MEM_AR,RETURN6
						; 8788	
						; 8789	;SUBROUTINES TO UPDATE STRING POINTERS
						; 8790	
						; 8791	IDST:	VMA_PC,P_P-S,SC/SCAD,BYTE DISP,	;[352] Init VMA section and
U 3515, 2664,0001,0000,5112,3121,0034,0200	; 8792			J/IDST2			; TEST FOR WORD OVERFLOW
						; 8793	=100
U 2664, 3521,0001,0406,0000,1000,0010,0144	; 8794	IDST2:	DSTP,ARX_AR,J/IDST2B		;[352] PRESEL #, fix ARX address
U 2665, 3516,4001,2000,0000,0020,0011,0000	; 8795		AR_AR+1,INH CRY18,J/IDST3
U 2666, 1172,3240,0206,0000,0020,5010,0145	; 8796		ARX_DSTP2,SKP PC SEC0,J/IDST4	;GET ADDR PART OF POINTER
U 2667, 3062,3200,2406,0000,1020,5010,0145	; 8797		ARX_AR,AR_DSTP2,SKP PC SEC0
U 3062, 3064,4001,2000,0101,0020,4516,0000	; 8798	=0	FE_P,AR_AR+1-AR0,SKP AR0,J/IDST5
U 3063, 3516,4013,2000,0000,0020,0011,0000	; 8799		AR_ARX+1 (AD),INH CRY18
U 3516, 2664,0001,0000,5312,3000,0110,0044	; 8800	IDST3:	P_#-S,#/36.,SC/SCAD,J/IDST2	;GO STORE SHORT POINTER AWAY
						; 8801	=0
U 3064, 1170,4001,0000,0000,3000,0110,0145	; 8802	IDST5:	P_FE.C,SEL DSTP2,J/IDST6	;PRESEL # TO FIX HARDW GLITCH
U 3065, 1170,4001,2000,0000,0020,0011,0145	; 8803		AR_AR+1,INH CRY18,SEL DSTP2
						; 8804	=00
						; 8805	IDST6:	DSTP2_AR,AR_ARX,ARX_AR (AD),	;INCR ADDR PART
U 1170, 3514,3703,4206,0000,2000,1050,0145	; 8806			CALL,J/RESETP		;GET P BACK TO 36-S
						; 8807	=10
U 1172, 3517,0001,0000,0000,0000,0010,0144	; 8808	IDST4:	SEL DSTP,J/IDST7		;PRESEL # TO PREVENT HARDW GLITCH
U 1173, 3520,4001,0000,0000,0000,0010,0144	; 8809		SEL DSTP,J/IDST8		;PRESEL # TO PREVENT HARDW GLITCH
						; 8810	
U 3517, 1040,4001,0006,2411,0000,1010,0144	; 8811	IDST7:	DSTP_AR,FE_S,J/BYTEI
						; 8812	IDST8:	DSTP_AR,ARX_AR,FE_S,		;[352][300]
U 3520, 1674,2301,0406,2411,1000,1036,0144	; 8813			EA MOD DISP,J/BFETCH
						; 8814	
						; 8815	IDST2B:	DSTP_AR,ARX_AR,FE_S,		;[352][300]STORE POINTER,
U 3521, 1674,2301,0406,2411,1000,1036,0144	; 8816			EA MOD DISP,J/BFETCH	; GO GET THE WORD ADDRESSED
						; 8817	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 16
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- EDIT FUNCTION					

						; 8818	.TOC	"EIS -- EDIT FUNCTION"
						; 8819	.IF/EDIT
						; 8820	;	HERE WITH E0, E1 SETUP, 0 IN AR, -1 IN ARX, AND 15 IN SC
						; 8821	
						; 8822	.IF/OWGBP
						; 8823	=0***					;[347]
U 0626, 2120,4001,0000,0000,0000,0050,0000	; 8824	EDIT:	CALL [EXT2WD]			;CHECK FOR OWGBP
U 0636, 3522,2341,0200,0302,0000,0410,0017	; 8825		CLR AR,ARX_1S,SC_#,#/15.	;SETUP FOR SHIFT
U 3522, 3523,3200,4200,0000,0020,1610,0000	; 8826		AR_SHIFT,ARX_AC0,SR_ED(PAT)	;MASK TO AR, FLAGS ETC TO ARX
						;;8827	.IFNOT/OWGBP
						;;8828	EDIT:	AR_SHIFT,ARX_AC0,SR_ED(PAT)	;MASK TO AR, FLAGS ETC TO ARX
						; 8829	.ENDIF/OWGBP
U 3523, 3524,4001,0007,0000,0000,1010,0167	; 8830		MSK_AR				;SAVE MASK FOR TRAN FUNC
U 3524, 3066,4001,4000,0000,2001,5010,0020	; 8831		AR_ARX,ARL_0.M,SKP PC SEC0	;DO WE ALLOW SECTION #?
U 3066, 3525,3711,4000,0000,2312,0010,0000	; 8832	=0	VMA_ARX,LOAD AR,AR_ARX,J/EDIT1	;YES.  PROVIDE IT
U 3067, 3525,3703,4000,0000,2312,0010,0000	; 8833		VMA_AR,LOAD AR,AR_ARX		;NO, GIVE 0
U 3525, 3526,4001,0000,7131,0000,0010,0003	; 8834	EDIT1:	FE_P AND #,#/3			;GET PBN IN FE
						; 8835	EDITLP:	SC_# AND AR0-8,#/30,		;PBN*8 IN SC
U 3526, 3527,4001,0400,7322,1000,1010,0030	; 8836			SFLGS_AR,ARX_AR		;UPDATED AC NOW IN AC AND ARX
U 3527, 3530,3240,0003,2002,0022,0010,0000	; 8837		AR_MEM,SC_FE+SC			;PATTERN IN AR, PBN*9 IN SC
U 3530, 2321,4001,4000,0302,0020,0007,0005	; 8838		AR_SHIFT,SH DISP,SC_#,#/5	;PATTERN BYTE TO AR0-8,
						; 8839	=0001					; DISP ON HIGH 3 BITS
						; 8840	EDDISP:	GEN #+AR0-8,#/-5,
U 2321, 3070,0001,0000,2320,0020,5110,0773	; 8841			SKP SCAD0,J/EDOPR	;(0XX) OPERATE GROUP
U 2323, 1230,3203,5000,0000,0000,4310,0000	; 8842		AR_AR*8,SKP ARX0,J/EDMSG	;(1XX) MESSAGE
U 2325, 3074,4001,0000,0000,0000,0010,0000	; 8843		J/EDNOP				;(2XX) UNDEFINED
U 2327, 3074,4001,0000,0000,0000,0010,0000	; 8844		J/EDNOP				;(3XX) UNDEFINED
U 2331, 3074,4001,0000,0000,0000,0010,0000	; 8845		J/EDNOP				;(4XX) UNDEFINED
						; 8846		MQ_ARX,ARX_ARX*4,
U 2333, 3533,3243,0610,4002,2000,0010,0000	; 8847			SC_FE+1,J/EDSKPT	;(5XX) SKIP IF MINUS
						; 8848		MQ_ARX,ARX_ARX*2,
U 2335, 3533,3701,0510,4002,2000,0010,0000	; 8849			SC_FE+1,J/EDSKPT	;(6XX) SKIP IF NON-ZERO
U 2337, 3075,3203,5000,4002,0000,0010,0000	; 8850		AR_AR*8,SC_FE+1,J/EDSKP		;(7XX) SKIP ALWAYS
						; 8851	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- EDIT FUNCTION					

						; 8852	;HERE TO DECODE OPERATE GROUP
						; 8853	=0
U 3070, 3074,4001,0000,0000,0000,0010,0000	; 8854	EDOPR:	J/EDNOP				;OPR .GE. 005 UNDEFINED
U 3071, 2670,0001,0000,0000,0020,0007,0000	; 8855		SH DISP,J/OPDISP		;(00X), DISP ON LOW 3 BITS
						; 8856	=000
						; 8857	OPDISP:	AR_ARX,SC_#,#/-4,		;(000) STOP
U 2670, 3531,4001,4000,0302,2200,0010,0774	; 8858			VMA_PC+1,J/EDSTOP
U 2671, 2174,0001,0000,0000,0000,1610,0101	; 8859		SR_ED(S),J/EDSEL		;(001) SELECT
U 2672, 1220,3200,2006,0000,0020,4310,0144	; 8860		AR_DSTP,SKP ARX0,J/EDSSIG	;(002) START SIGNIFICANCE
U 2673, 3537,0001,4000,0000,2000,0010,0000	; 8861		AR_ARX,J/EDFLDS			;(003) FIELD SEPARATOR
						; 8862		AR_DSTP,ARX/AD,MQ_ARX,		;(004) EXMD
U 2674, 3106,3200,2216,0000,2020,5010,0144	; 8863			SKP PC SEC0,J/EDEX0
						; 8864	=
						; 8865	;HERE TO TERMINATE EDIT INSTRUCTION
						; 8866	; SC HAS -4, FE HAS CURRENT PBN, VMA HAS PC IF ABORT, PC+1 IF DONE
						; 8867	
U 3531, 3072,0001,0000,5031,0020,5110,0003	; 8868	EDSTOP:	FE_FE-#,#/3,SKP SCAD0
						; 8869	=0	AR_AR+1,INH CRY18,
U 3072, 3532,4003,2000,7100,3020,0611,0200	; 8870			P_P AND SC,J/SFET1
U 3073, 3532,4001,0000,4100,3000,0110,0000	; 8871		P_P+1
						; 8872	.ENDIF/EDIT				;Other things need this
U 3532, 0216,4001,0000,0000,0017,3610,0000	; 8873	SFET1:	FETCH+1,J/STORAC
						; 8874	.IF/EDIT
						; 8875	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 18
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- EDIT FUNCTION					

						; 8876	;HERE FOR SKPM & SKPN, WITH APPROPRIATE BIT IN ARX0
						; 8877	
U 3533, 3074,3203,5300,0000,0000,4310,0000	; 8878	EDSKPT:	AR_AR*8,SKP ARX0,ARX/MQ		;SKIP DISTANCE TO AR0-5
						; 8879	
						; 8880	;HERE AT END OF OPERATION TO UPDATE PBN
						; 8881	=0
						; 8882	EDNOP:	FE_FE-#,#/3,SKP SCAD0,		;END OF PATTERN WORD?
U 3074, 3076,4001,4000,5031,2020,5110,0003	; 8883			AR_ARX,J/EDNXT1
U 3075, 3074,0001,0000,2101,0000,0010,0000	; 8884	EDSKP:	FE_P+SC,J/EDNOP			;ADD SKIP DISTANCE
						; 8885	=0
U 3076, 3102,4001,0000,0000,0000,5010,0000	; 8886	EDNXT1:	SKP PC SEC0,J/EDNXT2
U 3077, 3534,4001,0000,0000,0000,1610,0000	; 8887		SR_ED(PAT)
U 3534, 3100,4001,0000,2031,0000,5010,0004	; 8888		FE_FE+#,#/4,SKP PC SEC0		;RESTORE PBN POS, INCR IT
						; 8889	=0	SC_P AND #,#/74,VMA_AR,LOAD AR,	;FLAGS & EDIT BIT TO SC,
U 3100, 3104,3701,0000,7132,0312,7010,0074	; 8890			SKP INTRPT,J/EDNXT3	; GET PATTERN
U 3101, 3535,0001,0000,7132,0000,0010,0074	; 8891		SC_P AND #,#/74			;IN SEC0, MUST NOT LOAD FULL SEC
U 3535, 3536,4001,0400,0000,1001,0010,0020	; 8892		ARX_AR,ARL_0.M			;CLEAR SEC #
						; 8893		VMA_AR,LOAD AR,AR_ARX,		;GET PATTERN
U 3536, 3104,3703,4000,0000,2312,7010,0000	; 8894			SKP INTRPT,J/EDNXT3
						; 8895	
						; 8896	=0
						; 8897	EDNXT2:	AR_AR+1,FE_FE-#,#/4,		;REDUCE PBN
U 3102, 3076,4001,2000,5031,0020,5110,0004	; 8898			SKP SCAD0,J/EDNXT1
						; 8899		AR_AR+1,INH CRY18,		;BUMP TO NEXT WORD
						; 8900			FE_FE-#,#/4,		;REDUCE PBN
U 3103, 3076,4003,2000,5031,0020,5111,0004	; 8901			SKP SCAD0,J/EDNXT1
						; 8902	=0
U 3104, 3526,0001,0000,6000,3001,0010,0200	; 8903	EDNXT3:	P_FE OR SC,J/EDITLP		;SET NEW PBN, GO DO NEXT PATTERN
U 3105, 3775,4001,0000,6000,3001,0010,0200	; 8904		P_FE OR SC,J/PGFAC0		;GO RESTORE THINGS AND TAKE
						; 8905						; THE INTERUPT
						; 8906	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 19
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- EDIT FUNCTION					

						; 8907	;HERE FOR FIELD SEPARATOR (CLEAR FLAGS IN AC 0-2)
						; 8908	
U 3537, 1223,0001,0000,7130,3000,0110,0007	; 8909	EDFLDS:	P_P AND #,#/7,J/EDSEND		;EASY ENOUGH
						; 8910	
						; 8911	;HERE FOR SIG START
						; 8912	
						; 8913	=00
						; 8914	EDSSIG:	ARX_AR,VMA_AC3,AR/AD,ARL_0.M,
						; 8915			BYTE DISP,SCADA EN/0S,SCAD/A,
U 1220, 2734,3200,2405,0400,1341,5074,0020	; 8916			CALL,SKP PC SEC0,J/EDFLT
						; 8917		FE_FE-#,#/3,SKP SCAD0,		;S FLAG ALREADY SET, NOP
U 1221, 3076,4001,4000,5031,2020,5110,0003	; 8918			AR_ARX,J/EDNXT1
						; 8919	=11
U 1223, 3074,0001,0400,7131,1000,0010,0003	; 8920	EDSEND:	FE_P AND #,#/3,ARX_AR,J/EDNOP	;READY TO DO NEXT OP
						; 8921	
						; 8922	;HERE FOR MESSAGE CHAR
						; 8923	
						; 8924	=00
U 1230, 2722,4640,0007,0000,0332,0010,0176	; 8925	EDMSG:	VMA_E0+1,LOAD AR,J/EDSFIL	;NO SIG, PUT FILLER
U 1231, 3374,3401,2000,0102,0000,0050,0000	; 8926		SC_P,AR_0S,CALL,J/GETSC		;GET MESSAGE SELECT IN AR
U 1233, 2727,4600,0007,4000,0332,0010,0176	; 8927	=11	VMA_AR+E0+1,LOAD AR,J/EDMPUT	;STORE MESSAGE
						; 8928	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 20
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- EDIT FUNCTION					

						; 8929	;HERE TO EXCHANGE MARK AND DESTINATION POINTERS
						; 8930	
						; 8931	=0
						; 8932	EDEX0:	VMA_AC3,LOAD AR (WR TST),	;GET MARK POINTER
U 3106, 3542,3200,0065,0000,0331,0010,0440	; 8933			BR/AR,BRX/ARX,J/EDEX2	;DSTP IN BR & BRX,
U 3107, 3540,3240,2045,0000,0021,0010,0020	; 8934	EDEXMD:	BR/AR,AR_AC3,ARL_0.M
U 3540, 3541,3701,0000,0000,0311,0010,0440	; 8935		VMA_AR,LOAD AR (WR TST)		;GET MARK FROM SECT 0
U 3541, 2245,3200,0003,0000,0022,0010,0000	; 8936		AR_MEM
						; 8937	=101
U 2245, 3545,3202,2040,0000,0016,0010,0000	; 8938	EDDSNG:	BR/AR,AR_BR,STORE,J/EDEXX	;NEITHER POINTER IS DOUBLE
U 2247, 1002,4001,0000,0000,0000,0010,0000	; 8939		J/UUO				;SHORT DSTP, LONG MARK ILLEGAL
						; 8940	;;;FLUSH WHEN SURE THIS IS RIGHT
						; 8941	;	BR/AR,AR_BR,			;DSTP TO AR, MARK TO BR
						; 8942	;		VMA_VMA+1,LOAD ARX	;GET MARK2
						; 8943	;	FIN XFER,VMA_VMA-1,STORE,J/EDEXX;NOW STORE DSTP AS NEW MARK
						; 8944	
U 3542, 2675,3240,0003,0000,0042,0034,0000	; 8945	EDEX2:	AR_MEM,BYTE DISP		;WAIT FOR MARK, TEST DESTP
U 2675, 2245,4001,0000,0000,0020,0034,0000	; 8946	=101	BYTE DISP,J/EDDSNG		;NO, CHECK MARK
U 2677, 2705,3240,0206,0000,0040,0034,0145	; 8947		ARX_DSTP2,BYTE DISP		;YES, CHECK MARK
U 2705, 1002,4001,0000,0000,0000,0010,0000	; 8948	=101	J/UUO				;LONG DSTP SHORT MARK ABORT
						; 8949	;;;FLUSH WHEN SURE THE UUO IS RIGHT
						; 8950	;	BR/AR,AR_ARX,			;MARK TO BR, DSTP2 TO AR
						; 8951	;		VMA_VMA+1,STORE,J/EDEXM4 ; STORE DSTP2
						; 8952		BR/AR,AR_ARX,
U 2707, 3543,4001,4040,0000,2011,3610,0240	; 8953			VMA_VMA+1,LOAD ARX (WR TST)	;GET MARK2
U 3543, 3544,3240,0003,0000,0036,0010,0000	; 8954		FIN XFER,STORE			;PUT BACK DSTP2
						; 8955	;EDEXM4:
						; 8956		FIN STORE,AR_BRX,		;GET DSTP FROM BRX
U 3544, 3545,3202,6003,0000,0016,3510,0000	; 8957			VMA_VMA-1,STORE		;PUT THAT DOWN
						; 8958	EDEXX:	MEM_AR,AR_BR,SEL DSTP,		;PRESELECT # TO FIX HARDWARE GLITCH
U 3545, 3120,3242,2003,0000,0002,5010,0144	; 8959			SKP PC SEC0		;GET MARK FOR NEW DSTP
U 3120, 2715,4001,4006,0000,2020,1034,0144	; 8960	=0	DSTP_AR,AR_ARX,BYTE DISP,J/EDEX1
U 3121, 2715,0001,0006,0000,0000,1010,0144	; 8961		DSTP_AR
						; 8962	=101
						; 8963	EDEX1:	FE_FE-#,#/3,SKP SCAD0,
U 2715, 3076,3721,2000,5031,0020,5110,0003	; 8964			AR_MQ,J/EDNXT1
U 2717, 3546,0001,0000,0000,0000,0010,0145	; 8965		SEL DSTP2			;PRESELECT # TO FIX HARDWARE GLITCH
U 3546, 2715,4001,0006,0000,0000,1010,0145	; 8966		DSTP2_AR,J/EDEX1		;PUT OLD MARK2 AS DSTP2
						; 8967	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 21
; EIS.MIC[4,24]	10:37 27-May-86				EIS -- EDIT FUNCTION					

						; 8968	;HERE FOR SELECT
						; 8969	
						; 8970	=0*
						; 8971	EDSEL:	AR_SRCP,ARX_SRCP,FE_#,#/36.,
U 2174, 3036,3240,2201,0301,0040,0050,0044	; 8972			CALL,J/GSRC1		;GO GET SRC BYTE
U 2176, 2720,0301,7707,0000,0020,0027,0165	; 8973		AR_AR*.5 LONG,E1		;GOT IT, DIVIDE BY 2
U 2720, 3510,0600,0007,4000,0332,0050,0165	; 8974	=000	VMA_AR+E1,LOAD AR,CALL,J/TRNAR	;GO TRANSLATE BY HALFWORDS
						; 8975	=010
U 2722, 3547,3240,0003,0000,0022,0010,0000	; 8976	EDSFIL:	AR_MEM,J/EDSF1			;(2) NO SIGNIFICANCE, STORE FILL
U 2723, 1320,0001,0020,5110,0020,5110,0000	; 8977		GEN P-S,SKP SCAD0,BRX/ARX,J/EDSFLT ;(3) SIG START, DO FLOAT CHAR
U 2724, 1160,0001,0000,0000,0000,1650,0224	; 8978	EDSPUT:	SR_ED(+D),CALL,J/PUTDST		;(4) NORMAL, STORE AT DST
U 2725, 3531,4001,0000,0302,0100,0010,0774	; 8979		VMA/PC,SC_#,#/-4,J/EDSTOP	;(5) ABORT
U 2726, 1223,3200,2000,0000,0020,0010,0000	; 8980	EDFPUT:	AR_SFLGS,J/EDSEND		;(6) BUMP PBN AND GO TO NEXT
U 2727, 2724,3240,0003,0000,0022,0010,0000	; 8981	EDMPUT:	AR_MEM,J/EDSPUT			;FILL OR MSG IN AR, STORE IT
						; 8982	
						; 8983	
						; 8984	;HERE WHEN TIME TO STORE FILL CHAR
						; 8985	
U 3547, 2726,3703,0000,0000,0040,5410,0000	; 8986	EDSF1:	SKP AR NE,J/EDFPUT		;IS THERE ONE?
						; 8987	
						; 8988	;HERE WHEN SELECT STARTS SIGNIFICANCE
						; 8989	
						; 8990	=00
						; 8991	EDSFLT:	ARX_AR,VMA_AC3,AR/AD,ARL_0.M,
						; 8992			BYTE DISP,SCADA EN/0S,SCAD/A,
U 1320, 2734,3200,2405,0400,1341,5074,0020	; 8993			CALL,SKP PC SEC0,J/EDFLT
U 1321, 1320,4001,2000,0000,3021,0010,0200	; 8994		P_FE,AR_AR+1,J/EDSFLT		;FORCE STANDARD POINTER FORM
U 1323, 2724,3242,6000,0000,0000,1010,0000	; 8995	=11	SFLGS_AR,AR_BRX,J/EDSPUT	;SET S FLAG, GET BYTE, STORE IT
						; 8996	
						; 8997	;HERE IS SUBROUTINE TO STORE FLOAT CHAR
						; 8998	
						; 8999	=100
U 2734, 3552,4001,4000,0000,2016,0010,0000	; 9000	EDFLT:	AR_ARX,STORE,J/EDFLT1		;SHORT POINTER.  STORE IT
U 2735, 3552,3701,4000,0000,2316,0010,0000	; 9001		VMA_AR,AR_ARX,STORE,J/EDFLT1	; LIKEWISE.  FORCE SECTION 0
U 2736, 3550,0001,4000,0000,2016,0010,0000	; 9002		AR_ARX,STORE,J/EDFLTX		;LONG POINTER, DO MORE
U 2737, 3552,3701,4000,0000,2316,0010,0000	; 9003		VMA_AR,AR_ARX,STORE,J/EDFLT1	; IN SECTION 0, KEEP THERE
						; 9004	
U 3550, 3551,4001,0003,0000,0002,0010,0000	; 9005	EDFLTX:	MEM_AR				;FINISH STORE OF 1ST PART
U 3551, 3552,3240,2006,0000,0036,3610,0145	; 9006		AR_DSTP2,VMA_VMA+1,STORE	;NOW DO SECOND PART
						; 9007	
U 3552, 2214,4061,5003,0000,0022,0010,0000	; 9008	EDFLT1:	MEM_AR,AR_2			;MARK STORED, READY FOR FLOAT
U 2214, 1045,0600,0007,4000,0332,0050,0176	; 9009	=0*	VMA_AR+E0,LOAD AR,CALL,J/XFERW
U 2216, 2744,3701,0000,0000,0040,5410,0000	; 9010		SKP AR NE
U 2744, 3553,3240,2000,0302,0020,0010,0040	; 9011	=100	AR_SFLGS,SC_#,#/40,J/SETFLG	;NO FLOAT CHR, SET S FLAG
U 2745, 1160,0001,0000,0000,0000,1650,0224	; 9012		SR_ED(+D),CALL,J/PUTDST		;STORE FLOAT CHR IN DST
U 2747, 3553,3240,2000,0302,0020,0010,0040	; 9013	=111	AR_SFLGS,SC_#,#/40		;SET S FLAG AND RETURN
						; 9014	.ENDIF/EDIT				;Other stuff needs this
U 3553, 0003,0001,0000,6100,3000,0103,0000	; 9015	SETFLG:	P_P OR SC,RETURN3		;NO FLOAT CHR, SET S FLAG
						; 9016	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 1
; IO.MIC[4,24]	15:27 17-Mar-86				I/O INSTRUCTIONS					

						; 9017	.TOC	"I/O INSTRUCTIONS"
						; 9018	
						; 9019	; BITS 10-12 OF INSTRUCTION GET MAPPED TO IR 7-9 FOR I/O INSTRUCTIONS
						; 9020	; THE DEVICE ADDRESS IS BROKEN DOWN AS ONE OF THE FIRST 7, OR ALL OTHERS
						; 9021		.DCODE
						; 9022	
						; 9023	;DEVICE 000 (APR)
						; 9024	
D 0700, 3001,1212				; 9025	700:	W,		J/APRBI	;APRID (BLKI APR,)	OPTIONS, SERIAL #
D 0701, 3600,1210				; 9026		W,	DATAI,	J/APRDI	;DATAI APR,		ADDRESS COMPARE
D 0702, 0001,1316				; 9027		I,		J/APRBO	;WRFIL (BLKO APR,)	REFILL RAM
D 0703, 4400,1314				; 9028		R,	DATAO,	J/APRDO	;DATAO APR,		ADDRESS COMPARE
D 0704, 0400,1504				; 9029		I,	CONO,	J/APRCO	;CONO APR,		APR FLAGS
D 0705, 0600,1506				; 9030		I,	CONI,	J/APRCI	;CONI APR,
D 0706, 0101,1506				; 9031		I,	CONSZ,	J/APRCI	;CONSZ APR,
D 0707, 0500,1506				; 9032		I,	CONSO,	J/APRCI	;CONSO APR,
						; 9033	
						; 9034	;DEVICE 004 (PI)
						; 9035	
D 0710, 3601,1301				; 9036	710:	W,	M,	J/PIBI	;RDERA (BLKI PI,)	READ ERA
D 0711, 3300,1300				; 9037		W,	B/3,	J/PIDI	;DATAI PI,		Stats, or not used
D 0712, 4000,1302				; 9038		R,		J/PIBO	;SBDIAG (BLKO PI,)	SBUS DIAGNOSTIC
D 0713, 4001,1300				; 9039		R,	B/0,	J/PIDO	;DATAO PI,		More statistics
D 0714, 0400,1400				; 9040		I,	CONO,	J/PICO	;CONO PI,		PI SYSTEM CONTROL
D 0715, 0600,1404				; 9041		I,	CONI,	J/PICI	;CONI PI,		IN PROGRESS, ENABLE
D 0716, 0101,1404				; 9042		I,	CONSZ,	J/PICI
D 0717, 0500,1404				; 9043		I,	CONSO,	J/PICI
						; 9044	
						; 9045	;DEVICE 010 (PAG)
						; 9046	
D 0720, 6200,1516				; 9047	720:	RW,	BLKI,	J/PAGBI	;BLKI PAG,		UNASSIGNED
D 0721, 3600,1514				; 9048		W,	DATAI,	J/PAGDI	;DATAI PAG,		USER CONTEXT
D 0722, 0001,1510				; 9049		I,		J/PAGBO	;CLRPT (BLKO PAG,)	INVAL PAGE TABLE
D 0723, 4400,1512				; 9050		R,	DATAO,	J/PAGDO	;DATAO PAG,		USER CONTEXT
D 0724, 0401,1600				; 9051		I,	CONO,	J/PAGCO	;CONO PAG,		EXEC CONTEXT
D 0725, 0601,1602				; 9052		I,	CONI,	J/PAGCI	;CONI PAG,
D 0726, 0100,1602				; 9053		I,	CONSZ,	J/PAGCI
D 0727, 0501,1602				; 9054		I,	CONSO,	J/PAGCI
						; 9055	
						; 9056	;DEVICE 014 (CCA)
						; 9057	
D 0730, 0000,1704				; 9058	730:	I,	J/SWEEP	;BLKI CCA,	8 FUNCTIONS TO SWEEP THE CACHE
D 0731, 0000,1704				; 9059		I,	J/SWEEP	;SWPIA (DATAI CCA,)INVALIDATE CACHE, NO CORE UPDATE
D 0732, 0000,1704				; 9060		I,	J/SWEEP	;SWPVA (BLKO CCA,)VALIDATE CORE, LEAVE CACHE VALID
D 0733, 0000,1704				; 9061		I,	J/SWEEP	;SWPUA (DATAO CCA,)UNLOAD CACHE TO CORE, CLEAR CACHE
D 0734, 0000,1704				; 9062		I,	J/SWEEP	;CONO CCA,
D 0735, 0000,1704				; 9063		I,	J/SWEEP	;SWPIO (CONI CCA,)INVALIDATE ONE PAGE
D 0736, 0000,1704				; 9064		I,	J/SWEEP	;SWPVO (CONSZ CCA,)VALIDATE ONE PAGE
D 0737, 0000,1704				; 9065		I,	J/SWEEP	;SWPUO (CONSO CCA,)UNLOAD ONE PAGE
						; 9066	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 2
; IO.MIC[4,24]	15:27 17-Mar-86				I/O INSTRUCTIONS					

						; 9067	;I/O CONT'D
						; 9068	
						; 9069	;DEVICE 020 (TIM)
						; 9070	
D 0740, 3100,1706				; 9071	740:	W,	B/1,	J/RDMTR	;RDPERF (BLKI TIM,)	PERF CNT
D 0741, 3001,1706				; 9072		W,	B/0,	J/RDMTR	;RDTIME (DATAI TIM,)	TIME BASE
D 0742, 4401,1616				; 9073		R,	DATAO,	J/TIMBO	;WRPAE (BLKO TIM,)	PA ENABLES
D 0743, 4401,1615				; 9074		R,	DATAO,	J/TIMDO	;DATAO TIM,		UNDEFINED
D 0744, 0400,1604				; 9075		I,	CONO,	J/TIMCO	;CONO TIM,		SETUP INTERVAL TIMER
D 0745, 0600,1606				; 9076		I,	CONI,	J/TIMCI	;CONI TIM,		RD INTERVAL & PERIOD
D 0746, 0101,1606				; 9077		I,	CONSZ,	J/TIMCI
D 0747, 0500,1606				; 9078		I,	CONSO,	J/TIMCI
						; 9079	
						; 9080	;DEVICE 024 (MTR)
						; 9081	
						; 9082	
D 0750, 3301,1706				; 9083	750:	W,	B/3,	J/RDMTR	;RDMACT (BLKI MTR,)	CACHE CNT
D 0751, 3200,1706				; 9084		W,	B/2,	J/RDMTR	;RDEACT (DATAI MTR,)	EBOX CNT
D 0752, 0001,1002				; 9085		I,		J/UUO	;BLKO MTR,		UNDEFINED
D 0753, 0001,1002				; 9086		I,		J/UUO	;DATAO MTR,		UNDEFINED
D 0754, 0400,1610				; 9087		I,	CONO,	J/MTRCO	;WRTIME (CONO MTR,)	ACCT & TB CTL
D 0755, 0600,1612				; 9088		I,	CONI,	J/MTRCI	;CONI MTR,		SAME
D 0756, 0101,1612				; 9089		I,	CONSZ,	J/MTRCI
D 0757, 0500,1612				; 9090		I,	CONSO,	J/MTRCI
						; 9091	
						; 9092	;DEVICE 030
						; 9093	
D 0760, 6200,1200				; 9094	760:	RW,	BLKI,	J/BLKIO
D 0761, 3601,1206				; 9095		W,	DATAI,	J/IO
D 0762, 6001,1200				; 9096		RW,	BLKO,	J/BLKIO
D 0763, 4401,1206				; 9097		R,	DATAO,	J/IO
D 0764, 0401,1202				; 9098		I,	CONO,	J/CONO
D 0765, 3601,1206				; 9099		W,	CONI,	J/IO
D 0766, 0101,1202				; 9100		I,	CONSZ,	J/CONS
D 0767, 0500,1202				; 9101		I,	CONSO,	J/CONS
						; 9102	
						; 9103	;DEVICES 034-774 (ALL OTHERS)
						; 9104	
D 0770, 6200,1200				; 9105	770:	RW,	BLKI,	J/BLKIO
D 0771, 3601,1206				; 9106		W,	DATAI,	J/IO
D 0772, 6001,1200				; 9107		RW,	BLKO,	J/BLKIO
D 0773, 4401,1206				; 9108		R,	DATAO,	J/IO
D 0774, 0401,1202				; 9109		I,	CONO,	J/CONO
D 0775, 3601,1206				; 9110		W,	CONI,	J/IO
D 0776, 0101,1202				; 9111		I,	CONSZ,	J/CONS
D 0777, 0500,1202				; 9112		I,	CONSO,	J/CONS
						; 9113	
						; 9114		.UCODE
						; 9115	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 3
; IO.MIC[4,24]	15:27 17-Mar-86				EXTERNAL DEVICE I/O INSTRUCTIONS			

						; 9116	.TOC	"EXTERNAL DEVICE I/O INSTRUCTIONS"
						; 9117	
						; 9118	=0****00*000
U 1200, 3002,0001,0000,0000,0000,7350,0000	; 9119	BLKIO:	SKP IO LEGAL,CALL,J/IOCHK	;FIRST VERIFY INSTR VALIDITY
U 1201, 0363,0001,0000,0000,0020,0034,0000	; 9120		BYTE DISP,J/BLK1		;TEST FPD
						; 9121	CONS:					;HERE FOR CONSO, CONSZ TO LOAD
						; 9122						; BR IN CASE OF UUO
U 1202, 1206,4001,0040,0000,3000,0610,0004	; 9123	CONO:	BR/AR,ARL_ARR,ARR_ARR		;CONDITIONS TO BOTH HALVES
						; 9124	=10
U 1206, 1330,3200,0003,0000,0022,7350,0000	; 9125	IO:	AR_MEM,SKP IO LEGAL,CALL,J/GTEBUS;WAIT FOR MBOX IF BLKI/O
U 1207, 0022,0001,0000,0000,0005,2233,0000	; 9126	RELEEB:	REL ECL EBUS,B WRITE		;XFER DONE, WHAT TO DO?
						; 9127	=
						; 9128	=1*010
U 0022, 3554,4001,0003,0000,0002,0010,0000	; 9129	IOTEND:	MEM_AR,J/BLK4			;[414] BLKI/BLKO
U 0023, 0217,3602,0004,0000,0246,0010,0203	; 9130		TEST AR.BR,TEST FETCH,J/NOP	;CONSZ
U 0026, 0042,0001,0003,0000,0002,6510,0000	; 9131		MEM_AR,SKP PI CYCLE,J/IOFET	;DATA/CON I/O
U 0027, 0217,3602,0004,0000,0246,0010,0203	; 9132		TEST AR.BR,TEST FETCH,J/NOP	;CONSO
						; 9133	;BLKI/BLKO SCREWED AROUND WITH TO TRY TO STOP PI LOSSAGE
U 3554, 3122,5062,0000,0000,0246,6517,0203	; 9134	BLK4:	TEST BRL,TEST FETCH,SKP PI CYCLE
U 3122, 0133,4001,0000,0000,0000,0014,0000	; 9135	=0	CLR FPD,J/FINI			;[412] CAN'T DO THIS UNTIL STORE
U 3123, 0133,4001,0000,0000,0000,1610,0000	; 9136		SR_0,J/FINI			; COMPLETE--moved from termination
						; 9137	
						; 9138	.IF/IPA20				;[410] Tail of PI function 7
						; 9139	=1***00
U 0040, 3560,0001,0003,0000,0002,2250,0026	; 9140	ESEND:	MEM_AR,SET DATAO,CALL [EBUSO]	;Send data out over EBUS
						; 9141	.ENDIF/IPA20				;[410]
						; 9142	=1***10					;[410]
U 0042, 0217,4001,0000,0000,0217,0010,0000	; 9143	IOFET:	I FETCH,J/NOP			;HERE IF NOT PI CYCLE
U 0043, 2424,4001,0000,0000,0000,0024,0502	; 9144		DISMISS,J/PIFET			;DISMISS INTRPT AFTER DATA/CON I/O
						; 9145	
						; 9146	=1**010
U 0362, 0125,4001,0043,0000,0002,0633,0020	; 9147	BLK2:	MEM_AR,BR/AR,ARL_0.C,B DISP,J/BLK3
U 0363, 0362,4003,2000,0000,0036,0017,0000	; 9148	BLK1:	AR_AR+1,GEN CRY18,STORE,J/BLK2	;UPDATE POINTER WORD
U 0367, 0362,0001,0000,0000,0000,6510,0000	; 9149	=111	SKP PI CYCLE,J/BLK2		;IF FPD & NOT PI, DON'T INCREMENT
						; 9150	=1*101					;DO DATAI OR DATAO
U 0125, 1206,3703,0000,0000,0312,1110,0100	; 9151	BLK3:	VMA_AR,LOAD AR,SET FPD,J/IO	;GET DATA TO OUTPUT
U 0127, 1206,3703,0000,0000,0300,1110,0100	; 9152		VMA_AR,SET FPD,J/IO		;INPUT DO BEFORE MEM
						; 9153	
						; 9154	;;;NOTE NOTE NOTE SET FPD INHIBITED BY HARDWARE IF PI CYCLE (SCD5)
						; 9155	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 4
; IO.MIC[4,24]	15:27 17-Mar-86				EXTERNAL DEVICE I/O INSTRUCTIONS			

						; 9156	;SUBROUTINES TO HANDLE EBUS
						; 9157	;CALL WITH "SKP IO LEGAL"
						; 9158	;ENTER AFTER LOADING AR IF OUTPUT FUNCTION
						; 9159	
						; 9160	=00
U 1330, 1002,3242,2000,0000,0000,0010,0000	; 9161	GTEBUS:	AR_BR,J/UUO			;HERE IF IO ILLEGAL IN THIS MODE
U 1331, 3126,4001,0000,0000,0000,2250,0200	; 9162		REQ EBUS,CALL,J/WGRANT		;ASK PI SYSTEM FOR BUS
						; 9163	=11					;RETURN TO TRANSFER ROUTINE
						; 9164	
						; 9165	;SUBROUTINE TO PERFORM EBUS TRANSFER
						; 9166	;SETUP CONTROLLER SELECT AND FUNCTION LINES BEFORE CALL
						; 9167	;IF OUTPUT, ALSO PUT AR ONTO EBUS DATA LINES
						; 9168	
						; 9169	EBUSX:	GEN AR,TIME/5T,			;WAIT AFTER ASSERTING FUNCTION
U 1333, 3125,3701,0000,0000,0060,2210,0060	; 9170			SET EBUS DEMAND,J/WXFER	; AFTER 300 NS, ASSERT DEMAND
						; 9171	=0
						; 9172	EBUSW:	AR_EBUS,GEN AR,
U 3124, 3555,3703,3000,0000,0060,2210,0020	; 9173			CLR EBUS DEMAND,J/RELEB	;STROBE DATA AND DROP DEMAND
U 3125, 3124,3701,0000,0000,0000,6710,0000	; 9174	WXFER:	GEN AR,SKP -EBUS XFER,J/EBUSW	;WAIT FOR TRANSFER
						; 9175	
						; 9176	RELEB:	GEN AR,REL EBUS,TIME/5T,	;DROP DATA, CS, AND FCN
U 3555, 0003,3701,0000,0000,0060,2203,0100	; 9177			RETURN3			;AFTER 150 NS, THEN RELEASE BUS
						; 9178	
						; 9179	
						; 9180	;SUBROUTINE TO WAIT FOR PI SYSTEM TO GRANT EBUS
						; 9181	; IT WILL EITHER SEND EBUS GRANT, OR PI READY
						; 9182	
						; 9183	=0
U 3126, 3130,0001,0000,0000,0000,6610,0000	; 9184	WGRANT:	SKP -EBUS GRANT,J/WGRNT1	;GOT IT?
U 3127, 0361,0001,0000,0000,0000,2210,0000	; 9185		DROP EBUS REQ,J/TAKINT
						; 9186	=0
U 3130, 3556,3701,0000,0000,0000,2210,0030	; 9187	WGRNT1:	IO INIT,GEN AR,J/WGRNT2		;GOT IT, SETUP CS, FCN, AND DATA
U 3131, 3126,0001,0000,0000,0000,7010,0000	; 9188		SKP INTRPT,J/WGRANT		;DIDN'T GET IT, TEST FOR INTERUPT
						; 9189	WGRNT2:	GEN AR,TIME/5T,			;JUST WAIT
U 3556, 0003,3703,0000,0000,0060,0003,0030	; 9190			EBUS CTL/IO INIT,RETURN3
						; 9191	
						; 9192	;HERE TO START PI CYCLE TRANSFER.  HOLD EBUS CTL SELECTION
						; 9193	
U 3557, 1333,0001,0000,0000,0060,0010,0027	; 9194	EBUSI:	TIME/5T,EBUS CTL/DATAI,J/EBUSX
U 3560, 1333,3701,0000,0000,0060,0010,0026	; 9195	EBUSO:	GEN AR,TIME/5T,EBUS CTL/DATAO,J/EBUSX
						; 9196	
						; 9197	;SUBROUTINES TO CHECK IO LEGALITY FOR INTERNAL I/O INSTRUCTIONS
						; 9198	
						; 9199	3002:
U 3002, 1002,3242,2000,0000,0000,0010,0000	; 9200	IOCHK:	AR_BR,J/UUO			;NAUGHTY, MUST'NT DO
						; 9201	3003:
U 3003, 0001,4001,0000,0000,0000,0003,0000	; 9202	RET1:	RETURN1				;ONE-CYCLE NULL ROUTINE
						; 9203	
						; 9204	=0
U 3132, 1002,3242,2000,0000,0000,0010,0000	; 9205	GETEEB:	AR_BR,J/UUO			;IO ILLEGAL IN THIS MODE
U 3133, 0001,0001,0000,0000,0000,2203,0400	; 9206	GTEEB1:	GET ECL EBUS,RETURN1
						; 9207	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 5
; IO.MIC[4,24]	15:27 17-Mar-86				INTERNAL DEVICE FUNCTIONS -- APR, CCA			

						; 9208	.TOC	"INTERNAL DEVICE FUNCTIONS -- APR, CCA"
						; 9209	
						; 9210	=0****00***0
						; 9211	SWEEP:	BR/AR,SC_#,#/9.,CALL,
U 1704, 3002,0001,0040,0302,0000,7350,0011	; 9212			SKP IO LEGAL,J/IOCHK	;ALLOWED?
U 1705, 3561,4001,4000,0000,0000,0010,0000	; 9213		AR_SHIFT			;MOVE PAGE # TO PLACE
U 3561, 3562,3701,0000,0000,0307,0010,0606	; 9214	=	VMA_AR,SWEEP CACHE		;START A SWEEP
U 3562, 0016,4001,0000,0000,0002,0010,0000	; 9215	MBREL:	MB WAIT,J/IFNOP			;COMPLETE REG FUNC BEFORE FETCH
						; 9216	
						; 9217	
						; 9218	=0****00**00
U 1314, 3132,4001,0000,0000,0000,7350,0000	; 9219	APRDO:	CALL,SKP IO LEGAL,J/GETEEB	;SET ADDR BREAK
U 1315, 1207,3703,0000,0000,0060,2010,0417	; 9220		DATAO APR,J/RELEEB
U 1316, 3002,4001,0040,0000,0000,7350,0000	; 9221	APRBO:	BR/AR,CALL,SKP IO LEGAL,J/IOCHK	;SET CACHE REFILL ALGORITHM
U 1317, 3562,4001,0000,0000,0007,0010,0505	; 9222		WR REFILL RAM,J/MBREL		;INFO ALREADY IN VMA
						; 9223	=
						; 9224	
						; 9225	=0****00*000
U 1210, 3132,4001,0000,0000,0000,7350,0000	; 9226	APRDI:	CALL,SKP IO LEGAL,J/GETEEB	;READ ADDR BREAK
U 1211, 1207,0001,3000,0000,0060,2010,0513	; 9227		DATAI APR(L),J/RELEEB
						; 9228	=010
						; 9229	.IFNOT/DDT.BUG				;[346] Normal code
U 1212, 3002,0001,0000,0000,0000,7350,0000	; 9230	APRBI:	CALL,SKP IO LEGAL,J/IOCHK	;RETURN MICRO VERSION, SERIAL #
						; 9231		AR_SERIAL,TIME/3T,		;READ SERIAL NUMBER
U 1213, 0137,4001,0000,0000,0020,0750,0100	; 9232			CALL,J/UVERS		;GET MICRO-CODE VERSION IN AR
U 1217, 3563,3242,2000,0000,3000,0022,0004	; 9233	=111	ARL_ARR.S,AR_BR			;COMB SERIAL WITH VERSION
						; 9234	=	AR0-8_#,STORE,OPTIONS,		;SET OPTION FLAGS
U 3563, 0016,0001,0000,0000,0016,0110,0660	; 9235			J/STMEM
						;;9236	.IF/DDT.BUG				;[346] Gross hack to make EDDT work
						;;9237	APRBI:	AR_SERIAL,TIME/3T		;[346] Get hardware serial number
						;;9238	=
						;;9239	=0	AR_AR SWAP,FE_#,OPTIONS,	;Set to test bit 23
						;;9240			SKP IO LEGAL,CALL [IOCHK]; Is this a legal instruction here?
						;;9241		GEN P AND #,#/1,SKP SCAD NZ	;Was bit 23 set?
						;;9242	=0*0
						;;9243	SEROK:	AR_AR SWAP,CALL [UVERS]		;Maybe not. Get microcode version
						;;9244		P_P AND #,#/76,J/SERFIX		;Yes. Clean it and move it
						;;9245		ARL_ARR,ARR_BR			;It's fixed. Shuffle version to spot
						;;9246	=	AR0-8_FE,STORE,J/STMEM		;Move in options and store result
						;;9247	;
						;;9248	SERFIX:	FE_FE+#,#/10,J/SEROK		;OR in bit 5 for option
						; 9249	.ENDIF/DDT.BUG				;[346] All this code is GROSS!!
						; 9250	
						; 9251	=0****00**00
						; 9252	APRCO:	BR/AR,ARL_ARR.M,ARR_ARR,CALL.M,	;SET APR FLAGS
U 1504, 3132,0001,0040,0000,3001,7350,0004	; 9253			SKP IO LEGAL,J/GETEEB
U 1505, 3566,3701,0000,0000,0060,2010,0414	; 9254		CONO APR, J/APRCO7		;[272]
						; 9255	APRCI:	BR/AR,CALL,
U 1506, 3132,0001,0040,0000,0000,7350,0000	; 9256			SKP IO LEGAL,J/GETEEB	;READ APR FLAGS
U 1507, 3564,0001,3000,0000,0060,2010,0510	; 9257		CONI APR(R)			;GET RIGHT HALF OF APR CONDITIONS
U 3564, 3565,4001,3400,0000,3060,2010,0512	; 9258	=	ARX_AR SWAP,CONI APR(L)		;NOW LH COND TO AR LEFT
U 3565, 1207,0001,4000,0000,2000,0610,0000	; 9259		AR_ARX,ARL_ARL,J/RELEEB		;COMBINE HALVES
U 3566, 1207,3703,0000,0000,0060,2010,0414	; 9260	APRCO7:		CONO APR,J/RELEEB	;[272]
						; 9261	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 6
; IO.MIC[4,24]	15:27 17-Mar-86				INTERNAL DEVICE FUNCTIONS -- PI				

						; 9262	.TOC	"INTERNAL DEVICE FUNCTIONS -- PI"
						; 9263	
						; 9264	=0****00*000
						; 9265	.IFNOT/INSTR.STAT
						; 9266	PIDO:
U 1300, 1002,3242,2000,0000,0000,0010,0000	; 9267	PIDI:	AR_BR,J/UUO			;DATAI/O PI, UNASSIGNED
						; 9268	
						;;9269	.IF/INSTR.STAT
						;;9270	;DATAO PI, SETS UP BUFFER POINTERS FOR TRACKS
						;;9271	;DATAI PI, READS CURRENT BUFFER POINTER
						;;9272	
						;;9273	PIDI:
						;;9274	PIDO:	BR/AR,ARL+ARX_0.M,CALL.M,	;CHECK IO LEGALITY
						;;9275			SKP IO LEGAL,J/IOCHK
						;;9276		SC_#,#/9.,B DISP,SKP BR0,J/PIDX	;NOW, WHAT TO DO?
						; 9277	.ENDIF/INSTR.STAT
						; 9278	
U 1301, 0740,4001,0000,0000,0007,0010,0504	; 9279	PIBI:	READ ERA,J/RDEBRG		;GET AND STORE
						; 9280	=0
						; 9281	PIBO:	FE_#,#/7,CALL,			;NUMBER OF TIMES TO TRY
U 1302, 3002,0001,0000,0301,0000,7350,0007	; 9282			SKP IO LEGAL,J/IOCHK	;SBUS DIAGNOSTIC
U 1303, 3567,0001,0040,0000,0007,0010,0407	; 9283	DODIAG:	SBUS DIAG,BR/AR			;SEND THE DIAG FUNCTION FROM AR
						; 9284	=
						; 9285	.IFNOT/MOS.MULTI
U 3567, 0016,0001,1000,0000,0016,3610,0000	; 9286		AR/CACHE,VMA_VMA+1,STORE,J/STMEM ;STORE THE RESPONSE
						;;9287	.IF/MOS.MULTI
						;;9288		AR/CACHE,MB WAIT		;[225]GET THE DATA.
						;;9289		GEN AR+1,SKP AD NE		;IF MEMORY RETURNED -1 TRY AGAIN
						;;9290	=0
						;;9291		FE_FE-1,SKP SCAD0,J/SDTEST	;IT IS SEE IF TOO MANY TRIES
						;;9292	SDONE:	VMA_VMA+1,STORE,J/STMEM		;STORE THE RESPONSE
						;;9293	=0
						;;9294	SDTEST:	AR_BR,J/DIAG1			;[225]RECOVER THE FUNC AND RETRY.
						;;9295		AR_0S,J/SDONE			;TOO MANY TRIES QUIT RETURNING 0
						; 9296	.ENDIF/MOS.MULTI
						; 9297	
						; 9298	=0****00*000
						; 9299	PICO:	BR/AR,ARL_ARR.M,ARR_ARR,
U 1400, 3134,0001,0040,0000,3001,7350,0004	; 9300			CALL.M,SKP IO LEGAL,J/PICOM1
U 1403, 3573,3701,0000,0000,0060,2010,0415	; 9301	=11	CONO PI,J/PICOM2		;SEND THE DATA
						; 9302	=0****00*100
U 1404, 3134,0001,0040,0000,0000,7350,0000	; 9303	PICI:	BR/AR,CALL,SKP IO LEGAL,J/PICOM1
U 1407, 3570,4001,3000,0000,0060,2010,0500	; 9304	=11	CONI PI(R)			;READ RH TO AR LEFT
						; 9305	=	ARX_AR SWAP,			;RH COND TO ARX RH
U 3570, 3571,4001,3400,0000,3060,2010,0530	; 9306			CONI PI(PAR)		; AND PARITY ENABLES TO RH
						; 9307		BRX/ARX,ARX_AR,			;READY TO COMB RH PARTS
U 3571, 3572,0001,3420,0000,1060,2010,0501	; 9308			CONI PI(L)		; AND LH TO AR LEFT
U 3572, 3573,3302,6000,0000,0000,0610,0000	; 9309		AR_ARX*BRX,AD/OR,ARL_ARL	;COMBINE THEM
U 3573, 0022,3701,0000,0000,0005,2233,0100	; 9310	PICOM2:	REL EBUS,GEN AR,B WRITE,J/IOTEND
						; 9311	
						; 9312	=0
U 3134, 1002,3242,2000,0000,0000,0010,0000	; 9313	PICOM1:	AR_BR,J/UUO			;LOSE
U 3135, 3126,0001,0000,0000,0000,2210,0200	; 9314		REQ EBUS,J/WGRANT		;OK, WAIT TO GET FULL EBUS
						; 9315	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 7
; IO.MIC[4,24]	15:27 17-Mar-86				INTERNAL DEVICE FUNCTIONS -- PAG			

						; 9316	.TOC	"INTERNAL DEVICE FUNCTIONS -- PAG"
						; 9317	
						; 9318	=0****00**00
						; 9319	PAGBO:	AR_0S,BR/AR,CALL,		;CLEAR ONE PAGE TABLE ENTRY
U 1510, 3002,3441,2040,0000,0000,7350,0000	; 9320			SKP IO LEGAL,J/IOCHK
U 1511, 0016,4001,0000,0000,0020,2310,0010	; 9321		WR PT ENTRY,J/IFNOP
						; 9322	
						; 9323	PAGDO:	ARX_AR (AD),ARR_ARL,ARL_ARL.M,	;SETUP USER CONTEXT
U 1512, 3132,3703,4200,0000,3001,7350,0000	; 9324			CALL.M,SKP IO LEGAL,J/GETEEB
U 1513, 3574,3701,4000,0000,2060,2010,0620	; 9325		DATAO PAG(L),AR_ARX		;SETUP AC BLOCKS, PREV CTXT
U 3574, 3136,3203,0000,0302,0020,5510,0011	; 9326	=	SKP AR2,SC_#,#/9.
						; 9327	=0
U 3136, 1207,4001,0000,0000,0000,0010,0007	; 9328	TIMCO1:	MTR CTL/CONO TIM,J/RELEEB	;DO NOT CHANGE UBR
						;;9329	.IF/PAGCNT				;[327] Count DATAO PAG with bit 2
						;;9330		MQ_AR,AR_TRX3+1			;[327] Do the count
						;;9331		TRX3_AR,AR_MQ
						; 9332	.ENDIF/PAGCNT				;[327]
						; 9333		FE_P AND #,#/4,MQ_SHIFT,	;[333] Save bit 3 for keep test
U 3137, 0570,3441,2010,7131,0000,4410,0004	; 9334			SKP AR18,AR_0S		;STORE ACCT?
U 0570, 3140,0001,0000,0000,0000,0150,0100	; 9335	=0*0	AR0-8_#,#/100,CALL,J/PAGD2	;YES, START WITH EBOX CNT
U 0571, 3605,3721,0000,0000,0307,0050,0602	; 9336		VMA_MQ,LOAD UBR,CALL [CLRPT]	;[333] No. Set for page table clear
						; 9337	.IF/BIG.PT				;[333]
U 0575, 2756,4001,0000,2400,0020,5210,0000	; 9338	=1*1	SKP SC NE,J/KEEPME		;[333] Might keep keep me bits
						;;9339	.IFNOT/BIG.PT
						;;9340	=1*1	PT SEL_INVAL,J/PTLOOP		;SETUP INITIAL PT WR SELECT
						; 9341	.ENDIF/BIG.PT				;[333]
						; 9342	;
						; 9343	;	PAGD2 is set up as a subroutine for addressing convenience only.
						; 9344	;
						; 9345	=0
U 3140, 2772,0001,0400,0302,1000,0050,0015	; 9346	PAGD2:	ARX_AR,SC_#,#/13.,CALL,J/EMTR	;UPDATE THE EBOX ACCT
U 3141, 3575,0001,0000,0000,0002,0110,0140	; 9347		MB WAIT,AR0-8_#,#/140		;READY TO GET CACHE ACCT
U 3575, 2773,0001,0400,0302,1000,0010,0015	; 9348		ARX_AR,SC_#,#/13.,J/CMTR	;RETURN ABOVE TO CLR PT
						; 9349	
						; 9350	
						; 9351	=0****00**00
						; 9352	PAGDI:	SC_#,#/70,SKP IO LEGAL,
U 1514, 3132,4001,0000,0302,0000,7350,0070	; 9353			CALL,J/GETEEB
						; 9354		DATAI PAG(L),ARX_1B17-1,	;PUT AC BLKS IN AR,
U 1515, 3576,1761,3200,0000,0060,2057,0511	; 9355			CALL,J/PCTXT		; [0,,-1] IN ARX
U 1516, 1002,3242,2000,0000,0000,0010,0000	; 9356	PAGBI:	AR_BR,J/UUO			;BLKI PAG, IS UNASSIGNED
U 1517, 1207,3242,4000,0000,0000,0610,0002	; 9357		AR_SHIFT,ARL_BRL,J/RELEEB	;COMBINE UBR WITH AC BLKS, CWSX
						; 9358	=
U 3576, 3577,3243,0500,2400,3001,0010,0200	; 9359	PCTXT:	P_SC,ARX_ARX*8			;STUFF IN LOAD EN, ARX=7,,-10
U 3577, 3600,3203,0540,0000,2000,0210,0000	; 9360		BR/AR,AR12-17_PREV SEC,ARX_ARX*8
U 3600, 2220,3012,2044,0000,0000,0010,0000	; 9361		BR/AR,AR_ARX*BR,AD/ANDCA	;PCS TO BR, LD EN, AC BLKS TO AR
						; 9362	=0*	AR_AR*BR,AD/OR,READ UBR,	;LH READY IN AR.  GET UBR
U 2220, 1045,3302,2004,0000,0007,0050,0502	; 9363			CALL,J/XFERW
U 2222, 3601,4001,3040,0000,0060,2010,0567	; 9364		BR/AR,AR_EBUS REG		;LH TO BR.  READ UBR ADDRESS
						; 9365		ARX_AR,AR_0S,SC_#,#/27.,	;READY TO MOVE INTO POSITION
U 3601, 0003,3441,2400,0302,1000,0003,0033	; 9366			RETURN3
						; 9367	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 8
; IO.MIC[4,24]	15:27 17-Mar-86				INTERNAL DEVICE FUNCTIONS -- PAG			

						; 9368	;CONI/O PAG,
						; 9369	
						; 9370	=0****00**00
						; 9371	PAGCO:	BR/AR,ARL_ARR.M,ARR_ARR,ARX_0S,	;SET EXEC CONTEXT
U 1600, 3132,3401,0240,0000,3001,7350,0004	; 9372			SKP IO LEGAL,CALL.M,J/GETEEB
U 1601, 2750,3703,0000,0000,0060,2010,0416	; 9373		CONO PAG,J/SETEBR		;SET CACHE, SEC, TRAP EN FLAGS
						; 9374	
						; 9375	PAGCI:	BR/AR,AR_0S,CALL,SKP IO LEGAL,	;READ EXEC CONTEXT
U 1602, 3132,3441,2040,0302,0000,7350,0011	; 9376			SC_#,#/9.,J/GETEEB
U 1603, 2224,4001,3000,0000,0060,2010,0531	; 9377		CONI PAG			;READ CACHE, SEC, TRAP EN
						; 9378	=
						; 9379	=0*	ARX_AR SWAP,AR_0S,READ EBR,	;SETUP EPT LOC'N TO READ
U 2224, 1045,3401,2400,0000,3007,0050,0503	; 9380			CALL,J/XFERW
						; 9381		AR_EBUS REG,			;GET EBR IN AR
U 2226, 3602,3441,3220,0000,0060,2010,0567	; 9382			BRX/ARX,ARX_0S		;SAVE FLAGS IN LH OF BRX
U 3602, 3603,0001,0400,0000,0000,2210,0000	; 9383		ARX_SHIFT,REL ECL EBUS		;MOVE EBR LOC LEFT
U 3603, 3604,3302,0000,0000,0001,0010,0016	; 9384		ARR_0.M,ADB/BR,ADA EN/EN,AD/OR,ARL/ADX	;COMBINE, THEN PUT IN RH
U 3604, 0022,4001,4000,0000,3005,0033,0000	; 9385		AR_AR SWAP,B WRITE,J/IOTEND	;STORE THE RESULT
						; 9386	
						; 9387	;HERE TO FINISH CONO PAG,
						; 9388	
						; 9389	=000					;[342]
U 2750, 2604,4001,0000,0302,0000,0050,0011	; 9390	SETEBR:	SC_#,#/9.,CALL,J/SHIFT		;MOVE EBR LOC'N TO POSITION
U 2752, 3605,3701,0000,0000,0307,0050,0603	; 9391	=010	VMA_AR,LOAD EBR,CALL [CLRPT]	;[333]SETUP EBR
						; 9392	=110					;[342]SETUP INITIAL PT WR SELECT
U 2756, 2546,4001,0000,0000,0000,2310,0001	; 9393	KEEPME:	PT SEL_INVAL,J/PTLOOP		;[342] FOR NON KLPAGE THIS CAN SEND
						; 9394						; THE USER TO PAGBO1 AND SAVE 1
						; 9395						; UCODE LOCATION
						; 9396	.IF/BIG.PT				;[333] Entry from DATAO
U 2757, 2766,0001,0000,0000,0000,2310,0041	; 9397		PT SEL_INVAL (KEEP),J/KEEPCL	;[342] Hang on to KEEP pages
						; 9398	.ENDIF/BIG.PT				;[333]
						; 9399	=
						; 9400	=110
						; 9401	PTLOOP:	AR_AR+BR,VMA/AD,FE_FE-1,	;SELECT A LINE OF PT
						; 9402			CLR PT LINE,TIME/3T,	;DO THE WORK
U 2546, 2546,0602,2000,3001,0320,2334,0031	; 9403			BYTE DISP,J/PTLOOP	;LOOP TO CLEAR ALL
U 2547, 0016,4001,0000,0000,0000,2310,0000	; 9404		PT SEL_NORMAL,J/IFNOP		;RESET PT WR SELECTION
						; 9405	.IF/BIG.PT				;[333]
						; 9406	=110
						; 9407	KEEPCL:	AR_AR+BR,VMA/AD,FE_FE-1,TIME/3T,;SELECT A LINE OF PT
						; 9408			CLR PT LINE (KEEP),	;DO THE WORK
U 2766, 2766,0602,2000,3001,0320,2334,0061	; 9409			BYTE DISP,J/KEEPCL	;Hang onto lines with KEEP ME set
U 2767, 0016,4001,0000,0000,0000,2310,0000	; 9410		PT SEL_NORMAL,J/IFNOP		;RESET PT WR SELECTION
						; 9411	.ENDIF/BIG.PT				;[333]
						; 9412	;
						; 9413	;	[333] Set up to clear hardware page table after setting EBR or
						; 9414	;	UBR.  KEEP ME pages may or may not be cleared, depending upon the
						; 9415	;	setting of DATAO PAG bit 3.  (Clear everything for CONO PAG.)
						; 9416	;
U 3605, 3142,3441,2200,0000,0000,2210,0000	; 9417	CLRPT:	AR_0S,ARX_0S,REL ECL EBUS	;[334]DON'T HANG UP BUS FOR THIS
						; 9418	=0	AR0-8_#,#/10,MB WAIT,		;WAIT FOR U/E BR LOAD
U 3142, 0475,4001,0000,0000,0002,0153,0010	; 9419			SC_FE,CALL [ARSWAP]	;[334]GET 1B23
						; 9420		BR/AR,AR_0S,VMA/AD,		;[333][334] START CLEARING AT ZERO
U 3143, 0004,3441,2040,0301,0300,0003,0077	; 9421			FE_#,#/63.,RETURN4	;SETUP LOOP COUNT
						; 9422	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 9
; IO.MIC[4,24]	15:27 17-Mar-86				INTERNAL DEVICE FUNCTIONS -- TIM & MTR			

						; 9423	.TOC	"INTERNAL DEVICE FUNCTIONS -- TIM & MTR"
						; 9424	
						; 9425	=0****00***0
						; 9426	RDMTR:	AR_BR,CALL,			;GET E TO AR
U 1706, 3132,3242,2000,0000,0000,7350,0000	; 9427			SKP IO LEGAL,J/GETEEB	;GRAB CONTROL OF EBUS
						; 9428		MQ_AR,AR_0S,			;SAVE E IN MQ
U 1707, 1340,3401,2010,0302,1000,0033,0015	; 9429			SC_#,#/13.,B DISP	;WHICH COUNTER?
						; 9430	=
U 1340, 3144,0001,3000,0000,0060,2010,0510	; 9431	=00	AR_TIME BASE,J/RDMTR1		;DATAI TIM,
U 1341, 3144,4001,3000,0000,0060,2010,0511	; 9432		AR_PERF CNT,J/RDMTR1		;BLKI TIM,
U 1342, 3144,4001,3000,0000,0060,2010,0512	; 9433		AR_EBOX CNT,J/RDMTR1		;DATAI MTR,
U 1343, 3144,0001,3000,0000,0060,2010,0513	; 9434		AR_CACHE CNT,J/RDMTR1		;BLKI MTR,
						; 9435	
						; 9436	=0
						; 9437	RDMTR1:	ARL+ARX_0.M,B DISP,		;SHIFT COUNT INTO POSITION
U 3144, 1350,4001,0000,0000,0001,0073,0060	; 9438			CALL.M,J/MTRDBL		;ADD DOUBLE WORD FROM PT
U 3145, 3606,0602,2600,0000,0020,2227,0000	; 9439		AR_AR+BR LONG,REL ECL EBUS
U 3606, 0372,3721,0000,0302,0316,0010,0043	; 9440		VMA_MQ,STORE,SC_#,#/35.,J/DMVM1	;STORE TOTAL AT E & E+1
						; 9441	
						; 9442	=0****00**00
U 1604, 3132,0001,0040,0000,0000,7350,0000	; 9443	TIMCO:	BR/AR,CALL,SKP IO LEGAL,J/GETEEB
U 1605, 3136,3701,0000,0000,0060,2023,0407	; 9444		CONO TIM,J/TIMCO1
						; 9445	TIMCI:	BR/AR,AR_0S,CALL,
U 1606, 3132,3401,2040,0000,0000,7350,0000	; 9446			SKP IO LEGAL,J/GETEEB
U 1607, 3607,4001,3000,0000,0060,2010,0514	; 9447		AR_INTERVAL			;INTERVAL GOES TO LH
U 3607, 3610,3441,2400,0000,3000,0010,0000	; 9448	=	ARX_AR SWAP,AR_0S
U 3610, 3611,4001,3000,0000,0060,2010,0515	; 9449		AR_PERIOD			;PERIOD TO RH
						; 9450	TIMBO1:	MTR CTL/LD PA LH,		;KEEP MTR DECODE FOR TIMBO
U 3611, 1207,4001,0000,0000,2000,0022,0004	; 9451			ARL_ARXL,J/RELEEB	;COMBINE PERIOD WITH INTERVAL
						; 9452	
						; 9453	=0****00**00
						; 9454	MTRCO:	BR/AR,ARL_ARR.M,ARR_ARR,
U 1610, 3132,0001,0040,0000,3001,7350,0004	; 9455			CALL.M,SKP IO LEGAL,J/GETEEB
U 1611, 3612,3701,0000,0000,0060,2023,0406	; 9456		CONO MTR,J/MTRCO1
						; 9457	MTRCI:	BR/AR,AR_0S,CALL,
U 1612, 3132,3401,2040,0000,0000,7350,0000	; 9458			SKP IO LEGAL,J/GETEEB
U 1613, 3612,4001,3000,0000,0060,2010,0516	; 9459		CONI MTR			;READ BACK CONDITIONS
						; 9460	=
U 3612, 1207,0001,0000,0000,0000,0022,0026	; 9461	MTRCO1:	ARL_0.S,MTR CTL/CONO MTR,J/RELEEB
						; 9462	;
						; 9463	;	UUOCHK subroutine tucks in here.  No skip if legal; sideways exit
						; 9464	;	to UUO if not.
						; 9465	;
						; 9466	=0****00**00
U 1614, 0002,4001,0000,0000,0000,0003,0000	; 9467	UUOCHK:	RETURN2				;Tested condition OK
U 1615, 1002,3242,2000,0000,0000,0010,0000	; 9468	TIMDO:	AR_BR,J/UUO			;DATAO TIM, UNDEFINED
						; 9469	TIMBO:	ARX_AR,AR_0S,CALL,		;SAVE ENABLES, CLEAR AR
U 1616, 3132,3441,2400,0000,1000,7350,0000	; 9470			SKP IO LEGAL,J/GETEEB	;CHECK LEGALITY, GET BUS
U 1617, 3613,3701,4000,0000,2060,2023,0404	; 9471		BLKO TIM(L),AR_ARX		;TURN OFF BY CLEARING LH ENABLES
U 3613, 3614,3703,4000,0000,3060,2023,0405	; 9472	=	BLKO TIM(R),AR_AR SWAP		;SEND RH
U 3614, 3611,3703,0000,0000,0060,2023,0404	; 9473		BLKO TIM(L),J/TIMBO1		;SEND LH, TURNING ON AGAIN
						; 9474	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 10
; IO.MIC[4,24]	15:27 17-Mar-86				INTERNAL DEVICE FUNCTIONS -- TIM & MTR			

						; 9475	;HERE WHEN METER INCREMENT REQUEST DETECTED
						; 9476	
U 3615, 3616,4001,0000,0000,0000,2210,0400	; 9477	MTRREQ:	GET ECL EBUS			;TAKE CONTROL OF BUS
U 3616, 3146,0001,3000,0000,0060,2010,0517	; 9478		AR_MTR REQ			;WHAT TYPE REQUEST?
						; 9479	=0	MQ_AR,AR_AR*2,CALL,		;GET READY TO DISP
U 3146, 3620,3703,5010,0000,1100,3050,0514	; 9480			VMA_#,#/514,J/MTRRQ0
U 3147, 3617,4001,0000,0000,0000,2210,0000	; 9481		REL ECL EBUS			;DONE
U 3617, 2424,4001,0000,0000,0002,2110,0105	; 9482		MB WAIT,SET ACCOUNT EN,J/PIFET	;FETCH NEXT INSTR
						; 9483	
						; 9484	MTRRQ0:	ARX_AR SWAP,DISP/SH0-3,AR_0S,	;DISPATCH ON REQUEST TYPE
U 3620, 2770,3401,2400,0302,3020,0007,0015	; 9485			SC_#,#/13.
						; 9486	=000
U 2770, 3623,0001,3000,0000,0060,2023,0510	; 9487		RD+CLR TB,J/TMTR1		;TIME BASE
U 2771, 3624,0001,3000,0000,0060,2023,0511	; 9488		RD+CLR PA,J/PMTR1		;PERF ANALYSIS CNT
U 2772, 3625,4001,3000,0000,0060,2023,0512	; 9489	EMTR:	RD+CLR E CNT,J/EMTR1		;EBOX CNT
U 2773, 3626,0001,3000,0000,0060,2023,0513	; 9490	CMTR:	RD+CLR C CNT,J/CMTR1		;CACHE CNT
U 2774, 3621,4001,0000,0000,0000,2210,0000	; 9491		REL ECL EBUS			;INTERVAL -- VECTOR INTERRUPT
U 3621, 3622,4001,0000,0000,0000,1510,0714	; 9492	=	SET PI CYCLE
U 3622, 2363,4001,0000,0000,0000,2110,0105	; 9493		SET ACCOUNT EN,J/PIINST
						; 9494	
U 3623, 3627,4001,0000,0000,0000,0010,0000	; 9495	TMTR1:	MTR CTL/CLR TIME,J/MTRRQ1	;HOLD SELECTS FOR
U 3624, 3627,0001,0000,0000,0000,0010,0001	; 9496	PMTR1:	MTR CTL/CLR PERF,J/MTRRQ1	;MTR CTL FUNCTION
U 3625, 3627,0001,0000,0000,0000,0010,0002	; 9497	EMTR1:	MTR CTL/CLR E CNT,J/MTRRQ1	; TO PREVENT RACE
U 3626, 3627,4001,0000,0000,0000,0010,0003	; 9498	CMTR1:	MTR CTL/CLR M CNT,J/MTRRQ1	; AND POSSIBLE GLITCHES
						; 9499	
						; 9500	;HERE WITH RELEVANT COUNT IN ARR, GARBAGE IN ARL
						; 9501	
U 3627, 3150,0001,0000,0000,0000,1510,0004	; 9502	MTRRQ1:	ABORT INSTR
						; 9503	=0	ARL+ARX_0.M,ARX0-3 DISP,	;CLEAR GARBAGE & RE-DISPATCH
U 3150, 1350,0001,0000,0000,2021,0047,0060	; 9504			CALL.M,J/MTRDBL		; TO ADD DOUBLE COUNTER FROM PT
						; 9505		AR_AR+BR LONG,SC_#,#/35.,
U 3151, 3630,0602,2604,0302,0036,3527,0043	; 9506			VMA_VMA-1,STORE		;STORE BACK IN PROCESS TABLE
U 3630, 3631,3441,2003,0000,0002,0010,0000	; 9507		MEM_AR,AR_0S			;HI PART TO MEM
U 3631, 0001,4001,4000,0000,0016,3603,0000	; 9508		AR_SHIFT,VMA_VMA+1,STORE,RETURN1
						; 9509	
						; 9510	;HERE TO PICK UP DOUBLEWORD FROM PROCESS TABLE
						; 9511	; AND ADD CURRENT CONTENTS OF APPROPRIATE METER
						; 9512	
						; 9513	=00
						; 9514	MTRDBL:	AR_0S,ARX_SHIFT,
U 1350, 3632,3441,2400,0000,0100,3010,0510	; 9515			VMA_#,#/510,J/RDEMTR	;TIME BASE IN EPT 510-511
						; 9516		AR_0S,ARX_SHIFT,
U 1351, 3632,3401,2400,0000,0100,3010,0512	; 9517			VMA_#,#/512,J/RDEMTR	;PERF CNT IN EPT 512-513
						; 9518		AR_0S,ARX_SHIFT,
U 1352, 3633,3401,2400,0000,0100,3010,0504	; 9519			VMA_#,#/504,J/RDUMTR	;EBOX ACCT IN UPT 504-505
						; 9520		AR_0S,ARX_SHIFT,
U 1353, 3633,3441,2400,0000,0100,3010,0506	; 9521			VMA_#,#/506,J/RDUMTR	;CACHE ACCT IN UPT 506-507
						; 9522	
						; 9523	RDEMTR:	BR_AR LONG,			;SAVE COUNT IN BR!BRX
U 3632, 2230,4001,0060,0000,0012,0026,0113	; 9524			LOAD AR,EPT REF,J/RDMTR2;GET HIGH WORD FROM EPT
U 3633, 2230,4001,0060,0000,0012,0026,0223	; 9525	RDUMTR:	BR_AR LONG,LOAD AR,UPT REF	; OR UPT AS APPROP
						; 9526	=0*
						; 9527	RDMTR2:	FIN XFER,VMA_VMA+1,LOAD ARX,	;NOW GET LOW WORD
U 2230, 1045,3200,0003,0000,0033,3650,0000	; 9528			CALL,J/XFERW		;GO WAIT FOR IT
U 2232, 0001,3701,0500,0000,0000,0003,0000	; 9529		ARX_ARX*2,RETURN1
						; 9530	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11
; IO.MIC[4,24]	15:27 17-Mar-86				PRIORITY INTERRUPT PROCESSING				

						; 9531	.TOC	"PRIORITY INTERRUPT PROCESSING"
						; 9532	;HERE WHEN PRIORITY INTERRUPT REQUEST DETECTED
						; 9533	;PI LOGIC HAS DONE HANDSHAKE TO BRING FUNCTION WORD IN ON EBUS
						; 9534	; FUNCTION WORD IS NOW IN AR, SC=2
						; 9535	
						; 9536	;THE FORMAT OF THE FUNCTION WORD IS --
						; 9537	;    0-2	ADDRESS SPACE FOR THE FUNCTION
						; 9538	;	0=EPT
						; 9539	;	1=EXEC VIRTUAL
						; 9540	;	4=PHYSICAL
						; 9541	;	OTHERS UNDEFINED
						; 9542	;    3-5	FUNCTION TO PERFORM (SEE LIST BELOW AT PIDISP)
						; 9543	;      6	FUNCTION QUALIFIER
						; 9544	;   7-10	PHYSICAL DEVICE # ON EBUS
						; 9545	;  11-12	UNDEFINED
						; 9546	;  13-35	ADDRESS FOR FUNCTION
						; 9547	
U 3634, 3635,4001,0010,0000,1000,1510,0714	; 9548	PICYC1:	SET PI CYCLE,MQ_AR		;START PI CYCLE
U 3635, 3636,0001,0007,0000,0000,1010,0163	; 9549		FM[SV.IOP]_AR			;[234] save IOP function word
						; 9550						; in AC3.
U 3636, 2350,3600,0207,0000,0340,0007,0175	; 9551		VMA_AR AND ADMSK,ARX/AD,SH DISP	;EXTRACT ADDR, DISP ON FCN
						; 9552	=1000					;3-5 IS FUNCTION TO PERFORM
U 2350, 2363,4001,0000,0000,0100,3410,0040	; 9553	PIDISP:	VMA_40+PI*2,J/PIINST		;(0) STANDARD INTERRUPT
U 2351, 2363,4001,0000,0000,0100,3410,0040	; 9554		VMA_40+PI*2,J/PIINST		;(1) DITTO
U 2352, 3642,3243,2000,0302,0000,0010,0005	; 9555		AR_AR*4,SC_#,#/5,J/PIVECT	;(2) VECTOR
U 2353, 2417,0001,0000,0000,0015,0010,0000	; 9556		LOAD AR (RPW),J/PIINCR		;(3) INCREMENT [410] and interlock
U 2354, 2420,4001,0000,7310,0020,5210,0040	; 9557		SKP AR6,J/PIDATO		;(4) DATAO
U 2355, 0732,0001,0020,0000,0060,0010,0000	; 9558		BRX/ARX,TIME/5T,J/PIDATI	;(5) DATAI
U 2356, 3040,3203,2000,7310,0020,5210,0040	; 9559		AR_AR*4,SKP AR6,J/PIBYTE	;(6) BYTE TRANSFER
						;;9560	.IFNOT/IPA20				;[265]
						;;9561		VMA_40+PI*2,J/PIINST		;(7) UNDEFINED
						; 9562	.IF/IPA20				;[265]
U 2357, 3637,0001,0000,0000,0015,0026,0103	; 9563		LOAD AR (RPW),PHYS REF   	;[411] Increment word with interlock
U 3637, 3640,3200,0003,0000,0022,0010,0000	; 9564	AWAIT:	AR_MEM				;***HACK*** until real AWAIT ready
U 3640, 0040,4003,2000,0000,0036,0026,0103	; 9565		AR_AR+1,STORE,PHYS REF,J/ESEND	;[411] so SMP will work properly
						; 9566	.ENDIF/IPA20
						; 9567	
U 3641, 2363,0001,0000,0000,0100,3410,0041	; 9568	PICYC2:	VMA_41+PI*2,J/PIINST		;2ND PART OF STD INT
						; 9569	
U 3642, 2363,3401,2000,7311,0020,0007,0030	; 9570	PIVECT:	FE_# AND S,#/30,AR_0S,SH DISP	;WHAT KIND OF DEVICE?
						; 9571	=0011
U 2363, 0366,4001,0000,0000,0013,0026,0513	; 9572	PIINST:	EPT FETCH,J/XCTW		;CHAN 0-3
U 2367, 0366,4001,0000,0000,0013,0026,0513	; 9573		EPT FETCH,J/XCTW		;CHAN 4-7
U 2373, 2234,0001,0000,2030,2000,0110,0142	; 9574		AR0-8_FE+#,#/142,J/DTEVEC	;DTE 0-3
U 2377, 0366,0001,0000,0000,0013,0010,0000	; 9575		LOAD ARX,J/XCTW			;EXTERNAL DEVICE
						; 9576	=0*
						; 9577	DTEVEC:	ARX_AR (AD),CLR AR,
U 2234, 2604,3701,0200,0302,0000,0450,0011	; 9578		    SC_#,#/9.,CALL,J/SHIFT
U 2236, 0366,3701,0000,0000,0313,0026,0513	; 9579		VMA_AR,EPT FETCH,J/XCTW
						; 9580	
						; 9581	=0101
U 2405, 1045,4001,0000,0000,0012,0026,0113	; 9582	PILD:	LOAD AR,EPT REF,J/XFERW		;GET DATUM FROM EPT
U 2407, 1045,4001,0000,0000,0012,0010,0000	; 9583		LOAD AR,J/XFERW			; OR EXEC VIRTUAL ADDR SPACE
U 2415, 1470,3610,0207,4000,0320,0010,0175	; 9584		VMA_ARX AND ADMSK,ARX/AD,J/PHYS2;FORCE AC'S FOR 0-17
						; 9585	
						; 9586	;HERE TO PERFORM INCREMENT FUNCTION; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 11-1
; IO.MIC[4,24]	15:27 17-Mar-86				PRIORITY INTERRUPT PROCESSING				

						; 9587	
U 2417, 3152,3240,0003,7310,0042,5210,0040	; 9588	PIINCR:	AR_MEM,SKP AR6			;GET WORD, INCR OR DECR?
U 3152, 2423,4001,2000,0000,0036,0010,0000	; 9589	=0	AR_AR+1,STORE,J/PIDONE
U 3153, 2423,1703,2000,0000,0036,0010,0000	; 9590		AR_AR-1,STORE,J/PIDONE
						; 9591	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 12
; IO.MIC[4,24]	15:27 17-Mar-86				PRIORITY INTERRUPT PROCESSING				

						; 9592	;HERE FOR DATAO (EXAMINE) FUNCTION
						; 9593	
						; 9594	=0000
U 2420, 2405,0001,0000,0000,1020,0047,0000	; 9595	PIDATO:	AR0-3 DISP,CALL.M,J/PILD	;GET DATA FROM REQUESTED ADR SPC
U 2421, 3644,3243,2000,0000,0000,0050,0000	; 9596		AR_AR*4,CALL,J/RDEX		;RESTRICTED EXAMINE
U 2422, 3560,3240,0003,0000,0022,2250,0026	; 9597	PIOUT:	AR_MEM,SET DATAO,CALL,J/EBUSO	;SEND DATA
U 2423, 2424,0001,0003,0000,0002,0024,0502	; 9598	PIDONE:	MEM_AR,DISMISS			;DONE, DISMISS & RESUME NORMAL
U 2424, 0217,4001,0000,0000,0117,0010,0000	; 9599	PIFET:	VMA/PC,FETCH,J/NOP		;RESUME AS BEFORE
U 2425, 2320,1703,0000,0000,0320,0050,0000	; 9600	=0101	VMA_AR-1,CALL,J/DEXCHK		;GO PROT/RELOC THIS EXAM
U 2427, 1450,3610,0207,0000,0320,0010,0175	; 9601	=0111	VMA_ARX AND ADMSK,ARX/AD,J/PHYS1;FORCE AC'S FOR 0-17
U 2437, 2422,4001,0000,0000,0000,0010,0000	; 9602	=1111	J/PIOUT				;PROT VIOLATION.  SEND 0
						; 9603	
						; 9604	;HERE FOR DATAI (DEPOSIT) FUNCTION
						; 9605	
						; 9606	=10
U 0732, 3557,3441,2000,0000,0060,2250,0027	; 9607	PIDATI:	SET DATAI,TIME/5T,CALL,J/EBUSI	;READ THE DATA
U 0733, 3643,3723,2400,0000,1000,0010,0000	; 9608		ARX_AR,AR_MQ			;DATUM TO ARX, GET FCN WORD
U 3643, 2440,3242,0620,7310,0020,5210,0040	; 9609		BRX/ARX,ARX_BRX,SKP AR6		;RESTRICTED?
U 2440, 2465,3202,6000,0000,1020,0007,0000	; 9610	=0000	AR0-3 DISP,AR_BRX,J/PIST	;NO, STORE AS REQUESTED
U 2441, 3644,3243,2000,0000,0000,0050,0000	; 9611		AR_AR*4,CALL,J/RDEX		;YES, GET PROT/RELOC ADDR
U 2445, 2320,4001,0000,0000,0320,0050,0000	; 9612	=0101	VMA_AR+1,CALL,J/DEXCHK		;VERIFY LEGALITY
U 2447, 2475,3242,6000,0000,0000,0010,0000	; 9613	=0111	AR_BRX,J/PSTOR			;DATA IN AR, ADDR IN ARX. STORE PHYS
U 2457, 2423,0001,0000,0000,0000,0010,0000	; 9614	=1111	J/PIDONE			;PROT VIOLATION, STORE NOTHING
						; 9615	
						; 9616	=0101
U 2465, 2423,4001,0000,0000,0016,0026,0113	; 9617	PIST:	STORE,EPT REF,J/PIDONE
U 2467, 2423,4001,0000,0000,0016,0010,0000	; 9618		STORE,J/PIDONE
U 2475, 1530,3610,0207,4000,0320,0010,0175	; 9619	PSTOR:	VMA_ARX AND ADMSK,ARX/AD,J/PHYS3;FORCE AC'S FOR 0-17
U 2477, 2423,0001,0000,0000,0000,0010,0000	; 9620		J/PIDONE
						; 9621	
U 3644, 3645,4001,4000,7311,2000,0010,0030	; 9622	RDEX:	FE_# AND S,#/30,AR_ARX		;DTE# *8 TO FE, ADDR TO AR
U 3645, 3646,0001,0040,2030,2000,0110,0145	; 9623		BR/AR,AR0-8_FE+#,#/145		;SAVE ADDR TO BR, GET EPT LOC
U 3646, 3647,3401,2400,0302,1000,0010,0011	; 9624	GTAR08:	ARX_AR,AR_0S,SC_#,#/9.
U 3647, 0004,0001,4000,0000,0000,0003,0000	; 9625		AR_SHIFT,RETURN4
						; 9626	
						; 9627	=0*
U 2320, 2405,0001,0000,0000,0000,0050,0000	; 9628	DEXCHK:	CALL,J/PILD			;PROTECTION WORD FROM EPT
U 2322, 1440,3102,0004,0000,0020,5410,0000	; 9629		SKP AR GT BR			;ALLOWED?
U 1440, 0012,3401,2000,0000,0000,0003,0000	; 9630	=00	AR_0S,RETURN12			;NO, SEND 0, STORE NOTHING
U 1441, 2405,0001,0000,0000,0000,3650,0000	; 9631		VMA_VMA+1,CALL,J/PILD		;YES, GET RELOCATION WORD
U 1443, 3650,0602,2004,0000,0020,0010,0000	; 9632	=11	AR_AR+BR			;RELOCATE TO PHYSICAL ADDR
U 3650, 0002,3600,0207,4000,0020,0003,0175	; 9633		ARX_AR AND ADMSK,RETURN2	;STRIP TO 23 BITS
						; 9634	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 13
; IO.MIC[4,24]	15:27 17-Mar-86				PRIORITY INTERRUPT PROCESSING				

						; 9635	;FORCE AC'S FOR 0-17
						; 9636	
						; 9637	=00
U 1450, 3651,4001,0000,0302,0000,0050,0040	; 9638	PHYS1:	SC_#,#/32.,CALL,J/CHKAC		;DATAO (EXAMINE)
U 1452, 2422,4001,0000,0000,0012,0010,0000	; 9639	=10	LOAD AR,J/PIOUT			;AC REF DONT USE PHYS REF
U 1453, 2422,0001,0000,0000,0012,0026,0103	; 9640		LOAD AR,PHYS REF,J/PIOUT	;NOT AC'S GET PHYSICAL MEMORY
						; 9641	
						; 9642	=00
U 1470, 3651,4001,0000,0302,0000,0050,0040	; 9643	PHYS2:	SC_#,#/32.,CALL,J/CHKAC		;DATAO (EXAMINE)
U 1472, 1045,4001,0000,0000,0012,0010,0000	; 9644	=10	LOAD AR,J/XFERW
U 1473, 1045,0001,0000,0000,0012,0026,0103	; 9645		LOAD AR,PHYS REF,J/XFERW	;NOT AC'S GET PHYSICAL MEMORY
						; 9646	
						; 9647	=00
U 1530, 3651,4001,0000,0302,0000,0050,0040	; 9648	PHYS3:	SC_#,#/32.,CALL,J/CHKAC
U 1532, 2423,4001,0000,0000,0016,0010,0000	; 9649	=10	STORE,J/PIDONE
U 1533, 2423,0001,0000,0000,0016,0026,0103	; 9650		STORE,PHYS REF,J/PIDONE
						; 9651	
U 3651, 3652,4001,0400,0000,0000,0010,0000	; 9652	CHKAC:	ARX_SHIFT			;GET ADDRESS WITHOUT 32-35
						; 9653		ARX_ARX AND ADMSK,		;FLUSH GARBAGE IN 0-3
U 3652, 0002,3610,0207,0000,0020,5603,0175	; 9654			SKP AD NE,RETURN2	;AND MAKE THE TEST
						; 9655	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 14
; IO.MIC[4,24]	15:27 17-Mar-86				PRIORITY INTERRUPT PROCESSING				

						; 9656	;HERE FOR BYTE TRANSFERS
						; 9657	
						; 9658	=000
U 3040, 3653,4001,0000,7311,0000,0050,0030	; 9659	PIBYTE:	FE_# AND S,#/30,CALL,J/PIBPA	;OUT... GET BP ADDR
U 3041, 3557,3441,2000,0000,0000,2250,0027	; 9660		SET DATAI,CALL,J/EBUSI		;IN ... FIRST READ THE DATA
U 3043, 3050,0323,5400,0000,1020,0010,0000	; 9661	=011	ARX_AR,AR_MQ*4,J/PIDPB		;GOT IT, GO DEPOSIT IT
						; 9662	
U 3044, 0750,3701,0000,0000,0300,0050,0000	; 9663	=100	VMA_AR,CALL,J/PIIBP		;GO INCREMENT OUTPUT BP
U 3045, 2570,0001,0000,2002,0000,0050,0000	; 9664		SC_FE+SC,CALL,J/LDB1		;GO LOAD BYTE FROM IT
U 3047, 2422,4001,0000,0000,0000,0010,0000	; 9665	=111	J/PIOUT				;THEN SEND BYTE
						; 9666	
						; 9667	=000
						; 9668	PIDPB:	BRX/ARX,FE_# AND S,#/30,	;HERE WITH INPUT DATA
U 3050, 3653,0001,0020,7311,0000,0050,0030	; 9669			CALL,J/PIBPA
U 3054, 0750,4003,0000,0000,0320,0050,0000	; 9670	=100	VMA_AR+1,CALL,J/PIIBP		;GO INCREMENT INPUT BYTE PTR
						; 9671		AR_BRX,SC_#-SC,#/36.,		;STORE BYTE WITH IT
U 3055, 2610,3202,6000,5302,0020,5150,0044	; 9672			SKP SCAD0,CALL,J/DPB1
U 3057, 2423,0001,0000,0000,0000,0010,0000	; 9673	=111	J/PIDONE
						; 9674	
U 3653, 3646,4001,0000,2030,2000,0110,0140	; 9675	PIBPA:	AR0-8_FE+#,#/140,J/GTAR08
						; 9676	
						; 9677	=00*
U 0750, 2405,0001,0000,0000,0000,0050,0000	; 9678	PIIBP:	CALL,J/PILD			;GET POINTER FROM EPT
U 0752, 2562,0001,0000,5110,3021,5150,0200	; 9679		P_P-S,SKP SCAD0,CALL.M,J/IBPS	;INCREMENT IT
U 0756, 3370,0001,0400,0102,1000,0010,0000	; 9680	=11*	ARX_AR,SC_P,J/BYTEA		;NOW EVALUATE ITS ADDR
						; 9681	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 9682	.TOC	"KL-MODE PAGE REFILL LOGIC"
						; 9683	
						; 9684	;HERE ON ANY PAGE FAILURE
						; 9685	;THE POSSIBLE CAUSES ARE --
						; 9686	;  1:	A PARITY ERROR WAS DETECTED IN AR OR ARX FOLLOWING A READ
						; 9687	;	REFERENCE.  IN THIS CASE WE SAVE THE BAD WORD IN A RESERVED
						; 9688	;	LOCATION IN FAST MEMORY BLOCK 7, AND RETURN A PAGE FAIL CODE
						; 9689	;	INDICATING THE ERROR.
						; 9690	;  2:	THE MBOX DETECTED A PROPRIETARY VIOLATION OR PAGE TABLE PARITY
						; 9691	;	ERROR, OR THE EBOX FOUND THE SELECTED ADDRESS BREAK CONDITION.
						; 9692	;	IN THIS CASE, WE RETURN THE PAGE FAIL CODE GENERATED BY THE
						; 9693	;	MBOX (SEE PRINT PAG4).
						; 9694	;  3:	A REFERENCE OCCURRED FOR A VIRTUAL PAGE FOR WHICH THE HARDWARE
						; 9695	;	PAGE TABLE DIRECTORY HAD NO VALID MATCH.  IN THIS CASE, WE
						; 9696	;	WRITE THE PAGE TABLE DIRECTORY FROM THE VMA, AND CLEAR THE
						; 9697	;	ACCESS BITS FOR ALL PAGE ENTRIES CONTROLLED BY THE SELECTED
						; 9698	;	DIRECTORY ENTRY.  WE THEN JOIN THE REFILL CODE, BELOW.
						; 9699	;  4:	A REFERENCE OCCURRED FOR A VIRTUAL PAGE FOR WHICH THE ACCESS BIT
						; 9700	;	IN THE HARDWARE PAGE TABLE WAS OFF, OR A WRITE OCCURRED TO A
						; 9701	;	PAGE WHOSE WRITABLE BIT WAS OFF.  IN THIS CASE, WE EVALUATE THE
						; 9702	;	PAGING POINTERS IN CORE TO DETERMINE WHETHER THE ACCESS SHOULD
						; 9703	;	BE ALLOWED, AND IF SO, THE PHYSICAL PAGE TO WHICH IT SHOULD BE
						; 9704	;	TRANSLATED.  WE THEN EITHER PAGE FAIL, OR WRITE A PAGE ENTRY
						; 9705	;	INTO THE HARDWARE PAGE TABLE AND RESTART THE REFERENCE.
						; 9706	;
						; 9707	;	[322] Note that in the latter case, if a page should be accessible
						; 9708	;	but not writable, it is the microcode's responsibility to turn on
						; 9709	;	bit 2 of the page fail word (the Accessible bit) if a write
						; 9710	;	reference is attempted to such a page.  Currently, this is done only
						; 9711	;	if no CST is present (TOPS-10 operating mode), since TOPS-20
						; 9712	;	retraces the page map from scratch and thus generates the correct
						; 9713	;	information.  The bit can be made correct for the CST present case
						; 9714	;	(see code near NOTWR), but since we are now quite tight on control
						; 9715	;	store, we have chosen not to implement this.
						; 9716	;
						; 9717	;	If you are looking at this code for the first time, be aware that
						; 9718	;	only AR, ARX, SC, and FE are saved and restored here; thus BRX and
						; 9719	;	MQ are strictly off limits for this code.
						; 9720	;
U 3777, 3654,3203,7700,0000,0000,2110,0145	; 9721	3777:	CLR ACCOUNT EN,FORCE AR-ARX,J/PF1
U 1777, 3654,3203,7700,0000,0000,2110,0145	; 9722	1777:	CLR ACCOUNT EN,FORCE AR-ARX
U 3654, 3655,3701,4207,0000,2000,1010,0156	; 9723	PF1:	SV.AR_AR,AR_ARX,ARX_AR (AD)	;SAVE CURRENT AR
U 3655, 3656,3703,4207,0000,2000,1010,0157	; 9724		SV.ARX_AR,AR_ARX,ARX_AR (AD)	; AND ARX
						;;9725	.IF/PAGCNT				;[327] Page fault counting
						;;9726		AR_TRX2+1			;[327] Count this page fault
						;;9727		TRX2_AR
						;;9728		AR_SV.AR			;[346] Don't lose initial AR
						; 9729	.ENDIF/PAGCNT				;[327]
U 3656, 2514,4001,0000,0000,0000,2204,0400	; 9730		GET ECL EBUS,PF DISP,J/PF2	;PARITY ERROR?
						; 9731	=1100
						; 9732	PF2:
						; 9733	.IFNOT/PAGCNT				;[327]
U 2515, 3154,4001,0000,0301,0000,6410,0036	; 9734	=1101	FE_#,#/36,SKP RPW,J/PFPAR	;YES.  AR PARITY ERROR, CODE 36
						; 9735						;CHECK FOR MIDDLE OF RPW CYCLE
						;;9736	.IF/PAGCNT
						;;9737	=1101	AR_SV.AR,J/ARPAR		;[327] AR parity error. Get back; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 15-1
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 9738	.ENDIF/PAGCNT				;[327] saved AR
U 2516, 3154,0001,4000,0301,2000,6410,0037	; 9739		AR_ARX,FE_#,#/37,SKP RPW,J/PFPAR;[307] YES, ARX PARITY. COULD BE RPW
U 2517, 3657,4001,3000,0000,0062,2010,0567	; 9740		AR_EBUS REG,MB WAIT		;NO. GET PAGE FAIL WORD
U 3657, 2527,0001,0000,0000,0000,2204,0000	; 9741		REL ECL EBUS,PF DISP,J/PF4	;EBOX HANDLING REQUIRED?
						; 9742	;
						;;9743	.IF/PAGCNT				;[327]
						;;9744	ARPAR:	FE_#,#/36,SKP RPW,J/PFPAR	;Set code 36 and check for RPW
						; 9745	.ENDIF/PAGCNT
						; 9746	
						; 9747	;HERE ON ANY PARITY ERROR
						; 9748	;SKIP IF MIDDLE OF READ-PAUSE-WRITE CYCLE, IN WHICH CASE WE
						; 9749	; MUST WRITEBACK THE DATA TO PREVENT INCOMPLETE CYCLE
						; 9750	=0
U 3154, 3660,3731,0000,0000,0303,1710,0000	; 9751	PFPAR:	VMA_VMA HELD,J/PFPAR1		;MAY HAVE CHANGED AT MBWAIT
U 3155, 3660,0001,0000,0000,0016,0010,0000	; 9752		STORE				;WRITEBACK WITH GOOD PARITY
U 3660, 3661,0001,0000,0302,0007,0010,0140	; 9753	PFPAR1:	MAP,SC_#,#/140			;GET MAP INFO ON REF
U 3661, 3662,0001,0007,0000,0002,1010,0160	; 9754		SV.PAR_AR,MB WAIT		;[234]SAVE WORD WITH BAD PARITY
U 3662, 3663,4001,3000,0000,0060,2010,0567	; 9755		AR_EBUS REG			;READ MAP INFO
U 3663, 3664,4001,0000,7102,0000,2210,0000	; 9756		REL ECL EBUS,SC_P AND SC	;GET USER BIT FROM MAP WORD
U 3664, 2527,0001,0000,6000,3001,0010,0200	; 9757		P_FE OR SC,J/PF4		;STUFF IN PARITY ERROR CODE
						; 9758	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 16
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 9759	;HERE WITH PAGE FAIL WORD IN AR
						; 9760	; TESTING FOR EBOX HANDLING REQUIRED.
						; 9761	
						; 9762	=0111
U 2527, 0340,4001,0007,0000,0000,1005,0152	; 9763	PF4:	SV.PFW_AR,SR DISP,J/CLEAN	;NO, GO CLEAN UP
U 2537, 3665,3202,2047,0000,0000,1010,0152	; 9764		SV.PFW_AR,AR_BR,BR/AR		;YES, GET BR TOO
U 3665, 3666,3401,2007,0000,0000,1010,0150	; 9765		SV.BR_AR,AR_0S			;SAVE BR
U 3666, 3667,4001,0000,0000,2000,0022,0200	; 9766		AR0-8_FE			;NOW SAVE 10-BIT REGS
U 3667, 3670,3703,0200,2400,2000,0022,0200	; 9767		ARX_AR (AD),AR0-8_SC		;FE TO ARX, SC TO AR
						; 9768		ARR_ARL,ARL_ARX (ADX),		;FE IN ARL, SC IN ARR
U 3670, 3671,3733,4200,0000,3001,1710,0006	; 9769			ARX_VMA HELD		;GET VMA WHICH FAILED
						; 9770		SV.SC_AR,AR_ARX,		;HOLD SC & FE
U 3671, 2556,3201,4007,0000,2020,1032,0151	; 9771			GEN BR*2,SIGNS DISP	;TEST FOR PT DIR MATCH
						; 9772	=1110
						; 9773	PGRF1:	SV.VMA_AR,ARX_AR SWAP,ARR_ARL,	;GET SEC # TO AR32-35
U 2556, 2575,0001,4407,0000,3000,1036,0145	; 9774			DISP/EA MOD,J/PGRF2	; SEC < 20?
						; 9775	
						; 9776	;	HERE TO WRITE PT DIR, & CLR 4 PAGE ENTRIES.  If the expanded
						; 9777	;	page table ECO has been installed, this will only clear two entries
						; 9778	;	(since they go in pairs for that case), but in either case the
						; 9779	;	right thing will happen. [333]
						; 9780	;
						; 9781		AR_0S,COND/MBOX CTL,MBOX CTL/2,	;READY TO CLEAR EVEN PAIR
U 2557, 3672,3441,2000,7133,0000,2310,0002	; 9782			FE_P AND #,#/2,SC/SCAD	;GET WRITE REF BIT TO FE & SC
						; 9783		COND/MBOX CTL,MBOX CTL/33,	;CLR EVEN, WR DIR, SEL ODD
U 3672, 3673,4001,0000,2003,0020,2310,0033	; 9784			TIME/3T,FE_FE+SC,SC/SCAD; WR REF = 4 NOW
						; 9785		COND/MBOX CTL,MBOX CTL/10,	;CLR ODD, RESET NORMAL SELECT
U 3673, 3674,3202,2000,2001,0020,2310,0010	; 9786			TIME/3T,FE_FE+SC,AR_BR	;GET PFW BACK, WR REF = 10
U 3674, 3675,4001,0000,7322,0000,0010,0401	; 9787		SC_# AND AR0-8,#/401		;GET USER & PAGED REF BITS
U 3675, 3676,4001,0000,6000,2001,0010,0200	; 9788		AR0-8_FE OR SC			;COMBINE WITH WR REF BIT
U 3676, 2556,4001,4047,0000,2000,1010,0152	; 9789		SV.PFW_AR,BR/AR,AR_ARX,J/PGRF1	;REJOIN MAIN PATH
						; 9790	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 9791	;	HERE TO TRACE PAGE POINTERS FOR THIS ADDRESS
						; 9792	;	VMA WHICH FAILED IS IN ARX AND AR WITH THE HALVES SWAPPED
						; 9793	;	PAGE FAIL WORD IS IN BR
						; 9794	;	[333] Bit 5 of all access pointers is implemented as "Keep" for
						; 9795	;	pages which should not be swept from the page map on DATAO PAG
						; 9796	;	UBR reload unless bit 3 is also set.
						; 9797	;
						; 9798	
						; 9799	=1101
						; 9800	PGRF2:	VMA_#+AR32-35,#/540,		;YES.
U 2575, 2615,0001,0000,0000,0120,3332,0540	; 9801			SIGNS DISP,J/PGRF3	; USER REF?
						; 9802		VMA_#+AR32-35,#/560,		;NO
U 2577, 2615,4001,0000,0000,0120,3332,0560	; 9803			SIGNS DISP
						; 9804	=1101
U 2615, 3677,4001,0000,3401,0012,0026,0113	; 9805	PGRF3:	LOAD AR,EPT REF,FE_-1,J/SECPTR	;Initialize APWKC bits, get section
U 2617, 3677,4001,0000,3401,0012,0026,0223	; 9806		LOAD AR,UPT REF,FE_-1		; pointer from EPT or UPT
						; 9807	
						; 9808	;HERE TO FIND PAGE MAP WITH SECTION POINTER
						; 9809	
U 3677, 2630,3200,0003,0302,0022,7010,0011	; 9810	SECPTR:	AR_MEM,SC_#,#/9,SKP INTRPT	;GET SECTION POINTER
						; 9811	=1000	FE_FE AND AR0-8,BR/AR,		;COMBINE ACCESS BITS
U 2630, 2631,4001,0040,7021,1020,0007,0000	; 9812			AR0-3 DISP		;SPT INDEX IN ARR, DISP ON TYPE
U 2631, 3735,3200,2007,0000,0020,0010,0150	; 9813	=1001	AR_SV.BR,J/PFT			;NO ACCESS TO SECTION (OR INTRPT)
						; 9814	=1011
U 2633, 3707,0001,0400,0000,0001,0010,0020	; 9815	SECIMM:	ARX_SHIFT,ARL_0.M,J/PGRF5	;IMMEDIATE
U 2635, 3160,0001,0000,0000,0001,0010,0020	; 9816	=1101	ARL_0.M,J/LDIND			;SHARED
U 2637, 3700,0001,0000,0000,0001,0010,0020	; 9817	=1111	ARL_0.M				;INDIRECT SECTION POINTER
						; 9818	;
						; 9819	;	WARNING:  do not use the technique at LDIND to allow
						; 9820	;	interrupts out of section pointer loops, as that will have
						; 9821	;	adverse effects on byte transfers and console executes.
						; 9822	;
U 3700, 3701,0600,4007,0000,2320,0010,0143	; 9823		VMA_AR+SBR,AR_ARX		;LOOK IN SPT
U 3701, 3702,3242,2040,0000,0012,0026,0103	; 9824		BR/AR,AR_BR,LOAD AR,PHYS REF	;CALL FOR SPT ENTRY
U 3702, 3703,3240,0403,0000,0022,0010,0000	; 9825		ARX_SHIFT,AR_MEM		;SEC PTR INDEX TO ARX0-8
U 3703, 3704,3202,4200,0000,0000,0010,0000	; 9826		AR_SHIFT,ARX_BR			;NEW SEC PTR ADDR TO AR
						; 9827		GEN # AND AR0-8,#/77,SKP SCAD NE,
U 3704, 3156,3703,0000,7320,0320,5210,0077	; 9828			VMA_AR
U 3156, 3677,0001,0000,0000,0012,0026,0103	; 9829	=0	LOAD AR,PHYS REF,J/SECPTR
U 3157, 3735,3200,2007,0000,0020,0010,0150	; 9830		AR_SV.BR,J/PFT			;TRAP, SEC MAP NOT IN CORE
						; 9831	;
						; 9832	;	We must turn off special cycle for indirect pointers so that
						; 9833	;	we can take an interrupt.  However, we can't do it if PXCT or
						; 9834	;	SXCT might be active.  Thus, a kernel mode program can get into
						; 9835	;	a page fail loop that the microcode cannot exit.
						; 9836	;
						; 9837	=0
U 3160, 3705,0600,0007,4000,0320,0010,0143	; 9838	LDIND:	VMA_AR+SBR,J/LDIND1		;FOR INDIRECT PAGE POINTERS
U 3161, 3160,4001,0000,0000,0000,1510,0000	; 9839		CLR SPECIAL CYCLE,J/LDIND
						; 9840	;
						; 9841	;	This fixes the glitch that INSTR FETCH (NICOND) doesn't
						; 9842	;	clear 'CON4 INT DISABLE L' before fetching user's instruction.
						; 9843	;					;SHARED SEC = INDRCT PAG
U 3705, 3706,4001,0000,0000,0012,0026,0103	; 9844	LDIND1:	LOAD AR,PHYS REF		;GET PAGE MAP ADDR
U 3706, 2633,3200,0003,0000,0022,0010,0000	; 9845		AR_MEM,J/SECIMM
						; 9846	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17-1
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 9847	
						; 9848	;HERE WITH PAGE NO OF PAGE MAP IN AR,
						; 9849	; VIRTUAL PAGE NO WITHIN SECTION IN ARX0-8
						; 9850	
						; 9851	.IF/NOCST
U 3707, 3162,3200,0007,0000,0020,5610,0142	; 9852	PGRF5:	TEST CBR			;[247] CBR = 0 MEANS NO CST UPDATE
						; 9853	.IF/BIG.PT				;[346]
						; 9854	=0	AR_ARX,FE_FE AND #,#/174,	;[247][333] NO CST UPDATE
U 3162, 3714,4001,4000,7031,2000,0010,0174	; 9855			J/NO.CST		;[247]
						;;9856	.IFNOT/BIG.PT				;[346]
						;;9857	=0	AR_ARX,FE_FE AND #,#/164,	;[247] NO CST UPDATE. Eat bit 5
						;;9858			J/NO.CST		;[247] if no big page table
						; 9859	.ENDIF/BIG.PT				;[346]
						;;9860	.IFNOT/NOCST				;[247]
						;;9861	PGRF5:
						; 9862	.ENDIF/NOCST				;[247]
U 3163, 3710,0600,4007,4000,2320,0010,0142	; 9863		VMA_AR+CBR,AR_ARX		;GENERATE CST ADDRESS
						; 9864		GEN # AND AR0-8,#/77,		;IS PAGE MAP IN CORE?
U 3710, 3164,4001,0000,7320,0020,5210,0077	; 9865			SKP SCAD NE
						; 9866	=0	LOAD AR,PHYS REF,		;GET CST ENTRY FOR PAGE MAP
U 3164, 3711,0001,0040,0000,0012,0026,0103	; 9867			BR/AR,J/PGRF6		;SAVE PAGE PTR ADDR IN BR
U 3165, 3735,3200,2007,0000,0020,0010,0150	; 9868		AR_SV.BR,J/PFT			;NOT IN CORE, PAGE FAIL TRAP
						; 9869	;
						; 9870	.IF/BIG.PT				;[346]
U 3711, 3712,3240,0003,7031,0022,0010,0174	; 9871	PGRF6:	AR_MEM,FE_FE AND #,#/174	;[333]HERE IF CST FOR PAGE MAP
						;;9872	.IFNOT/BIG.PT				;[346]
						;;9873	PGRF6:	AR_MEM,FE_FE AND #,#/164	;HERE IF CST FOR PAGE MAP. No K bit
						; 9874	.ENDIF/BIG.PT				;[346] if no big page table
U 3712, 3166,3600,2007,0100,0040,5210,0140	; 9875		AR_AR AND CSMSK,SKP P NE	;BEGIN CST UPDATE
U 3166, 3735,3200,2007,0000,0020,0010,0150	; 9876	=0	AR_SV.BR,J/PFT			;AGE TRAP, MAP BEING SWAPPED
U 3167, 3713,3300,2007,4000,0036,0010,0141	; 9877		AR_AR OR PUR,STORE		;PUT CST WORD BACK
						;;9878	.IFNOT/NOCST				;[247]
						;;9879		MEM_AR,VMA_BR,PHYS REF		;PHYS REF MAKES MODEL.A LOAD
						;;9880						;LONG VMA
						;;9881		LOAD AR,PHYS REF		;GET PAGE MAP ENTRY
						;;9882		AR_MEM,FE_FE OR #,#/100,	;PAGE POINTER
						;;9883			SKP INTRPT		;CHECK FOR LONG INDIRECT
						;;9884						;POINTER
						; 9885	.IF/NOCST				;[247]
U 3713, 3170,3202,0003,0000,0302,0026,0103	; 9886		MEM_AR,VMA_BR,PHYS REF,J/NOCST0	;PHYS REF MAKES MODEL.A LOAD
						; 9887						;LONG VMA
						; 9888	NO.CST:	GEN # AND AR0-8, #/77,		;[247] page map in core ?
U 3714, 3170,3701,0000,7320,0320,5210,0077	; 9889			VMA_AR,SKP SCAD NE	;[247]
						; 9890	=0
U 3170, 3715,0001,0000,0000,0012,0026,0103	; 9891	NOCST0:	LOAD AR,PHYS REF, J/NOCST1	;[247] GET PAGE MAP ENTRY
U 3171, 3735,3200,2007,0000,0020,0010,0150	; 9892		AR_SV.BR, J/PFT			;[247] not in core, pf trap
						; 9893	NOCST1:	AR_MEM,FE_FE OR #,#/100,	;[247] PAGE POINTER
U 3715, 2650,3240,0003,6031,0022,7010,0100	; 9894			SKP INTRPT		;[247] CHECK FOR LONG INDIRECT
						; 9895	.ENDIF/NOCST				;[247] POINTER
						; 9896	;
						; 9897	;	HERE WITH PAGE MAP ENTRY IN AR
						; 9898	;	FE HAS ACCUMULATED ACCESS BITS -- APWKC*4
						; 9899	;	SC CONTAINS 9.
						; 9900	;
						; 9901	=1000	FE_FE AND AR0-8,AR0-3 DISP,	;COMBINE PWKC, DISP ON TYPE
U 2650, 2651,3200,0207,7021,1020,0007,0145	; 9902			ARX_SV.VMA,TIME/3T	;[346] GET BACK SAVED VMA; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17-2
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

U 2651, 3735,3200,2007,0000,0020,0010,0150	; 9903	=1001	AR_SV.BR,J/PFT			;0=NO ACCESS (OR HERE ON INTRPT)
						; 9904	=1011
						; 9905	LDIMM:	ARL_SHIFT,FE_FE SHRT,		;1=IMMEDIATE, LOAD PT
						; 9906			ARX_ARX*2 COMP,		; GET -WR REF TO ARX03
U 2653, 3721,2001,0500,0302,0001,1210,0004	; 9907			SC_#,#/4,J/LDPT		;[333] Set to move K bit
U 2655, 3716,4001,0000,0000,0001,0010,0020	; 9908	=1101	ARL_0.M,J/LDSHR			;2=SHARED, GET SPT ENTRY
						; 9909	=1111	ARL_0.M,ARX_SHIFT,SKP USER,	;3=INDIRECT, LOOP
U 2657, 3160,0001,0400,0000,0001,6210,0020	; 9910			J/LDIND
						; 9911	;
						; 9912	;	HERE TO GET SHARED PAGE POINTER OUT OF SPT
						; 9913	;
U 3716, 3717,0600,0007,4000,0320,0010,0143	; 9914	LDSHR:	VMA_AR+SBR			;ADDRESS OF SPT ENTRY
U 3717, 3720,0001,0000,0000,0012,0026,0103	; 9915		LOAD AR,PHYS REF
U 3720, 2653,3200,0003,6031,0022,0010,0100	; 9916		AR_MEM,FE_FE OR #,#/100,J/LDIMM	;TREAT SPT ENTRY AS IMMED
						; 9917	
						; 9918	;
						; 9919	;	HERE WITH IMMEDIATE PAGE NO IN AR TO LOAD INTO PT
						; 9920	;
						; 9921	
U 3721, 3172,0001,0000,7320,0020,5210,0077	; 9922	LDPT:	GEN # AND AR0-8,#/77,SKP SCAD NE;Test storage medium
						; 9923	.IF/BIG.PT				;[333]
						; 9924	=0	ARL_0.M,GEN FE AND SC,		;[333]In core. Is Keep bit set?
U 3172, 3174,4001,0000,7000,0021,5210,0020	; 9925			SKP SCAD NE,J/LDPT1
						;;9926	.IFNOT/BIG.PT
						;;9927	=0	ARL_0S,J/LDPT1
						; 9928	.ENDIF/BIG.PT				;[333]
U 3173, 3735,3200,2007,0000,0020,0010,0150	; 9929		AR_SV.BR,J/PFT			;PAGE NOT IN CORE
						; 9930	;
						; 9931	.IF/BIG.PT				;[333]
						; 9932	=0
						; 9933	.ENDIF/BIG.PT
U 3174, 3722,3711,7400,4402,1000,0010,0000	; 9934	LDPT1:	ARX_AR,AR_ARX*.25,SC_1,J/KMOVED	;[333]No K. GET -WR REF TO AR05
						; 9935	.IF/BIG.PT
U 3175, 3174,4001,0000,5001,0000,0110,0010	; 9936		AR0-8_#,#/10,FE_FE-SC,J/LDPT1	;[333]K set. Move to bit 5 for now
						; 9937	.ENDIF/BIG.PT
						; 9938	;
						; 9939	.IF/NOCST				;[247]
U 3722, 3200,3200,0007,7102,0020,5610,0142	; 9940	KMOVED:	TEST CBR, SC_P AND SC		;[247][333]CBR = 0? (-WR REF TO SC)
						; 9941	=0	SC_-SC,AR_ARX,ARX_AR (AD),
U 3200, 3725,3703,4200,5402,2000,0010,0000	; 9942			J/NOUPDT		;[247] YES, SKIP SOME
						;;9943	.IFNOT/NOCST				;[333]
						;;9944	KMOVED:					;[333]
						; 9945	.ENDIF/NOCST				;[247]
U 3201, 3723,0610,4007,7102,2320,0010,0542	; 9946		VMA_ARX+CBR,AR_ARX,SC_P AND SC	;PAGE IN CORE. SC_-WR REF
						; 9947	;
						; 9948	;	NOW GET CST ENTRY FOR THIS PAGE.
						; 9949	;
						; 9950	GTCST:	LOAD AR,PHYS REF,ARX_AR SWAP,	;Shuffle K over to bit 23
U 3723, 3724,4041,2400,5402,3032,0026,0103	; 9951			SC_-SC,AR_1		;SC=0 IF WR REF, ELSE -1
						; 9952		GEN FE AND #,#/10,SKP SCAD NE,	;SKIP IF WRITABLE
U 3724, 3202,4001,0040,7030,0022,5210,0010	; 9953			BR/AR,MB WAIT		;GET CST ENTRY & BIT FOR TESTING
						; 9954	=0	GEN P AND SC,SKP SCAD NE,	;FAIL IF WRITING OR AGE=0
U 3202, 3206,3600,2007,7100,2040,5210,0140	; 9955			AR_AR AND CSMSK,J/NOTWR	;STRIP OLD AGE FROM CST
						; 9956	;
						; 9957	;	[303] Looks like it's writable.  Make one final check by looking
						; 9958	;	at bit 18 of the CST entry, and abort if it's not set.; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 17-3
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 9959	;
						; 9960		TEST AR.BR,SKP CRY0,		;POSSIBLY WRITABLE--SKIP IF CST
U 3203, 2776,3602,0004,5300,0040,5434,0777	; 9961			GEN #-SC,#/-1,BYTE DISP	; WRITTEN OR THIS IS WRITE REF
U 2776, 3205,0001,0000,5031,0000,0010,0004	; 9962	=110	FE_FE-#,#/4,J/STCST		;[305] TEMPORARILY UNWRITABLE, SET S
						; 9963	.IF/CST.WRITE				;[314]
						; 9964		AR_AR*BR,AD/OR,FE_FE OR #,#/4,	;SET CST WRITTEN AND SOFT BITS
U 2777, 3204,3302,2000,6031,0000,4410,0004	; 9965			SKP AR18		;IS IT REALLY WRITABLE? [303]
U 3204, 3735,3200,2007,0000,0020,0010,0150	; 9966	=0	AR_SV.BR,J/PFT			;NOT REALLY. BAIL OUT
						;;9967	.IFNOT/CST.WRITE			;[314]
						;;9968		AR_AR*BR,AD/OR,FE_FE OR #,#/4	;[314] Set CST written and soft bits
						; 9969	.ENDIF/CST.WRITE			;[314]
U 3205, 3206,3600,2007,0100,0040,5210,0140	; 9970	STCST:	AR_AR AND CSMSK,SKP P NE	;[305] WRITABLE. CLEAR, TEST OLD AGE
						; 9971	;
						; 9972	;	[322] At this point we should check whether we got here as a result
						; 9973	;	of an age trap (in which case we just take the page failure) or not,
						; 9974	;	in which case the failure was due to a write reference and we should
						; 9975	;	set bit 2 (the A bit) in the PFW.  This is not currently done
						; 9976	;	because (1) nobody needs it now, and (2) we are very short on CRAM
						; 9977	;	space.
						; 9978	;
						; 9979	=0
U 3206, 3735,3200,2007,0000,0020,0010,0150	; 9980	NOTWR:	AR_SV.BR,J/PFT			;WRITE OR AGE TRAP
						;;9981	.IFNOT/NOCST				;[247]
						;;9982		AR_AR OR PUR,STORE		;SET USE BITS, STORE BACK CST
						;;9983		MB WAIT,VMA_SV.VMA,		;RELOAD VMA FOR ORIGINAL REF
						;;9984			AR_SV.VMA,SC_1		;READY TO TEST VMA USER
						;;9985		GEN P AND SC,SKP SCAD NE,	;TEST VMA USER, copy page # to ARL,
						;;9986			AR_ARX,			;[333] K to AR bit 23, APMWC0 TO SC
						;;9987			SC_FE,ARX_AR (AD)	; MAP BIT TO ARX
						;;9988	=0	P_P OR SC#,EXEC REF,J/WRHPT	;BUILD PT ENTRY, CLEAR VMA USER
						;;9989		P_P OR SC#,USER REF		; OR SET USER, AS NECESSARY
						;;9990	WRHPT:	WR PT ENTRY,FE_#,#/10,AR_ARX*4	;UPDATE HARDWARE TABLE
						; 9991	.IF/NOCST
U 3207, 3213,3300,2007,4000,0036,0010,0141	; 9992		AR_AR OR PUR,STORE,J/WRHPT	;[247]SET USE BITS,
						; 9993						; STORE BACK CST
						; 9994	;
						; 9995	NOUPDT:	GEN FE AND #, #/10, ARX_AR SWAP,
U 3725, 3210,3713,2400,7030,3020,5210,0010	; 9996			AR_ARX (AD),SKP SCAD NE	;[247] SKIP IF WRITABLE
						; 9997	=0	AR_SV.PFW,GEN P AND SC,		;[322] Get saved PFW and
U 3210, 3212,3200,2007,7100,0040,5210,0152	; 9998			SKP SCAD NE,J/NOT.WR	;[247]FAIL IF WRITING
U 3211, 3213,4001,0000,6031,0000,0010,0004	; 9999		FE_FE OR #, #/4, J/WRHPT	;[247]SET WRITABLE BIT
						; 10000	;
U 3726, 3206,0001,0007,0000,0000,1010,0152	; 10001	WRFAIL:	SV.PFW_AR,J/NOTWR		;[322]Restore PFW and page fail
						; 10002	;
						; 10003	=0					;[323]
U 3212, 3726,4001,0000,6320,2000,0110,0100	; 10004	NOT.WR:	AR0-8_# OR AR0-8,#/100,J/WRFAIL	;[322]Write failure. Set A in PFW
U 3213, 3727,3240,4007,0000,2123,0013,0145	; 10005	WRHPT:	RSTR VMA_SV.VMA,AR_ARX,SC_FE	;RELOAD ORIGINAL VMA
U 3727, 3730,0001,0000,6100,3000,0110,0000	; 10006		P_P OR SC			;[333]COMBINE APMWC WITH PAGE #, K
U 3730, 3731,0001,0000,0000,0020,2310,0010	; 10007		WR PT ENTRY			;UPDATE HARDWARE PAGE TABLE
						; 10008	.ENDIF/NOCST
						; 10009	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 18
; IO.MIC[4,24]	15:27 17-Mar-86				KL-MODE PAGE REFILL LOGIC				

						; 10010	;HERE WHEN MAP INFO WRITTEN INTO HARDWARE PAGE TABLE
						; 10011	; WE NOW NEED ONLY RESTORE THE REGISTERS WE HAVE USED, AND RESTART THE
						; 10012	; MEMORY REFERENCE WHICH FAILED, RETURNING TO THE MICROINSTRUCTION
						; 10013	; WHICH WAITS FOR ITS COMPLETION.
						; 10014	
U 3731, 3732,3200,2007,0000,0020,0010,0151	; 10015		AR_SV.SC
						; 10016		SC_EXP,FE_EXP,SKP AR0,		;RESTORE FE
U 3732, 3214,3240,4207,0203,3020,4510,0150	; 10017			AR_AR SWAP,ARX_SV.BR
						; 10018	=0
						; 10019	PGRST1:	SC_EXP,SKP AR0,			;RESTORE SC
U 3214, 3216,3200,4207,0202,2020,4510,0156	; 10020			AR_ARX,ARX_SV.AR,J/PGRST2
U 3215, 3214,4001,0000,1401,0000,0010,0000	; 10021		FE_-SC-1,J/PGRST1		;MAKE FE NEG
						; 10022	=0
						; 10023	PGRST2:	BR/AR,AR_ARX,			;RESTORE BR AND AR
U 3216, 3733,3200,4247,0000,2020,0010,0157	; 10024			ARX_SV.ARX,J/PGRST3	; AND ARX
						; 10025		SC_-SC-1,BR/AR,AR_ARX,		;NEGATE SC
U 3217, 3733,3240,4247,1402,2020,0010,0157	; 10026			ARX_SV.ARX
						; 10027	
						; 10028	;HERE RETURN TO POINT OF FAULT.  THERE MUST BE EXACTLY ONE MICRO-
						; 10029	; INSTRUCTION, OF 2 OR 3 TICKS, BETWEEN THE REQUEST AND THE RETURN.
						; 10030	; AT LEAST ONE IS REQUIRED TO GET NICOND LOGIC SET UP CORRECTLY IN
						; 10031	; CASE THIS IS A FETCH, BUT THERE MUST NOT BE TIME FOR A READ TO
						; 10032	; READ REFERENCE TO COMPLETE, BECAUSE THE MB WAIT INSTRUCTION TO WHICH
						; 10033	; WE RETURN MAY EXPECT TO GET SOMETHING OUT OF AR OR ARX BEFORE THE
						; 10034	; MBOX RESPONSE.  SEE DPB1.
						; 10035	
U 3733, 3734,3240,0007,0000,0130,0010,0145	; 10036	PGRST3:	REQ SV.VMA			;RESTART FAULTED REQUEST
U 3734, 0000,4001,0000,0000,0000,2103,0105	; 10037		SET ACCOUNT EN,RETURN0		;RETURN TO POINT OF FAILURE
						; 10038	
						; 10039	
						; 10040	;HERE ON A TRAP CONDITION DETECTED BY REFILL LOGIC
						; 10041	;AR CONTAINS SAVED BR
						; 10042	
						; 10043	PFT:	BR/AR,VMA_SV.VMA,		;RESTORE BR & VMA
U 3735, 0340,3200,0047,0000,0320,0005,0145	; 10044			SR DISP,J/CLEAN		;TAKE TRAP
						; 10045	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 19
; IO.MIC[4,24]	15:27 17-Mar-86				Page Fail Cleanup and Special Instruction Dispatch	

						; 10046	.TOC	"Page Fail Cleanup and Special Instruction Dispatch"
						; 10047	
						; 10048	;HERE ON PAGE FAIL OR INTERRUPT WHICH REQUIRES CLEANUP IN ORDER
						; 10049	; TO BE CORRECTLY RESTARTABLE AFTER SERVICE...
						; 10050	
						; 10051	=1*0000
U 0340, 3220,0001,0000,0301,0000,7010,0037	; 10052	CLEAN:	FE_#,#/37,SKP INTRPT,J/PGF1	;HERE FOR INTRPT OR PGF?
						; 10053	
						; 10054	;(1) HERE ON EDIT SOURCE FAIL
						; 10055	
U 0341, 3753,3200,2001,0000,0020,1610,0000	; 10056		AR_SRCP,SR_0,J/BACKS		;BACK UP SRC POINTER
						; 10057	
						; 10058	;(2) HERE ON ANY FAILURE IN DECIMAL TO BINARY
						; 10059	
U 0342, 3760,3202,2600,0000,0000,1610,0001	; 10060		AR_BR LONG,SR_1,J/D2BPF		;GET ACCUMULATED BINARY
						; 10061	
						; 10062	;(3) HERE ON DST FAIL IN BINARY TO DECIMAL FILL
						; 10063	
U 0343, 3764,2540,2005,0000,0020,1610,0004	; 10064		AR_DLEN COMP,SR_#,#/4,J/B2DFPF
						; 10065	
						; 10066	;(4) HERE ON EDIT DST FAIL WITH NO SRC POINTER UPDATE
						; 10067	
U 0344, 3755,4001,0000,0000,0000,1610,0000	; 10068		SR_0,J/BACKD			;BACK UP DST POINTER ONLY
						; 10069	
						; 10070	;(5) HERE ON EDIT DST FAIL AFTER UPDATING SRC POINTER
						; 10071	
U 0345, 3755,0001,0000,0000,0000,1610,0001	; 10072		SR_1,J/BACKD			;BACK UP DST, THEN SRC
						; 10073	
						; 10074	;(6) HERE ON DESTINATION FAILURE IN BINARY TO DECIMAL
						; 10075	
U 0346, 3755,0001,0000,0000,0000,1610,0010	; 10076		SR_BDT,J/BACKD			;BACK UP DST, THEN SAVE FRACTION
						; 10077	
						; 10078	;(7) HERE ON BLT FAILURE
						; 10079	
U 0347, 2412,2301,0200,0000,0000,1610,0000	; 10080	BLTFIX:	ARX_1S,SR_0,J/BLTPGF		;Only way to subtract 1 from BRX
						; 10081	
						; 10082	;(10) HERE ON TRANSLATION FAILURE IN BINARY TO DECIMAL
						; 10083	
U 0350, 3765,3242,2600,0000,0000,1610,0000	; 10084		AR_BR LONG,SR_0,J/B2DPF		;GET BINARY FRACTION
						; 10085	
						; 10086	;(11) HERE ON SRC FAILURE IN COMPARE OR MOVE STRING
						; 10087	
U 0351, 3771,3200,2005,0000,0020,1610,0001	; 10088		AR_DLEN,SR_1,J/STRPF		;PUT LENGTHS BACK, THEN BACK SRC
						; 10089	
						; 10090	;(12) HERE ON DST FAILURE IN COMPARE OR MOVE STRING
						; 10091	
U 0352, 3771,3200,2005,0000,0020,1610,0004	; 10092		AR_DLEN,SR_#,#/4,J/STRPF
						; 10093	
						; 10094	;(13) HERE ON DST FAILURE AFTER UPDATING SRC IN COMPARE OR MOVE
						; 10095	
U 0353, 3755,0001,0000,0000,0000,1610,0111	; 10096		SR_SRC,J/BACKD			;BACK DST, THEN HANDLE AS SRC FAIL
						; 10097	
						; 10098	;(14) HERE ON DST FILL FAILURE IN MOVRJ
						; 10099	
U 0354, 3776,3240,2005,0000,0020,1610,0004	; 10100		AR_DLEN,SR_#,#/4,J/STRPF4
						; 10101	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 19-1
; IO.MIC[4,24]	15:27 17-Mar-86				Page Fail Cleanup and Special Instruction Dispatch	

						; 10102	;(15) HERE ON PAGE FAILURE IN MAP INSTRUCTION.  RETURN PAGE FAIL WORD
						; 10103	
U 0355, 0744,3200,2007,0000,0020,7310,0152	; 10104		AR_SV.PFW,SKP IO LEGAL,J/MAP2	;RETURN PFW IN AC
						; 10105	
						; 10106	;(16) HERE ON PAGE FAIL IN XBLT
						; 10107	
U 0356, 2452,3240,0007,0000,0020,5510,0166	; 10108	XBLFIX:	GEN FM[T0],SKP AD0,J/XBLFRZ	;Test copy direction for backup
						; 10109	=
						; 10110	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 20
; IO.MIC[4,24]	15:27 17-Mar-86				Page Fail Cleanup and Special Instruction Dispatch	

						; 10111	;HERE ON ANY PAGE FAILURE OR PI REQUEST IN LONG INSTRUCTIONS
						; 10112	; SKIP IF PI REQUEST, WHICH TAKES PRIORITY
						; 10113	
						; 10114	=0
U 3220, 3736,0001,0000,0000,0102,3010,0500	; 10115	PGF1:	MB WAIT,VMA_#,#/500,J/PGF2
U 3221, 0361,4001,0000,0000,0000,2110,0105	; 10116		SET ACCOUNT EN,J/TAKINT		;CLEANUP DONE, SERVE INTRPT
U 3736, 1550,3240,2007,0000,0020,6510,0152	; 10117	PGF2:	AR_SV.PFW,SKP PI CYCLE		;GET BACK PAGE FAIL WORD
						; 10118	=00	ARX_AR,AR_VMA HELD,SC_#,#/13.,	;READY TO COMBINE PF WORD
U 1550, 0044,3731,2400,0302,1000,1750,0015	; 10119			CALL,J/ROTS		; WITH ADDRESS
						; 10120		ARX_AR,AR_VMA HELD,SC_#,#/13.,	;READY TO COMBINE PF WORD
U 1551, 3747,3731,2400,0302,1000,1710,0015	; 10121			J/IOPGF			; WITH ADDRESS
U 1553, 3737,0001,4000,0000,0000,1510,0004	; 10122	=11	AR_SHIFT,ABORT INSTR		;RECOVER TRAP FLAGS, IF ANY
U 3737, 3740,3731,0200,0000,0016,0026,0223	; 10123		STORE,UPT REF,ARX_PC		;PAGE FAULT WORD TO 500
U 3740, 3741,4001,0007,0000,0000,1010,0177	; 10124		FM[HARDPFW]_AR			;[406] Protect from soft page fail
						; 10125		AR_ARX ANDC ADMSK,MB WAIT,	;GET PC FLAGS FOR STORING
U 3741, 3742,3510,2227,0000,0022,0010,0175	; 10126			BRX/ARX,ARX/AD		;FULL PC IN BRX, FLAGS IN ARX
U 3742, 3222,0001,0000,0000,0000,6222,0030	; 10127		AR_0.S,SKP USER
U 3222, 0475,0001,0000,0000,2000,0250,0000	; 10128	=0	AR12-17_PREV SEC,CALL [ARSWAP]	;[334] GET PCS IF EXEC MODE
						; 10129		ARL_ARXL,ARR_ARR,		;FLAGS WORD IN AR
U 3223, 3743,4001,0000,0000,2016,3622,0004	; 10130			VMA_VMA+1,STORE		;STORE FLAGS WORD IN 501
						; 10131		AR_ARX*BRX,AD/ANDCA,		;GET PC ADDRESS
U 3743, 3744,3002,6004,0000,0016,3610,0000	; 10132			VMA_VMA+1,STORE		; STORE IT IN 502
U 3744, 3745,3441,2000,0000,0012,3610,0000	; 10133		AR_0S,VMA_VMA+1,LOAD AR		;GET NEW PC ADDRESS FROM 503
U 3745, 3746,4001,0000,0000,0000,0024,0020	; 10134		SET FLAGS_AR			;CLEAR ALL FLAGS
U 3746, 0335,3200,0003,0000,0022,2110,0105	; 10135		AR_MEM,SET ACCOUNT EN,J/ARJMP	;NEW ADDRESS FOR PC
						; 10136	; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page 21
; IO.MIC[4,24]	15:27 17-Mar-86				PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS	

						; 10137	.TOC	"PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS"
						; 10138	
						; 10139	;HERE ON PAGE FAIL DURING PI CYCLE
						; 10140	
						; 10141						;[224] DO FANCY STUFF FOR DTE.
U 3747, 3750,3200,2007,0000,0020,0010,0163	; 10142	IOPGF:	AR_FM[SV.IOP]			;[274] GET THE SAVED API WORD
U 3750, 3751,4001,0007,0000,0000,1010,0162	; 10143		SV.IOPF_AR			;[224] SAVE IT IN AC BLK 7.
U 3751, 3752,3200,2047,0000,0020,0010,0156	; 10144		BR/AR,AR_SV.AR			;[224] KEEP IOPF WORD AROUND,
U 3752, 1242,4001,0000,0000,0000,2310,0100	; 10145		SET IO PF			;[224][274] HANDLE DTE.
						; 10146	
						; 10147	;[223] THIS RESTARTS THE DTE'S CLOCK TO PREVENT A DEX FAILURE.
U 1242, 3560,4001,0000,0000,0000,2250,0026	; 10148	=10	SET DATAO,CALL,J/EBUSO		;[223] SEND THE DATA TO THE DTE.
U 1243, 2423,3242,2000,0000,0000,2110,0105	; 10149		AR_BR,SET ACCOUNT EN,J/PIDONE	;[223] AND FINISH THE INTRPT.
						; 10150	
						; 10151	;
						; 10152	;HERE ON VARIOUS CASES OF STRING/EDIT FAILURE
						; 10153	
U 3753, 3754,0001,0000,2110,3001,0010,0200	; 10154	BACKS:	P_P+S
U 3754, 0340,4001,0001,0000,0000,1005,0000	; 10155		SRCP_AR,SR DISP,J/CLEAN		;RE-DISPATCH FOR MORE
						; 10156	
U 3755, 3756,3240,2006,0000,0020,0010,0144	; 10157	BACKD:	AR_DSTP
U 3756, 3757,0001,0000,2110,3000,0110,0144	; 10158		P_P+S.C,SEL DSTP		;PRESEL NUM FIELD FOR HARDW GLITCH
U 3757, 0340,4001,0006,0000,0000,1005,0144	; 10159		DSTP_AR,SR DISP,J/CLEAN
						; 10160	
U 3760, 3761,3401,2005,0302,0000,1010,0043	; 10161	D2BPF:	AC3_AR,AR_0S,SC_#,#/35.		;PUT AWAY HIGH BINARY
U 3761, 3762,5140,4207,0000,0020,0010,0170	; 10162		AR_SHIFT,ARX_-SLEN		;LOW TO AR, REMAINING LEN TO ARX
U 3762, 3763,4001,0000,0000,0000,0010,0144	; 10163		SEL DSTP			;PRESEL NUM FIELD FOR HARDW GLITCH
U 3763, 3774,0001,4006,0000,2000,1010,0144	; 10164		AC4_AR,AR_ARX,J/STRPF2		;PUT LOW AWAY
						; 10165	
U 3764, 3770,3300,2007,0000,0020,0010,0166	; 10166	B2DFPF:	AR_AR*T0,AD/OR,J/B2DPF2
						; 10167	
U 3765, 3766,4001,4000,0000,2000,1010,0000	; 10168	B2DPF:	AC0_AR,AR_ARX			;HIGH FRACTION TO AC
U 3766, 3767,4001,0001,0000,0000,1010,0000	; 10169		AC1_AR				;LOW TO AC1
U 3767, 3770,3240,2005,0000,0020,0010,0000	; 10170		AR_AC3				;GET FLAGS
U 3770, 3773,5100,2007,0000,0020,0010,0170	; 10171	B2DPF2:	AR_AR*SLEN,AD/A-B,J/STRPF3	;REBUILD FLAGS+LEN
						; 10172	
U 3771, 3224,5140,2047,0000,0020,4510,0170	; 10173	STRPF:	BR/AR,AR_-SLEN,SKP AR0		;WHICH IS LONGER?
						; 10174	=0
U 3224, 3774,0602,2005,4000,0020,1010,0000	; 10175	STRPF1:	AC3_AR,AR_AR+BR,J/STRPF2	;SRC LONGER
U 3225, 3772,3300,2400,4000,1020,0010,0000	; 10176		ARX_AR,AR_AR*SFLGS,AD/OR	;DST.  BUILD SRC LEN+FLAGS
U 3772, 3773,5112,2000,0000,0020,1010,0000	; 10177		AC0_AR,AR_ARX-BR		;THAT'S AWAY, MAKE DST LEN
U 3773, 0340,0001,0005,0000,0000,1005,0000	; 10178	STRPF3:	AC3_AR,SR DISP,J/CLEAN		;OK, NOW BACK UP SRC IF REQ'D
						; 10179	
U 3774, 3775,3300,2000,0000,0020,0010,0000	; 10180	STRPF2:	AR_AR*SFLGS,AD/OR
U 3775, 0340,0001,0000,0000,0000,1005,0000	; 10181	PGFAC0:	AC0_AR,SR DISP,J/CLEAN		;BEGIN NORMAL PF WORK
						; 10182	
U 3776, 3224,2540,2047,0000,0020,4510,0170	; 10183	STRPF4:	BR/AR,AR_SLEN COMP,SKP AR0,J/STRPF1
						; 10184	


; Number of microwords used: 
;	D words= 512
;	U words= 2048, Highest= 2047

	END
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-1
; 							Cross Reference Listing					

(D) A				2259 #
	ADDR			2262 #	4088	4255	4618	4792	4794	4867	4868	4916	4918	4920	4921
				4922	4923	4937	4938	4939	4940	4941	4942	4946	4947	4948	4949
				4950	4951	4952	4953	4954	4955	4959	4960	4961	4962	4963	4964
				4965	4966	4967	4968	4969	4970	4971	4972	4973	4974	4975	4976
				4977	4978	4982	4983	4984	5115	5761	5775	5840	5884	6011	6012
				6899	6900	6907	6908	6909	7130	8117	8118	8119	8121	8122	8123
				8124	8126	8127	8128	8129	8131	8132	8133	8134
	IMMED			2260 #	3949	4032	4033	4060	4141	4231	4232	4271	4272	4316	4317
				4367	4368	4379	4380	4381	4382	4383	4384	4397	4398	4399	4400
				4401	4402	4415	4416	4417	4418	4419	4420	4433	4434	4435	4436
				4437	4438	4487	4488	4489	4490	4491	4492	4493	4563	4564	4565
				4566	4567	4568	4569	4578	4579	4580	4581	4582	4583	4584	4594
				4595	4596	4597	4598	4599	4600	4609	4610	4749	4767	4795	4869
				4870	5116	5155	5156	5157	5158	5159	5160	5161	5369	5407	5473
				5478	5767	5781	5846	5890	6014	9027	9029	9030	9031	9032	9040
				9041	9042	9043	9049	9051	9052	9053	9054	9058	9059	9060	9061
				9062	9063	9064	9065	9075	9076	9077	9078	9085	9086	9087	9088
				9089	9090	9098	9100	9101	9109	9111	9112
	IMMED-PF		2261 #	3948	3953	3958	3963	4093	4098	4103	4108	4113	4118	4123
				4130	4135	4140	4145	4150	4155	4160	4165	4229	4230	4235	4245
				4260	4269	4270	4275	4285	4295	4305	4314	4315	4325	4336	4346
				4356	4365	4366	4377	4378	4395	4396	4413	4414	4431	4432	4486
				4562	4577	4593	5344	5356
	RD-P-WR			2267 #	3955	3960	3965	3967	4089	4094	4095	4100	4105	4110	4115
				4120	4125	4131	4136	4137	4142	4147	4152	4157	4162	4167	4236
				4237	4246	4247	4261	4262	4276	4277	4286	4287	4296	4297	4306
				4307	4326	4327	4337	4338	4347	4348	4357	4358	4528	4529	4530
				4531	4532	4533	4534	4535	4544	4545	4546	4547	4548	4549	4550
				4551	5345	5346	5357	5358
	RD-WR			2266 #	3950	4090	4132	4257	5370	5371	5408	5409	5474	5475	5479
				5480	5763	5764	5768	5769	5777	5778	5782	5783	5842	5843	5847
				5848	5886	5887	5891	5892	7368	7461	9047	9094	9096	9105	9107
	READ			2264 #	4014	4015	4388	4389	4390	4391	4392	4393	4406	4407	4408
				4409	4410	4411	4424	4425	4426	4427	4428	4429	4442	4443	4444
				4445	4446	4447	4497	4498	4499	4500	4501	4502	4503	4513	4514
				4515	4516	4517	4518	4519	4520	4793	5141	5368	5406	5472	5477
				5637	5638	5639	5640	5757	5766	5771	5780	5836	5845	5880	5889
				6058	6059	6060	6245	6246	6247	6248	6423	6424	6425	6426	7369
				7462	7569	8042	9028	9038	9039	9050	9073	9074	9097	9108
	READ-PF			2265 #	3947	3952	3957	3962	4087	4092	4097	4102	4107	4112	4117
				4122	4129	4134	4139	4144	4149	4154	4159	4164	4234	4244	4254
				4259	4274	4284	4294	4304	4324	4335	4345	4355	4386	4387	4404
				4405	4422	4423	4440	4441	4496	5343	5355
	WR-TST			2263 #	3954	3959	3964	4061	4099	4104	4109	4114	4119	4124	4146
				4151	4156	4161	4166	4256	9025	9026	9036	9037	9048	9071	9072
				9083	9084	9095	9099	9106	9110
(U) AC#				2010 #	3740	3798	4656	4692	4808	4878	4883	5050	5089	5093	5097
				5101	5701	5717	5719	5739	6432	6437	6439	6445	6448	6452	6457
				6461	6465	6466	6467	6472	6473	6474	6476	6477	6503	6505	6507
				6508	6517	6519	6520	6523	6542	6549	6550	6559	6585	6598	6599
				6615	6619	6659	6660	6662	6666	6668	6693	6732	6739	6869	6870
				6876	6877	6912	6913	6920	6921	6922	6969	6976	7054	7056	7067
				7069	7088	7091	7097	7268	7604	7625	7650	7677	7751	8056	8099
				8102	8104	8107	8149	8186	8187	8188	8189	8190	8191	8192	8227
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-2
; 							Cross Reference Listing					

				8229	8230	8243	8257	8261	8267	8269	8278	8283	8301	8325	8344
				8391	8400	8415	8415	8437	8438	8451	8454	8459	8465	8485	8486
				8503	8538	8540	8549	8550	8559	8562	8564	8565	8576	8577	8611
				8613	8627	8628	8675	8678	8679	8680	8681	8689	8714	8716	8721
				8722	8726	8728	8729	8784	8784	8794	8796	8797	8802	8803	8805
				8808	8809	8811	8812	8815	8830	8860	8862	8925	8927	8947	8958
				8960	8961	8965	8966	8973	8974	9006	9009	9549	9551	9584	9601
				9619	9633	9653	9723	9724	9754	9763	9764	9765	9770	9773	9789
				9813	9823	9830	9838	9852	9863	9868	9875	9876	9877	9892	9902
				9903	9914	9929	9940	9946	9955	9966	9970	9980	9992	9997	10001
				10005	10015	10017	10020	10024	10026	10036	10043	10104	10108	10117	10124
				10125	10142	10143	10144	10157	10158	10159	10162	10163	10164	10166	10171
				10173	10183
(U) AC-OP			2017 #
	AC+#			2018 #	8186	8189	8190	8191	8192	8261	8415	8415	8438	8485	8486
				8722	8784	8784	8794	8796	8797	8802	8803	8805	8808	8809	8811
				8812	8815	8860	8862	8947	8958	8960	8961	8965	8966	9006	10157
				10158	10159	10163	10164
	OR			2020 #
	#			2019 #
(U) ACB				2006 #
	MICROB			2008 #	3740	3798	4656	4692	4808	4878	4883	5050	5089	5093	5097
				5101	5701	5717	5719	5739	6432	6437	6439	6445	6448	6452	6457
				6461	6465	6466	6467	6472	6473	6474	6476	6477	6503	6505	6507
				6508	6517	6519	6520	6523	6542	6549	6550	6559	6585	6598	6599
				6615	6619	6659	6660	6662	6666	6668	6693	6732	6739	6869	6870
				6876	6877	6912	6913	6920	6921	6922	6969	6976	7054	7056	7067
				7069	7088	7091	7097	7268	7604	7625	7650	7677	7751	8056	8099
				8102	8104	8107	8149	8187	8188	8227	8229	8230	8243	8257	8267
				8269	8278	8283	8301	8325	8344	8391	8400	8437	8451	8454	8459
				8465	8503	8538	8540	8549	8550	8559	8562	8564	8565	8576	8577
				8611	8613	8627	8628	8675	8678	8679	8680	8681	8689	8714	8716
				8721	8726	8728	8729	8830	8925	8927	8973	8974	9009	9549	9551
				9584	9601	9619	9633	9653	9754	10108	10124	10125	10142	10143	10162
				10166	10171	10173	10183
	PAGB			2007 #	9723	9724	9763	9764	9765	9770	9773	9789	9813	9823	9830
				9838	9852	9863	9868	9875	9876	9877	9892	9902	9903	9914	9929
				9940	9946	9955	9966	9970	9980	9992	9997	10001	10005	10015	10017
				10020	10024	10026	10036	10043	10104	10117	10144
(U) AD				1638 #
	A			1676 #	3740	3813	3817	3822	3824	3833	3835	4020	4023	4524	4628
				4659	4676	4678	4759	4811	4837	4839	5055	5078	5084	5150	5187
				5196	5212	5218	5220	5233	5235	5241	5258	5332	5393	5437	5438
				5439	5440	5442	5451	5452	5453	5454	5462	5508	5512	5519	5550
				5562	5629	5630	5631	5632	5652	5672	5678	5693	5740	5913	5913
				5925	6089	6095	6098	6126	6129	6154	6156	6175	6255	6262	6281
				6290	6296	6299	6340	6343	6356	6358	6377	6379	6384	6388	6401
				6432	6441	6443	6521	6530	6562	6570	6571	6572	6579	6592	6594
				6622	6676	6678	6683	6688	6703	6722	6766	6886	6925	6927	6957
				6959	6962	6966	6972	6978	7048	7053	7059	7062	7097	7101	7140
				7160	7161	7279	7296	7378	7382	7471	7475	7499	7605	7627	7632
				7633	7635	7680	7686	7859	7863	7872	7895	7900	7909	7929	7970
				7972	7980	8018	8064	8076	8078	8085	8201	8210	8219	8221	8300
				8362	8391	8393	8404	8406	8444	8589	8591	8770	8785	8805	8832
				8833	8848	8889	8893	8935	8964	9001	9003	9151	9152	9169	9172
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-3
; 							Cross Reference Listing					

				9174	9176	9187	9189	9195	9214	9220	9254	9260	9301	9310	9323
				9325	9336	9373	9391	9440	9444	9456	9471	9472	9473	9479	9529
				9577	9579	9608	9663	9723	9724	9751	9767	9768	9769	9828	9889
				9934	9941	9996	10118	10120	10123
	A*2			1643 #	4053	5579	6212	6381	6408	6680	6711	6725	6971	7636	8679
				8973	9661
	A*2+1			1644 #	8385
	A+1			1639 #	3729	3797	3992	4539	4540	4636	4641	4806	4874	4876	4895
				5092	5100	5124	5326	5513	6082	6382	6393	6410	6694	6708	6759
				6889	7137	7163	7167	7169	7186	7186	7199	7204	7219	7219	7221
				7221	7271	7284	7284	7592	7597	7602	7730	7732	7744	7750	7788
				7791	7794	7798	7812	7815	7817	7819	7821	7824	7829	7833	7837
				7841	7952	7953	8099	8102	8104	8107	8154	8166	8297	8363	8446
				8525	8555	8580	8760	8764	8768	8795	8798	8799	8803	8869	8897
				8899	8994	9008	9148	9565	9589	9612	9670	9951
	A+B			1647 #	3815	3819	3823	3825	3834	3837	5127	5130	5338	5350	5443
				5456	5458	5543	5548	5566	5594	5597	5598	5601	5611	5625	5628
				5659	5671	5732	5830	6001	6099	6209	6293	6332	6527	6548	6556
				6598	6710	6741	6975	7085	7238	7239	7268	7269	7314	7315	7328
				7329	7333	7624	7650	7684	7688	7692	7694	7762	7861	7865	7874
				7897	7902	7912	7931	7971	7973	7981	8077	8079	8087	8284	8447
				8467	8468	8537	8620	8627	8628	8642	8680	8689	8974	9009	9401
				9407	9439	9505	9632	9823	9838	9863	9914	9946	10175
	A+B+1			1648 #	4588	4589	4613	4804	4809	5337	7190	7225	7288	7327	7604
				8262	8269	8292	8304	8331	8345	8454	8611	8721	8726	8729	8925
				8927
	A+XCRY			1640 #	5399
	A-1			1658 #	4555	4556	4606	4830	4849	5082	5125	5318	7157	7280	7307
				7678	7700	8295	8364	8364	8408	8408	8409	8409	8725	9354	9590
				9600
	A-B			1652 #	3982	4044	4054	4069	5324	5363	5445	5447	5460	5489	5509
				5540	5542	5545	5552	5554	5557	5560	5565	5595	5596	5599	5600
				5614	5626	5627	5661	5712	5715	5730	5746	5750	5812	5997	5999
				6125	6227	6260	6399	6416	6438	6452	6459	6490	6545	6611	6615
				6700	6938	6943	7135	7139	7218	7270	7331	7630	7638	7649	7752
				8229	8254	8278	8281	8457	8481	8519	8523	8586	10162	10171	10173
				10177
	A-B-1			1651 #	6934	7158	7276	8542	8553
	AND			1675 #	4241	4808	4878	4883	5089	5101	5415	5680	6666	6734	6876
				8230	8714	8716	9551	9584	9601	9619	9633	9653	9875	9955	9970
	ANDC			1664 #	4301
	ANDCA			1669 #	4266	4479	4685	5133	6147	6414	6763	8228	8516	9361	10131
	ANDCB			1674 #	4251	4656	4692	4761	5050	5093	5097	5135	6214	6578	6869
				7089	7092	10125
	B			1671 #	3777	3788	3840	3841	3863	3869	3875	3882	3888	3987	3996
				4022	4024	4025	4040	4042	4043	4065	4067	4072	4176	4178	4181
				4184	4187	4191	4205	4574	4605	4654	4668	4669	4684	4689	4696
				4709	4755	4813	4827	4829	4832	4832	4834	4834	4851	4856	4862
				4862	4888	4891	4892	5008	5010	5022	5026	5028	5030	5037	5039
				5041	5043	5046	5072	5075	5085	5088	5167	5178	5205	5207	5207
				5209	5227	5228	5230	5251	5263	5264	5270	5271	5272	5279	5286
				5288	5292	5298	5301	5302	5312	5362	5386	5389	5413	5484	5487
				5490	5495	5616	5647	5649	5653	5677	5690	5691	5694	5701	5703
				5705	5706	5708	5710	5710	5719	5720	5722	5739	5742	5743	5745
				5749	5811	5815	5871	5874	5901	6052	6090	6131	6133	6152	6160
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-4
; 							Cross Reference Listing					

				6162	6165	6208	6258	6274	6283	6283	6292	6298	6300	6301	6311
				6311	6324	6325	6333	6353	6354	6361	6365	6389	6412	6447	6456
				6457	6460	6461	6462	6464	6467	6471	6472	6476	6479	6501	6503
				6507	6508	6515	6518	6519	6520	6524	6525	6526	6540	6544	6546
				6550	6553	6557	6577	6586	6589	6590	6600	6604	6660	6663	6664
				6689	6693	6709	6732	6739	6740	6761	6912	6913	6920	6921	6922
				6926	6937	6942	6944	6954	6967	7050	7056	7064	7069	7073	7075
				7076	7084	7098	7134	7155	7155	7175	7192	7193	7216	7216	7228
				7230	7266	7267	7274	7277	7278	7290	7292	7316	7317	7389	7410
				7412	7413	7414	7415	7416	7417	7419	7426	7428	7429	7430	7431
				7432	7434	7441	7443	7444	7446	7448	7449	7479	7497	7497	7515
				7517	7522	7523	7524	7525	7526	7527	7528	7530	7536	7537	7538
				7539	7540	7541	7543	7549	7550	7551	7553	7555	7556	7677	7695
				7739	7756	7797	7800	7868	7869	7905	7906	7976	7977	7991	7993
				8012	8012	8014	8047	8050	8055	8056	8063	8081	8082	8148	8150
				8156	8164	8167	8182	8186	8187	8188	8191	8222	8227	8242	8244
				8245	8261	8263	8270	8273	8290	8338	8344	8360	8365	8387	8388
				8388	8403	8403	8415	8415	8427	8438	8445	8451	8456	8464	8473
				8479	8504	8506	8513	8515	8527	8529	8551	8559	8563	8565	8576
				8577	8579	8583	8614	8623	8632	8633	8634	8639	8677	8691	8693
				8695	8719	8720	8722	8728	8739	8740	8740	8762	8763	8784	8784
				8796	8797	8826	8837	8842	8846	8850	8860	8862	8878	8914	8932
				8934	8936	8938	8945	8947	8954	8956	8958	8971	8971	8976	8980
				8981	8991	8995	9006	9011	9013	9125	9161	9200	9205	9233	9267
				9313	9326	9356	9357	9359	9360	9426	9468	9527	9555	9559	9564
				9588	9596	9597	9609	9610	9611	9613	9671	9721	9722	9764	9771
				9786	9810	9813	9824	9825	9826	9830	9845	9852	9868	9871	9876
				9886	9892	9893	9902	9903	9916	9929	9940	9966	9980	9997	10005
				10015	10017	10020	10024	10026	10036	10043	10056	10060	10084	10088	10092
				10100	10104	10108	10117	10135	10142	10144	10149	10157	10170
	CRY A EQ -1		1678 #	8630
	CRY A GE B		1681 #	5912	8412
	CRY A#0			1680 #	6123	6374	6672	8335	8522	8986	9010
	CRY A.B#0		1679 #	4475	4760	8681	9130	9132	9960
	EQV			1667 #	4311
	NOR			1665 #	6870
	OR			1672 #	4291	4481	5061	5096	6758	8185	8190	8285	8430	8472	9309
				9362	9384	9877	9964	9992	10166	10176	10180
	ORC			1661 #	4362	5555
	ORCA			1662 #	4352	5567
	ORCB			1668 #	4331	6668	6877
	ORCB+1			1650 #	4744	9134
	SETCA			1660 #	4342	4467	4469	4472	6375	6674	8256	8558	9906
	SETCB			1666 #	4321	5613	5714	6394	6394	6397	6397	6695	6695	6698	6698
				7273	7658	8255	8283	8301	8429	8459	8465	8550	10064	10183
	XCRY-1			1655 #	3899	3900	5129	5253	5255	5275	5276	5285	5402	5682	5747
				5821	5822	6066	6086	6092	6276	6279	6510	6512	6729	7058	7065
				7079	7753	7755	8483
	XOR			1670 #	4281	4480	4507	5260	5262	5290	6265	6587	8361	8400	9629
	0S			1673 #	4055	4079	4081	4211	4224	4682	4725	4756	4774	5080	5168
				5169	5239	5296	5328	5394	5396	5434	5434	5467	5517	5663	5663
				5670	5717	5802	5870	5926	6097	6189	6297	6329	6329	6404	6404
				6445	6542	6554	6554	6565	6705	6705	7392	7394	7573	7593	7626
				7641	7642	7734	7737	7746	7749	7996	7997	8257	8328	8390	8437
				8437	8442	8442	8616	8616	8621	8926	9319	9334	9365	9371	9375
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-5
; 							Cross Reference Listing					

				9379	9382	9417	9417	9420	9428	9445	9448	9457	9469	9484	9507
				9514	9516	9518	9520	9570	9607	9624	9630	9660	9765	9781	10133
				10161
	1S			1663 #	3731	3775	3783	3844	4203	4212	4218	4226	4372	4603	4604
				4649	6192	6574	7310	7326	7381	7386	7474	7484	7599	7740	7751
				7870	7871	7907	7908	7922	7923	7968	7978	7979	8065	8083	8084
				8109	8260	8265	8448	8449	8549	8759	8774	8813	8816	8825	10080
(U) ADA				1684 #
	AR			1685 #	3740	3797	3833	3834	3835	3837	4020	4023	4053	4241	4251
				4266	4281	4291	4301	4311	4331	4342	4352	4362	4475	4479	4480
				4481	4507	4524	4539	4540	4555	4556	4606	4628	4659	4676	4760
				4761	4808	4811	4830	4839	4849	4878	4883	5061	5089	5096	5127
				5130	5133	5135	5150	5187	5196	5218	5235	5260	5290	5350	5363
				5393	5399	5415	5437	5438	5439	5440	5442	5443	5445	5447	5451
				5452	5453	5454	5456	5458	5460	5462	5508	5512	5540	5543	5548
				5550	5560	5562	5579	5594	5595	5596	5597	5598	5599	5600	5601
				5611	5614	5625	5626	5627	5628	5659	5661	5671	5680	5730	5732
				5830	5925	5999	6001	6099	6126	6129	6175	6209	6212	6214	6265
				6293	6299	6332	6343	6377	6379	6381	6382	6384	6393	6401	6408
				6414	6443	6452	6490	6527	6545	6548	6556	6570	6571	6572	6579
				6587	6598	6615	6666	6668	6676	6678	6680	6683	6694	6703	6710
				6711	6722	6725	6734	6763	6766	6869	6870	6876	6877	6889	6925
				6927	6934	6938	6943	6962	6966	6971	6972	6975	6978	7048	7059
				7062	7085	7088	7091	7101	7135	7239	7268	7328	7378	7382	7471
				7475	7605	7627	7633	7636	7638	7650	7686	7688	7692	7788	7791
				7794	7798	7812	7815	7817	7819	7821	7824	7829	7833	7837	7841
				7859	7861	7863	7865	7895	7897	7900	7902	7952	7953	7970	7971
				7972	7973	8064	8076	8077	8078	8079	8219	8228	8229	8254	8281
				8284	8285	8297	8304	8361	8364	8400	8408	8409	8447	8457	8467
				8468	8472	8516	8522	8537	8542	8555	8580	8620	8627	8628	8642
				8679	8680	8681	8689	8714	8716	8725	8760	8764	8768	8770	8795
				8798	8803	8805	8833	8869	8889	8893	8897	8899	8927	8935	8973
				8974	8986	8994	9001	9003	9009	9010	9130	9132	9148	9151	9152
				9169	9172	9174	9176	9187	9189	9195	9214	9220	9254	9260	9301
				9309	9310	9323	9325	9362	9373	9391	9401	9407	9439	9444	9456
				9471	9472	9473	9479	9505	9551	9565	9577	9579	9589	9590	9600
				9612	9629	9632	9633	9663	9670	9723	9724	9767	9823	9828	9838
				9863	9875	9877	9889	9914	9941	9955	9960	9964	9970	9992	10131
				10166	10171	10175	10176	10180
	ARX			1686 #	3813	3815	3817	3819	3822	3823	3824	3825	4656	4678	4692
				4837	5050	5055	5093	5101	5125	5212	5220	5233	5241	5258	5554
				5566	5913	6123	6262	6441	6521	6530	6741	6758	7137	7139	7314
				7329	7624	7635	7684	7694	7752	7762	7872	7874	7909	7912	7929
				7931	7980	7981	8085	8087	8099	8102	8104	8107	8154	8166	8185
				8190	8210	8295	8300	8335	8393	8404	8406	8430	8553	8558	8799
				8832	9361	9584	9601	9619	9653	9934	9946	9996	10125	10177
	MQ			1687 #	3729	5262	5519	5555	5567	5629	5630	5631	5632	5672	5678
				5693	5740	6098	6156	6296	6340	6356	6358	6374	6375	6562	6578
				6592	6594	6622	6672	6674	6886	7140	7157	7158	7160	7161	7186
				7218	7219	7221	7276	7279	7280	7284	7296	7307	7499	7678	7680
				8018	8221	8230	8362	8363	8385	8391	8589	8591	8630	8785	8964
				9336	9440	9608	9661
	PC			1688 #	4636	4641	4759	4806	4874	4876	4895	5078	5084	5097	5100
				8201	9751	9769	10118	10120	10123
(U) ADA EN			1689 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-6
; 							Cross Reference Listing					

	EN			1690 #	4685	5318	5324	5332	5337	5338	5652	5913	6089	6095	6154
				6255	6281	6290	6388	6432	6688	6957	6959	7053	7097	7163	7167
				7169	7186	7199	7204	7219	7221	7238	7269	7271	7284	7315	7333
				7597	7602	7604	7632	7700	8256	8364	8408	8409	8444	8848	9384
				9529	9768	9906
	0S			1691 #	3982	3992	4044	4054	4069	4588	4589	4613	4744	4804	4809
				5082	5092	5124	5326	5489	5509	5513	5542	5545	5552	5557	5565
				5712	5715	5746	5750	5812	5912	5997	6082	6125	6147	6227	6260
				6399	6410	6416	6438	6457	6459	6461	6467	6472	6476	6503	6507
				6508	6519	6520	6550	6611	6660	6700	6708	6732	6739	6759	6912
				6913	6920	6921	6922	7190	7225	7270	7288	7327	7331	7592	7630
				7649	7730	7732	7744	7750	8227	8262	8269	8278	8292	8331	8345
				8412	8446	8454	8481	8519	8523	8525	8559	8586	8611	8721	8726
				8729	8925	9008	9134	9354	9951	10162	10173
(U) ADB				1693 #
	AR*4			1697 #	4054	4744	5324	5542	5545	5701	5812	5874	5997	6125	6131
				6133	6147	6152	6165	6227	6260	6416	6438	6462	6471	6611	6709
				6944	7752	8447	8456	8467	8523	8537	8550	8553	8620	8634	8642
				8842	8846	8850	8878	9326	9359	9360	9555	9559	9596	9611	9721
				9722
	BR			1696 #	3982	4022	4024	4044	4069	4669	4684	4685	4755	4760	4761
				4813	4851	4892	5008	5010	5022	5026	5028	5030	5037	5039	5041
				5043	5046	5061	5072	5085	5088	5096	5260	5262	5263	5264	5279
				5288	5290	5292	5298	5301	5302	5337	5338	5363	5415	5443	5447
				5456	5460	5489	5490	5540	5543	5548	5552	5554	5557	5560	5565
				5566	5594	5595	5596	5597	5598	5599	5600	5601	5611	5614	5625
				5626	5627	5628	5659	5661	5671	5680	5690	5694	5710	5710	5712
				5714	5715	5720	5722	5730	5732	5745	5746	5749	5750	5912	6090
				6099	6208	6209	6214	6283	6283	6293	6311	6311	6332	6389	6394
				6394	6397	6397	6399	6412	6414	6456	6459	6460	6490	6515	6527
				6544	6545	6548	6556	6577	6578	6586	6689	6695	6695	6698	6698
				6700	6710	6734	6740	6741	6758	6761	6763	6934	6937	6938	6942
				6943	6975	7056	7069	7075	7085	7135	7139	7155	7155	7158	7175
				7190	7216	7216	7218	7225	7238	7268	7269	7270	7273	7274	7276
				7288	7314	7315	7327	7331	7333	7604	7630	7638	7649	7658	7684
				7688	7692	7694	7695	7739	7756	7762	7800	8050	8055	8056	8063
				8167	8185	8190	8191	8222	8228	8254	8255	8262	8281	8284	8292
				8304	8331	8361	8387	8403	8403	8412	8430	8451	8464	8468	8473
				8479	8481	8516	8519	8542	8583	8586	8614	8623	8639	8938	8956
				8958	8995	9130	9132	9134	9161	9200	9205	9233	9267	9309	9313
				9356	9357	9361	9362	9384	9401	9407	9426	9439	9468	9505	9609
				9610	9613	9629	9632	9671	9764	9786	9824	9826	9886	9960	9964
				10060	10084	10131	10149	10175	10177
	BR*2			1695 #	5286	5445	5458	5706	5815	5830	5999	6001	6160	6162	8529
				9771
	FM			1694 #	3777	3788	3815	3819	3823	3825	3834	3837	3840	3841	3863
				3869	3875	3882	3888	3987	3996	4025	4040	4042	4043	4065	4067
				4072	4176	4178	4181	4184	4187	4191	4205	4241	4251	4266	4281
				4291	4301	4311	4321	4331	4352	4362	4475	4479	4480	4481	4507
				4574	4588	4589	4605	4613	4654	4656	4668	4689	4692	4696	4709
				4804	4808	4809	4827	4829	4832	4832	4834	4834	4856	4862	4862
				4878	4883	4888	4891	5050	5075	5089	5093	5097	5101	5127	5130
				5133	5135	5167	5178	5205	5207	5207	5209	5227	5228	5230	5251
				5270	5271	5272	5312	5350	5362	5386	5389	5413	5484	5487	5495
				5509	5555	5567	5613	5616	5647	5649	5653	5677	5691	5703	5705
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-7
; 							Cross Reference Listing					

				5708	5719	5739	5742	5743	5811	5871	5901	6052	6258	6265	6274
				6292	6298	6300	6301	6324	6325	6333	6353	6354	6361	6365	6447
				6452	6457	6461	6464	6467	6472	6476	6479	6501	6503	6507	6508
				6518	6519	6520	6524	6525	6526	6540	6546	6550	6553	6557	6587
				6589	6590	6598	6600	6604	6615	6660	6663	6664	6666	6668	6693
				6732	6739	6869	6870	6876	6877	6912	6913	6920	6921	6922	6926
				6954	6967	7050	7064	7073	7076	7084	7088	7091	7098	7134	7192
				7193	7228	7230	7239	7266	7267	7277	7278	7290	7292	7316	7317
				7328	7329	7389	7410	7412	7413	7414	7415	7416	7417	7419	7426
				7428	7429	7430	7431	7432	7434	7441	7443	7444	7446	7448	7449
				7479	7497	7497	7515	7517	7522	7523	7524	7525	7526	7527	7528
				7530	7536	7537	7538	7539	7540	7541	7543	7549	7550	7551	7553
				7555	7556	7624	7650	7677	7797	7861	7865	7868	7869	7874	7897
				7902	7905	7906	7912	7931	7971	7973	7976	7977	7981	7991	7993
				8012	8012	8014	8047	8077	8079	8081	8082	8087	8148	8150	8156
				8164	8182	8186	8187	8188	8227	8229	8230	8242	8244	8245	8261
				8263	8269	8270	8273	8278	8283	8285	8290	8301	8338	8344	8345
				8360	8365	8388	8388	8400	8415	8415	8427	8429	8438	8445	8454
				8457	8459	8465	8472	8504	8506	8513	8515	8527	8551	8559	8563
				8565	8576	8577	8579	8611	8627	8628	8632	8633	8677	8680	8681
				8689	8691	8693	8695	8714	8716	8719	8720	8721	8722	8726	8728
				8729	8739	8740	8740	8762	8763	8784	8784	8796	8797	8826	8837
				8860	8862	8914	8925	8927	8932	8934	8936	8945	8947	8954	8971
				8971	8974	8976	8980	8981	8991	9006	9009	9011	9013	9125	9527
				9551	9564	9584	9588	9597	9601	9619	9633	9653	9810	9813	9823
				9825	9830	9838	9845	9852	9863	9868	9871	9875	9876	9877	9892
				9893	9902	9903	9914	9916	9929	9940	9946	9955	9966	9970	9980
				9992	9997	10005	10015	10017	10020	10024	10026	10036	10043	10056	10064
				10088	10092	10100	10104	10108	10117	10125	10135	10142	10144	10157	10162
				10166	10170	10171	10173	10176	10180	10183
(U) AR				1705 #
	AD			1710 #	3729	3797	3813	3815	3817	3819	3822	3823	3824	3825	3863
				3899	3900	3982	3987	3996	4022	4024	4025	4055	4065	4067	4079
				4081	4178	4181	4187	4205	4211	4212	4224	4226	4241	4251	4266
				4281	4291	4301	4311	4321	4331	4342	4352	4362	4372	4479	4480
				4481	4539	4540	4555	4556	4574	4588	4589	4603	4604	4605	4606
				4613	4656	4668	4669	4682	4725	4759	4761	4774	4806	4808	4827
				4829	4830	4874	4876	4878	4883	4888	4891	4895	5050	5061	5078
				5080	5093	5096	5097	5101	5127	5130	5167	5169	5178	5207	5212
				5220	5228	5233	5241	5251	5253	5255	5258	5263	5264	5271	5272
				5275	5276	5285	5286	5288	5301	5312	5350	5362	5363	5389	5394
				5396	5402	5413	5434	5495	5517	5519	5552	5565	5598	5599	5600
				5601	5611	5614	5629	5630	5631	5632	5653	5659	5661	5663	5671
				5672	5677	5678	5682	5690	5693	5694	5703	5708	5712	5714	5715
				5719	5720	5722	5739	5740	5742	5743	5745	5746	5747	5749	5750
				5811	5815	5821	5822	5871	5901	6052	6066	6090	6092	6097	6098
				6099	6131	6160	6189	6208	6209	6274	6276	6279	6283	6292	6293
				6296	6300	6301	6311	6324	6325	6329	6332	6333	6340	6356	6358
				6361	6365	6375	6382	6389	6393	6394	6397	6399	6404	6445	6447
				6452	6456	6457	6459	6460	6461	6464	6467	6472	6476	6501	6503
				6507	6508	6510	6512	6515	6518	6520	6521	6524	6525	6526	6527
				6530	6542	6544	6545	6546	6548	6550	6553	6554	6556	6557	6562
				6577	6586	6592	6594	6598	6600	6604	6615	6622	6660	6664	6666
				6668	6674	6689	6694	6695	6698	6700	6705	6710	6729	6732	6734
				6739	6740	6741	6758	6869	6870	6876	6877	6886	6912	6913	6920
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-8
; 							Cross Reference Listing					

				6921	6922	6937	6942	6943	6975	7056	7058	7065	7069	7073	7075
				7079	7085	7088	7091	7098	7134	7135	7139	7157	7190	7225	7239
				7266	7268	7288	7314	7328	7331	7392	7394	7499	7641	7642	7650
				7677	7680	7684	7688	7692	7694	7739	7751	7753	7755	7762	7794
				7798	7824	7829	7833	7837	7841	7953	7996	7997	8018	8050	8055
				8063	8148	8150	8154	8156	8164	8166	8167	8182	8185	8186	8190
				8210	8227	8229	8230	8244	8245	8254	8255	8257	8261	8263	8265
				8269	8270	8273	8278	8281	8283	8284	8285	8290	8297	8301	8328
				8338	8344	8345	8360	8387	8388	8391	8400	8403	8404	8406	8415
				8427	8429	8430	8437	8442	8445	8446	8449	8451	8454	8459	8464
				8465	8467	8472	8473	8479	8481	8483	8504	8506	8513	8516	8519
				8527	8549	8551	8565	8577	8579	8580	8583	8586	8589	8591	8611
				8614	8616	8621	8623	8628	8639	8677	8680	8693	8695	8716	8719
				8720	8722	8725	8726	8728	8729	8739	8740	8760	8763	8764	8768
				8784	8785	8795	8797	8798	8799	8803	8860	8862	8869	8897	8899
				8914	8926	8934	8938	8958	8964	8971	8980	8991	8994	9006	9011
				9013	9148	9161	9200	9205	9233	9267	9313	9319	9334	9356	9361
				9362	9365	9375	9379	9401	9407	9417	9420	9426	9428	9439	9445
				9448	9457	9468	9469	9484	9505	9507	9514	9516	9518	9520	9555
				9559	9565	9570	9589	9590	9596	9607	9608	9611	9624	9630	9632
				9660	9764	9765	9781	9786	9813	9824	9830	9868	9875	9876	9877
				9892	9903	9929	9951	9955	9964	9966	9970	9980	9992	9996	9997
				10015	10056	10060	10064	10084	10088	10092	10100	10104	10117	10118	10120
				10125	10133	10142	10144	10149	10157	10161	10166	10170	10171	10173	10175
				10176	10177	10180	10183
	AD*.25			1715 #	4054	5442	5443	5445	5447	5456	5458	5460	5462	5542	5545
				5550	5562	5812	5830	5913	5925	5997	6125	6126	6156	6175	6212
				6227	6260	6377	6379	6381	6401	6408	6416	6438	6571	6611	6676
				6678	6680	6703	6711	6722	6725	6962	6971	6972	8523	8525	8529
				8553	8620	8679	8973	9721	9722	9934
	AD*2			1713 #	3740	5270	5437	5438	5439	5440	5451	5452	5453	5454	5484
				5540	5543	5548	5560	5594	5595	5596	5597	5625	5626	5627	5628
				5649	5730	5732	6129	6133	6148	6152	6162	6165	6258	6353	6354
				6384	6462	6471	6479	6540	6570	6572	6589	6590	6663	6683	6944
				7638	8447	8468	8537	8642	8842	8850	8878	9008	9479	9661
	ADX			1714 #	4685	5088	5298	5337	5338	5489	5490	5710	6095	6154	6281
				6290	6388	6688	7238	7269	7327	7333	7604	7632	7695	7756	8056
				8256	8262	8292	8304	8331	8364	8408	8409	8412	8956	8995	9309
				9610	9613	9671	10131
	AR			1706 #	3992	4176	4188	4201	4203	4684	4694	4845	5124	5129	7175
				9123	9252	9299	9371	9454	10129
	ARMM			1707 #	3942	4716	4890	5095	5108	5109	5110	6213	6409	6489	6543
				6597	6733	6755	6933	6935	6941	6974	7055	7068	7600	7730	7732
				7734	7737	7740	7744	7746	7749	7750	7763	7799	7811	7814	7822
				7823	7830	7834	7838	7842	8058	8253	8433	8438	8441	8552	8643
				8706	8707	8708	8709	8710	8711	8731	8778	8800	8802	8871	8909
				9015	9231	9234	9335	9347	9360	9418	9574	9623	9675	9936	10004
				10006	10128	10158
	CACHE			1709 #	9286
	EBUS			1711 #	3744	3746	4776	5082	9172	9227	9257	9258	9304	9306	9308
				9354	9364	9377	9381	9431	9432	9433	9434	9447	9449	9459	9478
				9487	9488	9489	9490	9740	9755
	MEM			1708 #	3869	3882	3888	4654	4689	4696	4832	4834	4856	4862	6926
				6967	7050	7064	7192	7193	7228	7230	7277	7278	7290	7292	7316
				7317	7446	7497	7522	7523	7524	7525	7526	7527	7528	7530	7536
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-9
; 							Cross Reference Listing					

				7537	7538	7539	7540	7541	7543	7549	7550	7551	7555	7556	7797
				7991	7993	8012	8014	8242	8365	8563	8632	8691	8837	8936	8945
				8976	8981	9125	9564	9588	9597	9810	9825	9845	9871	9893	9916
				10135
	SH			1712 #	3774	3808	3844	3904	3927	3971	4044	4046	4053	4073	4184
				4191	4192	4217	4218	4469	4472	4474	4667	4720	4756	4814	4819
				4821	4892	5085	5098	5103	5180	5183	5197	5209	5214	5218	5239
				5243	5292	5296	5302	5315	5317	5388	5392	5398	5401	5507	5511
				5688	5689	5691	5701	5704	5706	5748	5800	5827	5865	5911	6123
				6286	6288	6289	6297	6298	6385	6387	6405	6412	6468	6473	6519
				6685	6687	6706	6760	6888	6890	6945	6976	7076	7081	7084	7137
				7140	7267	7330	7501	7502	7625	7686	7801	7922	8004	8021	8023
				8065	8099	8102	8104	8107	8218	8266	8336	8390	8448	8450	8484
				8517	8538	8540	8564	8622	8637	8703	8732	8765	8770	8805	8826
				8831	8832	8833	8838	8857	8861	8883	8893	8918	8952	8960	9000
				9001	9002	9003	9213	9259	9323	9325	9357	9385	9471	9472	9508
				9622	9625	9723	9724	9739	9768	9770	9773	9789	9823	9826	9854
				9863	9941	9946	10005	10017	10020	10023	10025	10122	10162	10164	10168
(U) AR CTL			2054 #	5514
	AR0-8 LOAD		2055 #	6213	6409
	AR9-17 LOAD		2056 #
	ARL LOAD		2058 #
	ARR LOAD		2057 #	9231
(U) AR0-8			2029 #
	LOAD			2030 #	3993	5048	5072	5579	5802	5814	5866	5870	5872	5996	6053
				6252	6310	6337	6341	6970	6977	7082	7602	7693	7700	7752	7779
				7794	7796	7953	8028	8209	8258	8297	8299	8440	8514	8557	8754
				8761	8767	8791	8870	8903	8904	8994	9359	9679	9757	9766	9767
				9788	10154
(U) ARL				2044 #
	AD			2048 #	3992	4067	4176	4184	4191	4203	4218	4684	4892	5085	5092
				5129	5251	5487	5509	5519	6052	6283	6356	6358	6592	6594	7140
				7800	8109	8504	8589	8591	9357
	AD*.25			2053 #
	AD*2			2051 #	5484	5549	5561	5649
	ADX			2052 #	7631	9384	9768
	ARL			2045 #	3996	4178	4192	4224	4226	5098	5388	7134	7628	7684	9259
				9309	9323
	ARMM			2046 #	3993	5048	5072	5579	5802	5814	5866	5870	5872	5996	6053
				6252	6310	6337	6341	6728	6970	6977	7082	7602	7693	7700	7752
				7779	7794	7796	7953	8028	8209	8258	8297	8299	8440	8514	8557
				8754	8761	8767	8791	8870	8903	8904	8994	9359	9679	9757	9766
				9767	9788	10154
	CACHE			2047 #
	EBUS			2049 #
	SH			2050 #	4181	4187	4188	4211	4212	4694	4895	5124	5315	5392	5827
				6123	6286	7175	7688	7694	9123	9233	9252	9299	9371	9451	9454
				9905	10129
(U) ARMM			1790 #
	EXP_SIGN		1792 #	5802	5814	5866	5870	5872	5996	6053	6252	6310	6337	6977
				7082
	SCAD EXP		1793 #	6213	6341	6409	6728	6970	8028	8058	8209	8258	8438	8440
				8441	8514	8557	9574	9623	9675	9766	9767	9788	10004
	SCAD POS		1794 #	3993	5048	5072	5579	6755	7600	7602	7693	7700	7730	7732
				7734	7737	7740	7744	7746	7749	7750	7752	7763	7779	7794	7796
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-10
; 							Cross Reference Listing					

				7799	7811	7814	7822	7823	7830	7834	7838	7842	7953	8297	8299
				8643	8706	8707	8708	8709	8710	8711	8731	8754	8761	8767	8778
				8791	8800	8802	8870	8871	8903	8904	8909	8994	9015	9359	9679
				9757	10006	10154	10158
	#			1791 #	3942	5108	5109	5110	6489	6543	6597	6733	6933	6935	6941
				6974	7055	7068	8253	8433	8552	9234	9335	9347	9418	9936
(U) ARX				1716 #
	AD			1720 #	3731	3777	3783	4053	4072	4636	4641	4657	4692	4756	4804
				4809	4849	5082	5100	5124	5150	5168	5178	5187	5196	5205	5209
				5218	5227	5230	5235	5239	5279	5296	5328	5386	5434	5487	5509
				5647	5663	5691	5705	5706	5710	5717	5802	5870	5926	6086	6192
				6297	6298	6329	6404	6410	6519	6554	6574	6705	6759	7057	7084
				7134	7135	7137	7267	7310	7326	7329	7515	7517	7573	7593	7624
				7626	7627	7636	7686	7734	7737	7740	7744	7746	7749	7750	7752
				8047	8187	8188	8201	8228	8260	8295	8301	8364	8388	8390	8408
				8409	8415	8437	8438	8442	8448	8515	8550	8555	8576	8616	8633
				8714	8740	8760	8762	8770	8784	8796	8805	8825	8826	8862	8947
				8971	9323	9354	9371	9382	9417	9551	9577	9584	9601	9619	9633
				9653	9723	9724	9767	9769	9826	9902	9941	10017	10020	10024	10026
				10080	10123	10126	10162
	ADX			1724 #	4755	5008	5010	5022	5026	5028	5030	5037	5039	5041	5043
				5046	5072	5286	5292	5302	5318	5557	5598	5599	5600	5601	5611
				5614	5659	5661	5671	5712	5714	5715	5720	5722	5745	5746	6131
				6160	6214	6283	6293	6311	6332	6382	6394	6397	6399	6412	6414
				6515	6527	6556	6577	6695	6698	6700	6709	6710	6761	6763	7155
				7163	7167	7169	7175	7186	7199	7204	7216	7219	7221	7270	7271
				7273	7284	7315	7597	7602	7630	7649	7658	7700	8191	8262	8292
				8331	8403	8451	8464	8467	8473	8479	8481	8525	8580	8583	8586
				8614	8623	8639	8846	9439	9505	9609	10060	10084
	ADX*.25			1725 #	4054	5442	5443	5445	5447	5456	5458	5460	5462	5542	5545
				5550	5562	5830	5913	5925	5997	6082	6125	6126	6175	6260	6377
				6379	6381	6401	6408	6416	6438	6571	6611	6676	6678	6680	6703
				6711	6722	6725	6962	8385	8523	8529	8620	8679	8973	9721	9722
	ADX*2			1723 #	5324	5326	5332	5437	5438	5439	5440	5451	5452	5453	5454
				5541	5544	5548	5560	5594	5595	5596	5597	5625	5626	5627	5628
				5652	5701	5730	5732	5749	5750	6089	6129	6133	6148	6152	6162
				6165	6255	6384	6432	6570	6572	6683	6708	6957	6959	7053	7097
				7592	7599	7638	7730	7732	8444	8468	8537	8642	8848	9359	9360
				9529	9906
	ARX			1717 #
	CACHE			1719 #
	MEM			1718 #	3788	3840	3841	4043	4832	4834	4862	5075	6954	7497	7868
				7869	7905	7906	7976	7977	8012	8081	8082
	MQ			1721 #	4044	4837	4839	5102	5197	5212	5214	5220	5233	5241	5243
				5259	5301	5507	5511	5513	5612	5615	5689	5821	5827	6282	6286
				6289	6292	6361	6387	6389	6600	6687	6689	7685	7689	7695	8076
				8077	8085	8088	8401	8878
	SH			1722 #	3808	3987	4067	4653	4737	4888	5097	5169	5253	5255	5261
				5271	5272	5275	5276	5288	5300	5489	5490	5495	5517	5519	5653
				5690	5694	5702	5708	5719	5822	5824	5825	6065	6092	6098	6156
				6274	6276	6279	6365	6508	6510	6512	6514	6521	6526	6604	6664
				6729	6731	6744	6746	6930	6968	7065	7079	7139	7328	7373	7376
				7392	7394	7466	7469	7499	7501	7632	7634	7642	7692	7753	7755
				7756	7797	7804	7996	7997	8018	8021	8029	8050	8210	8217	8254
				8257	8290	8328	8360	8386	8404	8406	8457	8506	8513	8579	8621
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-11
; 							Cross Reference Listing					

				8634	8692	8694	8758	8763	8774	8794	8797	8812	8815	8836	8892
				8914	8920	8991	9258	9305	9307	9346	9348	9365	9379	9383	9448
				9469	9484	9514	9516	9518	9520	9608	9624	9652	9661	9680	9773
				9815	9825	9909	9934	9950	9995	10118	10120	10176
(D) B				2269 #	3949	3967	4014	4015	4032	4033	4061	4271	4272	4793	4794
				5637	5638	5639	6245	6246	6247	6248	6423	6424	6425	6426	7569
				8121	8126	8127	8128	8129	8131	8132	8133	8134	9026	9028	9029
				9030	9037	9039	9040	9041	9048	9050	9051	9052	9071	9072	9073
				9074	9075	9076	9083	9084	9087	9088	9095	9097	9098	9099	9106
				9108	9109	9110
	AC			2273 #	3947	3948	3952	3953	3957	3958	3962	3963	4087	4088	4092
				4093	4097	4098	4102	4103	4107	4108	4112	4113	4117	4118	4122
				4123	4129	4130	4134	4135	4139	4140	4144	4145	4149	4150	4154
				4155	4159	4160	4164	4165	4229	4230	4234	4235	4244	4245	4254
				4255	4259	4260	4274	4275	4284	4285	4294	4295	4304	4305	4314
				4315	4324	4325	4335	4336	4345	4346	4355	4356	4365	4366	4395
				4404	4413	4422	4431	4440	4593	4597	4767	5343	5344	5355	5356
				5368	5369	6899	7368	7369
	BOTH			2275 #	4232	4237	4247	4262	4277	4287	4297	4307	4317	4327	4338
				4348	4358	4368	5346	5358	5371
	DBL AC			2270 #	5406	5407	5472	5473	5477	5478
	DBL BOTH		2271 #	5409	5475	5480
	MEM			2274 #	3954	3959	3964	4089	4094	4099	4104	4109	4114	4119	4124
				4131	4136	4146	4151	4156	4161	4166	4231	4236	4246	4261	4276
				4286	4296	4306	4316	4326	4337	4347	4357	4367	5345	5357	5370
				5408	5474	5479	7461	7462	9036
	SELF			2272 #	3955	3960	3965	4095	4100	4105	4110	4115	4120	4125	4137
				4142	4147	4152	4157	4162	4167
	SJC-			2277 #
	SJCA			2281 #	4517	4532	4548
	SJCE			2279 #	4488	4498	4515	4530	4546	4564	4579	4595	8118
	SJCG			2284 #	4493	4503	4520	4535	4551	4569	4584	4600	8124
	SJCGE			2282 #	4491	4501	4518	4533	4549	4567	4582	4598	4609	8122
	SJCL			2278 #	4487	4497	4514	4529	4545	4563	4578	4594	4610	8117
	SJCLE			2280 #	4489	4499	4516	4531	4547	4565	4580	4596	8119
	SJCN			2283 #	4492	4502	4519	4534	4550	4568	4583	4599	8123
(D) B0				2285 #	5757	5763	5764	5766	5767	5768	5769	5771	5777	5778	5780
				5781	5782	5783	5836	5842	5843	5846	5880	5886	5887	5890
	CRY0(0)			2286 #	4379	4380	4381	4382	4388	4389	4390	4391	4397	4398	4399
				4400	4406	4407	4408	4409	4415	4416	4417	4418	4424	4425	4426
				4427	4433	4434	4435	4436	4442	4443	4444	4445	4490	4500	9031
				9042	9047	9053	9077	9089	9094	9096	9100	9105	9107	9111
	CRY0(1)			2287 #	4383	4384	4392	4393	4396	4401	4402	4405	4410	4411	4414
				4419	4420	4423	4428	4429	4432	4437	4438	4441	4446	4447	4749
				9032	9043	9054	9078	9090	9101	9112
(D) B1-2			2288 #	4379	4380	4381	4382	4383	4384	4388	4389	4390	4391	4392
				4393	4396	4397	4398	4399	4400	4401	4402	4405	4406	4407	4408
				4409	4410	4411	4414	4415	4416	4417	4418	4419	4420	4423	4424
				4425	4426	4427	4428	4429	4432	4433	4434	4435	4436	4437	4438
				4441	4442	4443	4444	4445	4446	4447	4490	4500	4749	9031	9032
				9042	9043	9047	9053	9054	9077	9078	9089	9090	9094	9096	9100
				9101	9105	9107	9111	9112
	AC			2289 #	5757	5766	5767	5771	5780	5781	5836	5845	5846	5880	5889
				5890	6014	6060
	BOTH			2291 #	5764	5769	5778	5783	5843	5848	5887	5892
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-12
; 							Cross Reference Listing					

	MEM			2290 #	5763	5768	5777	5782	5842	5847	5886	5891
(U) BR				1726 #
	AR			1727 #	3863	3869	3875	3882	3888	3942	3974	3977	4054	4649	4682
				4759	4771	4806	4827	4891	5050	5094	5258	5275	5285	5286	5362
				5394	5396	5434	5484	5487	5612	5615	5649	5688	5704	5706	5710
				5712	5714	5715	5720	5722	5745	5746	5811	5815	5901	6089	6090
				6152	6165	6189	6192	6258	6262	6266	6311	6375	6387	6441	6443
				6448	6455	6488	6511	6515	6520	6525	6544	6546	6553	6574	6577
				6588	6674	6681	6687	6727	6739	6740	6741	6932	6937	6939	6942
				6973	7052	7066	7073	7075	7134	7137	7141	7192	7228	7267	7290
				7316	7328	7599	7625	7678	7680	7690	7798	8201	8220	8227	8231
				8245	8278	8360	8365	8402	8411	8433	8446	8454	8464	8514	8516
				8518	8525	8527	8583	8586	8610	8933	8934	8938	8952	9123	9147
				9211	9221	9252	9255	9283	9299	9303	9319	9360	9361	9364	9371
				9375	9420	9443	9445	9454	9457	9523	9525	9623	9764	9789	9811
				9824	9867	9953	10023	10025	10043	10144	10173	10183
(U) BRX				1728 #
	ARX			1729 #	3774	4054	4656	5076	5279	5285	5288	5301	5326	5328	5394
				5396	5434	5495	5517	5519	5552	5553	5564	5565	5579	5612	5615
				5653	5702	5710	5712	5714	5715	5720	5722	5745	5746	5811	5901
				6090	6152	6165	6189	6192	6258	6262	6266	6311	6375	6410	6415
				6511	6515	6522	6525	6553	6574	6577	6588	6674	6681	6727	7135
				7175	7190	7203	7206	7225	7266	7274	7288	7310	7599	7625	7627
				7630	7690	7740	7753	8219	8227	8258	8301	8325	8365	8402	8411
				8446	8454	8464	8515	8525	8527	8583	8586	8610	8933	8977	9307
				9382	9523	9525	9558	9609	9668	10126
(U) CALL			1915 #
	CALL			1916 #	3732	4043	4068	4630	4633	4634	4636	4637	4639	4683	4688
				4693	4757	4775	4811	4850	5049	5054	5055	5073	5077	5081	5083
				5289	5395	5397	5414	5485	5488	5549	5551	5556	5561	5563	5568
				5651	5670	5678	5695	5718	5734	5740	5873	5902	5903	6254	6274
				6331	6340	6353	6354	6357	6359	6367	6431	6436	6440	6444	6447
				6454	6459	6465	6474	6501	6504	6516	6522	6547	6551	6555	6558
				6566	6576	6586	6589	6590	6593	6595	6605	6614	6622	6931	6961
				7052	7074	7100	7136	7156	7217	7238	7374	7381	7386	7388	7467
				7576	7639	7641	7691	7868	7905	7976	8081	8148	8149	8156	8182
				8183	8186	8187	8211	8242	8243	8269	8272	8291	8326	8329	8337
				8344	8389	8399	8416	8427	8428	8455	8503	8528	8549	8563	8564
				8578	8585	8588	8636	8675	8754	8771	8784	8786	8806	8824	8916
				8926	8972	8974	8978	8993	9009	9012	9119	9125	9140	9162	9211
				9219	9221	9226	9230	9232	9252	9255	9281	9300	9303	9319	9324
				9335	9336	9346	9353	9355	9363	9372	9375	9380	9390	9391	9419
				9426	9438	9443	9445	9455	9457	9469	9479	9504	9528	9578	9595
				9596	9597	9600	9607	9611	9612	9628	9631	9638	9643	9648	9659
				9660	9663	9664	9669	9670	9672	9678	9679	10119	10128	10148
(U) CLR				2031 #
	AR			2036 #	4641	5094	6574	7270	8300	8555	10127
	AR+ARX			2037 #
	AR+ARX+MQ		2040 #	6173	7078	8326	8675
	AR+MQ			2038 #	5227	6681	8457
	ARL			2034 #	4201	4205	4217	4834	4845	8101	8634	8637	8831	8892	8914
				8934	8991	9147	9461	9815	9816	9817	9908	9909	9924
	ARL+ARX			2041 #	9437	9503
	ARL+ARX+MQ		2042 #
	ARR			2035 #	5509	9384
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-13
; 							Cross Reference Listing					

	ARR+MQ			2043 #	5487	7628	7631
	ARX			2033 #	5392	6124	6977	8630
	ARX+MQ			2039 #	5314	5485	5866	6051	8504
	MQ			2032 #	4043	4067	5251	5520	5651	5824	5825	5828	6252	6284	6287
				6357	6359	6431	6593	6595	6623	6759	6926	7050	8589	8591
(U) COND			1879 #
	AD FLAGS		1893 #	3982	4045	4069	4539	4540	4555	4556	4588	4589	4606	5350
				5363	5659	5661
	AR CLR			1885 #	3731	6089	6192	6488	6539	6541	6596	6727	6932	6939	6968
				6973	7053	7066	7636	8260	8576	8825	9577
	ARL IND			1887 #	4176	4178	4181	4184	4187	4188	4191	4192	4201	4203	4205
				4211	4212	4217	4218	4224	4226	4641	4684	4892	5098	5124	5129
				5388	6574	6977	7134	7175	7684	7794	7800	7953	8109	8297	8761
				8870	9123	9147	9259	9309	9357
	ARX CLR			1886 #	5298	5389	5413	6090	6095	6155	6208	6209	6291	6388	6522
				6530	6688	7411	7420	7427	7435	7442	8265	8446
	DIAG FUNC		1898 #	4678	4776	5082	5084	9220	9227	9254	9257	9258	9260	9301
				9304	9306	9308	9325	9354	9364	9373	9377	9381	9431	9432	9433
				9434	9444	9447	9449	9456	9459	9471	9472	9473	9478	9487	9488
				9489	9490	9740	9755
	EBOX STATE		1899 #	3743	3774	4696	4787	9482	9493	9721	9722	10037	10116	10135
				10149
	EBUS CTL		1900 #	3827	3828	3829	3830	4675	4679	4777	5087	9126	9140	9162
				9170	9173	9176	9185	9187	9206	9310	9314	9383	9417	9439	9477
				9481	9491	9597	9607	9660	9730	9741	9756	10148
	FE SHRT			1892 #	9905
	FM WRITE		1890 #	3740	3798	3905	3936	4046	4081	4884	5137	5193	5313	5676
				5682	5689	5693	5696	5701	5717	5736	5737	5747	5749	5750	6281
				6289	6297	6356	6358	6412	6432	6437	6439	6445	6448	6465	6466
				6468	6469	6473	6474	6475	6477	6502	6505	6517	6523	6542	6549
				6559	6585	6592	6594	6599	6619	6659	6662	6760	6892	6969	6976
				7054	7056	7067	7069	7097	7140	7268	7269	7270	7271	7330	7331
				7604	7625	7627	7630	7695	7751	8056	8099	8102	8104	8107	8149
				8185	8190	8192	8194	8243	8255	8256	8257	8259	8267	8280	8300
				8302	8304	8325	8336	8347	8391	8404	8408	8430	8431	8437	8442
				8444	8448	8449	8451	8473	8483	8486	8503	8517	8529	8538	8540
				8549	8550	8560	8562	8564	8589	8591	8613	8618	8675	8703	8705
				8727	8732	8758	8770	8773	8774	8805	8811	8812	8815	8830	8836
				8960	8961	8966	8995	9549	9723	9724	9754	9763	9764	9765	9770
				9773	9789	10001	10124	10143	10155	10159	10161	10164	10168	10169	10175
				10177	10178	10181
	LD AR0-8		1882 #	3942	5108	5109	5110	6489	6543	6597	6728	6733	6755	6933
				6935	6941	6974	7055	7068	7600	7730	7732	7734	7737	7740	7744
				7746	7749	7750	7763	7799	7811	7814	7822	7823	7830	7834	7838
				7842	8058	8253	8433	8438	8441	8552	8643	8706	8707	8708	8709
				8710	8711	8731	8778	8800	8802	8871	8909	9015	9234	9335	9347
				9418	9574	9623	9675	9936	10004	10006	10158
	LD AR18-35		1884 #
	LD AR9-17		1883 #	4716	4890	5095	9360	10128
	LD VMA HELD		1913 #
	LOAD IR			1894 #	3777	3788	5150	8064
	LONG EN			1903 #	3840	7868	7905	7976	8081
	MBOX CTL		1901 #	9321	9393	9397	9402	9404	9408	9410	9781	9783	9785	10007
				10145
	PCF_#			1891 #	4815	4820	4833	4856	5138	5264	5294	5402	5418	5583	5684
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-14
; 							Cross Reference Listing					

				5735	5904	6088	6369	6607	6748	6752	6753	6891	6936	6940	7094
				7095	7380	7385	7388	7473	7483	7486	7606	7778	7968	8579	9151
				9152
	REG CTL			1888 #	5207	5260	5437	5438	5439	5440	5451	5452	5453	5454	5513
				5554	5566	5647	6213	6213	6384	6401	6409	6409	6409	6570	6571
				6572	6578	6683	6703	6967	7064	7085	7186	7221	7280	7284	7307
				7479	7678	8222	9231
	SEL VMA			1897 #	8201	9751	9769	10118	10120
	SPEC INSTR		1895 #	3730	3783	4725	4740	4743	5149	9492	9502	9548	9839	10122
	SR_#			1896 #	3919	3921	3927	4772	4784	5088	5722	5742	5789	5861	5925
				6125	6223	6227	6237	6300	6301	6327	6354	6376	6404	6414	6590
				6618	6675	6705	6764	7138	7157	7167	7217	7224	7228	7267	7275
				7287	7290	7311	7316	7328	7503	7756	7762	8262	8266	8272	8274
				8285	8290	8291	8292	8305	8329	8331	8337	8389	8390	8399	8450
				8484	8513	8563	8580	8623	8636	8639	8677	8826	8859	8887	8978
				9012	9136	10056	10060	10064	10068	10072	10076	10080	10084	10088	10092
				10096	10100
	VMA DEC			1911 #	4040	7801	8957	9506
	VMA INC			1912 #	4038	4073	4686	4687	4709	4880	4894	5056	5064	5079	5102
				5104	5650	5705	6253	6430	6929	7051	7203	7387	7485	7690	7707
				7796	8479	8481	8617	8873	8953	9006	9286	9508	9527	9631	10130
				10132	10133
	VMA_#			1906 #	5052	5061	5075	9480	9515	9517	9519	9521	10115
	VMA_#+AR32-35		1909 #	8212	9800	9802
	VMA_#+MODE		1908 #	5096
	VMA_#+PI*2		1910 #	9553	9554	9568
	VMA_#+TRAP		1907 #	3776	3778
(U) DIAG FUNC			2222 #
	CONI APR(L)		2241 #	9258
	CONI APR(R)		2237 #	9257
	CONI MTR		2247 #	9459
	CONI PAG		2250 #	9377
	CONI PI(L)		2236 #	9308
	CONI PI(PAR)		2249 #	9306
	CONI PI(R)		2235 #	9304
	CONO APR		2228 #	9254	9260
	CONO MTR		2226 #	9456
	CONO PAG		2230 #	9373
	CONO PI			2229 #	9301
	CONO TIM		2227 #	9444
	DATAI APR		2243 #	9227
	DATAI PAG(L)		2239 #	5082	9354
	DATAO APR		2231 #	9220
	DATAO PAG		2232 #	9325
	LD AC BLKS		2233 #
	LD PA LEFT		2224 #	9471	9473
	LD PA RIGHT		2225 #	9472
	LD PCS+CWSX		2234 #	4678	5084
	RD CACHE CNT		2244 #	9434	9490
	RD EBOX CNT		2242 #	9433	9489
	RD EBUS REG		2251 #	4776	9364	9381	9740	9755
	RD INTRVL		2245 #	9447
	RD MTR REQ		2248 #	9478
	RD PERF CNT		2240 #	9432	9488
	RD PERIOD		2246 #	9449
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-15
; 							Cross Reference Listing					

	RD TIME			2238 #	9431	9487
	.5 USEC			2223 #
(U) DISP			1920 #
	BYTE			1933 #	4740	5208	7374	7377	7466	7470	7679	7711	7779	8200	8505
				8755	8791	8915	8945	8946	8947	8960	8992	9120	9403	9409	9961
	DIAG			1921 #
	DIV			1930 #	5540	5543	5548	5560	5594	5595	5596	5597	5598	5599	5600
				5601	5625	5626	5627	5628	7638	8542
	DRAM A RD		1923 #	3822	3823	3824	3825	3833	3834	8077	8085	8087
	DRAM B			1932 #	3971	3980	3982	4020	4041	4176	4178	4181	4184	4188	4192
				4201	4203	4211	4212	4217	4218	4224	4226	4241	4251	4266	4281
				4291	4301	4311	4321	4331	4342	4352	4362	4372	4467	4470	4472
				4475	4777	4819	5350	5363	5401	5417	5418	5489	5490	5557	5569
				5654	5803	5861	5865	6223	6227	6255	6432	8076	8097	8259	8439
				8447	8622	8678	8715	8719	9126	9147	9310	9385	9429	9437
	DRAM J			1922 #	3864	3870	3876	3883	3889	8100	8103	8105	8108
	EA MOD			1935 #	3775	3844	4649	7381	7386	7474	7484	7870	7871	7907	7908
				7922	7923	7968	7978	7979	8065	8083	8084	8759	8774	8813	8816
				9774
	MUL			1929 #	5395	5397	5435	5442	5444	5446	5448	5457	5459	5461	5462
				5467	5670	6565
	NICOND			1927 #	3905	3922	5137	5193
	NORM			1934 #	5831	5874	5913	5925	6125	6127	6153	6158	6166	6176	6209
				6294	6343	6362	6382	6390	6395	6398	6399	6528	6580	6601	6667
				6669	6691	6696	6699	6701	6710	6766	6964	6978	7059	7070	7101
	PG FAIL			1925 #	9730	9741
	RETURN			1924 #	3808	4055	4704	4709	4720	4856	4862	5098	5104	5437	5438
				5439	5440	5451	5452	5453	5454	5552	5553	5564	5565	5582	5613
				5616	5629	5630	5631	5632	6000	6002	6314	6316	6479	6490	6869
				6870	6876	6877	7176	7780	7781	7787	7790	7802	7804	7811	7814
				7822	7823	7830	7834	7838	7842	7860	7862	7873	7875	7924	7926
				7930	7932	7951	7954	7970	7971	7980	7982	8004	8005	8014	8023
				8166	8188	8192	8202	8222	8231	8350	8677	8681	8705	8716	8721
				8722	8732	8739	8778	8787	9015	9177	9190	9202	9206	9366	9421
				9467	9508	9529	9625	9630	9633	9654	10037
	SH0-3			1928 #	5080	6743	7086	7379	7383	7405	7406	7472	7476	7515	7517
				7592	7778	7821	8692	8694	8838	8855	9484	9503	9551	9570	9595
				9610	9812	9901
	SIGNS			1931 #	5497	5518	5520	5721	5723	7163	7167	7169	7187	7199	7204
				7220	7222	7271	7285	7308	7626	8296	8300	8327	8347	8390	8391
				8405	8437	8558	8615	8640	8676	8704	8727	9771	9801	9803
	SR			1926 #	5742	5743	5748	6128	6130	6132	6134	6161	6163	6164	6174
				6193	6213	6392	6409	6750	6759	7193	7206	7677	7977	7993	8348
				9763	10044	10155	10159	10178	10181
(U) EA CALC			2118 #
	A IND			2127 #	3813	3815	3817	3819	3835	3837	8078	8079
	BYTE IND		2132 #	7387	7485	7863	7865	7900	7902	7929	7931	7972	7973
	BYTE LD			2128 #	7859	7861	7872	7874
	BYTE RD			2129 #	7378	7382	7970	7971	7980	7981
	BYTE RD PC		2130 #
	BYTE RPW		2131 #	7471	7475	7895	7897	7909	7912
	LD ARX+WR		2140 #	8953
	LD AR(EA)		2138 #	7155
	LD AR+WR		2139 #	8932	8935
	POP AR			2134 #	4827
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-16
; 							Cross Reference Listing					

	POP AR-ARX		2136 #	4829
	POP ARX			2135 #
	PUSH			2133 #	4804	4809
	WRITE(E)		2137 #	4851	7160	7161
(U) EBUS CTL			2206 #	4679	5087	9185
	DATAI			2220 #	9194	9607	9660
	DATAO			2219 #	9140	9195	9597	10148
	EBUS DEMAND		2210 #	9170
	EBUS NODEMAND		2211 #	9173
	GRAB EEBUS		2207 #	3827	3828	3829	3830	4675	9206	9477	9730
	IO INIT			2216 #	9187	9190
	REL EBUS		2209 #	9176	9310
	REL EEBUS		2221 #	4777	9126	9383	9417	9439	9481	9491	9741	9756
	REQ EBUS		2208 #	9162	9314
(U) EXP TST			2059 #	5514
	AR_EXP			2060 #	6213	6409
(U) FE				1779 #	9905
	SCAD			1780 #	3845	3869	3882	3905	3922	4725	4737	4743	5137	5193	5205
				5207	5219	5228	5239	5314	5317	5387	5389	5392	5394	5396	5414
				5435	5437	5438	5439	5440	5442	5444	5446	5448	5451	5452	5453
				5454	5457	5459	5461	5462	5467	5496	5507	5510	5511	5513	5517
				5540	5543	5548	5550	5560	5562	5594	5595	5596	5597	5598	5599
				5600	5601	5612	5615	5625	5626	5627	5628	5629	5630	5631	5632
				5664	5670	5679	5707	5713	5734	5802	5815	5824	5825	5828	5871
				5874	5901	5911	5914	6053	6065	6081	6085	6126	6129	6131	6133
				6153	6157	6160	6162	6166	6173	6175	6212	6252	6263	6312	6333
				6356	6358	6366	6378	6380	6381	6384	6385	6405	6408	6457	6461
				6463	6472	6526	6552	6557	6563	6565	6592	6594	6596	6604	6663
				6677	6678	6680	6683	6685	6706	6711	6945	6956	6959	6963	6975
				7049	7063	7081	7380	7385	7387	7405	7406	7448	7449	7473	7479
				7480	7483	7485	7498	7500	7515	7517	7555	7556	7592	7593	7595
				7597	7633	7636	7638	7641	7642	7680	7684	7686	7700	7751	7788
				7791	7925	7952	7968	8017	8019	8148	8156	8164	8209	8217	8253
				8295	8386	8433	8526	8537	8539	8552	8584	8587	8590	8592	8703
				8705	8722	8740	8758	8764	8773	8774	8798	8811	8812	8815	8834
				8868	8882	8884	8888	8897	8900	8917	8920	8963	8971	9281	9333
				9401	9407	9421	9570	9622	9659	9668	9734	9739	9782	9784	9786
				9805	9806	9811	9854	9871	9893	9901	9916	9936	9962	9964	9999
				10016	10021	10052
(U) FETCH			2107 #
	COMP			2110 #	4507
	JFCL			2114 #	4760
	JUMP			2113 #	4574	4589	4606	4613
	SKIP			2111 #	4524	8393
	TEST			2112 #	4467	4469	4475	8335	9130	9132	9134
	UNCOND			2108 #
(U) FLAG CTL			2086 #
	DISMISS			2090 #	4659	4661	4673	9144	9598
	DISMISS+LD		2092 #
	HALT			2093 #	4651
	JFCL			2088 #	4759
	JFCL+LD			2089 #	4762
	PORTAL			2095 #	4628
	RSTR FLAGS		2087 #	4653	4669	4689
	SET FLAGS		2094 #	3793	5107	10134
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-17
; 							Cross Reference Listing					

(U) FMADR			1740 #
	AC+#			1747 #	8186	8190	8192	8261	8415	8415	8438	8486	8722	8784	8784
				8794	8796	8797	8805	8811	8812	8815	8860	8862	8947	8960	8961
				8966	9006	10157	10159	10164
	AC0			1741 #	3863	3905	3987	3996	4025	4065	4081	4176	4178	4181	4184
				4187	4191	4205	4241	4251	4266	4281	4291	4301	4311	4321	4331
				4352	4362	4475	4479	4480	4481	4507	4574	4588	4589	4605	4613
				4804	4809	4827	4829	4830	4849	4884	4888	4891	5125	5127	5130
				5133	5135	5137	5167	5178	5193	5207	5207	5209	5228	5230	5251
				5271	5272	5312	5350	5362	5386	5389	5413	5487	5495	5509	5555
				5567	5613	5616	5653	5677	5682	5708	5811	5871	5901	6052	6265
				6274	6281	6289	6292	6300	6301	6333	6365	6412	6447	6464	6468
				6501	6502	6526	6546	6557	6587	6604	6664	6760	6892	7073	7076
				7098	7134	7140	7239	7271	7479	7515	7517	7624	7627	7630	8047
				8148	8156	8245	8259	8285	8338	8360	8408	8429	8442	8444	8472
				8473	8506	8515	8579	8589	8591	8618	8677	8693	8695	8703	8705
				8720	8732	8826	8836	8980	8995	9011	9013	10168	10176	10177	10180
				10181
	AC1			1742 #	3936	4046	4067	4072	5205	5227	5270	5313	5484	5649	5689
				5705	5736	5737	5742	5743	6258	6297	6298	6324	6325	6353	6354
				6356	6358	6361	6469	6475	6479	6518	6524	6525	6540	6553	6589
				6590	6592	6594	6600	6663	7084	7266	7269	7328	7330	7695	8182
				8194	8290	8300	8388	8388	8427	8431	8504	8740	8740	8758	8773
				8774	8971	8971	10056	10155	10169
	AC2			1745 #	5693	5703	5747	7267	7270	7329	7331	8185	8430	8762	8763
				8770
	AC3			1746 #	5647	5676	5691	5696	5749	5750	8150	8164	8244	8255	8256
				8263	8270	8273	8280	8302	8304	8336	8345	8347	8404	8445	8448
				8449	8457	8483	8513	8517	8527	8529	8551	8560	8633	8719	8727
				8739	8914	8932	8934	8991	10064	10088	10092	10100	10161	10170	10175
				10178
	VMA			1744 #	3777	3788	3840	3841	3869	3875	3882	3888	3900	3902	3908
				3910	4040	4042	4043	4055	4073	4079	4524	4654	4685	4687	4689
				4696	4709	4813	4815	4818	4820	4832	4832	4834	4834	4856	4862
				4862	4880	4894	5056	5064	5075	5078	5088	5100	5103	6926	6954
				6967	7050	7064	7189	7190	7192	7193	7201	7203	7206	7223	7225
				7228	7230	7277	7278	7286	7288	7290	7292	7310	7312	7316	7317
				7376	7380	7384	7389	7410	7412	7413	7414	7415	7416	7417	7419
				7426	7428	7429	7430	7431	7432	7434	7441	7443	7444	7446	7448
				7449	7469	7473	7477	7483	7497	7497	7522	7523	7524	7525	7526
				7527	7528	7530	7536	7537	7538	7539	7540	7541	7543	7549	7550
				7551	7553	7555	7556	7577	7797	7801	7826	7868	7869	7905	7906
				7968	7976	7977	7991	7993	8012	8012	8014	8081	8082	8242	8365
				8563	8632	8691	8787	8837	8936	8945	8954	8956	8958	8976	8981
				9005	9008	9125	9129	9131	9140	9147	9507	9527	9564	9588	9597
				9598	9810	9825	9845	9871	9886	9893	9916	10135
	XR			1743 #	3775	3815	3819	3823	3825	3834	3837	3844	4668	7861	7865
				7870	7871	7874	7897	7902	7907	7908	7912	7922	7923	7931	7971
				7973	7978	7979	7981	8077	8079	8083	8084	8087
	#B#			1748 #	3740	3798	4656	4692	4808	4878	4883	5050	5089	5093	5097
				5101	5701	5717	5719	5739	6432	6437	6439	6445	6448	6452	6457
				6461	6465	6466	6467	6472	6473	6474	6476	6477	6503	6505	6507
				6508	6517	6519	6520	6523	6542	6549	6550	6559	6585	6598	6599
				6615	6619	6659	6660	6662	6666	6668	6693	6732	6739	6869	6870
				6876	6877	6912	6913	6920	6921	6922	6969	6976	7054	7056	7067
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-18
; 							Cross Reference Listing					

				7069	7088	7091	7097	7268	7604	7625	7650	7677	7751	8056	8099
				8102	8104	8107	8149	8187	8188	8227	8229	8230	8243	8257	8267
				8269	8278	8283	8301	8325	8344	8391	8400	8437	8451	8454	8459
				8465	8503	8538	8540	8549	8550	8559	8562	8564	8565	8576	8577
				8611	8613	8627	8628	8675	8678	8679	8680	8681	8689	8714	8716
				8721	8726	8728	8729	8830	8925	8927	8973	8974	9009	9549	9551
				9584	9601	9619	9633	9653	9723	9724	9754	9763	9764	9765	9770
				9773	9789	9813	9823	9830	9838	9852	9863	9868	9875	9876	9877
				9892	9902	9903	9914	9929	9940	9946	9955	9966	9970	9980	9992
				9997	10001	10005	10015	10017	10020	10024	10026	10036	10043	10104	10108
				10117	10124	10125	10142	10143	10144	10162	10166	10171	10173	10183
(U) ISTAT			1993 #
	OPTIONS			1997 #	9234
(U) J				1631 #	3808	3822	3823	3824	3825	3833	3834	3864	3870	3876	3883
				3889	4055	4704	4709	4720	4856	4862	5098	5104	5437	5438	5439
				5440	5451	5452	5453	5454	5552	5553	5564	5565	5582	5613	5616
				5629	5630	5631	5632	6000	6002	6314	6316	6479	6490	6869	6870
				6876	6877	7176	7780	7781	7787	7790	7802	7804	7811	7814	7822
				7823	7830	7834	7838	7842	7860	7862	7873	7875	7924	7926	7930
				7932	7951	7954	7970	7971	7980	7982	8004	8005	8014	8023	8100
				8103	8105	8108	8166	8188	8192	8202	8222	8231	8350	8677	8681
				8705	8716	8721	8722	8732	8739	8778	8787	9015	9177	9190	9202
				9206	9366	9421	9467	9508	9529	9625	9630	9633	9654	10037
	ACNORM			6662 #	6457
	ACNRM1			6666 #	6660
	ADD			5350 #	4603	4604	5343	5344	5345	5346
	ADJBP			7592 #	7575
	ADJDIV			7638 #	7643
	ADJOIN			7624 #	7756
	ADJOWG			7729 #	7592
	ADJP			7700 #	7685	7689	7701
	ADJPDL			5133 #	5128
	ADJSP			5124 #	5116
	ADJTRP			5137 #	5134
	ADJTWG			7684 #	7679
	ADJUST			7677 #	7649	7650
	AND			4241 #	4234	4235	4236	4237
	ANDCA			4251 #	4244	4245	4246	4247
	ANDCB			4301 #	4294	4295	4296	4297
	ANDCM			4266 #	4259	4260	4261	4262	4395	4404
	AOBJ			4613 #	4609	4610
	AOJ			4589 #	4578	4579	4580	4582	4583	4584
	AOJA			4587 #	4581
	AONJ			4588 #	4577
	AONS			4539 #	4528
	AOS			4540 #	4529	4530	4531	4532	4533	4534	4535
	APRBI			9230 #	9025
	APRBO			9221 #	9027
	APRCI			9255 #	9030	9031	9032
	APRCO			9252 #	9029
	APRCO7			9260 #	9254
	APRDI			9226 #	9026
	APRDO			9219 #	9028
	ARJMP			4676 #	4669	4679	4689	4696	10135
	ARSWAP			4720 #	9419	10128
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-19
; 							Cross Reference Listing					

	ASH			5251 #	5155
	ASHC			5270 #	5159
	ASHL			5258 #	5254
	ASHL1			5284 #	5271
	ASHL2			5288 #	5297
	ASHL3			5292 #	5294
	ASHL4			5296 #	5293
	ASHR1			5275 #	5277
	ASHR2			5279 #	5275
	ASHX			5302 #	5279
	AWAIT			9564 #
	B2DFPF			10166 #	10064
	B2DPF			10168 #	8623	10084
	B2DPF2			10171 #	10166
	BACKD			10157 #	10068	10072	10076	10096
	BACKLD			7314 #	7311
	BACKS			10154 #	10056
	BACLUP			7307 #	7316
	BD1			8522 #	8507
	BD2			8525 #	8522	8529
	BD3			8527 #	8523
	BD4			8537 #	8526	8542
	BD6			8542 #	8537
	BD7			8549 #	8538	8540
	BD8			8557 #	8555
	BD9			8560 #	8565
	BDD1			8610 #	8580	8643
	BDD2			8620 #	8615
	BDD3			8627 #	8622
	BDD4			8630 #	8627
	BDD5			8632 #	8633
	BDD6			8634 #	8632
	BDD7			8636 #	8628	8637
	BDDR1			8513 #
	BDDR4			8613 #	8519
	BDDV1			8576 #	8560
	BDDV2			8583 #	8578
	BDEC			8503 #	8154
	BDF1			8562 #	8559
	BEXT2			8097 #	8077	8086
	BFETCH			7970 #	7978	8759	8774	8813	8816
	BLBACK			7310 #	7273
	BLK1			9148 #	9120
	BLK2			9147 #	9148	9149
	BLK3			9151 #	9147
	BLK4			9134 #	9129
	BLKIO			9119 #	9094	9096	9105	9107
	BLSTOR			7186 #	7192
	BLT			7134 #	7130
	BLTFIX			10080 #	7230
	BLTLOD			7167 #	7162
	BLTLUP			7189 #	7164	7168	7170
	BLTPGF			7238 #	10080
	BLTPX			7216 #	7157
	BLUUO			5022 #	4918	4920	4921	4922	4923
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-20
; 							Cross Reference Listing					

	BYTEA			7968 #	9680
	BYTEI			7976 #	7972	8773	8811
	BYTINC			7787 #	7779
	BYTIND			7922 #	7868	7905	7976	8081
	CAIM			4507 #	4487	4488	4489	4491	4492	4493	4497	4498	4499	4501	4502
				4503
	CAPHGH			7602 #	7603
	CAPLOW			7597 #	7598
	CDBLST			6416 #	6764
	CHALT			4725 #	3798	4651
	CHKAC			9652 #	9638	9643	9648
	CLEAN			10052 #	7193	7206	7977	7993	8348	9763	10044	10155	10159	10178	10181
	CLEFT			5239 #	5229	5240
	CLRPT			9417 #	9336	9391
	CMPDST			8415 #	8399
	CMPS			8360 #	8150
	CMPS1			8364 #	8362
	CMPS3			8385 #	8412
	CMPS4			8386 #	8365
	CMPS5			8399 #	8391
	CMPS6			8408 #	8405
	CMPS7			8411 #	8408
	CMPSX			8393 #	8385
	CMTR			9490 #	9348
	CMTR1			9498 #	9490
	CNV01			8217 #	8218
	CNV02			8219 #	8217
	CNV2WD			8209 #	8201
	COMPEA			3833 #	3775
	CONO			9123 #	9098	9109
	CONS			9121 #	9100	9101	9111	9112
	CONT			3795 #
	CRIGHT			5233 #	5236
	D2BPF			10161 #	10060
	DASMD			5649 #	5637	5638	5639
	DBABT			8472 #	8459	8465
	DBFLGS			8429 #	8431
	DBIN			8427 #	8153
	DBIN2			8464 #	8458
	DBIN3			8467 #	8464
	DBINGO			8433 #	8429
	DBINLP			8454 #	8468
	DBLST			4081 #	6416
	DBS1			8444 #	8438
	DBS2			8446 #	8442
	DBS3			8450 #	8448
	DBST			8442 #	8440
	DBXIT			8479 #	8456
	DDIV			5647 #	5640
	DDIV0			5701 #	5647
	DDIV1			5710 #	5714	5715
	DDIV2			5717 #	5711
	DDIV3			5730 #	5721	5723
	DDIV4			5734 #	5731
	DDIV6			5739 #	5736
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-21
; 							Cross Reference Listing					

	DDVLP			5625 #	5625	5626	5627	5628	5734	8590
	DDVSUB			5627 #	6368	6606	8585	8588	8592
	DDVX1			5745 #	5742
	DDVX2			5747 #	5745	5746
	DEAIND			7905 #	7486	7901
	DEPBYT			7497 #	7479
	DEPOWG			7499 #	7523	7524	7525	7526	7527	7528	7537	7538	7539	7540	7541
				7555	7556
	DEPTWG			7483 #	7477
	DEXCHK			9628 #	9600	9612
	DFAS			6258 #	6260
	DFAS1			6274 #	6258
	DFAS2			6276 #
	DFAS3			6286 #	6278
	DFAS4			6292 #
	DFAS5			6293 #	6287	6300	6301
	DFAS6			6296 #	6282
	DFDV			6353 #	6267
	DFDV1			6365 #	6353	6354
	DFLOAT			6252 #	6245	6246	6247	6248
	DFMP			6324 #	6263
	DFMP1			6329 #	6324	6326
	DFMP2			6340 #	6341
	DIV			5484 #	5477	5478	5479	5480
	DIV+			5597 #	6357	6593
	DIV-			5596 #	5903	6359	6595
	DIV1			5495 #	5485
	DIVLP			5594 #	5549	5561	5594	5595	5596	5597	5741
	DIVS1			5540 #	5497	5518	5520	5542	7641
	DIVS2			5543 #	5545
	DIVS3			5548 #	5541	7639
	DIVS4			5560 #	5544
	DIVX			5611 #	5598	5599	5600	5601
	DLOAD			4046 #	4042	5243	6404	6705
	DMOVEM			4065 #	4060
	DMOVE			4038 #	4032
	DMOVNM			4067 #	4061
	DMOVN			4037 #	4033
	DMUL1			5682 #	5684
	DMUL2			5688 #	5683
	DMULT			5670 #	5664
	DMVM1			4079 #	9440
	DNHI			6401 #	6378
	DNNEG			6392 #	6376
	DNNEG1			6397 #	6393
	DNORM			6374 #	6294	6343	6362	6390	6395	6398	6399
	DNSHFT			6387 #	6405
	DNSUB			4053 #	4043	4068
	DNTRY			6343 #	6284	6401
	DNZERO			6404 #	6374
	DODIAG			9283 #
	DPB			7469 #	7462
	DPB1			8012 #	8786	9672
	DPB2			8017 #	8013
	DPEA			7895 #	7474	7484	7907
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-22
; 							Cross Reference Listing					

	DRND1			6408 #	6383
	DROUND			6382 #	6380	6384
	DSHIFT			5243 #	3899	3901	5213	5221	5234
	DTEVEC			9577 #	9574
	EBUSI			9194 #	9607	9660
	EBUSO			9195 #	9140	9597	10148
	EBUSW			9172 #	9174
	EBUSX			9169 #	9194	9195
	EDDISP			8840 #
	EDDSNG			8938 #	8946
	EDEX0			8932 #	8863
	EDEX1			8963 #	8960	8966
	EDEX2			8945 #	8933
	EDEXMD			8934 #
	EDEXX			8958 #	8938
	EDFL1			6436 #	6439
	EDFLDS			8909 #	8861
	EDFLOT			6430 #	6423	6424	6425	6426
	EDFLTX			9005 #	9002
	EDFLT			9000 #	8916	8993
	EDFLT1			9008 #	9000	9001	9003
	EDFPUT			8980 #	8986
	EDIT			8824 #	8152
	EDIT1			8834 #	8832
	EDITLP			8835 #	8903
	EDMPUT			8981 #	8927
	EDMSG			8925 #	8842
	EDNOP			8882 #	8843	8844	8845	8854	8884	8920
	EDNXT1			8886 #	8883	8898	8901	8918	8964
	EDNXT2			8897 #	8886
	EDNXT3			8903 #	8890	8894
	EDOPR			8854 #	8841
	EDSEL			8971 #	8859
	EDSEND			8920 #	8909	8980
	EDSF1			8986 #	8976
	EDSFIL			8976 #	8925
	EDSFLT			8991 #	8977	8994
	EDSKPT			8878 #	8847	8849
	EDSKP			8884 #	8850
	EDSPUT			8978 #	8981	8995
	EDSSIG			8914 #	8860
	EDSTOP			8868 #	8858	8979
	EDVCHK			6610 #	6605	6611
	EDVCH1			6614 #	6610
	EDVCH2			6619 #
	EDVCH3			6622 #
	EEOV			6755 #	6748	6752
	EF1			6447 #	6437
	EF10			6519 #	6510
	EF11			6526 #	6518	6524
	EF12			6530 #	6516	6522
	EF3A			6459 #	6455
	EF3B			6462 #	6460
	EF5			6471 #	6456
	EF5A			6501 #	6469	6477
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-23
; 							Cross Reference Listing					

	EF5B			6479 #	6465	6474
	EFDV0			6585 #	6445
	EFDV1			6604 #	6589	6590
	EFIW			3813 #	3845
	EFMP			6539 #	6442
	EFMP1			6542 #	6539
	EFMPP1			6562 #	6563
	EFMPP2			6565 #	6562
	EFMPP3			6574 #	6570	6571
	EIGHT7			7744 #	7735
	EMTR			9489 #	9346
	EMTR1			9497 #	9489
	ENFNL0			6746 #	6756
	ENFNL1			6758 #	6745	6747
	ENHI			6703 #	6677
	ENNEG			6693 #	6675
	ENNEG1			6698 #	6694
	ENORM			6672 #	6528	6580	6601	6667	6691	6696	6699	6701	6766	6979	7059
				7070	7101
	ENSHFT			6687 #	6706
	ENTRY			6766 #	6703
	ENZERO			6705 #	6673
	EQV			4311 #	4304	4305	4306	4307
	ERND1			6708 #	6682
	ERND2			6722 #
	EROUND			6681 #	6679	6684
	ESEND			9140 #	9565
	EXCH			3987 #	3967
	EXMSK			3797 #	3740
	EXPD			6310 #	6274
	EXPD1			6315 #	6312
	EXPDIF			6488 #	6454	6459
	EXT01			8186 #	8184	8194	8503
	EXT02			8194 #	8185
	EXT2			8099 #	8076
	EXT2WD			8182 #	8149	8243	8824
	EXT3			8107 #	8101	8109
	EXTEND			8046 #	8042
	EXTF2			8065 #
	EXTI			8081 #	8078
	EXTLA			8076 #	8083
	FAD			5788 #	5757	5763	5764
	FADFSB			5814 #	5811
	FADR			5801 #	5766	5768	5769	5789
	FADRI			5799 #	5767
	FAS			5811 #	5812
	FAS1			5819 #	5815
	FAS2			5821 #	5817
	FAS3			5827 #	5821
	FAS5			5830 #	5824	5825
	FDV			5860 #	5880	5886	5887
	FDVCHK			5996 #	5902	5997	6367
	FDVCK1			5999 #	5996	6623
	FDVGO			5901 #	5867
	FDVNEG			5925 #	5912	5926
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-24
; 							Cross Reference Listing					

	FDVR			5866 #	5861	5889	5891	5892
	FDVRI			5864 #	5890
	FINI			3922 #	3936	4478	4627	4762	4884	5696	8486	9135	9136
	FIX			6085 #	6058
	FIX1			6088 #	6082
	FIX2			6098 #	6096
	FIXR			6081 #	6059
	FLGTST			8164 #	8148	8156
	FLTR			6065 #	6060
	FMP			5861 #	5836	5842	5843
	FMPR			5870 #	5845	5847	5848
	FMPRI			5865 #	5846
	FORFRZ			7328 #	7333
	FORSUB			7333 #	7326
	FSB			5789 #	5771	5777	5778
	FSBR			5802 #	5780	5782	5783
	FSBRI			5800 #	5781
	FSC			6051 #	6014
	GETAC1			4072 #	4065
	GETEEB			9205 #	4775	9219	9226	9253	9256	9324	9353	9372	9376	9427	9443
				9446	9455	9458	9470
	GETNXT			4709 #	4634	4688
	GETSC			8028 #	8549	8926
	GETSRC			8740 #
	GLBIND			7929 #	7923
	GSRC			8739 #	8327	8676
	GSRC1			8754 #	8389	8972
	GSRC2			8758 #	8755	8761
	GSRC3			8760 #	8765
	GSRC4			8773 #	8762
	GSRC5			8767 #	8764
	GSRC6			8770 #	8767
	GTAR08			9624 #	3942	9675
	GTCST			9950 #
	GTEBUS			9161 #	9125
	GTEEB1			9206 #	5107
	GUDSIZ			7479 #	7480	7896	7899	7911	7914
	HALT			4633 #	4635
	HALT1			4736 #	4741
	HALT2			4740 #	4737
	HLL			4178 #	4087	4131	7551
	HLLE			4221 #	4117	4119	4120
	HLLO			4226 #	4107	4108	4109	4110
	HLLZ			4224 #	4097	4098	4099	4100
	HLR			4184 #	4134	4135
	HLRE			4215 #	4164	4166	4167
	HLRM			4191 #	4136
	HLRO			4218 #	4154	4155	4156	4157
	HLRS			4192 #	4137
	HLRZ			4217 #	4144	4145	4146	4147	7443
	HRL			4181 #	4092	4093
	HRLE			4208 #	4122	4123	4124	4125
	HRLM			4187 #	4094	7550
	HRLO			4212 #	4112	4113	4114	4115
	HRLS			4188 #	4095
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-25
; 							Cross Reference Listing					

	HRLZ			4211 #	4102	4103	4104	4105
	HRR			4176 #	4089	4129	4130
	HRRE			4198 #	4159	4160	4161	4162
	HRRO			4203 #	4149	4150	4151	4152
	HRRZ			4201 #	3963	4139	4140	4142	7444
	HRRZM			4205 #	4141
	IBP			7573 #	7569
	IBPS			7951 #	9679
	IBPTST			7575 #
	IDIV			5487 #	5472	5473	5474	5475
	IDIVGO			5507 #	5488
	IDPB			7466 #	7461
	IDST			8791 #	8416	8784
	IDST2B			8815 #	8794
	IDST2			8794 #	8792	8800
	IDST3			8800 #	8795
	IDST4			8808 #	8796
	IDST5			8802 #	8798
	IDST6			8805 #	8802
	IDST7			8811 #	8808
	IDST8			8812 #	8809
	IDVLNG			5519 #	5514
	IDVOPT			5517 #	5508	5512
	IFNOP			3907 #	4256	5904	6088	6369	6607	9215	9321	9404	9410
	IFSTAC			3909 #	4257	4821
	ILDB			7373 #	7368
	ILLOWG			7826 #	7446	7553
	IMDONE			5401 #	5402
	IMFAST			5392 #	5388
	IMLONG			5396 #	5389
	IMUL			5386 #	5368	5369
	IMULM			5389 #	5370	5371
	INCRBP			7778 #	7374	7467	7576
	INDR1			3844 #	3840
	INDRCT			3840 #	3814	3816	3818	3820	3836
	INTRPT			3746 #
	IO			9125 #	9095	9097	9099	9106	9108	9110	9151	9152
	IOCHK			9200 #	7826	9119	9212	9221	9230	9282	9320
	IOFET			9143 #	9131
	IOPGF			10142 #	10121
	IOR			4291 #	4284	4285	4286	4287	4431	4440
	IOTEND			9129 #	9310	9385
	ISOEXP			6869 #	6436	6440	6444	6447	6547	6614	6931	7074
	JFBITS			5332 #	5327
	JFCL			4755 #	4749
	JFFO			5312 #	5158
	JFGRUP			5317 #	5318
	JRA			4890 #	4870
	JRA1			4811 #	4892
	JRSNDX			4667 #	4649
	JRST			4627 #	4618
	JRSTF			4630 #	4661
	JRSTOK			4703 #	4630	4633	4636	4637	4639
	JSA			4888 #	4869
	JSA1			4894 #	4888
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-26
; 							Cross Reference Listing					

	JSP			4874 #	4868
	JSP1			4883 #	4874
	JSR			4876 #	4867
	JSR1			4880 #	4878
	JSTAC			4813 #	4809	4815
	JUMP			4574 #	4563	4564	4565	4567	4568	4569
	JUMPA			4572 #	4566
	KEEPCL			9407 #	9397	9409
	KEEPME			9393 #	9338
	KMOVED			9940 #	9934
	L-BDEC			5041 #	8128	8129
	L-CMS			5045 #	8117	8118	8119	8122	8123	8124
	L-DBIN			5039 #	8126	8127
	L-DFLT			7062 #	6921
	L-DFSC			7073 #	6922
	L-DITE			5026 #	6907
	L-EDBL			6966 #	6913
	L-EDIT			5037 #	8121
	L-EFSC			5030 #	6909
	L-FLTR			7048 #	6920
	L-FSC2			7088 #	7094	7095
	L-FSC3			7097 #	7090	7093
	L-GTPI			5008 #	6899
	L-GTSP			6925 #	6912
	L-GTS1			6937 #	6935
	L-GTS6			6941 #	6939
	L-GTS7			6961 #	6958
	L-MVS			5043 #	8131	8132	8133	8134
	L-SFTE			5010 #	6900
	L-SITE			5028 #	6908
	LDB			7376 #	7369
	LDB1			7991 #	8417	8756	9664
	LDB2			7996 #	7992
	LDEA			7859 #	7381	7386	7870
	LDIMM			9905 #	9916
	LDIND			9838 #	9816	9839	9910
	LDIND1			9844 #	9838
	LDPCS			4675 #	4672
	LDPT			9922 #	9907
	LDPT1			9934 #	9925	9936
	LDSHR			9914 #	9908
	LEAIND			7868 #	7388	7864
	LOWBIT			5337 #	5329	5333
	LSH			5167 #	5157
	LSHC			5227 #	5161
	LUUO			5046 #	4937	4938	4939	4940	4941	4942
	LUUO1			5048 #	5008	5010	5022	5026	5028	5030	5037	5039	5041	5043
	LUUO2			5061 #	5051
	LUUO3			5064 #	5062
	MAP			4771 #	4767
	MAP2			4784 #	10104
	MAXCHK			5582 #	5556	5568
	MAXDIV			5579 #	5551	5563
	MBREL			9215 #	9222
	MEMNRM			6659 #	6461
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-27
; 							Cross Reference Listing					

	MOVE			3980 #	3947	3948	3974	4254
	MOVELP			8269 #	8274
	MOVEM			3986 #	3949	4271	4272
	MOVES			3984 #	3950	4090	4132	4513
	MOVF1			8344 #	8291	8337	8351
	MOVF2			8347 #	8345	8564
	MOVF3			8350 #	8347
	MOVM			3974 #	3962	3964	3965
	MOVN			3977 #	3957	3958	3959	3960
	MOVNEG			3982 #	3977
	MOVPUT			8272 #
	MOVRJ			8290 #	8263
	MOVS			3971 #	3952	3953	3954	3955	4187	4191
	MOVS2			8266 #	8260
	MOVSTX			8328 #	8270
	MOVST1			8325 #	8262	8292	8331
	MOVST2			8335 #	8328
	MTRCI			9457 #	9088	9089	9090
	MTRCO			9454 #	9087
	MTRCO1			9461 #	9456
	MTRDBL			9514 #	9438	9504
	MTRINT			3743 #	3841	4738	5063	5147	7869	7906	8082
	MTRREQ			9477 #	3743
	MTRRQ0			9484 #	9480
	MTRRQ1			9502 #	9495	9496	9497	9498
	MUL			5413 #	5406	5407	5408	5409
	MULM			5451 #	5446	5448	5461	5462
	MULP			5437 #	5395	5397	5435	5442	5444	5457	5459	5467	5670	6566
	MULREE			5467 #	5679	6331	6340	6555
	MULSUB			5434 #	5414	5873
	MUUO			5072 #	4946	4947	4948	4949	4950	4951	4952	4953	4954	4955	4959
				4960	4961	4962	4963	4964	4965	4966	4967	4968	4969	4970	4971
				4972	4973	4974	4975	4976	4977	4978	5023
	MUUOF			5107 #	5081	5108	5109	5110
	MVABT			8278 #	8273
	MVABT1			8280 #	8281
	MVABT2			8283 #	8280
	MVEND			8285 #	8336
	MVSK1			8299 #	8296
	MVSK3			8304 #	8301
	MVSK4			8331 #	8302
	MVSKP			8295 #	8290	8297	8299
	MVSO3			8265 #	8261
	MVST			8242 #	8157
	MVST2			8257 #	8255
	NEGADJ			7649 #	7658
	NEXT			3729 #	3905	3922	5137	5193
	NO.CST			9888 #	9855
	NOCST0			9891 #	9886
	NOCST1			9893 #	9891
	NOP			3921 #	3908	4269	4270	4377	4378	4386	4387	4486	4496	4507	4572
				4574	4628	4659	4676	4758	4880	5089	5284	5313	5583	5735	6891
				6892	6936	6940	7189	7201	7223	7265	7286	7312	7577	7606	8393
				8556	9130	9132	9143	9599
	NOT.WR			10004 #	9998
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-28
; 							Cross Reference Listing					

	NOTWR			9980 #	9955	10001
	NOUPDT			9995 #	9942
	NXTOWG			7823 #	7825	7829	7833	7837	7841
	ODHIGH			7534 #	7518
	ODLOW			7521 #	7516
	OFSHFT			7756 #	7754
	OPDISP			8857 #	8855
	ORCA			4331 #	4324	4325	4326	4327
	ORCB			4362 #	4355	4356	4357	4358
	ORCM			4352 #	4345	4346	4347	4348
	OVER6			7829 #	7813
	OVER7			7833 #	7818
	OVER8			7837 #	7816
	OVER9			7841 #	7820
	OVTEST			6888 #
	OVTST1			6886 #
	OVTST2			6892 #	6888	6890
	OWGCOM			7751 #	7731	7733	7734	7737	7741	7745	7746	7749
	OWGDPB			7515 #	7472	7476
	OWGHIG			7424 #	7406
	OWGINC			7810 #	7778
	OWGLDB			7405 #	7379	7383
	OWGLOD			7394 #	7412	7413	7414	7415	7416	7417	7428	7429	7430	7431	7432
				7448	7449
	OWGLOW			7409 #	7405
	OWLCPY			7710 #	7594
	OWLINC			7794 #	7789
	PAGBI			9356 #	9047
	PAGBO			9319 #	9049
	PAGCI			9375 #	9052	9053	9054
	PAGCO			9371 #	9051
	PAGD2			9346 #	9335
	PAGDI			9352 #	9048
	PAGDO			9323 #	9050
	PCSLOD			4678 #	4675
	PCTXT			9359 #	5083	9355
	PF1			9723 #	9721
	PF2			9732 #	9730
	PF24			3827 #	7925
	PF4			9763 #	9741	9757
	PFPAR			9751 #	3827	3828	3829	3830	9734	9739
	PFPAR1			9753 #	9751
	PFT			10043 #	9813	9830	9868	9876	9892	9903	9929	9966	9980
	PGF1			10115 #	10052
	PGF2			10117 #	10115
	PGFAC0			10181 #	7239	7331	8904
	PGRF1			9773 #	9789
	PGRF2			9800 #	9774
	PGRF3			9805 #	9801
	PGRF5			9852 #	9815
	PGRF6			9871 #	9867
	PGRST1			10019 #	10021
	PGRST2			10023 #	10020
	PGRST3			10036 #	10024
	PHYS1			9638 #	9601
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-29
; 							Cross Reference Listing					

	PHYS2			9643 #	9584
	PHYS3			9648 #	9619
	PIBI			9279 #	9036
	PIBO			9281 #	9038
	PIBPA			9675 #	9659	9669
	PIBYTE			9659 #	9559
	PICI			9303 #	9041	9042	9043
	PICO			9299 #	9040
	PICOM1			9313 #	9300	9303
	PICOM2			9310 #	9301
	PICYC1			9548 #	3744	3746
	PICYC2			9568 #	3730
	PIDATI			9607 #	9558
	PIDATO			9595 #	9557
	PIDI			9267 #	9037
	PIDISP			9553 #
	PIDO			9266 #	9039
	PIDONE			9598 #	9589	9590	9614	9617	9618	9620	9649	9650	9673	10149
	PIDPB			9668 #	9661
	PIFET			9599 #	9144	9482
	PIIBP			9678 #	9663	9670
	PIINCR			9588 #	9556
	PIINST			9572 #	9493	9553	9554	9568
	PILD			9582 #	9595	9628	9631	9678
	PIOUT			9597 #	9602	9639	9640	9665
	PIST			9617 #	9610
	PIVECT			9570 #	9555
	PMOVEM			4020 #	4015
	PMOVE			4019 #	4014
	PMTR1			9496 #	9488
	POP			4827 #	4794
	POP2			4849 #	4827
	POPJ			4829 #	4795
	POPJ2			4837 #	4846
	POPJT			4845 #	4833
	POPTRP			4856 #	4850
	POSADJ			7632 #	7629
	PRVSEC			4716 #	4683	4693
	PSTOR			9619 #	9613
	PTLOOP			9401 #	9393	9403
	PUSH			4804 #	4793
	PUSHJ			4806 #	4792
	PUTDST			8784 #	8272	8329	8344	8636	8978	9012
	PXCT			5149 #	5146
	PXPUT			7221 #	7229
	RDEBRG			4774 #	9279
	RDEMTR			9523 #	9515	9517
	RDEX			9622 #	9596	9611
	RDMTR			9426 #	9071	9072	9083	9084
	RDMTR1			9437 #	9431	9432	9433	9434
	RDMTR2			9527 #	9524
	RDUMTR			9525 #	9519	9521
	RELEB			9176 #	9173
	RELEEB			9126 #	9220	9227	9259	9260	9328	9357	9451	9461
	RELMEM			7502 #	7522	7530	7536	7543	7549
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-30
; 							Cross Reference Listing					

	REPLIC			7175 #	7136	7238
	RESETP			8778 #	8771	8806
	RET1			9202 #	8563
	RET2			8202 #	8242
	RFLAGS			4669 #	4667
	ROT			5178 #	5156
	ROTC			5205 #	5160
	ROTCL			5218 #	5208	5219
	ROTCR			5212 #	5215
	ROTDUN			5197 #	5187
	ROTL			5191 #	5181
	ROTS			3808 #	3732	10119
	RTLLUP			5195 #	5192	5195
	RTRLUP			5187 #	5188
	SECIMM			9815 #	9845
	SECPTR			9810 #	9805	9829
	SETCA			4321 #	4314	4315	4316	4317
	SETCM			4342 #	4335	4336	4337	4338
	SETEBR			9390 #	9373
	SETFLG			9015 #	8528	9011
	SETO			4372 #	4365	4366	4367	4368
	SETPC			5089 #	3793
	SETZ			4223 #	4118	4165	4229	4230	4231	4232
	SEVEN9			7749 #	7736
	SFET1			8873 #	8338	8870
	SGNEXT			6876 #	6501	6504	6551	6558	6586	6622	6961	7100
	SHFLOD			3904 #	4838	4840	5168	5170	5256	5263	5264	7393	7395	7411	7420
				7427	7435	7442
	SHIFT			8004 #	4757	5289	5695	6530	6576	7996	8029	8211	9390	9578
	SIZE8D			7555 #	7531	7532	7533	7534
	SIZE8L			7448 #	7421	7422	7423	7424
	SIZE9D			7556 #	7544	7545	7546	7547
	SIZE9L			7449 #	7436	7437	7438	7439
	SKIP			4524 #	4514	4515	4516	4517	4518	4519	4520	4540	4556
	SLFEND			3902 #	4539	4555
	SMALDV			7641 #	7635
	SN1			6160 #	6153	6166
	SNARFP			7804 #	7795
	SNATCH			7762 #	7681
	SNORM			6123 #	5831	5874	5914	5925	6066	6125	6127	6158	6176	6964
	SNR2			6126 #	6053
	SNZERO			6173 #	6124
	SOJ			4605 #	4594	4595	4596	4598	4599	4600
	SOJA			4604 #	4597
	SONJ			4603 #	4593
	SONS			4555 #	4544
	SOS			4556 #	4545	4546	4547	4548	4549	4550	4551
	SPRAYL			7201 #	7205	7220
	SPRAY			7199 #	7160	7296
	SRCMOD			8675 #	8269	8455	8726	8728	8729
	SRND2			6209 #	6189
	SRND3			6212 #
	SRND4			6213 #	6208
	SRND5			6223 #	6174
	SROUND			6189 #	6128	6130	6132	6134	6161	6163	6164
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-31
; 							Cross Reference Listing					

	ST0			3897 #	3971	3980	3982	4176	4178	4181	4184	4188	4192	4201	4203
				4211	4212	4217	4218	4224	4226	4241	4251	4266	4281	4291	4301
				4311	4321	4331	4342	4352	4362	4372	5350	5363	5417	5418	5489
				5490	5557	5569
	ST2AC			3899 #	5303	5659	5661	5749	5751	7078
	ST6			3906 #	4777	5401	6223	6227
	STAC			3905 #	3919	3992	3993	4046	4479	4480	4481	4588	4883	4895	5130
				5138	5183	5197	6099	7763	8285
	STAC1			3936 #	5337	5338	6237
	STAC4			8486 #
	START			3793 #	4744
	STBOTH			3910 #	4023
	STCST			9970 #	9962
	STD1			3927 #	4081	6413	6762
	STLOW			4073 #	4069	4079
	STMAC			4818 #	3987	4804	4851
	STMEM			3908 #	4025	4074	4205	4694	7503	9235	9286
	STOR34			8483 #	8474	8480
	STORAC			3919 #	3910	4589	4606	4613	4786	4814	5398	6097	6928	7702	7710
				8873
	STORTG			7801 #	7799
	STR2WD			8227 #	8183	8187	8428
	STRAC1			6237 #	8618
	STRAC3			5696 #
	STRNC			6208 #	6193
	STRPF			10173 #	10088	10092
	STRPF1			10175 #	10183
	STRPF2			10180 #	8305	10164	10175
	STRPF3			10178 #	10171
	STRPF4			10183 #	10100
	STSELF			3918 #	3903	3984	4525	4819
	SUB			5362 #	5355	5356	5357	5358
	SWEEP			9211 #	9058	9059	9060	9061	9062	9063	9064	9065
	TAKINT			3841 #	4787	9185	10116
	TDN			4478 #	4467	4470	4472	4562
	TDX			4467 #	4381	4382	4390	4391	4399	4408	4417	4426	4435	4444	4490
				4500
	TDXX			4475 #	4379	4383	4388	4392	4397	4401	4406	4410	4415	4419	4424
				4428	4433	4437	4442	4446
	TIMBO			9469 #	9073
	TIMBO1			9450 #	9473
	TIMCI			9445 #	9076	9077	9078
	TIMCO			9443 #	9075
	TIMCO1			9328 #	9444
	TIMDO			9468 #	9074
	TLX			4472 #	4396	4405	4414	4423	4432	4441
	TLXA			4469 #	4400	4409	4418	4427	4436	4445
	TMTR1			9495 #	9487
	TRAP			3776 #
	TRAPX			3782 #	3776	3778
	TRNABT			8705 #	8709
	TRNAR			8691 #	8974
	TRNFNC			8703 #	8693	8695	8706	8707	8708	8711
	TRNNS1			8725 #	8719
	TRNNS2			8727 #	8725
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-32
; 							Cross Reference Listing					

	TRNRET			8714 #	8704
	TRNSIG			8708 #	8710
	TRNSS			8719 #	8715
	TRNSS1			8731 #	8720
	TST2WD			8200 #	8182	8186	8427
	TSXX			4474 #	4380	4384	4389	4393	4398	4402	4407	4411	4416	4420	4425
				4429	4434	4438	4443	4447
	TWGCPY			7702 #	7711
	TWGDUN			7695 #	7693
	TWJUNK			4040 #	7707
	UNHALT			4743 #	4740
	UUO			5071 #	4022	4024	4631	4638	4640	4643	4644	4703	4784	4916	4982
				4983	4984	5115	5761	5775	5840	5884	6011	6012	7739	8055	8063
				8167	8220	8939	8948	9085	9086	9161	9200	9205	9267	9313	9356
				9468
	UUOC2			5100 #	5055	5077
	UUOCHK			9467 #
	UUOCOM			5092 #	5049	5073
	UVERS			3942 #	9232
	UXCT			5150 #	4743
	WGRANT			9184 #	9162	9188	9314
	WGRNT1			9187 #	9184
	WGRNT2			9189 #	9187
	WRFAIL			10001 #	10004
	WRHPT			10005 #	9992	9999
	WXFER			9174 #	9170
	XBLFIX			10108 #	7292	7317
	XBLFRZ			7326 #	10108
	XBLPX			7284 #	7291
	XBLT			7265 #	8047
	XBLTGO			7169 #	7279
	XCT			5145 #	5141
	XCTGO			3774 #	3777	3788	5150
	XCTW			3788 #	3795	5065	9572	9573	9575	9579
	XFERW			4862 #	4811	5054	5651	5718	6254	6431	7052	7156	7217	7691	9009
				9363	9380	9528	9582	9583	9644	9645
	XHLLI			3995 #	4088
	XJRST			4696 #	4642	5057
	XJSPLT			4672 #	4654
	XLATE			8689 #	8679
	XMOVEI			3990 #	4255
	XMOVEI1			3992 #	3996
	XOR			4281 #	4274	4275	4276	4277	4413	4422
	XPCW			4682 #	4657
	XSFM			4692 #	4641
	XSPRAY			7296 #	7277
(D) J				2304 #
(U) KLPAGE			1974 #
	OPTIONS			1975 #	9234
(U) LONGPC			1977 #
	OPTIONS			1978 #	9234
(U) MACRO%
	A INDRCT		2330 #	3813	3815	3817	3819	3835	3837
	A READ			2331 #	3822	3823	3824	3825	3833	3834
	ABORT INSTR		2332 #	9502	10122
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-33
; 							Cross Reference Listing					

	AC0			2333 #	4830	4849	5125	5127	5130	7239	7479	7515	7517	7624
	AC0_AR			2334 #	3905	4081	4884	5137	5193	5682	6281	6289	6412	6468	6502
				6760	6892	7140	7271	7627	7630	8408	8473	8589	8591	8618	10168
				10177	10181
	AC1			2335 #	7328
	AC1_AR			2336 #	3936	4046	5313	5689	5736	5737	6297	6356	6358	6469	6475
				6592	6594	7269	7330	7695	8194	8431	10169
	AC2			2337 #	7329
	AC2_AR			2338 #	5693	5747	7270	7331	8185	8430
	AC3_AR			2339 #	5676	5696	5749	5750	8280	8304	8336	8404	8448	8449	8483
				8517	8529	8560	10161	10175	10178
	AC4			2340 #	8186	8192	8438	8486	10164
	AC4_AR			2341 #	8192	8486	10164
	AC5			2342 #	8190
	AC5_AR			2343 #	8190
	AD FLAGS		2344 #	3982	4045	4069	4539	4540	4555	4556	4588	4589	4606	5350
				5363	5659	5661
	AD LONG			2345 #	5750	6099	8522
	ADMSK			2346 #	3740	4656	4692	4808	4878	4883	5050	5089	5093	5097	5101
				8229	8230	9551	9584	9601	9619	9633	9653	10125
	AR+ARX+MQ_0.M		2574 #	6173	7078	8326	8675
	AR+MQ_0.M		2575 #	5227	8457
	AR+MQ_0.S		2576 #	6681
	AR0-3 DISP		2578 #	7379	7383	7472	7476	7592	7778	8692	9595	9610	9812	9901
	AR0-8_#			2579 #	3942	5108	5109	5110	6489	6543	6597	6733	6933	6935	6941
				6974	7055	7068	8253	8433	8552	9234	9335	9347	9418	9936
	AR0-8_# AND AR0-8	2580 #
	AR0-8_# OR AR0-8	2581 #	10004
	AR0-8_#+SC		2582 #	8058
	AR0-8_-SC-1		2583 #
	AR0-8_FE		2584 #	8258	8440	9766
	AR0-8_FE OR #		2585 #	8441
	AR0-8_FE OR SC		2586 #	9788
	AR0-8_FE#		2587 #	8438
	AR0-8_FE+#		2588 #	9574	9623	9675
	AR0-8_FE+1		2589 #
	AR0-8_FE+SC		2590 #
	AR0-8_FE-SC		2591 #
	AR0-8_FE.M		2592 #	8557
	AR0-8_FE.R		2593 #
	AR0-8_SC		2594 #	8028	8209	9767
	AR0-8_SCAD		2595 #	8028	8209	8258	8440	9766	9767
	AR0-8_SCAD#		2596 #	8058	8441	9574	9623	9675	10004
	AR0-8_SCAD.M		2597 #	8557	9788
	AR0-8_SCAD.R		2598 #
	AR12-17_PC SEC		2599 #	4890
	AR12-17_PREV SEC	2600 #	4716	5095	9360	10128
	AR18-21 DISP		2601 #	8694
	ARL+ARX+MQ_0.M		2626 #
	ARL+ARX_0.M		2627 #	9437	9503
	ARL_0S			2606 #	4201	4205	4217
	ARL_0.C			2603 #	9147
	ARL_0.M			2604 #	8101	8634	8637	8831	8892	8914	8934	8991	9815	9816	9817
				9908	9909	9924
	ARL_0.S			2605 #	4834	4845	9461
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-34
; 							Cross Reference Listing					

	ARL_1S			2608 #	4203	4218	8109
	ARL_1S.M		2609 #
	ARL_1.M			2607 #	3992	5092
	ARL_AC0			2610 #	4176	4184	4191
	ARL_ARL			2611 #	4178	4192	4224	4226	5098	5388	7134	7684	9259	9309
	ARL_ARL.M		2612 #	9323
	ARL_ARL.S		2613 #	3996	7628
	ARL_ARR			2614 #	4181	4187	4188	4211	4212	5124	7175	9123
	ARL_ARR.M		2615 #	9252	9299	9371	9454
	ARL_ARR.S		2616 #	9233
	ARL_ARX (ADX)		2617 #	9768
	ARL_ARXL		2618 #	4694	4895	7688	7694	9451	10129
	ARL_ARXL.M		2619 #
	ARL_BRL			2620 #	4684	4892	7800	9357
	ARL_BRL.M		2621 #
	ARL_BRL.S		2622 #	5085
	ARL_MQL			2623 #	7140
	ARL_SHIFT		2624 #	9905
	ARL_SIGN		2625 #	5129
	ARR+MQ_0.S		2649 #	5487	7628	7631
	ARR_0S			2632 #	4211	4224
	ARR_0.C			2629 #
	ARR_0.M			2630 #	5509	9384
	ARR_0.S			2631 #
	ARR_1S			2633 #	4212	4226
	ARR_AC0			2634 #	4178	4181	4187	4205	7134
	ARR_AC0.S		2635 #	3996
	ARR_AR+1		2636 #	7794	8760
	ARR_AR+BR		2638 #	7688
	ARR_AR-1		2637 #
	ARR_ARL			2639 #	4184	4191	4192	4217	4218	4892	9323	9768	9773
	ARR_ARR			2640 #	3992	4176	4188	4201	4203	4684	4694	4845	5124	5129	7175
				9123	9252	9299	9371	9454	10129
	ARR_ARX			2641 #	5098	7140
	ARR_ARX+1		2642 #
	ARR_ARX+BRX		2643 #
	ARR_ARX+BR		2644 #	7684	7694
	ARR_ARX-1		2645 #
	ARR_BR			2646 #
	ARR_PC+1		2647 #	4895
	ARR_SHIFT		2648 #	5388
	ARX+MQ_0.M		2754 #	5314	5485	5866	6051	8504
	ARX+MQ_0.S		2755 #
	ARX0-3 DISP		2758 #	9503
	ARX0_AR35		2756 #
	ARX0_MQ35		2757 #	8385
	ARX_-AC0		2654 #	5509
	ARX_-BRX		2655 #	5557	7270	7630	7649
	ARX_-FM[]		2656 #
	ARX_-SLEN		2657 #	10162
	ARX_-2+MQ0		2653 #	7599
	ARX_0S			2661 #	4756	5168	5239	5296	5328	5434	5663	5717	5802	5870	5926
				6297	6329	6404	6554	6705	7573	7593	7626	7734	7737	7746	7749
				8390	8437	8442	8616	9371	9382	9417
	ARX_0.C			2658 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-35
; 							Cross Reference Listing					

	ARX_0.M			2659 #	5392	6124	6977	8630
	ARX_0.S			2660 #
	ARX_1B1			2663 #	6082
	ARX_1B17-1		2664 #	5082	9354
	ARX_1S			2665 #	3731	3783	6192	6574	7310	7326	7740	8260	8448	8825	10080
	ARX_1			2662 #	5124	6410	6759	7744	7750
	ARX_2+MQ0		2666 #	5326	6708	7592	7730	7732
	ARX_AC0			2668 #	5209	5230	5386	5487	8047	8515	8826
	ARX_AC0 COMP		2669 #
	ARX_AC0+1		2670 #	4804	4809
	ARX_AC1			2671 #	4072	5205	5227	5705	6298	7084
	ARX_AC2			2672 #	7267
	ARX_AC3			2673 #	5647	5691	8633
	ARX_AC4			2674 #	8438
	ARX_AR			2676 #	3987	4067	4737	5169	5253	5255	5271	5272	5276	5288	5489
				5490	5495	5653	5822	5825	6065	6274	6276	6279	6365	6508	6510
				6512	6526	6604	6664	6729	6744	6746	6930	6968	7065	7079	7139
				7328	7373	7376	7392	7466	7469	7692	7753	7755	7797	7804	7997
				8029	8050	8210	8217	8254	8257	8290	8328	8360	8386	8404	8406
				8457	8513	8579	8621	8692	8758	8763	8774	8794	8797	8812	8815
				8836	8892	8914	8920	8991	9307	9346	9348	9365	9469	9608	9624
				9661	9680	9934	10118	10120	10176
	ARX_AR (AD)		2677 #	5150	5187	5196	5218	5235	7627	7686	8770	8805	9323	9577
				9723	9724	9767	9941
	ARX_AR ANDCA BR		2679 #	8228
	ARX_AR AND ADMSK	2678 #	9633
	ARX_AR SIGN		2680 #	6086
	ARX_AR SWAP		2681 #	4653	4888	5097	5517	7642	8634	8694	9258	9305	9379	9448
				9484	9773	9950	9995
	ARX_AR*2		2682 #	4053	7636
	ARX_AR*4 COMP		2683 #	8550
	ARX_AR*MSK		2684 #	8714
	ARX_AR+1		2685 #	8555
	ARX_AR+CBR		2686 #
	ARX_AR+FM[]		2687 #
	ARX_AR-1		2688 #	4849	8364	8408	8409
	ARX_AR-BR		2689 #
	ARX_AR-FM[]		2690 #
	ARX_AR-FM[]-1		2691 #
	ARX_ARX AND ADMSK	2693 #	9653
	ARX_ARX ANDC ADMSK	2694 #	4692
	ARX_ARX*-6		2695 #	5324
	ARX_ARX*.25		2696 #	5913
	ARX_ARX*.5		2697 #
	ARX_ARX*2		2698 #	5332	5652	6089	6255	6432	6957	6959	7053	7097	8444	8848
				9529
	ARX_ARX*2 COMP		2699 #	9906
	ARX_ARX*4		2700 #	6709	8846
	ARX_ARX*4 COMP		2701 #
	ARX_ARX*8		2702 #	5701	9359	9360
	ARX_ARX*BRX		2703 #	6214	6414	6763
	ARX_ARX*EXPMSK		2704 #
	ARX_ARX+1		2705 #	7163	7167	7169	7186	7199	7204	7219	7221	7271	7284	7597
				7602
	ARX_ARX+1 (AD)		2706 #	7137
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-36
; 							Cross Reference Listing					

	ARX_ARX+AC0		2707 #
	ARX_ARX+BRX		2708 #	7315
	ARX_ARX+CBR		2709 #
	ARX_ARX+FM[]		2710 #	7329	7624
	ARX_ARX-1		2711 #	5318	7700
	ARX_ARX-1 (AD)		2712 #	8295
	ARX_ARX-AR*4		2713 #	7752
	ARX_ARX-FM[]		2714 #
	ARX_ARX-FM[]-1		2715 #
	ARX_BR			2717 #	5279	5710	9826
	ARX_BR*2		2718 #	5706
	ARX_BR+1		2719 #
	ARX_BRX			2720 #	4755	5008	5010	5022	5026	5028	5030	5037	5039	5041	5043
				5046	5072	5292	5302	6283	6311	6412	6761	7155	7175	7216	8191
				8403	9609
	ARX_BRX COMP		2721 #	6394	6397	6695	6698	7273	7658
	ARX_BRX+1		2722 #	8262	8292	8331
	ARX_DSTP		2723 #	8415	8784
	ARX_DSTP2		2724 #	8796	8947
	ARX_E1			2725 #
	ARX_FILL		2726 #	8187	8188
	ARX_FM			2727 #	8187	8188	8388	8415	8576	8740	8762	8784	8796	8947	8971
				9902	10017	10020	10024	10026
	ARX_FM(VMA)		2731 #	3777
	ARX_FM[]		2728 #	7515	7517
	ARX_FM[] COMP		2729 #
	ARX_FM[]+1		2730 #
	ARX_MEM			2732 #	3788	3840	3841	4043	4832	4834	4862	5075	6954	7497	7868
				7869	7905	7906	7976	7977	8012	8081	8082
	ARX_MQ			2733 #
	ARX_MQ+1		2734 #
	ARX_MQ-1		2735 #
	ARX_MQ-FM[]		2736 #
	ARX_MQ-FM[]-1		2737 #
	ARX_PC			2739 #	8201	9769	10123
	ARX_PC+1		2740 #	4636	4641	5100
	ARX_SHIFT		2741 #	3808	5261	5275	5300	5519	5690	5694	5702	5708	5719	5824
				6092	6098	6156	6514	6521	6731	7394	7499	7501	7632	7634	7756
				7996	8018	8021	8506	9383	9514	9516	9518	9520	9652	9815	9825
				9909
	ARX_SRCP		2742 #	8388	8740	8971
	ARX_SRCP2		2743 #	8762
	ARX_SV.AR		2744 #	10020
	ARX_SV.ARX		2745 #	10024	10026
	ARX_SV.BR		2746 #	10017
	ARX_SV.VMA		2747 #	9902
	ARX_T0			2748 #
	ARX_T2			2749 #	8576
	ARX_VMA HELD		2753 #	8201	9769
	AR_(AR+2BR)*.25		2351 #	5458	5830
	AR_(AR+BR)*.25		2352 #	5443	5456
	AR_(AR-2BR)*.25		2353 #	5445
	AR_(AR-BR)*.25		2354 #	5447	5460
	AR_(ARX OR AR*4)*.25	2355 #
	AR_-AC0			2356 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-37
; 							Cross Reference Listing					

	AR_-AR			2357 #	5812	6227
	AR_-AR LONG		2358 #	4054	5542	5545	5997	6260	6416	6438	6611	8523
	AR_-BR			2359 #	3982	5552	5565	5750	6399	6459	6700	7331	8519	8586
	AR_-BR LONG		2360 #	5712	5715	5746	8481
	AR_-BR*2 LONG		2361 #
	AR_-BRX			2362 #	5489
	AR_-DLEN		2363 #
	AR_-FM[]		2364 #
	AR_-SLEN		2365 #	8278	10173
	AR_0.C			2366 #	4641	6574
	AR_0.M			2367 #	7270	8300	8555
	AR_0.S			2368 #	5094	10127
	AR_0S			2369 #	4055	4079	4081	4211	4224	4682	4725	4774	5080	5169	5394
				5396	5434	5517	5663	6097	6189	6329	6404	6445	6542	6554	6705
				7392	7394	7641	7642	7996	7997	8257	8328	8437	8442	8616	8621
				8926	9319	9334	9365	9375	9379	9417	9420	9428	9445	9448	9457
				9469	9484	9507	9514	9516	9518	9520	9570	9624	9630	9765	9781
				10133	10161
	AR_1			2370 #	8446	9951
	AR_1 LONG		2371 #	8525
	AR_1S			2372 #	4212	4226	4372	4603	4604	7751	8265	8449	8549
	AR_2			2373 #	9008
	AR_2(AR*BR)		2374 #	5540	5543	5548	5560	5594	5595	5596	5597	5625	5626	5627
				5628	5730	5732	7638	8468
	AR_2(AR+1)		2375 #
	AR_2(AR+BR)		2376 #	5543	5548	5594	5597	5625	5628	5732
	AR_2(AR+BR) LONG	2377 #	8468
	AR_2(AR-BR)		2378 #	5540	5560	5595	5596	5626	5627	5730	7638
	AR_AC0			2380 #	3863	3987	4025	4065	4178	4181	4187	4205	4574	4605	4827
				4829	4888	4891	5167	5178	5207	5228	5251	5271	5272	5312	5362
				5389	5413	5495	5653	5677	5708	5811	5871	5901	6052	6274	6292
				6300	6301	6333	6365	6447	6464	6501	6526	6546	6557	6604	6664
				7073	7098	7134	8148	8156	8245	8360	8506	8579
	AR_AC0 COMP		2381 #	4321	8429
	AR_AC0+1		2382 #	4588	4589	4613
	AR_AC1			2383 #	4067	5742	5743	6324	6325	6361	6518	6524	6525	6553	6600
				7266	8182	8427	8504
	AR_AC1 COMP		2384 #
	AR_AC1*2		2385 #	5270	5484	5649	6258	6353	6354	6479	6540	6589	6590	6663
	AR_AC2			2386 #	5703
	AR_AC3			2387 #	8150	8164	8244	8445	8513	8527	8551	8934	10170
	AR_AC3*2		2388 #
	AR_AC4			2389 #	8186
	AR_ADMSK AND VMA HEL	2391 #
	AR_AD*.25 LONG		2390 #	4054	5542	5545	5997	6260	6416	6438	6611	8523	8620
	AR_ARX			2442 #	3774	3844	4046	4053	4667	4814	4819	4821	5098	5103	5209
				5214	5218	5239	5292	5296	5392	5507	5511	5689	5701	5706	5911
				6123	6286	6289	6297	6387	6468	6473	6687	6945	6976	7140	7267
				7330	7625	7686	7801	7922	8065	8099	8102	8104	8107	8336	8390
				8448	8517	8538	8540	8564	8637	8732	8765	8770	8805	8831	8832
				8833	8857	8861	8883	8893	8918	8952	8960	9000	9001	9002	9003
				9259	9325	9471	9622	9723	9724	9739	9770	9789	9823	9854	9863
				9941	9946	10005	10020	10023	10025	10164	10168
	AR_ARX (ADX)		2444 #	6095	6154	6281	6290	6388	6688	7632
	AR_ARX (AD)		2443 #	3813	3817	3822	3824	5212	5220	5233	5241	5258	6521	6530
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-38
; 							Cross Reference Listing					

				8210	8404	8406	9996
	AR_ARX ANDC ADMSK	2446 #	4656	5050	5093	10125
	AR_ARX AND ADMSK	2445 #	5101
	AR_ARX COMP		2447 #	8256
	AR_ARX COMP AND BRX	2448 #	4685
	AR_ARX OR PUR		2449 #
	AR_ARX*.25		2450 #	5913	9934
	AR_ARX*.25-AR-1		2451 #	8553
	AR_ARX*2		2452 #
	AR_ARX*4		2453 #
	AR_ARX*4 COMP		2454 #
	AR_ARX*AC1		2455 #
	AR_ARX*BR		2456 #	6758	9361
	AR_ARX*BRX		2457 #	8304	9309	10131
	AR_ARX*E1		2458 #
	AR_ARX+1		2459 #
	AR_ARX+1 (AD)		2460 #	8154	8166	8799
	AR_ARX+AC0		2461 #
	AR_ARX+AR*4		2462 #
	AR_ARX+BR		2463 #	6741	7314	7684	7694	7762
	AR_ARX+BRX		2464 #	5338	7238	7269	7333
	AR_ARX+BRX+1		2465 #	5337	7604
	AR_ARX+FM[]		2466 #
	AR_ARX+XR		2467 #	3815	3819	3823	3825
	AR_ARX-1		2468 #	8364	8408	8409
	AR_ARX-AC3		2469 #
	AR_ARX-BR		2470 #	7139	10177
	AR_AR AND ADMSK		2393 #	4808	4878	4883
	AR_AR AND CSMSK		2394 #	9875	9955	9970
	AR_AR OR PUR		2395 #	9877	9992
	AR_AR SWAP		2396 #	3971	4469	4472	4474	4720	5800	5865	7076	8218	9385	9472
				10017
	AR_AR*.25		2397 #	6972
	AR_AR*.25 LONG		2398 #	5442	5462	5550	5562	5925	6126	6175	6377	6379	6676	6678
				6722	6962
	AR_AR*.5		2399 #	6212	6971
	AR_AR*.5 LONG		2400 #	6381	6408	6680	6711	6725	8679	8973
	AR_AR*1.25 LONG		2401 #	8620
	AR_AR*10		2402 #	8447
	AR_AR*10 LONG		2403 #	8537	8642
	AR_AR*2			2404 #	3740	9479
	AR_AR*2 LONG		2405 #	6129
	AR_AR*4			2406 #	9555	9559	9596	9611
	AR_AR*4 LONG		2407 #	6131
	AR_AR*5 LONG		2408 #	8467
	AR_AR*8			2409 #	6462	6471	6944	8842	8850	8878
	AR_AR*8 LONG		2410 #	6133	6152	6165
	AR_AR*AC0		2411 #	4241	4251	4266	4281	4291	4301	4311	4331	4352	4362	4479
				4480	4481	5350
	AR_AR*AC1		2412 #
	AR_AR*BR		2413 #	4761	5061	5096	6734	8516	9362	9964
	AR_AR*EXPMSK		2414 #
	AR_AR*MSK		2415 #	8716
	AR_AR*SFLGS		2416 #	8285	8472	10176	10180
	AR_AR*SLEN		2417 #	10171
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-39
; 							Cross Reference Listing					

	AR_AR*T0		2418 #	8400	10166
	AR_AR+1			2420 #	3797	4539	4540	6382	6393	6694	7794	7798	7824	7829	7833
				7837	7841	7953	8297	8580	8760	8768	8795	8803	8869	8897	8899
				8994	9148	9565	9589
	AR_AR+1 LONG		2421 #	8580
	AR_AR+1-AR0		2422 #	8764	8798
	AR_AR+BR		2423 #	5598	5601	5611	5659	5671	6099	6209	6293	6332	6527	6548
				6556	6710	6975	7085	7268	7688	7692	8284	9401	9407	9439	9505
				9632	10175
	AR_AR+BR LONG		2424 #	5611	5659	5671	6332	6556	9439	9505
	AR_AR+E1		2425 #	8628	8680
	AR_AR+FM[]		2426 #	5127	5130	7239	7328	7650
	AR_AR+SBR		2427 #
	AR_AR+T0		2428 #
	AR_AR+T1		2429 #
	AR_AR+XR		2434 #
	AR_AR-1			2435 #	4555	4556	4606	4830	8725	9590
	AR_AR-BR		2436 #	5363	5599	5600	5614	5661	6545	6943	7135	8254	8281
	AR_AR-BR LONG		2437 #	5614	5661
	AR_AR-BR-1		2438 #
	AR_AR-FM[]		2439 #
	AR_AR-T0		2440 #
	AR_BRX			2485 #	5088	5298	5490	5710	7695	7756	8056	8956	8995	9610	9613
				9671
	AR_BRX+1		2486 #	7327
	AR_BR			2472 #	4022	4024	4669	5263	5264	5288	5301	5690	5694	5749	6090
				6208	6283	6311	6389	6456	6460	6544	6586	6689	6740	6937	6942
				7056	7069	7075	7739	8050	8055	8063	8167	8387	8403	8938	8958
				9161	9200	9205	9233	9267	9313	9356	9426	9468	9764	9786	9824
				10149
	AR_BR COMP		2473 #	6394	6397	6695	6698	8255
	AR_BR COMP LONG		2474 #	5714
	AR_BR LONG		2475 #	5720	5722	5745	6515	6577	8451	8464	8473	8479	8583	8614
				8623	8639	10060	10084
	AR_BR OR ARX		2476 #	8185	8190	8430
	AR_BR*.5		2477 #
	AR_BR*.5 LONG		2478 #	8529
	AR_BR*2			2479 #	5815
	AR_BR*2 LONG		2480 #	5286	6160
	AR_BR*4			2481 #
	AR_BR*4 LONG		2482 #	6162
	AR_BR+1			2483 #	7190	7225	7288
	AR_BR+1 LONG		2484 #
	AR_CACHE CNT		2488 #	9434	9490
	AR_DLEN			2489 #	8263	8270	8273	8719	8739	10088	10092	10100
	AR_DLEN COMP		2490 #	10064
	AR_DLEN+1		2491 #	8345
	AR_DSTP			2492 #	8261	8415	8722	8784	8860	8862	10157
	AR_DSTP+1		2493 #
	AR_DSTP2		2494 #	8797	9006
	AR_DSTP2+1		2495 #
	AR_DSTW			2496 #
	AR_E0			2497 #
	AR_E1			2498 #
	AR_EBOX CNT		2499 #	9433	9489
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-40
; 							Cross Reference Listing					

	AR_EBUS			2500 #	3744	3746	9172
	AR_EBUS REG		2501 #	4776	9364	9381	9740	9755
	AR_FILL			2502 #	8344
	AR_FM[]			2504 #	10142
	AR_FM[] COMP		2505 #
	AR_FM[]+1		2506 #
	AR_FM			2503 #	5719	5739	7677	8261	8263	8270	8273	8290	8338	8344	8388
				8415	8565	8577	8677	8693	8695	8719	8720	8722	8728	8739	8740
				8763	8784	8797	8860	8862	8971	8980	9006	9011	9013	9813	9830
				9868	9876	9892	9903	9929	9966	9980	9997	10015	10056	10088	10092
				10100	10104	10117	10144	10157
	AR_FM(#)		2507 #
	AR_FM(VMA)		2508 #
	AR_INTERVAL		2509 #	9447
	AR_MEM			2511 #	3869	3882	3888	4654	4689	4696	4832	4834	4856	4862	6926
				6967	7050	7064	7192	7193	7228	7230	7277	7278	7290	7292	7316
				7317	7446	7497	7522	7523	7524	7525	7526	7527	7528	7530	7536
				7537	7538	7539	7540	7541	7543	7549	7550	7551	7555	7556	7797
				7991	7993	8012	8014	8242	8365	8563	8632	8691	8837	8936	8945
				8976	8981	9125	9564	9588	9597	9810	9825	9845	9871	9893	9916
				10135
	AR_MQ			2512 #	5519	5629	5630	5631	5632	5672	5678	5693	5740	6098	6296
				6340	6356	6358	6562	6592	6594	6622	6886	7499	7680	8018	8391
				8589	8591	8785	8964	9608
	AR_MQ COMP		2513 #	6375	6674
	AR_MQ*.25		2514 #	6156
	AR_MQ*2			2515 #
	AR_MQ*4			2516 #	9661
	AR_MQ*AC1		2517 #
	AR_MQ*AC2		2518 #
	AR_MQ*AC3		2519 #
	AR_MQ+1			2520 #	3729
	AR_MQ+AC0		2521 #
	AR_MQ+BR		2522 #
	AR_MQ+FM[]		2523 #
	AR_MQ-1			2524 #	7157
	AR_MQ-AC3		2525 #
	AR_MQ-BR		2526 #
	AR_MQ-BR-1		2527 #
	AR_MTR REQ		2528 #	9478
	AR_PC			2529 #	4759	5078	10118	10120
	AR_PC FLAGS		2530 #	5097
	AR_PC+1			2531 #	4806	4874	4876
	AR_PERF CNT		2532 #	9432	9488
	AR_PERIOD		2533 #	9449
	AR_PUR+AR0		2534 #
	AR_SERIAL		2536 #	9231
	AR_SFLGS		2537 #	8338	8677	8693	8695	8720	8980	9011	9013
	AR_SHIFT		2538 #	3808	3904	3927	4044	4073	4756	5085	5180	5183	5197	5243
				5302	5315	5317	5388	5398	5401	5688	5691	5704	5748	5827	6288
				6298	6385	6405	6412	6519	6685	6706	6760	6888	6890	7081	7084
				7137	7501	7502	8004	8021	8023	8266	8450	8484	8622	8703	8826
				8838	9213	9357	9508	9625	9826	10122	10162
	AR_SIGN			2539 #	3899	3900	5253	5255	5275	5276	5285	5402	5682	5747	5821
				5822	6066	6092	6276	6279	6510	6512	6729	7058	7065	7079	7753
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-41
; 							Cross Reference Listing					

				7755	8483
	AR_SLEN			2540 #	8728
	AR_SLEN COMP		2541 #	8283	8301	8459	8465	10183
	AR_SLEN+1		2542 #	8269	8454	8611	8726	8729
	AR_SRCP			2543 #	8290	8388	8740	8971	10056
	AR_SRCP+1		2544 #
	AR_SRCP2		2545 #	8763
	AR_SRCP2+1		2546 #
	AR_SV.AR		2547 #	10144
	AR_SV.ARX		2548 #
	AR_SV.BR		2549 #	9813	9830	9868	9876	9892	9903	9929	9966	9980
	AR_SV.PFW		2550 #	9997	10104	10117
	AR_SV.SC		2551 #	10015
	AR_SV.VMA		2552 #
	AR_SWD			2553 #
	AR_T0			2555 #	5719	7677	8565
	AR_T1			2556 #	5739	8577
	AR_T2			2557 #
	AR_TIME BASE		2558 #	9431	9487
	AR_VMA HELD		2569 #	10118	10120
	AR_XR			2570 #	4668
	AR_[] AND FM[]		2350 #	8230
	B DISP			2762 #	4020	4041	4467	4470	4472	4475	4819	5654	5803	5861	5865
				6255	6432	8097	8259	8439	8447	8622	8678	8715	8719	9147	9429
				9437
	B STORE			2763 #	4054
	B WRITE			2764 #	4777	5401	6223	6227	9126	9310	9385
	BLKO TIM(L)		2765 #	9471	9473
	BLKO TIM(R)		2766 #	9472
	BR_AR LONG		2767 #	4054	5285	5394	5396	5434	5612	5615	5710	5712	5714	5715
				5720	5722	5745	5746	6090	6152	6165	6189	6192	6258	6262	6266
				6574	6577	6588	6681	8446	8454	8464	8525	8527	8583	8586	8610
				9523	9525
	BYTE DISP		2768 #	4740	5208	7374	7377	7466	7470	7679	7711	7779	8200	8505
				8755	8791	8915	8945	8946	8947	8960	8992	9120	9403	9409	9961
	BYTE INDRCT		2769 #	7863	7865	7900	7902	7929	7931	7972	7973
	BYTE LOAD		2770 #	7859	7861	7872	7874
	BYTE PREV & CLR SR3	2771 #
	BYTE PREV & SET SR2	2772 #
	BYTE PREV & SET SR3	2773 #
	BYTE READ		2774 #	7970	7971	7980	7981
	BYTE READ PC		2775 #
	BYTE RPW		2776 #	7895	7897	7909	7912
	CALL			2778 #	3732	4043	4068	4630	4633	4634	4636	4637	4639	4683	4688
				4693	4757	4775	4811	4850	5049	5054	5055	5073	5077	5081	5083
				5289	5395	5397	5414	5488	5551	5556	5563	5568	5678	5695	5718
				5734	5740	5902	5903	6274	6331	6340	6353	6354	6367	6431	6436
				6440	6444	6447	6454	6459	6465	6474	6501	6504	6516	6522	6547
				6551	6555	6558	6566	6576	6586	6589	6590	6593	6595	6605	6614
				6622	6931	6961	7052	7074	7100	7136	7156	7217	7238	7374	7381
				7386	7388	7467	7576	7639	7641	7691	7868	7905	7976	8081	8148
				8149	8156	8182	8183	8186	8187	8211	8242	8243	8269	8272	8291
				8329	8337	8344	8389	8399	8416	8427	8428	8455	8503	8528	8549
				8563	8564	8578	8585	8588	8636	8771	8784	8786	8806	8824	8916
				8926	8972	8974	8978	8993	9009	9012	9119	9125	9140	9162	9211
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-42
; 							Cross Reference Listing					

				9219	9221	9226	9230	9232	9255	9281	9303	9319	9335	9336	9346
				9353	9355	9363	9375	9380	9390	9391	9419	9426	9443	9445	9457
				9469	9479	9528	9578	9596	9597	9600	9607	9611	9612	9628	9631
				9638	9643	9648	9659	9660	9663	9664	9669	9670	9672	9678	10119
				10128	10148
	CALL []			2779 #	4043	4068	4630	4633	4634	4636	4637	4639	4683	4688	4693
				4850	5049	5073	5395	5397	5488	5551	5556	5563	5568	6431	6436
				6440	6444	6447	6454	6459	6465	6474	6501	6504	6516	6522	6547
				6551	6555	6558	6566	6576	6586	6589	6590	6593	6595	6605	6614
				6622	6931	6961	7052	7074	7100	7136	7156	7217	7238	7374	7381
				7386	7388	7467	7576	7639	7641	7691	7868	7905	7976	8081	8148
				8149	8156	8182	8183	8186	8187	8211	8242	8243	8427	8428	8503
				8824	9140	9336	9391	9419	10128
	CALL.C			2781 #
	CALL.M			2782 #	5485	5549	5561	5670	5873	6357	6359	8326	8675	8754	9252
				9300	9324	9372	9438	9455	9504	9595	9679
	CALL.S			2783 #	5651	6254
	CALL[]			2780 #
	CBR			2784 #	9852	9863	9940	9946
	CLR ACC+SET UCODE	2785 #
	CLR ACCOUNT EN		2786 #	3743	9721	9722
	CLR AR			2787 #	3731	6089	6192	6488	6539	6541	6596	6727	6932	6939	6968
				6973	7053	7066	7636	8260	8576	8825	9577
	CLR ARX			2788 #	5298	5389	5413	6090	6095	6155	6208	6209	6291	6388	6522
				6530	6688	7411	7420	7427	7435	7442	8265	8446
	CLR EBUS DEMAND		2789 #	9173
	CLR EXP			2790 #	8514
	CLR FE			2791 #	3905	3922	4725	4743	5137	5193	6173	6457	6461	6596	6663
				7081
	CLR FPD			2792 #	7392	7394	7410	7419	7426	7434	7441	7443	7444	7502	7550
				7551	7577	7826	8616	9135
	CLR MQ			2793 #	5647	6409	6967	7064	7085
	CLR MTR PA EN		2794 #
	CLR PT LINE		2796 #	9402
	CLR PT LINE (KEEP)	2797 #	9408
	CLR P			2795 #	3993	5048	5072
	CLR SC			2798 #	3775	3905	3922	4725	5137	5193	5736	6545	7412	7421	7428
				7436	7522	7530	7536	7543	7549
	CLR SPECIAL CYCLE	2799 #	9839
	CLR SR2			2800 #
	CLR SR3			2801 #	6300
	CLR TRACKS EN		2802 #
	CLR TRK+PA EN		2803 #
	CMS FETCH		2804 #	8393
	COMP FETCH		2805 #	4507
	CONI APR(L)		2806 #	9258
	CONI APR(R)		2807 #	9257
	CONI MTR		2808 #	9459
	CONI PAG		2809 #	9377
	CONI PI(L)		2810 #	9308
	CONI PI(PAR)		2811 #	9306
	CONI PI(R)		2812 #	9304
	CONO APR		2813 #	9254	9260
	CONO MTR		2814 #	9456
	CONO PAG		2815 #	9373
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-43
; 							Cross Reference Listing					

	CONO PI			2816 #	9301
	CONO TIM		2817 #	9444
	CONTINUE		2818 #	4740
	CSMSK			2819 #	9875	9955	9970
	DATAI APR(L)		2821 #	9227
	DATAI PAG(L)		2822 #	5082	9354
	DATAO APR		2823 #	9220
	DATAO PAG(L)		2824 #	9325
	DIAG IN			2825 #	4776	5082	9227	9257	9258	9304	9306	9308	9354	9364	9377
				9381	9431	9432	9433	9434	9447	9449	9459	9478	9487	9488	9489
				9490	9740	9755
	DIAG OUT		2826 #	9220	9254	9260	9301	9325	9373	9444	9456	9471	9472	9473
	DISMISS			2827 #	4659	4661	4673	9144	9598
	DIVIDE			2828 #	5540	5543	5548	5560	5594	5595	5596	5597	5598	5599	5600
				5601	5625	5626	5627	5628	7638
	DLEN			2829 #	8255	8256	8263	8270	8273	8302	8345	8347	8719	8727	8739
				10064	10088	10092	10100
	DLEN_AR			2830 #	8255	8256	8302	8347	8727
	DROP EBUS REQ		2831 #	9185
	DSTP			2832 #	8261	8415	8415	8722	8784	8784	8794	8811	8812	8815	8860
				8862	8960	8961	10157	10159
	DSTP2_AR		2835 #	8805	8966
	DSTP2			2834 #	8796	8797	8805	8947	8966	9006
	DSTP_AR			2833 #	8811	8812	8815	8960	8961	10159
	DSTW			2836 #
	DSTW_AR			2837 #
	E0			2841 #	6432	6439	6467	6472	6550	6660	8056	8721	8925	8927	9009
	E0_AR			2842 #	8056
	E1			2843 #	6448	6457	6476	6517	6523	6542	6585	6693	6912	6913	6920
				6921	6922	6969	7054	7067	8099	8102	8104	8107	8627	8628	8678
				8679	8680	8689	8973	8974
	E1_AR			2844 #	8099	8102	8104	8107
	EA MOD DISP		2845 #	3775	3844	4649	7381	7386	7474	7484	7870	7871	7907	7908
				7922	7923	7968	7978	7979	8065	8083	8084	8759	8774	8813	8816
	EPT FETCH		2846 #	9572	9573	9579
	EPT REF			2847 #	9524	9582	9617	9805
	EPT REF CACHE		2848 #	8213
	EXEC REF		2849 #
	EXIT			2850 #	3971	3980	3982	4176	4178	4181	4184	4188	4192	4201	4203
				4211	4212	4217	4218	4224	4226	4241	4251	4266	4281	4291	4301
				4311	4321	4331	4342	4352	4362	4372	5350	5363	5417	5418	5489
				5490	5557	5569
	EXP TEST		2851 #
	EXP TST			2852 #	6213	6409
	EXPMSK			2853 #	3798	6666	6668	6732	6869	6870	6876	6877	7088	7091	8227
	EXP_-SC-1		2854 #
	EXP_-SC-1 TST		2855 #
	EXP_1			2856 #	6341
	EXP_FE TST		2857 #	6213	6409
	EXP_SC			2858 #	6970
	EXP_SC.MS		2859 #	6728
	EXP_SCAD		2860 #	6341	6970	8514
	EXP_SCAD.C		2861 #	6213	6409
	EXP_SCAD.MS		2862 #	6728
	EXP_SIGN		2863 #	5802	5814	5866	5870	5872	5996	6053	6310	6337	7082
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-44
; 							Cross Reference Listing					

	EXP_SIGN.C		2864 #	6977
	EXP_SIGN.MS		2865 #
	EXP_SIGN.S		2866 #	6252
	EXT ADDR		2867 #	8076
	EXT BYTE READ		2868 #	7378	7382
	EXT BYTE RPW		2869 #	7471	7475
	EXT INDEX		2870 #	8077	8085	8087
	EXT INDRCT		2871 #	8078	8079
	FETCH			2921 #	3795	4572	4587	4604	4628	4659	4676	4813	4837	4839	4874
				4880	4894	5089	5256	5329	5333	5335	8483	8618	9599
	FETCH WAIT		2923 #	5138	5184	5191
	FETCH+1			2922 #	8873
	FE_#			2873 #	3845	3869	3882	5205	5394	5396	5414	5496	5507	5510	5511
				5517	5550	5562	5629	5630	5631	5632	5664	5679	5734	5871	5901
				6065	6263	6333	6366	6526	6552	6557	6604	6975	7049	7063	7405
				7406	7448	7449	7515	7517	7555	7556	7633	7636	7641	7642	7788
				7791	7925	7952	8148	8156	8295	8386	8526	8584	8587	8722	8740
				8971	9281	9421	9734	9739	10052
	FE_# AND S		2874 #	9570	9622	9659	9668
	FE_#+AR0-8		2875 #
	FE_#+SC			2876 #	5228	5874	7480	7498
	FE_#-SC			2877 #	7500	8019
	FE_+#			2878 #
	FE_-1			2879 #	5389	5392	5713	9805	9806
	FE_-SC			2881 #
	FE_-SC-1		2882 #	7479	10021
	FE_-S			2880 #	7473	7483	7485
	FE_0			2883 #
	FE_1			2884 #	5707
	FE_AR0-8		2885 #	4737	6463	6472	6945	8253	8552
	FE_AR0-8 AND #		2886 #
	FE_AR0-8 COMP		2887 #	8433
	FE_EXP			2888 #	5802	6252	10016
	FE_EXP+1		2889 #
	FE_EXP+SC		2890 #	6053
	FE_EXP-#		2891 #	6081	6085
	FE_EXP-1		2892 #
	FE_FE AND AR0-8		2894 #	8164	9811	9901
	FE_FE AND #		2893 #	9854	9871
	FE_FE OR #		2895 #	9893	9916	9964	9999
	FE_FE OR AR0-8		2896 #
	FE_FE SHRT		2897 #	9905
	FE_FE+#			2898 #	5513	5911	5914	6126	6160	6378	6380	6677	6678	6956	6963
				8888
	FE_FE+1			2899 #	5435	5442	5444	5446	5448	5457	5459	5461	5462	5467	5670
				5824	5825	5828	6162	6212	6356	6358	6381	6408	6563	6565	6592
				6594	6680	6711
	FE_FE+S			2900 #
	FE_FE+SC		2901 #	5207	5815	6312	9784	9786
	FE_FE-#			2902 #	5219	5239	6131	6133	6153	6157	6166	6175	8868	8882	8897
				8900	8917	8963	9962
	FE_FE-1			2903 #	5540	5543	5548	5560	5594	5595	5596	5597	5598	5599	5600
				5601	5625	5626	5627	5628	6129	6384	6683	6959	7638	8537	8539
				9401	9407
	FE_FE-S			2904 #	7595	7597	7700
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-45
; 							Cross Reference Listing					

	FE_FE-SC		2905 #	6385	6405	6685	6706	7684	7686	9936
	FE_FE-SC-1		2906 #	7751
	FE_P			2907 #	5314	5317	7592	7593	8703	8764	8798
	FE_P AND #		2908 #	8705	8834	8920	9333	9782
	FE_P AND SC		2909 #
	FE_P OR #		2910 #
	FE_P+S			2913 #
	FE_P+SC			2914 #	8884
	FE_P+#			2911 #
	FE_P+1			2912 #	7680
	FE_P-S			2916 #
	FE_P-#			2915 #
	FE_S			2917 #	7380	7385	7387	7968	8217	8758	8773	8774	8811	8812	8815
	FE_S+#			2918 #
	FE_SC			2919 #	5437	5438	5439	5440	5451	5452	5453	5454	5612	5615	8017
				8590	8592
	FILL			2924 #	8149	8187	8188	8243	8344	8503	8559	8564
	FILL_AR			2925 #	8149	8243	8503	8564
	FIN LOAD		2926 #	4040	4042	4709	7389	7410	7412	7413	7414	7415	7416	7417
				7419	7426	7428	7429	7430	7431	7432	7434	7441	7443	7444	7448
				7449	7553
	FIN STORE		2927 #	3900	3902	3908	3910	4073	4524	4685	4687	4813	4818	4880
				4894	5056	5064	5103	7189	7190	7201	7203	7223	7225	7286	7288
				7312	7577	7801	8956
	FIN XFER		2928 #	3875	8954	9527
	FINISH			2929 #	3936	5696	8486
	FM(#)_AR		2932 #
	FM[]_AR			2931 #	3740	3798	6432	6437	6439	6445	6448	6465	6466	6473	6474
				6477	6505	6517	6523	6542	6549	6559	6585	6599	6619	6659	6662
				6969	6976	7054	7056	7067	7069	7097	7268	9549	10124
	FM_AR			2930 #	3740	3798	5701	5717	6432	6437	6439	6445	6448	6465	6466
				6473	6474	6477	6505	6517	6523	6542	6549	6559	6585	6599	6619
				6659	6662	6969	6976	7054	7056	7067	7069	7097	7268	7604	7625
				7751	8056	8099	8102	8104	8107	8149	8243	8255	8256	8257	8259
				8267	8300	8302	8325	8347	8391	8437	8442	8444	8451	8503	8538
				8540	8549	8550	8562	8564	8613	8675	8703	8705	8727	8732	8758
				8770	8773	8774	8805	8811	8812	8815	8830	8836	8960	8961	8966
				8995	9549	10124	10155	10159
	FORCE AR-ARX		2933 #	9721	9722
	GEN # AND AR0-8		2937 #	9827	9864	9888	9922
	GEN # AND SC		2938 #
	GEN #+AR0-8		2939 #	8046	8840
	GEN #+SC		2940 #	5259	5292	6277
	GEN #-S			2941 #
	GEN #-SC		2942 #	9961
	GEN -AC0		2943 #
	GEN -AR LONG		2944 #	4054	5542	5545	5997	6260	6416	6438	6611	8523
	GEN -AR*4		2945 #
	GEN -BR LONG		2946 #	4044	4069
	GEN -SC			2947 #
	GEN -SC-1		2948 #
	GEN 0S			2949 #
	GEN 2AR			2950 #
	GEN AC0			2952 #	7076
	GEN AC0+1		2953 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-46
; 							Cross Reference Listing					

	GEN AR			2954 #	3833	3835	6299	6343	6443	6579	6766	6927	6978	7059	7101
				7378	7382	7471	7475	7859	7863	7895	7900	7970	7972	8064	8076
				8078	8219	9169	9172	9174	9176	9187	9189	9195	9310
	GEN AR*2 LONG		2955 #	5579
	GEN AR*AC0		2956 #	4507	5133	5135	6265	6587
	GEN AR*BR		2957 #	5415	5680	6934	8542
	GEN AR*T0		2958 #
	GEN AR+1		2959 #	6889	7788	7791	7812	7815	7817	7819	7821	7952
	GEN AR+2BR		2960 #	6001
	GEN AR+BR		2961 #
	GEN AR+E1		2962 #
	GEN AR+FM[]		2963 #
	GEN AR+XR		2964 #	3834	3837	7861	7865	7897	7902	7971	7973	8077	8079
	GEN AR-2BR		2965 #	5999
	GEN AR-AC3		2966 #	8457
	GEN AR-BR		2967 #	6490	6938
	GEN AR-BR-1		2968 #	6934	8542
	GEN AR-FM[]		2969 #
	GEN AR-FM[]-1		2970 #
	GEN AR0-8		2971 #
	GEN ARX			2972 #	6262	6441	7872	7909	7929	7980	8085	8300	8393
	GEN ARX COMP		2973 #	8558
	GEN ARX*BRX		2975 #
	GEN ARX*BR		2974 #
	GEN ARX+1		2976 #
	GEN ARX+XR		2977 #	3815	3819	3823	3825	7874	7912	7931	7981	8087
	GEN ARX-1		2978 #	5125
	GEN ARX-FM[]		2979 #
	GEN BR			2981 #	4851	7155
	GEN BR*2		2982 #	9771
	GEN BR+ARX		2983 #
	GEN BRX+1		2984 #
	GEN CRY18		2985 #	4613	6150	7138	7139	9134	9148
	GEN E1			2986 #	6693
	GEN FE			2987 #
	GEN FE AND #		2988 #	5324	5326	5328	5332	5334	8214	9952	9995
	GEN FE AND AR0-8	2989 #
	GEN FE AND S		2990 #
	GEN FE AND SC		2991 #	9924
	GEN FE OR AR0-8		2992 #	8165
	GEN FE+#		2993 #
	GEN FE-#		2994 #	6954	7735	7736	7738
	GEN FE-1		2995 #	4740
	GEN FE-S		2996 #
	GEN FE-SC		2997 #
	GEN FE-SC-1		2998 #	7895	7898	7910	7913	8012
	GEN FM[]		2999 #	10108
	GEN MQ			3000 #	7160	7161
	GEN MQ*AC0		3001 #	5555	5567
	GEN MQ-BR		3002 #	7218
	GEN MQ-BR-1		3003 #	7158	7276
	GEN P AND SC		3005 #	9954	9997
	GEN P AND #		3004 #
	GEN P+SC		3006 #
	GEN P-#			3007 #	7812	7815	7817	7819
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-47
; 							Cross Reference Listing					

	GEN P-S			3008 #	8977
	GEN P-SC		3009 #
	GEN SC			3010 #	8062
	GEN SCAD 0S		3011 #
	GEN T1			3012 #
	GEN T2			3013 #
	GET ECL EBUS		3014 #	3827	3828	3829	3830	4675	9206	9477	9730
	GLOBAL			3015 #	3813	3815	3817	3819	3822	3823	3824	3825	7378	7382	7471
				7475	7872	7874	7909	7912	7929	7931	7980	7981	8085	8087
	HALT			3017 #	4651
	HARDPFW			3018 #	10124
	I FETCH			3019 #	3875	3899	3901	3902	3908	3910	3984	3990	3995	4042	4045
				4081	4758	4786	4818	5126	5167	5179	5213	5221	5234	5242	5262
				5284	5313	5398	5583	5694	5735	6096	6097	6404	6413	6705	6762
				6888	6890	6891	6928	6936	6940	7189	7201	7223	7265	7286	7312
				7390	7410	7412	7413	7414	7415	7416	7417	7419	7426	7428	7429
				7430	7431	7432	7434	7441	7443	7444	7448	7449	7577	7606	7681
				7702	7710	8283	8556	9143
	INDEXED			3020 #	3834	3837	7861	7865	7897	7902	7971	7973	8077	8079
	INH CRY18		3021 #	5127	7226	7239	8760	8768	8795	8799	8803	8869	8899
	IO INIT			3022 #	9187
	IR DISP			3023 #	3864	3870	3876	3883	3889	8100	8103	8105	8108
	JFCL FETCH		3025 #	4760
	JFCL S			3026 #	4762
	JFCL T			3027 #	4759
	JUMP FETCH		3028 #	4574	4589	4606	4613
	LD PCS			3030 #	4678
	LD PREV CTXT		3031 #	5084
	LOAD AR			3032 #	4023	4038	4634	4642	4687	4709	5056	5711	6925	6966	7048
				7062	7191	7216	7226	7274	7289	7314	7690	7707	7796	8157	8213
				8364	8559	8627	8689	8721	8832	8833	8889	8893	8925	8927	8974
				9009	9151	9524	9525	9582	9583	9639	9640	9644	9645	9805	9806
				9824	9829	9844	9866	9891	9915	9950	10133
	LOAD AR (RPW)		3033 #	9556	9563
	LOAD AR (WR TST)	3034 #	8932	8935
	LOAD ARX		3035 #	3782	4040	4811	5054	5064	5074	5650	6253	6430	6946	7051
				9527	9575
	LOAD ARX (WR TST)	3036 #	8953
	LOAD EBR		3037 #	9391
	LOAD IR			3038 #	3777	3788	5150	8064
	LOAD UBR		3039 #	9336
	LOAD VMA(EA)		3040 #	7155
	LONG EN			3041 #	3840	7868	7905	7976	8081
	MAP			3045 #	4771	9753
	MB WAIT			3046 #	3863	4774	8214	9215	9347	9418	9482	9740	9754	9953	10115
				10125
	MEM_AR			3047 #	4055	4079	4815	4820	5078	5088	5100	7206	7310	7376	7380
				7384	7469	7473	7477	7483	7826	7968	8787	8958	9005	9008	9129
				9131	9140	9147	9507	9598	9886
	MQ_0.C			3049 #
	MQ_0.M			3050 #	5251	5520	5824	5825	5828	6284	6287	6357	6359	6593	6595
				6623	6759	8589	8591
	MQ_0.S			3051 #	4043	4067	5651	6252	6431	6926	7050
	MQ_1			3052 #	5513
	MQ_1S			3053 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-48
; 							Cross Reference Listing					

	MQ_AC0			3054 #	5207
	MQ_AD			3055 #	5207	5260	5513	5554	5566	6578	7186	7221	7280	7284	7307
				7479	7678	8222
	MQ_ARX			3062 #	5187	5196	5209	5214	5218	5230	5235	5717	6313	6315	7057
				7269	7523	7524	7525	7526	7527	7528	7531	7532	7533	7534	7537
				7538	7539	7540	7541	7544	7545	7546	7547	7575	7595	8150	8209
				8438	8846	8848	8862
	MQ_ARX COMP		3063 #
	MQ_ARX*2		3064 #
	MQ_ARX+BR		3065 #	5566
	MQ_ARX-BR		3066 #	5554
	MQ_AR			3056 #	3899	3900	4055	4832	4834	5093	5386	5389	5413	5629	5630
				5631	5632	5672	5678	5740	5816	5819	5871	6329	6333	6340	6394
				6397	6399	6554	6557	6562	6610	6695	6698	6700	7135	7216	7677
				8012	8056	8257	8386	8577	8613	8784	9428	9479	9548
	MQ_AR (AD)		3057 #
	MQ_AR COMP		3058 #
	MQ_AR SWAP		3059 #
	MQ_AR XOR BR		3060 #	5260
	MQ_AR-1			3061 #
	MQ_BR			3067 #	8222
	MQ_BR COMP		3068 #
	MQ_FM[]			3069 #	7479
	MQ_MQ*.25		3070 #	5692	6377	6676	6723
	MQ_MQ*2			3071 #	5690	5730	5732	8401
	MQ_MQ*BR		3072 #	6578
	MQ_MQ+1			3073 #	7186	7221	7284
	MQ_MQ-1			3074 #	7280	7307	7678
	MQ_SHIFT		3075 #	5212	5220	5233	5241	5258	5298	5663	5821	6094	6154	6281
				6290	6292	6296	6388	6389	6530	6688	6689	9333
	MSK			3077 #	8267	8451	8681	8714	8716	8830
	MSK_AR			3078 #	8267	8451	8830
	MUL			3079 #	5435	5442	5444	5446	5448	5457	5459	5461	5462	5467	5670
	MUL DISP		3080 #	5395	5397
	NO CRY			3082 #	4467	4469	4472
	NORM			3083 #	5831	5913	5925	6127	6153	6158	6166	6176	6209	6294	6343
				6362	6382	6390	6395	6398	6399	6528	6580	6601	6667	6669	6691
				6696	6699	6701	6710	6766	6964	6978	7059	7070	7101
	NORM AR			3085 #	5874
	NORM -AR		3084 #	6125
	NXT INSTR		3086 #	3905	3922	5137	5193
	OPTIONS			3088 #	9234
	P0			3122 #	9875	9955	9970
	P1			3123 #	9877	9992
	P10			3124 #	9765	9813	9830	9868	9876	9892	9903	9929	9966	9980	10017
	P11			3125 #	9770	10015
	P12			3126 #	9763	9764	9789	9997	10001	10104	10117
	P13			3127 #
	P14			3128 #
	P15			3129 #
	P16			3130 #	9723	10020	10144
	P17			3131 #	9724	10024	10026
	P2			3132 #	9852	9863	9940	9946
	P3			3133 #	9823	9838	9914
	P4			3134 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-49
; 							Cross Reference Listing					

	P5			3135 #	9773	9902	10005	10036	10043
	P6			3136 #
	P7			3137 #
	PC_VMA			3138 #	4726
	PF DISP			3139 #	9730	9741
	PFA			3140 #
	PFA_AR			3141 #
	PHYS REF		3142 #	4023	4025	9563	9565	9640	9645	9650	9824	9829	9844	9866
				9886	9891	9915	9950
	PHYS REF CACHE		3143 #
	POP AR			3144 #	4827
	POP AR-ARX		3145 #	4829
	POP ARX			3146 #
	PORTAL			3147 #	4628
	PT FETCH		3148 #
	PT REF			3149 #	3782	5054
	PT SEL_INVAL		3150 #	9393
	PT SEL_INVAL (KEEP)	3151 #	9397
	PT SEL_NORMAL		3152 #	9404	9410
	PUR			3153 #	9877	9992
	PUSH			3154 #	4804	4809
	P_#			3090 #	7730	7732	7734	7737	7740	7744	7746	7749	7750
	P_# AND SC		3091 #
	P_#+SC			3092 #
	P_#-SC			3094 #	7600
	P_#-S			3093 #	8778	8800
	P_-SC			3095 #
	P_0			3096 #
	P_1S			3097 #
	P_FE			3098 #	5579	7752	8767	8994
	P_FE AND #		3099 #
	P_FE OR SC		3100 #	8903	8904	9757
	P_FE+S			3101 #
	P_FE+SC			3102 #	7763
	P_FE-S			3103 #	7794	7953	8297	8761
	P_FE-S.S		3104 #	7796
	P_FE.C			3105 #	8802
	P_P AND #		3106 #	6755	8643	8706	8710	8909
	P_P AND SC		3107 #	8870
	P_P OR #		3108 #	8707	8708	8709	8711
	P_P OR SC		3109 #	8731	9015	10006
	P_P+#			3110 #
	P_P+1			3111 #	7811	7814	7830	7834	7838	7842	8871
	P_P+S			3112 #	10154
	P_P+S.C			3113 #	10158
	P_P-S			3114 #	7602	7779	8299	8754	8791	9679
	P_SC			3115 #	7693	9359
	P_SC#			3116 #	7799	7822	7823
	P_SCAD			3117 #	3993	5048	5072	5579	7602	7693	7700	7752	7779	8299	8754
				8767	8791	8903	8904	8994	9359	9679	9757	10154
	P_SCAD#			3118 #	6755	7600	7730	7732	7734	7737	7740	7744	7746	7749	7750
				7763	7799	7811	7814	7822	7823	7830	7834	7838	7842	8643	8706
				8707	8708	8709	8710	8711	8731	8778	8800	8802	8871	8909	9015
				10006	10158
	P_SCAD.C		3119 #	7794	7953	8297	8761	8870
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-50
; 							Cross Reference Listing					

	P_SCAD.S		3120 #	7796
	R0			3157 #	9754
	R1			3158 #
	R10			3159 #	8257	8269	8278	8283	8301	8325	8437	8454	8459	8465	8550
				8611	8613	8675	8726	8728	8729	10162	10171	10173	10183
	R11			3160 #	5717	5739	6466	6474	6507	6519	7625	7650	8538	8540	8577
	R12			3161 #	6437	6445	6452	6461	6477	6549	6598	6599	6615	6619	6659
				6662	6739	6976	7056	7069	7097	8549	8576
	R13			3162 #	8149	8187	8188	8243	8344	8503	8559	8564
	R14			3163 #
	R15			3164 #	3740	4656	4692	4808	4878	4883	5050	5089	5093	5097	5101
				8229	8230	9551	9584	9601	9619	9633	9653	10125
	R16			3165 #	6432	6439	6467	6472	6550	6660	8056	8721	8925	8927	9009
	R17			3166 #	10124
	R2			3167 #	10143
	R3			3168 #	9549	10142
	R4			3169 #	3798	6666	6668	6732	6869	6870	6876	6877	7088	7091	8227
	R5			3170 #	6448	6457	6476	6517	6523	6542	6585	6693	6912	6913	6920
				6921	6922	6969	7054	7067	8099	8102	8104	8107	8627	8628	8678
				8679	8680	8689	8973	8974
	R6			3171 #	5701	5719	6465	6473	6503	6505	6508	6520	6559	7268	7604
				7677	7751	8391	8400	8562	8565	10108	10166
	R7			3172 #	8267	8451	8681	8714	8716	8830
	RD+CLR C CNT		3174 #	9490
	RD+CLR E CNT		3175 #	9489
	RD+CLR PA		3176 #	9488
	RD+CLR TB		3177 #	9487
	READ BP2		3178 #	7387	7485
	READ EBR		3179 #	9379
	READ ERA		3180 #	9279
	READ UBR		3181 #	9362
	REL EBUS		3182 #	9176	9310
	REL ECL EBUS		3183 #	4777	9126	9383	9417	9439	9481	9491	9741	9756
	REQ EBUS		3184 #	9162	9314
	REQ SV.VMA		3185 #	10036
	REQ VMA HELD		3186 #
	RETURN []		3189 #	4055	4709	4856	7176
	RETURN0			3190 #	7930	7932	10037
	RETURN1			3191 #	4720	5104	6479	7970	7971	7980	7982	8222	8677	8739	9202
				9206	9508	9529
	RETURN10		3192 #	8188	8192
	RETURN12		3193 #	9630
	RETURN15		3194 #	7787	7804
	RETURN16		3195 #
	RETURN17		3196 #	5582	7790
	RETURN2			3197 #	4862	6000	6002	6490	7926	8004	8005	8202	8231	8350	8721
				8778	9467	9633	9654
	RETURN20		3198 #	4704	5098	8166
	RETURN3			3199 #	3808	5629	5632	7924	8014	8023	8722	9015	9177	9190	9366
	RETURN30		3200 #
	RETURN37		3201 #	7860	7862	7873	7875
	RETURN4			3202 #	5451	5452	5453	5454	6314	6316	7780	7781	7802	7811	7814
				7822	7823	7830	7834	7838	7842	7951	7954	8681	8716	8732	9421
				9625
	RETURN5			3203 #	5630	5631	8705
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-51
; 							Cross Reference Listing					

	RETURN6			3204 #	5437	5438	5439	5440	5552	5564	5613	5616	8787
	RETURN7			3205 #	5553	5565
	RET[]			3188 #	6869	6870	6876	6877
	RSTR FLAGS_AR		3206 #	4653	4669	4689
	RSTR VMA_ARX		3207 #
	RSTR VMA_MQ		3208 #	8221
	RSTR VMA_SV.VMA		3209 #	10005
	SBR			3213 #	9823	9838	9914
	SBUS DIAG		3214 #	9283
	SC_#			3216 #	3731	3744	3746	4053	4065	4755	4757	4838	4839	5078	5079
				5261	5301	5302	5312	5386	5399	5415	5417	5489	5490	5555	5567
				5654	5683	5703	5747	6091	6149	6155	6293	6343	6361	6374	6383
				6389	6454	6459	6527	6575	6579	6600	6672	6690	6724	6730	6735
				6742	6744	6746	6766	6978	7079	7083	7099	7136	7411	7413	7414
				7415	7416	7417	7420	7422	7423	7424	7427	7429	7430	7431	7432
				7435	7437	7438	7439	7442	7523	7524	7525	7526	7527	7528	7531
				7532	7533	7534	7537	7538	7539	7540	7541	7544	7545	7546	7547
				7624	7754	7755	7824	7829	7833	7837	7841	8029	8210	8260	8447
				8473	8480	8482	8507	8523	8526	8584	8587	8620	8691	8720	8825
				8838	8857	8979	9011	9013	9211	9326	9346	9348	9352	9365	9376
				9390	9429	9440	9485	9505	9555	9578	9624	9638	9643	9648	9753
				9810	9907	10118	10120	10161
	SC_# AND AR0-8		3217 #	8835	9787
	SC_# AND S		3218 #
	SC_# OR SC		3219 #
	SC_#+AR0-8		3220 #	8049
	SC_#+SC			3221 #	5169	5180	5182	5184	5188	5191	5195	5196	5209	5214	5230
				5235	5253	5255	5272	5276	5296	5299	5816	5823	6000	6002	6280
				6313	6330
	SC_#-S			3222 #
	SC_#-SC			3223 #	3808	5819	6315	6509	6513	7389	7991	8020	8785	9671
	SC_#-SC-1		3224 #
	SC_-SC			3226 #	9941	9951
	SC_-SC-1		3227 #	10025
	SC_-S			3225 #	7575
	SC_0			3228 #
	SC_1			3229 #	5488	5701	5707	8504	8613	9934
	SC_1S			3230 #	4635	5737
	SC_AR0-8		3231 #
	SC_EA			3232 #	5167	5178	5205	5227	5251	5270	6051	7762
	SC_EXP			3233 #	5870	6968	10016	10019
	SC_EXP+1		3234 #	5866
	SC_EXP+SC		3235 #	5872	6337
	SC_EXP-#		3236 #
	SC_EXP-1		3237 #
	SC_EXP-SC		3238 #	5814	5996	6310
	SC_FE			3239 #	5218	5239	6508	6511	7479	7498	7500	7634	7636	7996	8018
				9419	10005
	SC_FE AND #		3240 #
	SC_FE#			3241 #	7378	7382	7395	7471	7475	7778
	SC_FE+#			3242 #	6093
	SC_FE+1			3243 #	8847	8849	8850
	SC_FE+S			3244 #
	SC_FE+SC		3245 #	6726	7392	7860	7862	7873	7875	7998	8417	8756	8837	9664
	SC_FE-#			3246 #	8554
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-52
; 							Cross Reference Listing					

	SC_FE-1			3247 #	5388	6267
	SC_FE-SC		3248 #	7501	8022
	SC_FE-SC-1		3249 #	8538	8540
	SC_P			3250 #	7692	7798	8926	9680
	SC_P AND #		3251 #	8889	8891
	SC_P AND SC		3252 #	9756	9940	9946
	SC_P+S			3255 #	7678
	SC_P+#			3253 #
	SC_P+1			3254 #	7821
	SC_P-SC-1		3257 #
	SC_P-#			3256 #	7373	7377	7466	7469	7573	7787	7790	7804	8200
	SC_S			3258 #	8265
	SC_SC AND #		3259 #
	SEL AC4			3261 #	8191	8485
	SEL AC5			3262 #
	SEL DSTP		3263 #	8808	8809	8958	10158	10163
	SEL DSTP2		3264 #	8189	8802	8803	8965
	SET ACC+CLR UCODE	3265 #
	SET ACCOUNT EN		3266 #	3774	4696	4787	9482	9493	10037	10116	10135	10149
	SET AROV		3267 #	5264	5294	5402	5418	5684	6088	6891
	SET CONS XCT		3268 #	4743
	SET DATAI		3269 #	9607	9660
	SET DATAO		3270 #	9140	9597	10148
	SET EBUS DEMAND		3271 #	9170
	SET FL NO DIV		3272 #	5904	6369	6607
	SET FLAGS_AR		3273 #	3793	5107	10134
	SET FLOV		3274 #	6748	6753	6936	7094
	SET FPD			3275 #	7380	7385	7388	7473	7483	7486	7778	7968	8579	9151	9152
	SET FXU			3276 #	6752	6940	7095
	SET HALTED		3277 #	4725
	SET IO PF		3278 #	10145
	SET MTR PA EN		3279 #
	SET NO DIVIDE		3280 #	5583	5735	7606
	SET PC+1 INH		3281 #	3783
	SET PI CYCLE		3282 #	3730	9492	9548
	SET PXCT		3283 #	5149
	SET SR1			3284 #	5925	6125
	SET SR2			3285 #	5722	6354	6376	6618
	SET SR3			3286 #	5742	6301	6327	6675
	SET SXCT		3287 #
	SET TRACKS EN		3288 #
	SET TRK+PA EN		3289 #
	SFLGS_AR		3291 #	8259	8442	8444	8703	8705	8732	8836	8995
	SFLGS			3290 #	8259	8285	8338	8442	8444	8472	8677	8693	8695	8703	8705
				8720	8732	8836	8980	8995	9011	9013	10176	10180
	SH DISP			3292 #	5080	6743	7405	7406	7515	7517	7821	8838	8855	9551	9570
	SIGNS DISP		3293 #	5497	5518	5520	5721	5723	7163	7167	7169	7187	7199	7204
				7220	7222	7271	7285	7308	7626	8296	8300	8327	8347	8390	8391
				8405	8437	8558	8615	8640	8676	8704	8727	9771	9801	9803
	SKIP FETCH		3294 #	4524
	SKP -EBUS GRANT		3296 #	9184
	SKP -EBUS XFER		3297 #	9174
	SKP -LOCAL AC ADDR	3298 #	3990	3995	5048	5072
	SKP -START		3299 #	4736
	SKP -VMA SEC0		3300 #	7384	7477	7594	7687	7792
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-53
; 							Cross Reference Listing					

	SKP AC EQ 0		3301 #	4755	7573
	SKP AC REF		3302 #	7161	7162
	SKP AC#0		3303 #	3903	3984	4525
	SKP AC0+		3304 #	5613
	SKP AC0-		3305 #	5616
	SKP AD NE		3306 #	6262	6299	6325	6441	6540	6693	6886	6889	6927	7077	8219
				9654	9852	9940
	SKP AD NZ		3307 #	5312	5580	7158	7218	7276	8047
	SKP AD0			3308 #	5133	5135	5416	5488	5510	5556	5568	5680	5708	5902	6265
				6365	6443	6447	6453	6490	6501	6503	6546	6550	6558	6586	6587
				6604	6616	6622	6660	6664	6934	6938	7073	7098	8254	8263	8506
				10108
	SKP ADX0		3309 #
	SKP AR EQ		3310 #	4744
	SKP AR EQ -1		3311 #
	SKP AR GT BR		3312 #	8361	9629
	SKP AR NE		3313 #	8522	8986	9010
	SKP AR NE BR		3314 #	5290
	SKP AR NZ		3315 #	5393	5508	5512	7605	7633
	SKP AR SIG		3316 #	5399
	SKP AR0			3317 #	3974	4215	4221	5128	6065	6338	6436	6440	6614	6930	6961
				7658	7692	7798	7876	8278	8328	8764	8798	10016	10019	10173	10183
	SKP AR1			3318 #
	SKP AR18		3319 #	4198	4208	5167	5178	5206	5227	5252	5270	5387	8101	8715
				9334	9965
	SKP AR2			3320 #	8456	9326
	SKP AR6			3321 #	9557	9559	9588	9609
	SKP ARX LE BRX		3322 #
	SKP ARX LT BRX		3323 #
	SKP ARX NE		3324 #	6123
	SKP ARX NZ		3325 #	7635
	SKP ARX+MQ NE		3326 #	6374	6672
	SKP ARX0		3327 #	7701	7752	8387	8691	8842	8860	8878
	SKP ARX2		3328 #	8634
	SKP BR EQ		3330 #	5912	8412
	SKP BR EQ -1		3331 #
	SKP BR0			3332 #	5734	5903	5996	6368	6606	6623	8283	8403	8578
	SKP CRY0		3333 #	4804	4809	4831	4850	5126	5713	5731	5732	5999	6001	6150
				6393	6694	8458	8519	8553	8611	8681	9960
	SKP EVEN PAR		3334 #
	SKP EXP NE		3335 #
	SKP FE NZ		3336 #
	SKP FE0			3337 #	5719
	SKP FETCH		3338 #
	SKP INTRPT		3339 #	3814	3816	3818	3820	3836	3838	4741	4785	5051	5145	7191
				7202	7227	7289	7315	7864	7866	7901	7903	7930	7932	7972	7974
				8078	8079	8296	8345	8417	8621	8756	8890	8894	9188	9810	9894
				10052
	SKP IO LEGAL		3340 #	4020	4635	4636	4637	4639	4774	9119	9125	9212	9219	9221
				9226	9230	9253	9256	9282	9300	9303	9320	9324	9352	9372	9375
				9427	9443	9446	9455	9458	9470	10104
	SKP KERNEL		3341 #	4633
	SKP MQ EQ -1		3342 #	8630
	SKP MQ NE		3343 #
	SKP MQ NE BR		3344 #	5262
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-54
; 							Cross Reference Listing					

	SKP MQ NZ		3345 #
	SKP PC SEC0		3348 #	4630	4806	4835	4846	4874	4876	5008	5010	5022	5026	5028
				5030	5037	5039	5041	5043	5046	8200	8762	8763	8796	8797	8831
				8863	8886	8888	8916	8959	8993
	SKP PI CYCLE		3349 #	9131	9134	9149	10117
	SKP P NE		3346 #	9875	9970
	SKP P!S XCT		3347 #	7141	7278
	SKP RPW			3350 #	9734	9739
	SKP RUN			3351 #
	SKP SC NE		3352 #	5271	9338
	SKP SC NZ		3353 #
	SKP SC0			3354 #	4654	5741	7390	7575	7576	7593	8557
	SKP SCAD NE		3355 #	7813	7816	7818	7820	8062	9827	9865	9889	9922	9925	9952
				9954	9996	9998
	SKP SCAD NZ		3356 #	5314	5317	5325	5327	5329	5333	5334	8046	8165	8215
	SKP SCAD0		3357 #	5170	5181	5182	5185	5188	5192	5195	5210	5215	5219	5229
				5231	5236	5240	5256	5259	5273	5277	5293	5814	5817	5819	5823
				6081	6085	6093	6278	6280	6310	6314	6316	6509	6955	7393	7595
				7597	7602	7735	7736	7738	7896	7898	7910	7913	7991	7998	8013
				8049	8299	8785	8841	8868	8882	8898	8901	8917	8963	8977	9672
				9679
	SKP USER		3358 #	4672	4673	4682	4692	5094	5146	9909	10127
	SLEN			3360 #	8257	8269	8278	8283	8301	8325	8437	8454	8459	8465	8550
				8611	8613	8675	8726	8728	8729	10162	10171	10173	10183
	SLEN_AR			3361 #	8257	8325	8437	8550	8613	8675
	SR DISP			3362 #	5742	5743	5748	6128	6130	6132	6134	6161	6163	6164	6174
				6193	6213	6392	6409	6750	6759	7193	7206	7677	7977	7993	8348
				9763	10044	10155	10159	10178	10181
	SRCP			3386 #	8290	8300	8388	8388	8740	8740	8758	8773	8774	8971	8971
				10056	10155
	SRCP2_AR		3389 #	8770
	SRCP2			3388 #	8762	8763	8770
	SRCP_AR			3387 #	8300	8758	8773	8774	10155
	SR_#			3363 #	5789	5861	10064	10092	10100
	SR_0			3364 #	3919	3921	3927	4784	5088	6223	6227	6237	6404	6414	6705
				6764	7328	7503	7762	8285	8305	8390	8484	8623	8677	9136	10056
				10068	10080	10084
	SR_1			3365 #	6590	7756	10060	10072	10088
	SR_2			3366 #
	SR_BDD			3367 #	8636
	SR_BDF			3368 #	8563
	SR_BDT			3369 #	8513	8580	8639	10076
	SR_BLT			3370 #	7167
	SR_BLT(AC)		3371 #	7138
	SR_BLT(PXCT DST)	3372 #	7217	7228
	SR_BLT(PXCT SRC)	3373 #	7157	7224
	SR_DB			3374 #	8450
	SR_DST			3375 #	8337
	SR_DSTF			3376 #	8291
	SR_ED(+D)		3377 #	8399	8978	9012
	SR_ED(PAT)		3378 #	8826	8887
	SR_ED(S)		3379 #	8389	8859
	SR_MAP			3380 #	4772
	SR_SRC			3381 #	8262	8266	8274	8290	8292	8331	10096
	SR_SRC+DST		3382 #	8272	8329
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-55
; 							Cross Reference Listing					

	SR_WORD			3383 #
	SR_XBLT(DST)		3384 #	7275	7290	7316
	SR_XBLT(SRC)		3385 #	7267	7287	7311
	STACK UPDATE		3391 #	4831	4849	5125
	STORE			3392 #	3987	4025	4065	4074	4205	4539	4540	4555	4556	4684	4686
				4694	4878	4879	4888	5055	5062	5076	5086	5102	5104	7186	7204
				7219	7221	7279	7279	7284	7296	7307	7502	7553	7787	7790	7794
				7799	7800	7802	7811	7814	7822	7823	7830	7834	7838	7842	7951
				7954	8023	8938	8954	8957	9000	9001	9002	9003	9006	9148	9234
				9286	9440	9506	9508	9565	9589	9590	9617	9618	9649	9650	9752
				9877	9992	10123	10130	10132
	STORE VMA(EA)		3393 #	7160	7161
	SV.ARX			3396 #	9724	10024	10026
	SV.ARX_AR		3397 #	9724
	SV.AR_AR		3395 #	9723
	SV.AR			3394 #	9723	10020	10144
	SV.BR_AR		3399 #	9765
	SV.BR			3398 #	9765	9813	9830	9868	9876	9892	9903	9929	9966	9980	10017
	SV.IOP			3400 #	9549	10142
	SV.IOPF			3401 #	10143
	SV.IOPF_AR		3402 #	10143
	SV.PAR			3403 #	9754
	SV.PAR_AR		3404 #	9754
	SV.PFW			3405 #	9763	9764	9789	9997	10001	10104	10117
	SV.PFW_AR		3406 #	9763	9764	9789	10001
	SV.SC_AR		3408 #	9770
	SV.SC			3407 #	9770	10015
	SV.VMA			3409 #	9773	9902	10005	10036	10043
	SV.VMA_AR		3410 #	9773
	SWD			3411 #
	SWD_AR			3412 #
	SWEEP CACHE		3413 #	9214
	T0			3417 #	5701	5719	6465	6473	6503	6505	6508	6520	6559	7268	7604
				7677	7751	8391	8400	8562	8565	10108	10166
	T0_AR			3418 #	5701	7604	7751	8391	8562
	T1			3419 #	5717	5739	6466	6474	6507	6519	7625	7650	8538	8540	8577
	T1_AR			3420 #	5717	7625	8538	8540
	T2			3421 #	6437	6445	6452	6461	6477	6549	6598	6599	6615	6619	6659
				6662	6739	6976	7056	7069	7097	8549	8576
	T2_AR			3422 #	8549
	TAKE INTRPT		3423 #	3841	4738	5063	5147	7869	7906	8082
	TEST AR			3424 #
	TEST AR.AC0		3425 #	4475
	TEST AR.BR		3426 #	4760	9130	9132	9960
	TEST AR.MSK		3427 #	8681
	TEST ARX		3428 #	8335
	TEST ARX.AR*4		3429 #
	TEST BRL		3430 #	9134
	TEST CBR		3431 #	9852	9940
	TEST FETCH		3432 #	4467	4469	4475	8335	9130	9132	9134
	TRAP1			3433 #
	TRAP2			3434 #	4815	4820	4833	4856	5138
	TRAP3			3435 #
	UNCSH PHYS REF		3449 #
	UPT FETCH		3450 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-56
; 							Cross Reference Listing					

	UPT REF			3451 #	5074	5076	9525	9806	10123
	USER REF		3452 #
	VMA_#			3454 #	5052	5061	5075	9480	9515	9517	9519	9521	10115
	VMA_#+AR32-35		3455 #	8212	9800	9802
	VMA_40			3456 #
	VMA_40+PI*2		3457 #	9553	9554
	VMA_41			3458 #
	VMA_41+PI*2		3459 #	9568
	VMA_420+TRAP		3460 #	3776	3778
	VMA_430+MODE		3461 #	5096
	VMA_AC3			3462 #	8914	8932	8991
	VMA_AR			3463 #	4020	4023	4628	4659	4676	4811	4839	6925	6966	7048	7062
				8833	8889	8893	8935	9001	9003	9151	9152	9214	9391	9579	9663
				9828	9889
	VMA_AR AND ADMSK	3464 #	5089	9551
	VMA_AR+1		3465 #	9612	9670
	VMA_AR+BR		3466 #
	VMA_AR+CBR		3467 #	9863
	VMA_AR+E0		3468 #	9009
	VMA_AR+E0+1		3469 #	8927
	VMA_AR+E1		3470 #	8627	8689	8974
	VMA_AR+SBR		3471 #	9823	9838	9914
	VMA_AR+XR		3475 #
	VMA_AR-1		3476 #	9600
	VMA_ARX			3477 #	4837	5055	8832
	VMA_ARX AND ADMSK	3478 #	9584	9601	9619
	VMA_ARX+1		3479 #	8099	8102	8104	8107
	VMA_ARX+BR		3480 #
	VMA_ARX+CBR		3481 #	9946
	VMA_ARX+FM[]		3482 #
	VMA_ARX+XR		3483 #
	VMA_BR			3485 #	4813	7216	7274	9886
	VMA_E0+1		3486 #	8721	8925
	VMA_FM[]		3487 #	8559
	VMA_MQ			3488 #	7279	7296	8362	9336	9440
	VMA_MQ+1		3489 #	7219	8363
	VMA_PC			3490 #	8754	8791
	VMA_PC+1		3491 #	8338	8456	8474	8518	8610	8858
	VMA_SV.VMA		3492 #	10043
	VMA_VMA HELD		3496 #	9751
	VMA_VMA+1		3497 #	4038	4073	4686	4687	4709	4880	4894	5056	5064	5079	5102
				5104	5650	5705	6253	6430	6929	7051	7203	7690	7707	7796	8479
				8481	8617	8953	9006	9286	9508	9527	9631	10130	10132	10133
	VMA_VMA-1		3498 #	4040	7801	8957	9506
	WR PT ENTRY		3500 #	9321	10007
	WR REFILL RAM		3501 #	9222
	WRITE (E)		3502 #	4851
	XR			3504 #	3775	3844	7870	7871	7907	7908	7922	7923	7978	7979	8083
				8084
	[]_#[]			2320 #
	[]_ADA[]		2321 #
	[]_ADB[]		2322 #
	[]_FM[]			2323 #	6457	6461	6467	6472	6476	6503	6507	6508	6519	6520	6550
				6660	6732	6739	6912	6913	6920	6921	6922	8227
	[]_[]*FM[]		2318 #	6452	6598	6615	6666	6668	6869	6870	6876	6877	7088	7091
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-57
; 							Cross Reference Listing					

	[]_[]*[]		2317 #
	[]_[]-FM[]		2319 #	8229
	(AR+ARX+MQ)*.25		2325 #	6401	6571	6703
	(AR+ARX+MQ)*2		2326 #	5437	5438	5439	5440	5451	5452	5453	5454	6384	6570	6572
				6683
	(MQ)*.25		2327 #	6401	6571	6703
	(MQ)*2			2328 #	5437	5438	5439	5440	5451	5452	5453	5454	6384	6570	6572
				6683
(D) MACRO%
	AC			3530 #	3947	3948	3952	3953	3957	3958	3962	3963	4087	4088	4092
				4093	4097	4098	4102	4103	4107	4108	4112	4113	4117	4118	4122
				4123	4129	4130	4134	4135	4139	4140	4144	4145	4149	4150	4154
				4155	4159	4160	4164	4165	4229	4230	4234	4235	4244	4245	4254
				4255	4259	4260	4274	4275	4284	4285	4294	4295	4304	4305	4314
				4315	4324	4325	4335	4336	4345	4346	4355	4356	4365	4366	4395
				4404	4413	4422	4431	4440	4593	4597	4767	5343	5344	5355	5356
				5368	5369	6899	7368	7369
	B			3533 #	4232	4237	4247	4262	4277	4287	4297	4307	4317	4327	4338
				4348	4358	4368	5346	5358	5371
	BLKI			3566 #	9047	9094	9105
	BLKO			3567 #	9096	9107
	CONI			3570 #	9030	9041	9052	9076	9088	9099	9110
	CONO			3571 #	9029	9040	9051	9075	9087	9098	9109
	CONSO			3572 #	9032	9043	9054	9078	9090	9101	9112
	CONSZ			3573 #	9031	9042	9053	9077	9089	9100	9111
	DATAI			3568 #	9026	9048	9095	9106
	DATAO			3569 #	9028	9050	9073	9074	9097	9108
	DBL AC			3534 #	5406	5407	5472	5473	5477	5478
	DBL B			3535 #	5409	5475	5480
	EA			3519 #	4088	4255	4618	4792	4794	4867	4868	4916	4918	4920	4921
				4922	4923	4937	4938	4939	4940	4941	4942	4946	4947	4948	4949
				4950	4951	4952	4953	4954	4955	4959	4960	4961	4962	4963	4964
				4965	4966	4967	4968	4969	4970	4971	4972	4973	4974	4975	4976
				4977	4978	4982	4983	4984	5115	5761	5775	5840	5884	6011	6012
				6899	6900	6907	6908	6909	7130	8117	8118	8119	8121	8122	8123
				8124	8126	8127	8128	8129	8131	8132	8133	8134
	FL-AC			3536 #	5757	5766	5767	5771	5780	5781	5836	5845	5846	5880	5889
				5890	6014	6060
	FL-BOTH			3538 #	5764	5769	5778	5783	5843	5848	5887	5892
	FL-MEM			3537 #	5763	5768	5777	5782	5842	5847	5886	5891
	I			3517 #	3949	4032	4033	4060	4141	4231	4232	4271	4272	4316	4317
				4367	4368	4379	4380	4381	4382	4383	4384	4397	4398	4399	4400
				4401	4402	4415	4416	4417	4418	4419	4420	4433	4434	4435	4436
				4437	4438	4487	4488	4489	4490	4491	4492	4493	4563	4564	4565
				4566	4567	4568	4569	4578	4579	4580	4581	4582	4583	4584	4594
				4595	4596	4597	4598	4599	4600	4609	4610	4749	4767	4795	4869
				4870	5116	5155	5156	5157	5158	5159	5160	5161	5369	5407	5473
				5478	5767	5781	5846	5890	6014	9027	9029	9030	9031	9032	9040
				9041	9042	9043	9049	9051	9052	9053	9054	9058	9059	9060	9061
				9062	9063	9064	9065	9075	9076	9077	9078	9085	9086	9087	9088
				9089	9090	9098	9100	9101	9109	9111	9112
	I-PF			3518 #	3948	3953	3958	3963	4093	4098	4103	4108	4113	4118	4123
				4130	4135	4140	4145	4150	4155	4160	4165	4229	4230	4235	4245
				4260	4269	4270	4275	4285	4295	4305	4314	4315	4325	4336	4346
				4356	4365	4366	4377	4378	4395	4396	4413	4414	4431	4432	4486
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-58
; 							Cross Reference Listing					

				4562	4577	4593	5344	5356
	M			3531 #	3954	3959	3964	4089	4094	4099	4104	4109	4114	4119	4124
				4131	4136	4146	4151	4156	4161	4166	4231	4236	4246	4261	4276
				4286	4296	4306	4316	4326	4337	4347	4357	4367	5345	5357	5370
				5408	5474	5479	7461	7462	9036
	R			3521 #	4014	4015	4388	4389	4390	4391	4392	4393	4406	4407	4408
				4409	4410	4411	4424	4425	4426	4427	4428	4429	4442	4443	4444
				4445	4446	4447	4497	4498	4499	4500	4501	4502	4503	4513	4514
				4515	4516	4517	4518	4519	4520	4793	5141	5368	5406	5472	5477
				5637	5638	5639	5640	5757	5766	5771	5780	5836	5845	5880	5889
				6058	6059	6060	6245	6246	6247	6248	6423	6424	6425	6426	7369
				7462	7569	8042	9028	9038	9039	9050	9073	9074	9097	9108
	R-PF			3522 #	3947	3952	3957	3962	4087	4092	4097	4102	4107	4112	4117
				4122	4129	4134	4139	4144	4149	4154	4159	4164	4234	4244	4254
				4259	4274	4284	4294	4304	4324	4335	4345	4355	4386	4387	4404
				4405	4422	4423	4440	4441	4496	5343	5355
	RPW			3524 #	3955	3960	3965	3967	4089	4094	4095	4100	4105	4110	4115
				4120	4125	4131	4136	4137	4142	4147	4152	4157	4162	4167	4236
				4237	4246	4247	4261	4262	4276	4277	4286	4287	4296	4297	4306
				4307	4326	4327	4337	4338	4347	4348	4357	4358	4528	4529	4530
				4531	4532	4533	4534	4535	4544	4545	4546	4547	4548	4549	4550
				4551	5345	5346	5357	5358
	RW			3523 #	3950	4090	4132	4257	5370	5371	5408	5409	5474	5475	5479
				5480	5763	5764	5768	5769	5777	5778	5782	5783	5842	5843	5847
				5848	5886	5887	5891	5892	7368	7461	9047	9094	9096	9105	9107
	S			3532 #	3955	3960	3965	4095	4100	4105	4110	4115	4120	4125	4137
				4142	4147	4152	4157	4162	4167
	SJC-			3557 #
	SJCA			3561 #	4517	4532	4548
	SJCE			3559 #	4488	4498	4515	4530	4546	4564	4579	4595	8118
	SJCG			3564 #	4493	4503	4520	4535	4551	4569	4584	4600	8124
	SJCGE			3562 #	4491	4501	4518	4533	4549	4567	4582	4598	4609	8122
	SJCL			3558 #	4487	4497	4514	4529	4545	4563	4578	4594	4610	8117
	SJCLE			3560 #	4489	4499	4516	4531	4547	4565	4580	4596	8119
	SJCN			3563 #	4492	4502	4519	4534	4550	4568	4583	4599	8123
	TC-			3548 #	4414	4423
	TCA			3550 #	4417	4418	4426	4427
	TCE			3549 #	4415	4416	4424	4425
	TCN			3551 #	4419	4420	4428	4429
	TN-			3540 #
	TNA			3542 #	4381	4382	4390	4391	4490	4500
	TNE			3541 #	4379	4380	4388	4389
	TNN			3543 #	4383	4384	4392	4393	4749
	TO-			3552 #	4432	4441
	TOA			3554 #	4435	4436	4444	4445
	TOE			3553 #	4433	4434	4442	4443
	TON			3555 #	4437	4438	4446	4447
	TZ-			3544 #	4396	4405
	TZA			3546 #	4399	4400	4408	4409
	TZE			3545 #	4397	4398	4406	4407
	TZN			3547 #	4401	4402	4410	4411
	W			3520 #	3954	3959	3964	4061	4099	4104	4109	4114	4119	4124	4146
				4151	4156	4161	4166	4256	9025	9026	9036	9037	9048	9071	9072
				9083	9084	9095	9099	9106	9110
(U) MAJVER			1967 #
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-59
; 							Cross Reference Listing					

(U) MARK			1961 #
(U) MBOX CTL			2184 #	9781	9783	9785
	CLR PT LINE		2189 #	9402
	CLR PT LINE(NK)		2187 #	9408
	NORMAL			2193 #	9404	9410
	PT DIR CLR		2192 #	9393
	PT DIR CLR(NK)		2188 #	9397
	PT DIR WR		2190 #
	PT WR			2191 #	9321	10007
	SET IO PF ERR		2186 #	10145
	SET PAGE FAIL		2185 #
(U) MEM				1817 #
	A RD			1825 #	3822	3823	3824	3825	3833	3834	8076	8077	8085	8087
	AD FUNC			1829 #	10036
	ARL IND			1822 #	3992	3993	5048	5072	5092	5227	5251	5314	5392	5485	5509
				5520	5579	5802	5814	5824	5825	5828	5866	5866	5870	5872	5996
				6051	6053	6124	6173	6284	6287	6310	6337	6341	6357	6359	6593
				6595	6623	6728	6759	6970	6977	7078	7082	7140	7270	7602	7693
				7700	7752	7779	8101	8299	8300	8326	8457	8504	8514	8555	8557
				8589	8591	8630	8634	8637	8675	8754	8767	8791	8831	8892	8903
				8904	8914	8934	8991	8994	9252	9299	9323	9359	9371	9384	9437
				9454	9503	9679	9757	9768	9788	9815	9816	9817	9905	9908	9909
				9924	10154
	B WRITE			1826 #	3971	3980	3982	4054	4176	4178	4181	4184	4188	4192	4201
				4203	4211	4212	4217	4218	4224	4226	4241	4251	4266	4281	4291
				4301	4311	4321	4331	4342	4352	4362	4372	4777	5350	5363	5401
				5417	5418	5489	5490	5557	5569	6223	6227	9126	9310	9385
	EA CALC			1830 #	3813	3815	3817	3819	3835	3837	4804	4809	4827	4829	4851
				7155	7160	7161	7378	7382	7387	7471	7475	7485	7859	7861	7863
				7865	7872	7874	7895	7897	7900	7902	7909	7912	7929	7931	7970
				7971	7972	7973	7980	7981	8078	8079	8932	8935	8953
	FETCH			1827 #	4467	4469	4475	4507	4524	4574	4589	4606	4613	4760	8335
				8393	9130	9132	9134
	IFET			1836 #	3795	3875	3899	3901	3902	3908	3910	3984	3990	3995	4042
				4045	4081	4572	4587	4604	4628	4659	4676	4758	4786	4813	4818
				4837	4839	4874	4880	4894	5089	5126	5167	5179	5213	5221	5234
				5242	5256	5262	5284	5313	5329	5333	5335	5398	5583	5694	5735
				6096	6097	6404	6413	6705	6762	6888	6890	6891	6928	6936	6940
				7189	7201	7223	7265	7286	7312	7390	7410	7412	7413	7414	7415
				7416	7417	7419	7426	7428	7429	7430	7431	7432	7434	7441	7443
				7444	7448	7449	7577	7606	7681	7702	7710	8283	8483	8556	8618
				8873	9143	9599
	LOAD AR			1831 #	4023	4038	4634	4642	4687	4709	5056	5711	6925	6966	7048
				7062	7191	7216	7226	7274	7289	7314	7690	7707	7796	8157	8213
				8364	8559	8627	8689	8721	8832	8833	8889	8893	8925	8927	8974
				9009	9151	9524	9525	9582	9583	9639	9640	9644	9645	9805	9806
				9824	9829	9844	9866	9891	9915	9950	10133
	LOAD ARX		1832 #	3782	4040	4811	5054	5064	5074	5650	6253	6430	6946	7051
				9527	9572	9573	9575	9579
	MB WAIT			1823 #	3788	3840	3841	3863	3869	3882	3888	3905	3922	4043	4055
				4079	4654	4689	4696	4774	4815	4820	4832	4832	4834	4834	4856
				4862	4862	5075	5078	5088	5100	5137	5138	5184	5191	5193	6926
				6954	6967	7050	7064	7192	7193	7206	7228	7230	7277	7278	7290
				7292	7310	7316	7317	7376	7380	7384	7446	7469	7473	7477	7483
				7497	7497	7522	7523	7524	7525	7526	7527	7528	7530	7536	7537
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-60
; 							Cross Reference Listing					

				7538	7539	7540	7541	7543	7549	7550	7551	7555	7556	7797	7826
				7868	7869	7905	7906	7968	7976	7977	7991	7993	8012	8012	8014
				8081	8082	8214	8242	8365	8563	8632	8691	8787	8837	8936	8945
				8958	8976	8981	9005	9008	9125	9129	9131	9140	9147	9215	9347
				9418	9482	9507	9564	9588	9597	9598	9740	9754	9810	9825	9845
				9871	9886	9893	9916	9953	10115	10125	10135
	REG FUNC		1828 #	4771	9214	9222	9279	9283	9336	9362	9379	9391	9753
	RESTORE VMA		1824 #	8221	9751	10005
	RPW			1834 #	9556	9563
	RW			1833 #
	WRITE			1835 #	3987	4025	4065	4074	4205	4539	4540	4555	4556	4684	4686
				4694	4878	4879	4888	5055	5062	5076	5086	5102	5104	7186	7204
				7219	7221	7279	7279	7284	7296	7307	7502	7553	7787	7790	7794
				7799	7800	7802	7811	7814	7822	7823	7830	7834	7838	7842	7951
				7954	8023	8938	8954	8957	9000	9001	9002	9003	9006	9148	9234
				9286	9440	9506	9508	9565	9589	9590	9617	9618	9649	9650	9752
				9877	9992	10123	10130	10132
(U) MINVER			1968 #
(U) MQ				1730 #
	MQ SEL			1734 #	5437	5438	5439	5440	5451	5452	5453	5454	5647	6384	6409
				6570	6572	6683	6967	7064	7085
	MQ*.25			1733 #	5395	5397	5435	5442	5444	5446	5448	5457	5459	5461	5462
				5467	5670	5692	6377	6566	6676	6723
	MQ*2			1732 #	5540	5543	5548	5560	5594	5595	5596	5597	5598	5599	5600
				5601	5625	5626	5627	5628	5690	5730	5732	7638	8401
	MQM SEL			1735 #	5207	5260	5513	5554	5566	6401	6571	6578	6703	7186	7221
				7280	7284	7307	7479	7678	8222
	SH			1731 #	3899	3900	4055	4832	4834	5093	5187	5196	5209	5212	5214
				5218	5220	5230	5233	5235	5241	5258	5298	5386	5389	5413	5629
				5630	5631	5632	5663	5672	5678	5717	5740	5816	5819	5821	5871
				6094	6154	6281	6290	6292	6296	6313	6315	6329	6333	6340	6388
				6389	6394	6397	6399	6530	6554	6557	6562	6610	6688	6689	6695
				6698	6700	7057	7135	7216	7269	7523	7524	7525	7526	7527	7528
				7531	7532	7533	7534	7537	7538	7539	7540	7541	7544	7545	7546
				7547	7575	7595	7677	7899	8012	8056	8150	8209	8257	8386	8438
				8577	8613	8784	8846	8848	8862	9333	9428	9479	9548
(U) MQ CTL			2061 #
	AD			2069 #	5207	5260	5513	5554	5566	6578	7186	7221	7280	7284	7307
				7479	7678	8222
	MQ*.25			2067 #	6401	6571	6703
	MQ*2			2063 #	5437	5438	5439	5440	5451	5452	5453	5454	6384	6570	6572
				6683
	SH			2066 #
	0S			2065 #	5647	6409	6967	7064	7085
	1S			2068 #
(U) MREG FNC			2174 #
	LOAD CCA		2180 #	9214
	LOAD EBR		2182 #	9391
	LOAD UBR		2181 #	9336
	MAP			2183 #	4771	9753
	READ EBR		2177 #	9379
	READ ERA		2178 #	9279
	READ UBR		2176 #	9362
	SBUS DIAG		2175 #	9283
	WR REFILL RAM		2179 #	9222
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-61
; 							Cross Reference Listing					

(U) MTR CTL			2194 #
	CLR E CNT		2197 #	9497
	CLR M CNT		2198 #	9498
	CLR PERF		2196 #	9496
	CLR TIME		2195 #	9495
	CONO MTR		2201 #	9461
	CONO TIM		2202 #	9328
	LD PA LH		2199 #	9450
	LD PA RH		2200 #
(U) NONSTD			1980 #
	OPTIONS			1984 #	9234
(U) PARITY			2253 #
(D) PARITY			2293 #
(U) PC FLAGS			2073 #
	AROV			2081 #	5264	5294	5402	5418	5684	6088	6891
	DIV CHK			2084 #	5583	5735	7606
	FDV CHK			2085 #	5904	6369	6607
	FLOV			2082 #	6748	6753	6936	7094
	FPD			2076 #	7380	7385	7388	7473	7483	7486	7778	7968	8579	9151	9152
	FXU			2083 #	6752	6940	7095
	TRAP1			2078 #
	TRAP2			2077 #	4815	4820	4833	4856	5138
(U) PMOVE			1990 #
	OPTIONS			1991 #	9234
(U) PV				1987 #
	OPTIONS			1988 #	9234
(U) PXCT			2000 #	3777	3788	5150	8064
(U) SC				1775 #
	AR SHIFT		1778 #	5167	5178	5205	5227	5251	5270	6051	7762
	FE			1776 #	5218	5239	6508	6511	7479	7498	7500	7634	7636	7996	8018
				9419	10005
	SCAD			1777 #	3731	3744	3746	3775	3808	3905	3922	4053	4065	4635	4725
				4755	4757	4838	4839	5078	5079	5137	5169	5180	5182	5184	5188
				5191	5193	5195	5196	5209	5214	5230	5235	5253	5255	5261	5272
				5276	5296	5299	5301	5302	5312	5386	5388	5399	5415	5417	5488
				5489	5490	5555	5567	5654	5683	5701	5703	5707	5736	5737	5747
				5802	5814	5816	5819	5823	5866	5870	5872	5996	6000	6002	6091
				6093	6149	6155	6252	6267	6280	6293	6310	6313	6315	6330	6337
				6343	6361	6374	6383	6389	6454	6459	6509	6513	6527	6545	6575
				6579	6600	6672	6690	6724	6726	6730	6735	6742	6744	6746	6766
				6968	6978	7079	7083	7099	7136	7373	7377	7378	7382	7389	7392
				7395	7411	7412	7413	7414	7415	7416	7417	7420	7421	7422	7423
				7424	7427	7428	7429	7430	7431	7432	7435	7436	7437	7438	7439
				7442	7466	7469	7471	7475	7501	7522	7523	7524	7525	7526	7527
				7528	7530	7531	7532	7533	7534	7536	7537	7538	7539	7540	7541
				7543	7544	7545	7546	7547	7549	7573	7575	7593	7624	7678	7692
				7731	7733	7734	7737	7740	7744	7746	7749	7750	7754	7755	7778
				7787	7790	7798	7804	7821	7824	7829	7833	7837	7841	7860	7862
				7873	7875	7991	7998	8020	8022	8029	8049	8058	8200	8210	8260
				8265	8417	8447	8473	8480	8482	8504	8507	8523	8526	8538	8540
				8554	8584	8587	8613	8620	8691	8720	8754	8756	8761	8778	8785
				8791	8800	8825	8835	8837	8838	8847	8849	8850	8857	8889	8891
				8926	8979	9011	9013	9211	9326	9346	9348	9352	9365	9376	9390
				9429	9440	9485	9505	9555	9578	9624	9638	9643	9648	9664	9671
				9680	9753	9756	9782	9784	9787	9810	9907	9934	9940	9941	9946
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-62
; 							Cross Reference Listing					

				9951	10016	10019	10025	10118	10120	10161
(U) SCAD			1752 #
	A			1753 #	3731	3744	3746	3775	3845	3869	3882	3905	3905	3922	3922
				3993	4053	4065	4725	4725	4743	4755	4757	4838	4839	5048	5072
				5078	5079	5137	5137	5193	5193	5205	5261	5301	5302	5312	5314
				5317	5386	5394	5396	5399	5414	5415	5417	5489	5490	5496	5507
				5510	5511	5517	5550	5555	5562	5567	5579	5629	5630	5631	5632
				5654	5664	5679	5683	5703	5719	5734	5736	5747	5802	5870	5871
				5901	6065	6091	6149	6155	6173	6213	6252	6263	6293	6333	6343
				6361	6366	6374	6383	6389	6409	6454	6457	6459	6461	6526	6527
				6545	6552	6557	6575	6579	6596	6600	6604	6663	6672	6690	6724
				6730	6735	6742	6744	6746	6766	6968	6975	6978	7049	7063	7079
				7081	7083	7099	7136	7378	7382	7395	7405	7406	7411	7412	7413
				7414	7415	7416	7417	7420	7421	7422	7423	7424	7427	7428	7429
				7430	7431	7432	7435	7436	7437	7438	7439	7442	7448	7449	7471
				7475	7515	7517	7522	7523	7524	7525	7526	7527	7528	7530	7531
				7532	7533	7534	7536	7537	7538	7539	7540	7541	7543	7544	7545
				7546	7547	7549	7555	7556	7592	7593	7624	7633	7636	7641	7642
				7692	7730	7732	7734	7737	7740	7744	7746	7749	7750	7752	7754
				7755	7778	7788	7791	7798	7824	7829	7833	7837	7841	7925	7952
				8029	8148	8156	8210	8258	8260	8295	8386	8438	8440	8447	8473
				8480	8482	8507	8514	8523	8526	8526	8557	8584	8584	8587	8587
				8620	8691	8703	8720	8722	8740	8764	8767	8798	8802	8825	8838
				8857	8915	8926	8971	8979	8992	8994	9011	9013	9211	9281	9326
				9346	9348	9352	9365	9376	9390	9421	9429	9440	9485	9505	9555
				9578	9624	9638	9643	9648	9680	9734	9739	9753	9766	9810	9875
				9907	9970	10016	10016	10019	10052	10118	10120	10161
	A+1			1757 #	5435	5442	5444	5446	5448	5457	5459	5461	5462	5467	5488
				5670	5701	5707	5707	5824	5825	5828	5866	6162	6212	6341	6356
				6358	6381	6408	6563	6565	6592	6594	6680	6711	7680	7811	7814
				7821	7830	7834	7838	7842	8504	8613	8847	8849	8850	8871	9934
	A+B			1755 #	4737	5169	5180	5182	5184	5188	5191	5195	5196	5207	5209
				5214	5228	5230	5235	5253	5255	5259	5271	5272	5276	5292	5296
				5299	5437	5438	5439	5440	5451	5452	5453	5454	5513	5612	5615
				5815	5816	5823	5872	5874	5911	5914	6000	6002	6053	6093	6126
				6160	6277	6280	6312	6313	6330	6337	6378	6380	6463	6472	6677
				6678	6726	6728	6945	6956	6963	6970	7380	7385	7387	7392	7480
				7498	7678	7693	7763	7799	7822	7823	7860	7862	7873	7875	7968
				7998	8017	8028	8046	8049	8058	8062	8209	8217	8253	8265	8417
				8552	8590	8592	8756	8758	8773	8774	8811	8812	8815	8837	8840
				8884	8888	9338	9359	9574	9623	9664	9675	9767	9784	9786	10154
				10158
	A-1			1756 #	4635	4740	5388	5389	5392	5540	5543	5548	5560	5594	5595
				5596	5597	5598	5599	5600	5601	5625	5626	5627	5628	5713	5737
				6129	6267	6384	6683	6959	7638	8537	8539	9401	9407	9805	9806
	A-B			1758 #	3808	5219	5239	5814	5819	5996	6081	6085	6131	6133	6153
				6157	6166	6175	6310	6315	6385	6405	6509	6513	6685	6706	6954
				7373	7377	7389	7466	7469	7473	7483	7485	7500	7501	7573	7575
				7595	7597	7600	7602	7684	7686	7700	7735	7736	7738	7779	7787
				7790	7794	7796	7804	7812	7815	7817	7819	7953	7991	8019	8020
				8022	8200	8297	8299	8554	8754	8761	8778	8785	8791	8800	8868
				8882	8897	8900	8917	8963	8977	9671	9679	9936	9941	9951	9961
				9962
	A-B-1			1754 #	7479	7751	7895	7898	7910	7913	8012	8433	8538	8540	10021
				10025
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-63
; 							Cross Reference Listing					

	AND			1760 #	5324	5326	5328	5332	5334	6755	8164	8214	8643	8705	8706
				8710	8834	8835	8870	8889	8891	8909	8920	9333	9557	9559	9570
				9588	9609	9622	9659	9668	9756	9782	9787	9811	9827	9854	9864
				9871	9888	9901	9922	9924	9940	9946	9952	9954	9995	9997
	OR			1759 #	8165	8441	8707	8708	8709	8711	8731	8903	8904	9015	9757
				9788	9893	9916	9964	9999	10004	10006
(U) SCADA			1761 #
	AR EXP			1764 #	5802	5814	5866	5870	5872	5996	6053	6081	6085	6252	6310
				6337	6968	10016	10016	10019
	AR0-5			1763 #	5314	5317	6755	7373	7377	7466	7469	7573	7592	7593	7602
				7678	7680	7692	7779	7787	7790	7798	7804	7811	7812	7814	7815
				7817	7819	7821	7830	7834	7838	7842	8200	8299	8643	8703	8705
				8706	8707	8708	8709	8710	8711	8731	8754	8764	8791	8798	8834
				8870	8871	8884	8889	8891	8909	8920	8926	8977	9015	9333	9679
				9680	9756	9782	9875	9940	9946	9954	9970	9997	10006	10154	10158
	FE			1762 #	4740	5207	5219	5239	5324	5326	5328	5332	5334	5388	5435
				5442	5444	5446	5448	5457	5459	5461	5462	5467	5513	5540	5543
				5548	5560	5579	5594	5595	5596	5597	5598	5599	5600	5601	5625
				5626	5627	5628	5670	5719	5815	5824	5825	5828	5911	5914	6093
				6126	6129	6131	6133	6153	6157	6160	6162	6166	6175	6212	6213
				6267	6312	6356	6358	6378	6380	6381	6384	6385	6405	6408	6409
				6563	6565	6592	6594	6677	6678	6680	6683	6685	6706	6711	6726
				6954	6956	6959	6963	7378	7382	7392	7395	7471	7475	7501	7595
				7597	7638	7684	7686	7700	7735	7736	7738	7751	7752	7763	7778
				7794	7796	7860	7862	7873	7875	7895	7898	7910	7913	7953	7998
				8012	8022	8164	8165	8214	8258	8297	8417	8438	8440	8441	8537
				8538	8539	8540	8554	8557	8756	8761	8767	8802	8837	8847	8849
				8850	8868	8882	8888	8897	8900	8903	8904	8917	8963	8994	9401
				9407	9574	9623	9664	9675	9757	9766	9784	9786	9788	9811	9854
				9871	9893	9901	9916	9924	9936	9952	9962	9964	9995	9999
	#			1765 #	3731	3744	3746	3808	3845	3869	3882	4053	4065	4755	4757
				4838	4839	5078	5079	5169	5180	5182	5184	5188	5191	5195	5196
				5205	5209	5214	5228	5230	5235	5253	5255	5259	5261	5272	5276
				5292	5296	5299	5301	5302	5312	5386	5394	5396	5399	5414	5415
				5417	5489	5490	5496	5507	5510	5511	5517	5550	5555	5562	5567
				5629	5630	5631	5632	5654	5664	5679	5683	5703	5734	5747	5816
				5819	5823	5871	5874	5901	6000	6002	6065	6091	6149	6155	6263
				6277	6280	6293	6313	6315	6330	6333	6343	6361	6366	6374	6383
				6389	6454	6459	6509	6513	6526	6527	6552	6557	6575	6579	6600
				6604	6672	6690	6724	6730	6735	6742	6744	6746	6766	6975	6978
				7049	7063	7079	7083	7099	7136	7389	7405	7406	7411	7413	7414
				7415	7416	7417	7420	7422	7423	7424	7427	7429	7430	7431	7432
				7435	7437	7438	7439	7442	7448	7449	7480	7498	7500	7515	7517
				7523	7524	7525	7526	7527	7528	7531	7532	7533	7534	7537	7538
				7539	7540	7541	7544	7545	7546	7547	7555	7556	7600	7624	7633
				7636	7641	7642	7730	7732	7734	7737	7740	7744	7746	7749	7750
				7754	7755	7788	7791	7824	7829	7833	7837	7841	7925	7952	7991
				8019	8020	8029	8046	8049	8058	8148	8156	8210	8260	8295	8386
				8447	8473	8480	8482	8507	8523	8526	8526	8584	8584	8587	8587
				8620	8691	8720	8722	8740	8778	8785	8800	8825	8835	8838	8840
				8857	8971	8979	9011	9013	9211	9281	9326	9346	9348	9352	9365
				9376	9390	9421	9429	9440	9485	9505	9555	9557	9559	9570	9578
				9588	9609	9622	9624	9638	9643	9648	9659	9668	9671	9734	9739
				9753	9787	9810	9827	9864	9888	9907	9922	9961	10004	10052	10118
				10120	10161
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-64
; 							Cross Reference Listing					

(U) SCADA EN			1766 #
	0S			1767 #	3775	3905	3905	3922	3922	3993	4635	4725	4725	4737	4743
				5048	5072	5137	5137	5193	5193	5271	5389	5392	5437	5438	5439
				5440	5451	5452	5453	5454	5488	5612	5615	5701	5707	5707	5713
				5736	5737	6173	6341	6457	6461	6463	6472	6545	6596	6663	6728
				6945	6970	7081	7380	7385	7387	7412	7421	7428	7436	7473	7479
				7483	7485	7522	7530	7536	7543	7549	7575	7693	7799	7822	7823
				7968	8017	8028	8062	8209	8217	8253	8265	8433	8504	8514	8552
				8590	8592	8613	8758	8773	8774	8811	8812	8815	8915	8992	9338
				9359	9767	9805	9806	9934	9941	9951	10021	10025
(U) SCADB			1769 #
	AR0-8			1772 #	4737	6463	6472	6945	8046	8049	8164	8165	8253	8433	8552
				8835	8840	9787	9811	9827	9864	9888	9901	9922	10004
	AR6-11			1771 #	7380	7385	7387	7473	7483	7485	7575	7595	7597	7602	7678
				7700	7779	7794	7796	7953	7968	8217	8265	8297	8299	8754	8758
				8761	8773	8774	8778	8791	8800	8811	8812	8815	8977	9557	9559
				9570	9588	9609	9622	9659	9668	9679	10154	10158
	SC			1770 #	3808	5169	5180	5182	5184	5188	5191	5195	5196	5207	5209
				5214	5228	5230	5235	5253	5255	5259	5271	5272	5276	5292	5296
				5299	5437	5438	5439	5440	5451	5452	5453	5454	5612	5615	5814
				5815	5816	5819	5823	5872	5874	5996	6000	6002	6053	6277	6280
				6310	6312	6313	6315	6330	6337	6385	6405	6509	6513	6685	6706
				6726	6728	6970	7389	7392	7479	7480	7498	7500	7501	7600	7684
				7686	7693	7751	7763	7799	7822	7823	7860	7862	7873	7875	7895
				7898	7910	7913	7991	7998	8012	8017	8019	8020	8022	8028	8058
				8062	8209	8417	8538	8540	8590	8592	8731	8756	8785	8837	8870
				8884	8903	8904	9015	9338	9359	9664	9671	9756	9757	9767	9784
				9786	9788	9924	9936	9940	9941	9946	9951	9954	9961	9997	10006
				10021	10025
	#			1773 #	5219	5239	5324	5326	5328	5332	5334	5513	5911	5914	6081
				6085	6093	6126	6131	6133	6153	6157	6160	6166	6175	6378	6380
				6677	6678	6755	6954	6956	6963	7373	7377	7466	7469	7573	7735
				7736	7738	7787	7790	7804	7812	7815	7817	7819	8200	8214	8441
				8554	8643	8705	8706	8707	8708	8709	8710	8711	8834	8868	8882
				8888	8889	8891	8897	8900	8909	8917	8920	8963	9333	9574	9623
				9675	9782	9854	9871	9893	9916	9952	9962	9964	9995	9999
(U) SH				1785 #	3813	3815	3817	3819	3822	3823	3824	3825	3834	3837	7378
				7382	7471	7475	7861	7865	7872	7874	7897	7902	7909	7912	7929
				7931	7971	7973	7980	7981	8077	8079	8085	8087
	AR			1787 #	3899	3900	3987	4055	4067	4737	4832	4834	5093	5169	5253
				5255	5271	5272	5276	5288	5386	5389	5413	5489	5490	5495	5629
				5630	5631	5632	5653	5672	5678	5740	5816	5819	5822	5825	5871
				6065	6274	6276	6279	6329	6333	6340	6365	6394	6397	6399	6508
				6510	6512	6526	6554	6557	6562	6604	6610	6664	6695	6698	6700
				6729	6744	6746	6930	6968	7065	7079	7086	7135	7139	7216	7328
				7373	7376	7379	7383	7392	7466	7469	7472	7476	7592	7677	7692
				7753	7755	7778	7797	7804	7997	8012	8029	8050	8056	8210	8217
				8254	8257	8257	8290	8328	8360	8386	8386	8404	8406	8457	8513
				8577	8579	8613	8621	8692	8692	8758	8763	8774	8784	8794	8797
				8812	8815	8836	8892	8914	8920	8991	9307	9346	9348	9365	9428
				9469	9479	9548	9595	9608	9610	9624	9661	9680	9812	9901	9934
				10118	10120	10176
	AR SWAP			1789 #	3971	4181	4184	4187	4188	4191	4192	4211	4212	4217	4218
				4469	4472	4474	4653	4720	4888	4892	5097	5124	5517	5800	5865
				7076	7175	7642	8218	8634	8694	8694	9123	9233	9252	9258	9299
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-65
; 							Cross Reference Listing					

				9305	9323	9371	9379	9385	9448	9454	9472	9484	9768	9773	9773
				9950	9995	10017
	ARX			1788 #	3774	3844	4046	4053	4667	4694	4814	4819	4821	4895	5098
				5103	5187	5196	5209	5209	5214	5214	5218	5218	5230	5235	5239
				5292	5296	5392	5507	5511	5689	5701	5706	5717	5911	6123	6286
				6289	6297	6313	6315	6387	6468	6473	6687	6945	6976	7057	7140
				7267	7269	7330	7523	7524	7525	7526	7527	7528	7531	7532	7533
				7534	7537	7538	7539	7540	7541	7544	7545	7546	7547	7575	7595
				7625	7686	7688	7694	7801	7922	8065	8099	8102	8104	8107	8150
				8209	8336	8390	8438	8448	8517	8538	8540	8564	8637	8732	8765
				8770	8805	8831	8832	8833	8846	8848	8857	8861	8862	8883	8893
				8918	8952	8960	9000	9001	9002	9003	9259	9325	9451	9471	9503
				9622	9723	9724	9739	9770	9789	9823	9854	9863	9941	9946	10005
				10020	10023	10025	10129	10164	10168
	SHIFT AR!ARX		1786 #	3808	3808	3904	3927	4044	4073	4756	5080	5085	5180	5183
				5197	5212	5220	5233	5241	5243	5258	5261	5275	5298	5300	5302
				5315	5317	5388	5398	5401	5519	5663	5688	5690	5691	5694	5702
				5704	5708	5719	5748	5821	5824	5827	6092	6094	6098	6154	6156
				6281	6288	6290	6292	6296	6298	6385	6388	6389	6405	6412	6514
				6519	6521	6530	6685	6688	6689	6706	6731	6743	6760	6888	6890
				7081	7084	7137	7394	7405	7406	7499	7501	7501	7502	7515	7517
				7632	7634	7756	7821	7996	8004	8018	8021	8021	8023	8266	8450
				8484	8506	8622	8703	8826	8838	8838	8855	9213	9333	9357	9383
				9508	9514	9516	9518	9520	9551	9570	9625	9652	9815	9825	9826
				9905	9909	10122	10162
(U) SKIP			1840 #
	AC REF			1874 #	7161	7162
	AC#0			1848 #	3903	3984	4525	4755	7573
	AD CRY0			1854 #	4744	4804	4809	4831	4850	5126	5713	5731	5732	5912	5999
				6001	6123	6150	6374	6393	6672	6694	8361	8412	8458	8519	8522
				8553	8611	8630	8681	8986	9010	9629	9960
	AD#0			1856 #	5262	5290	5312	5393	5399	5508	5512	5580	6262	6299	6325
				6441	6540	6693	6886	6889	6927	7077	7158	7218	7276	7605	7633
				7635	8047	8219	9654	9852	9940
	AD0			1855 #	5133	5135	5416	5488	5510	5556	5568	5613	5616	5680	5708
				5902	6265	6365	6443	6447	6453	6490	6501	6503	6546	6550	6558
				6586	6587	6604	6616	6622	6660	6664	6934	6938	7073	7098	8254
				8263	8456	8506	9326	10108
	ADX0			1853 #	8634
	AR0			1847 #	3974	4215	4221	5128	6065	6338	6436	6440	6614	6930	6961
				7658	7692	7798	7876	8278	8328	8764	8798	10016	10019	10173	10183
	AR18			1846 #	4198	4208	5167	5178	5206	5227	5252	5270	5387	8101	8715
				9334	9965
	ARX0			1845 #	7701	7752	8387	8691	8842	8860	8878
	BR0			1844 #	5734	5903	5996	6368	6606	6623	8283	8403	8578
	EVEN PAR		1843 #
	FETCH			1860 #
	INTRPT			1868 #	3814	3816	3818	3820	3836	3838	4741	4785	5051	5145	7191
				7202	7227	7289	7315	7864	7866	7901	7903	7930	7932	7972	7974
				8078	8079	8296	8345	8417	8621	8756	8890	8894	9188	9810	9894
				10052
	IO LEGAL		1871 #	4020	4635	4636	4637	4639	4774	9119	9125	9212	9219	9221
				9226	9230	9253	9256	9282	9300	9303	9320	9324	9352	9372	9375
				9427	9443	9446	9455	9458	9470	10104
	KERNEL			1861 #	4633
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-66
; 							Cross Reference Listing					

	P!S XCT			1872 #	7141	7278
	PC SEC0			1850 #	4630	4806	4835	4846	4874	4876	5008	5010	5022	5026	5028
				5030	5037	5039	5041	5043	5046	8200	8762	8763	8796	8797	8831
				8863	8886	8888	8916	8959	8993
	PI CYCLE		1865 #	9131	9134	9149	10117
	PUBLIC			1863 #
	RPW REF			1864 #	9734	9739
	RUN			1870 #
	SC0			1849 #	4654	5741	7390	7575	7576	7593	8557
	SCAD#0			1852 #	5271	5314	5317	5325	5327	5329	5333	5334	7813	7816	7818
				7820	8046	8062	8165	8215	9338	9557	9559	9588	9609	9827	9865
				9875	9889	9922	9925	9952	9954	9970	9996	9998
	SCAD0			1851 #	5170	5181	5182	5185	5188	5192	5195	5210	5215	5219	5229
				5231	5236	5240	5256	5259	5273	5277	5293	5719	5814	5817	5819
				5823	6081	6085	6093	6278	6280	6310	6314	6316	6509	6955	7393
				7595	7597	7602	7735	7736	7738	7896	7898	7910	7913	7991	7998
				8013	8049	8299	8785	8841	8868	8882	8898	8901	8917	8963	8977
				9672	9679
	USER			1862 #	4672	4673	4682	4692	5094	5146	9909	10127
	-EBUS GRANT		1866 #	9184
	-EBUS XFER		1867 #	9174
	-LOCAL AC ADDR		1857 #	3990	3995	5048	5072
	-MTR REQ		1875 #	3841	4738	5063	5147	7869	7906	8082
	-START			1869 #	4736
	-VMA SEC0		1873 #	7384	7477	7594	7687	7792
(U) SP MEM			2142 #
	CACHE INH		2149 #
	EPT			2163 #	9524	9582	9617	9805
	EPT CACHE		2164 #	8213
	EPT EN			2148 #
	EPT FETCH		2165 #	9572	9573	9579
	EXEC			2145 #
	FETCH			2143 #
	PT			2168 #	3782	5054
	PT FETCH		2169 #
	SEC 0			2146 #
	UNCSH+UNPAGE		2150 #
	UNPAGED			2162 #	4023	4025	9563	9565	9640	9645	9650	9824	9829	9844	9866
				9886	9891	9915	9950
	UNPAGED+CACHED		2151 #
	UPT			2166 #	5074	5076	9525	9806	10123
	UPT EN			2147 #
	UPT FETCH		2167 #
	USER			2144 #
(U) SPEC			1941 #
	AD LONG			1957 #	4044	4054	4054	4069	5542	5542	5545	5545	5579	5611	5614
				5659	5661	5671	5712	5715	5746	5750	5997	5997	6099	6260	6260
				6332	6374	6381	6408	6416	6416	6438	6438	6556	6611	6611	6672
				6680	6711	6725	8467	8468	8481	8522	8523	8523	8537	8580	8620
				8642	8679	8973	9439	9505
	ARL IND			1952 #	3996	3996	4043	4067	4694	4834	4845	4895	5085	5094	5487
				5651	6252	6252	6431	6681	6926	7050	7628	7628	7631	7688	7694
				7796	8028	8209	8258	8440	9233	9451	9461	9766	9767	10127	10129
	CLR FPD			1946 #	7392	7394	7410	7419	7426	7434	7441	7443	7444	7502	7550
				7551	7577	7826	8616	9135
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-67
; 							Cross Reference Listing					

	FLAG CTL		1954 #	3793	4628	4651	4653	4659	4661	4669	4673	4689	4759	4762
				5107	9144	9598	10134
	GEN CRY18		1949 #	3992	4613	5082	5092	6150	7138	7139	9134	9148	9354
	INH CRY18		1943 #	5127	7226	7239	8760	8768	8795	8799	8803	8869	8899
	LOAD PC			1947 #	4726
	MQ SHIFT		1944 #	5690	5692	5730	5732	6377	6676	6723	8401
	MTR CTL			1953 #	9444	9456	9471	9472	9473	9487	9488	9489	9490
	SAVE FLAGS		1955 #	4636	4641	4806	4874	4876	5100
	SCM ALT			1945 #	5167	5178	5205	5218	5227	5239	5251	5270	6051	6508	6511
				7479	7498	7500	7634	7636	7762	7996	8018	9419	10005
	SP MEM CYCLE		1956 #	3782	4023	4025	5054	5074	5076	8213	9524	9525	9563	9565
				9572	9573	9579	9582	9617	9640	9645	9650	9805	9806	9824	9829
				9844	9866	9886	9891	9915	9950	10123
	STACK UPDATE		1950 #	4804	4809	4831	4849	5125
	XCRY AR0		1948 #	3899	3900	5129	5253	5255	5275	5276	5285	5399	5402	5682
				5747	5821	5822	6066	6086	6092	6276	6279	6510	6512	6729	7058
				7065	7079	7753	7755	8483	8764	8798
(U) SPEC INSTR			2096 #	9839
	CONS XCT		2105 #	4743
	CONT			2106 #	4740
	HALTED			2104 #	4725
	INH PC+1		2099 #	3783
	INSTR ABORT		2103 #	9502	10122
	INTRPT INH		2102 #
	KERNEL CYCLE		2098 #
	PXCT			2101 #	5149
	SET PI CYCLE		2097 #	3730	9492	9548
	SXCT			2100 #
(U) SWITCH%
	BIG.PT			1311	9337	9339	9341	9396	9398	9405	9411	9853	9856	9859	9870
				9872	9874	9923	9926	9928	9931	9933	9935	9937
	CST.WRITE		1310	9963	9967	9969
	DDT.BUG			1312	9229	9236	9249
	EDIT			1314	8819	8872	8874	9014
	EXTEXP			5	3733	3739	3741	3796	3799	3804	4917	4925	4936	4986	4991
				5003	5031	5035	6420	6534	6537	6581	6584	6624	6627	6767	6770
				6893	6897	7103	8048	8051	8054	8057	8059	8061
	FPLONG			4	1257	5758	5760	5762	5772	5774	5776	5786	5790	5797	5837
				5839	5841	5851	5858	5862	5881	5883	5885	5930	5988	6007	6010
				6013	6017	6046	6184	6186	6188	6194	6202	6218	6220	6222	6224
				6226	6228	6236	6238	6240
	GFTCNV			10	1313	4919	4924	5005	5007	5011	5020	5024	6531	6533	6771
				6860	6901	6906	6914	6919	6981	7047
	INSTR.STAT		1292	1294	1297	1994	1996	1998	9265	9269	9277
	IPA20			9	1308	9138	9141	9560	9562	9566
	MOS.MULTI		1264	9285	9287	9296
	MULTI			6	1261	2152	2161	2170
	NOCST			7	1309	9851	9860	9862	9878	9885	9895	9939	9943	9945	9981
				9991	10008
	NONSTD			1306	1981	1983	1985
	OP.CNT			1239	1276	1278	3755	3758
	OP.TIME			1242	1280	1282	3759	3762
	OWGBP			8	1307	3734	3736	3738	3800	3803	8137	8144	8168	8180	8232
				8240	8246	8252	8359	8367	8378	8425	8434	8436	8501	8508	8512
				8822	8827	8829
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-68
; 							Cross Reference Listing					

	PAGCNT			1253	1299	1302	4727	4730	9329	9332	9725	9729	9733	9736	9738
				9743	9745
	SNORM.OPT		3	1268	6135	6146	6167
	SO.CNT			1245	1284	1286	3763	3767
	SO2.CNT			1248	1288	1290	3768	3772
	TRACKS			1235	1272	1274	3751	3754
	TRXDEF			1304	2430	2433	2559	2568	2750	2752	3436	3447	3472	3474	3493
				3495
(U) TIME			1807 #
	2T			1810 #	4479	4480	4481	4741	5124	5337	5338	5513	7626	7711	7730
				7732	7744	7750	7789	7791	7794	7824	7829	7833	7837	7841	7952
				7953	8678
	3T			1811 #	3777	3845	3864	3869	3876	3882	3888	4736	4830	4832	4835
				4862	5208	5496	7163	7168	7169	7187	7199	7205	7220	7222	7272
				7285	7308	7315	7497	7516	7518	7870	7871	7907	7908	7922	7923
				7978	7979	7992	8013	8083	8084	8364	8630	8680	9231	9402	9407
				9784	9786	9902
	4T			1812 #
	5T			1813 #	3744	3746	4678	4776	5082	5084	9169	9172	9176	9189	9194
				9195	9220	9227	9254	9257	9258	9260	9301	9304	9306	9308	9325
				9354	9364	9373	9377	9381	9431	9432	9433	9434	9444	9447	9449
				9456	9459	9471	9472	9473	9478	9487	9488	9489	9490	9558	9607
				9740	9755
(U) U0				1630 #
(U) U21				1692 #
(U) U23				1698 #
(U) U42				1768 #
(U) U45				1774 #
(U) U48				1781 #
(U) U51				1800 #
(U) U73				1959 #
(U) VMA				1801 #	10036
	AD			1806 #	3729	4020	4023	4628	4659	4676	4811	4813	4837	4839	5055
				5089	6925	6966	7048	7062	7186	7191	7216	7219	7221	7226	7274
				7279	7284	7289	7296	7307	7314	8099	8102	8104	8107	8362	8363
				8559	8627	8689	8721	8832	8833	8889	8893	8914	8925	8927	8932
				8935	8974	8991	9001	9003	9009	9151	9152	9214	9336	9391	9401
				9407	9420	9440	9551	9579	9584	9600	9601	9612	9619	9663	9670
				9751	9823	9828	9838	9863	9886	9889	9914	9946	10043
	LOAD			1804 #	3776	3778	3813	3815	3817	3819	3835	3837	4804	4809	4827
				4829	4851	5052	5061	5075	5096	7155	7160	7161	7378	7382	7471
				7475	7859	7861	7863	7865	7872	7874	7895	7897	7900	7902	7909
				7912	7929	7931	7970	7971	7972	7973	7980	7981	8077	8078	8079
				8212	8221	9480	9515	9517	9519	9521	9553	9554	9568	9800	9802
				10005	10115
	PC			1803 #	3795	4726	8754	8791	8979	9599
	PC+1			1805 #	3822	3823	3824	3825	3833	3834	3875	3899	3901	3902	3908
				3910	3984	3990	3995	4042	4045	4081	4467	4469	4475	4507	4524
				4574	4589	4606	4613	4758	4760	4786	4818	5126	5167	5179	5213
				5221	5234	5242	5252	5262	5284	5313	5398	5583	5694	5735	6096
				6097	6404	6413	6705	6762	6888	6890	6891	6928	6936	6940	7189
				7201	7223	7265	7286	7312	7390	7410	7412	7413	7414	7415	7416
				7417	7419	7426	7428	7429	7430	7431	7432	7434	7441	7443	7444
				7448	7449	7577	7606	7681	7702	7710	8283	8335	8338	8393	8456
				8474	8518	8556	8610	8858	9130	9132	9134	9143
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page CRF-69
; 							Cross Reference Listing					

	VMA			1802 #	4038	4040	4073	4686	4687	4709	4880	4894	5056	5064	5079
				5102	5104	5650	5705	6253	6430	6929	7051	7203	7387	7485	7690
				7707	7796	7801	8479	8481	8617	8953	8957	9006	9286	9506	9508
				9527	9631	10130	10132	10133
(U) VMAX			1795 #
	AD12-17			1799 #
	PC SEC			1797 #	4890
	PREV SEC		1798 #	4716	5095	9360	10128
	VMAX			1796 #
(U) #				1965 #	3731	3743	3744	3746	3774	3776	3778	3808	3822	3823	3824
				3825	3833	3834	3845	3869	3882	3905	3919	3921	3922	3927	4053
				4065	4696	4755	4757	4772	4784	4787	4838	4839	5052	5061	5075
				5078	5079	5088	5096	5108	5109	5110	5137	5150	5169	5180	5182
				5184	5188	5191	5193	5195	5196	5205	5210	5215	5219	5228	5230
				5236	5240	5253	5255	5259	5261	5272	5276	5292	5296	5299	5301
				5302	5312	5324	5327	5328	5332	5334	5386	5394	5396	5399	5414
				5415	5417	5489	5490	5496	5507	5510	5511	5513	5518	5550	5555
				5562	5567	5629	5630	5631	5632	5654	5664	5679	5683	5703	5722
				5734	5742	5747	5789	5816	5819	5823	5861	5871	5874	5901	5911
				5914	5925	6000	6002	6065	6081	6085	6091	6093	6125	6126	6131
				6133	6149	6153	6155	6157	6160	6166	6175	6223	6227	6237	6263
				6277	6280	6293	6300	6301	6313	6315	6327	6330	6333	6343	6354
				6361	6366	6374	6376	6378	6380	6383	6389	6404	6414	6454	6459
				6489	6509	6513	6526	6527	6543	6552	6557	6575	6579	6590	6597
				6600	6605	6618	6672	6675	6677	6679	6690	6705	6724	6730	6733
				6735	6742	6744	6746	6755	6764	6766	6933	6935	6941	6955	6956
				6963	6974	6975	6978	7049	7055	7063	7068	7080	7083	7099	7136
				7138	7157	7167	7217	7224	7228	7267	7275	7287	7290	7311	7316
				7328	7373	7377	7389	7405	7406	7411	7413	7414	7415	7416	7417
				7420	7422	7423	7424	7427	7429	7430	7431	7432	7435	7437	7438
				7439	7442	7448	7449	7466	7469	7480	7498	7500	7503	7515	7517
				7523	7524	7525	7526	7527	7528	7531	7532	7533	7534	7537	7538
				7539	7540	7541	7544	7545	7546	7547	7555	7556	7573	7600	7624
				7633	7636	7641	7642	7730	7732	7734	7735	7736	7737	7738	7740
				7744	7746	7749	7750	7754	7755	7756	7762	7787	7788	7790	7791
				7804	7812	7815	7817	7819	7824	7829	7833	7837	7841	7925	7952
				7991	8019	8020	8029	8046	8049	8058	8076	8077	8085	8087	8148
				8156	8200	8210	8212	8214	8253	8260	8262	8266	8272	8274	8285
				8290	8291	8292	8295	8305	8329	8331	8337	8386	8389	8390	8399
				8433	8441	8447	8450	8473	8480	8482	8484	8507	8513	8523	8526
				8552	8554	8563	8580	8584	8587	8620	8623	8636	8639	8643	8677
				8691	8705	8706	8707	8708	8709	8710	8711	8720	8722	8740	8778
				8785	8800	8825	8826	8834	8835	8838	8840	8857	8859	8868	8882
				8887	8888	8889	8891	8897	8900	8909	8917	8920	8963	8971	8978
				8979	9011	9012	9013	9136	9211	9281	9326	9333	9335	9346	9347
				9348	9352	9365	9376	9390	9418	9421	9429	9440	9480	9482	9485
				9493	9505	9515	9517	9519	9521	9553	9554	9555	9557	9559	9568
				9570	9574	9578	9588	9609	9622	9623	9624	9638	9643	9648	9659
				9668	9671	9675	9721	9722	9734	9739	9753	9782	9787	9800	9802
				9810	9827	9854	9864	9871	9888	9893	9907	9916	9922	9936	9952
				9961	9962	9964	9995	9999	10004	10037	10052	10056	10060	10064	10068
				10072	10076	10080	10084	10088	10092	10096	10100	10115	10116	10118	10120
				10135	10149	10161
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLD-1
; 							Location / Line Number Index
; Dcode Loc'n	0	1	2	3	4	5	6	7					

D 0000		4916	8117	8118	8119	8121	8122	8123	8124
D 0010		8126	8127	8128	8129	8131	8132	8133	8134
D 0020		4918	6899	6900	4920	4921	4922	4923	6907
D 0030		6908	6909	4937	4938	4939	4940	4941	4942
D 0040		4946	4947	4948	4949	4950	4951	4952	4953
D 0050		4954	4955	4014	4015	4959	4960	4961	4962
D 0060		4963	4964	4965	4966	4967	4968	4969	4970
D 0070		4971	4972	4973	4974	4975	4976	4977	4978

D 0100		4982	4983	6423	6424	5115	5116	6425	6426
D 0110		6245	6246	6247	6248	5637	5638	5639	5640
D 0120		4032	4033	6058	8042	4060	4061	6059	6060
D 0130		6011	6012	6014	7569	7368	7369	7461	7462
D 0140		5757	5761	5763	5764	5766	5767	5768	5769
D 0150		5771	5775	5777	5778	5780	5781	5782	5783
D 0160		5836	5840	5842	5843	5845	5846	5847	5848
D 0170		5880	5884	5886	5887	5889	5890	5891	5892

D 0200		3947	3948	3949	3950	3952	3953	3954	3955
D 0210		3957	3958	3959	3960	3962	3963	3964	3965
D 0220		5368	5369	5370	5371	5406	5407	5408	5409
D 0230		5472	5473	5474	5475	5477	5478	5479	5480
D 0240		5155	5156	5157	5158	5159	5160	5161	4984
D 0250		3967	7130	4609	4610	4618	4749	5141	4767
D 0260		4792	4793	4794	4795	4867	4868	4869	4870
D 0270		5343	5344	5345	5346	5355	5356	5357	5358

D 0300		4486	4487	4488	4489	4490	4491	4492	4493
D 0310		4496	4497	4498	4499	4500	4501	4502	4503
D 0320		4562	4563	4564	4565	4566	4567	4568	4569
D 0330		4513	4514	4515	4516	4517	4518	4519	4520
D 0340		4577	4578	4579	4580	4581	4582	4583	4584
D 0350		4528	4529	4530	4531	4532	4533	4534	4535
D 0360		4593	4594	4595	4596	4597	4598	4599	4600
D 0370		4544	4545	4546	4547	4548	4549	4550	4551

D 0400		4229	4230	4231	4232	4234	4235	4236	4237
D 0410		4244	4245	4246	4247	4254	4255	4256	4257
D 0420		4259	4260	4261	4262	4269	4270	4271	4272
D 0430		4274	4275	4276	4277	4284	4285	4286	4287
D 0440		4294	4295	4296	4297	4304	4305	4306	4307
D 0450		4314	4315	4316	4317	4324	4325	4326	4327
D 0460		4335	4336	4337	4338	4345	4346	4347	4348
D 0470		4355	4356	4357	4358	4365	4366	4367	4368

D 0500		4087	4088	4089	4090	4092	4093	4094	4095
D 0510		4097	4098	4099	4100	4102	4103	4104	4105
D 0520		4107	4108	4109	4110	4112	4113	4114	4115
D 0530		4117	4118	4119	4120	4122	4123	4124	4125
D 0540		4129	4130	4131	4132	4134	4135	4136	4137
D 0550		4139	4140	4141	4142	4144	4145	4146	4147
D 0560		4149	4150	4151	4152	4154	4155	4156	4157
D 0570		4159	4160	4161	4162	4164	4165	4166	4167

; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLD-2
; 							Location / Line Number Index
; Dcode Loc'n	0	1	2	3	4	5	6	7					

D 0600		4377	4378	4379	4380	4381	4382	4383	4384
D 0610		4386	4387	4388	4389	4390	4391	4392	4393
D 0620		4395	4396	4397	4398	4399	4400	4401	4402
D 0630		4404	4405	4406	4407	4408	4409	4410	4411
D 0640		4413	4414	4415	4416	4417	4418	4419	4420
D 0650		4422	4423	4424	4425	4426	4427	4428	4429
D 0660		4431	4432	4433	4434	4435	4436	4437	4438
D 0670		4440	4441	4442	4443	4444	4445	4446	4447

D 0700		9025	9026	9027	9028	9029	9030	9031	9032
D 0710		9036	9037	9038	9039	9040	9041	9042	9043
D 0720		9047	9048	9049	9050	9051	9052	9053	9054
D 0730		9058	9059	9060	9061	9062	9063	9064	9065
D 0740		9071	9072	9073	9074	9075	9076	9077	9078
D 0750		9083	9084	9085	9086	9087	9088	9089	9090
D 0760		9094	9095	9096	9097	9098	9099	9100	9101
D 0770		9105	9106	9107	9108	9109	9110	9111	9112
; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLU-1
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 0000		3793:	3795:	3971=	3864:	3870:	3876:	3883:	3889:
U 0010		3977=	3899=	3901=	3903=	3904=	3905=	3908=	3910=
U 0020		3992=	3993=	9129=	9130=	6189=	6193=	9131=	9132=
U 0030		3797 	6208=	7575=	7576=	4850=	6209=	4851=	7577=
U 0040		9140=	3798 	9143=	9144=	3808 	7411=	7412=	7413=
U 0050		7414=	7415=	7416=	7417=	7420=	7421=	7422=	7423=
U 0060		7424=	7427=	7428=	7429=	7430=	7431=	7432=	7435=
U 0070		7436=	7437=	7438=	7439=	7442=	7443=	7444=	7446=

U 0100		3980:	3982:	3990:	4020=	4065=	4068=	4038=	4069=
U 0110		4266:	4281:	4291:	4472:	4187=	4188=	4667=	4668=
U 0120		4022=	4023=	4024=	4025=	5289=	9151=	5290=	9152=
U 0130		4757=	4758=	4759=	3922 	6212=	6214=	3937:	3942:
U 0140		3730=	3927 	3732=	3740=	3743=	3744=	3746=	4041 
U 0150		8725=	6223=	3775=	3776=	8726=	6227=	3777=	3778=
U 0160		3814=	3816=	3818=	3820=	3822=	3823=	3824=	3825=
U 0170		3827=	3828=	3829=	3830=	3833=	3834=	3836=	3838=

U 0200		4201:	4203:	3974:	4205:	4587=	4588=	4589=	4181=
U 0210		4191=	4192=	4184=	4467:	4470:	4507:	3919:	3921:
U 0220		4811=	8503=	4814=	4815=	4042=	4043=	4046 	4045=
U 0230		6274=	8505=	5107=	5108=	6278=	6280=	5109=	5110=
U 0240		5054=	4053 	5055=	5057=	4054 	7522=	7523=	7524=
U 0250		7525=	7526=	7527=	7528=	7530=	7531=	7532=	7533=
U 0260		7534=	7536=	7537=	7538=	7539=	7540=	7541=	7543=
U 0270		7544=	7545=	7546=	7547=	7549=	7550=	7551=	7553=

U 0300		4211=	4212=	4572:	4574:	4478:	4479:	4480:	4481:
U 0310		4198=	7134:	4525:	3984:	3987:	4176:	4178:	3996:
U 0320		5081=	5083=	4055 	5084=	4672=	4673=	4743=	4744=
U 0330		6367=	4072 	6368=	6369=	4675=	4676=	7479:	7480:
U 0340		10052=	10056=	10060=	10064=	10068=	10072=	10076=	10080=
U 0350		10084=	10088=	10092=	10096=	10100=	10104=	10108=	4074 
U 0360		3840=	3841=	9147=	9148=	3782=	3783=	3788=	9149=
U 0370		3845=	6516=	4079 	6517=	5541=	5542=	5544=	5545=

U 0400		7374=	4208=	5865=	5867=	7377=	4241=	4251=	5870=
U 0410		4603=	4604=	4605=	4301=	7379=	7381=	7383=	7384=
U 0420		6454=	6455=	6456=	6457=	4683=	4684=	5219=	5221=
U 0430		6459=	4081 	6460=	6461=	7386=	7387=	7388=	7390=
U 0440		5049=	5051=	5549=	5551=	4693=	4694=	5552=	5553=
U 0450		5659=	6522=	5661=	6523=	5664=	4606 	4669 	5554=
U 0460		5052=	4678 	5395=	5397=	4703=	4704=	5398=	5399=
U 0470		5695=	6576=	5696=	6577=	4716=	4720=	5556=	5557=

U 0500		7467=	4311=	4217=	4218=	7470=	4321=	4474=	4475=
U 0510		5387=	4331=	5388=	5389=	7472=	7474=	7476=	7477=
U 0520		8455=	8456=	5561=	5563=	8458=	8459=	5564=	5565=
U 0530		5670=	4679 	6762=	6764=	5671=	4686 	5672=	5566=
U 0540		5437=	5438=	5439=	5440=	5442=	5444=	5446=	5448=
U 0550		6605=	4696 	6606=	6607=	4737=	4738=	5568=	5569=
U 0560		5451=	5452=	5453=	5454=	5457=	5459=	5461=	5462=
U 0570		9335=	9336=	5873=	5731=	4709 	9338=	5874=	5732=

; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLU-2
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 0600		4627:	4628:	4630:	4631:	4633:	4634:	4635:	4636:
U 0610		4637:	4638:	4639:	4640:	4641:	4642:	4643:	4644:
U 0620		4726 	8242=	4649:	8243=	4651:	4760 	8824=	4657:
U 0630		4659:	4761 	4661:	8244=	4688=	5745=	8825=	5746=
U 0640		6258=	4762 	6260=	5811=	6263=	4654:	6267=	5812=
U 0650		7156=	7157=	7158=	4772 	4740=	4741=	5749=	5751=
U 0660		5594=	5595=	5596=	5597=	5598=	5599=	5600=	5601=
U 0670		7689=	7691=	4777 	7692=	4689=	4831 	6393=	6395=

U 0700		4755:	4342=	4215:	4221:	4224:	4226:	4539=	4540=
U 0710		5414=	4352=	4555=	4556=	5416=	4362=	5417=	5418=
U 0720		7868=	7869=	7870=	7871=	5611=	5613=	5614=	5616=
U 0730		6331=	4846 	9607=	9608=	6332=	6413=	6333=	6415=
U 0740		4775=	4776=	8344=	6745=	4784=	4785=	8345=	6747=
U 0750		9678=	4880 	9679=	6748=	4786=	4787=	9680=	6750=
U 0760		5625=	5626=	5627=	5628=	5629=	5630=	5631=	5632=
U 0770		7905=	7906=	7907=	7908=	5718=	6752=	5719=	6753=

U 1000		5124:	5789:	5073:	5861:	5227:	5046:	5037:	4372=
U 1010		5039:	5041:	5043:	4613=	5350=	4891 	4804=	4806=
U 1020		4808=	4809=	5074:	7090=	4819=	4821=	4892 	7093=
U 1030		4833=	4835=	4894 	7094=	4838=	4840=	4895 	7095=
U 1040		7976=	7977=	7978=	7979=	4856=	4862=	5065 	7189=
U 1050		8081=	8082=	8083=	8084=	4878=	4879=	5075 	7191=
U 1060		8290=	8291=	5087 	8292=	4883=	4884=	5088 	7201=
U 1070		8336=	8337=	5089 	8338=	5062=	5063=	5094 	7202=

U 1100		5485=	6254=	5488=	6255=	4827=	4829=	5489=	5490=
U 1110		5022:	5026:	5028:	5030:	5010:	5362=	5008:	5097 
U 1120		8427=	8428=	8429=	8430=	5077=	5078=	5098 	7223=
U 1130		8526=	8528=	5100 	8529=	5092=	5093=	5102 	7224=
U 1140		8563=	8564=	5104 	8565=	5095=	5096=	5126 	7273=
U 1150		8771=	5130 	8773=	8774=	5128=	5129=	5197 	7275=
U 1160		8784=	8786=	5243 	8787=	5134=	5135=	5259 	7286=
U 1170		8806=	5279 	8808=	8809=	5137=	5138=	5286 	7287=

U 1200		9119=	9120=	9123=	5300 	4874=	4876=	9125=	9126=
U 1210		9226=	9227=	9230=	9232=	4888=	4890=	5301 	9233=
U 1220		8916=	8918=	5303 	8920=	5146=	5147=	5363 	7311=
U 1230		8925=	8926=	5393 	8927=	5149=	5150=	5435 	7312=
U 1240		5679=	5467 	10148=	10149=	5680=	5497 	5683=	5684=
U 1250		5168=	5170=	5580 	7405=	5181=	5182=	5654 	7406=
U 1260		5734=	5735=	5676 	5736=	5677 	5737=	6408=	6410=
U 1270		5183=	5185=	5688 	7516=	5187=	5188=	5689 	7518=

U 1300		9267=	9279=	9282=	9283=	5647=	5651=	5690 	5652=
U 1310		5145:	4771:	5800=	5803=	9219=	9220=	9221=	9222=
U 1320		8993=	8994=	5692 	8995=	5192=	5193=	5693 	7629=
U 1330		9161=	9162=	5701 	9170=	5195=	5196=	5703 	7631=
U 1340		9431=	9432=	9433=	9434=	5741=	5705 	5742=	5743=
U 1350		9515=	9517=	9519=	9521=	5208=	5210=	7679=	7681=
U 1360		5902=	5707 	5903=	5904=	5213=	5215=	5912=	5914=
U 1370		5708 	7731=	7733=	7734=	7735=	7736=	7737=	7738=

; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLU-3
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 1400		9300=	5739 	5747 	9301=	9303=	5748 	5814 	9304=
U 1410		6086:	8046:	5167:	5312:	5252:	5179:	5270:	5206:
U 1420		6124=	6125=	6127=	6128=	6130=	6132=	6134=	6150=
U 1430		5819 	7811=	7813=	7814=	7816=	7818=	7820=	7821=
U 1440		9630=	9631=	5828 	9632=	6161=	6163=	6164=	6166=
U 1450		9638=	5831 	9639=	9640=	7822=	7823=	7825=	7826=
U 1460		5229=	5231=	6340=	6341=	5871 	7685=	6343=	7687=
U 1470		9643=	6053 	9644=	9645=	7860=	7862=	7864=	7866=

U 1500		6066=	6082=	6052:	7573:	9253=	9254=	9256=	9257=
U 1510		9320=	9321=	9324=	9325=	9353=	9355=	9356=	9357=
U 1520		6353=	6354=	6091 	6357=	7052=	6359=	7053=	6362=
U 1530		9648=	6093 	9649=	9650=	5234=	5236=	7873=	7876=
U 1540		6374=	6376=	6378=	6380=	6381=	6383=	6384=	6385=
U 1550		10119=	10121=	6098 	10122=	7896=	7899=	7901=	7903=
U 1560		6436=	6437=	6438=	6439=	6440=	6442=	6444=	6445=
U 1570		5240=	5242=	5254=	5256=	5260=	5262=	7911=	7914=

U 1600		9372=	9373=	9376=	9377=	9443=	9444=	9446=	9447=
U 1610		9455=	9456=	9458=	9459=	9467=	9468=	9470=	9471=
U 1620		5263=	5264=	6099 	7923=	5271=	5273=	6158 	7924=
U 1630		5275=	5277=	6237 	7925=	5284=	5285=	6289 	7926=
U 1640		6555=	6291 	5293=	5294=	6556=	6292 	6558=	6559=
U 1650		5297=	5299=	5313=	5315=	5318=	5325=	7930=	7932=
U 1660		6566=	6294 	6296 	8507=	6570=	6571=	6572=	8513=
U 1670		5327=	5329=	5333=	5335=	7970=	7971=	7972=	7974=

U 1700		6431=	7217=	6432=	7218=	9212=	9213=	9427=	9429=
U 1710		5337=	5338=	5401=	5402=	5508=	5510=	7980=	7982=
U 1720		6589=	6590=	6297 	6593=	8211=	6595=	8212=	6596=
U 1730		5512=	5514=	5518=	5520=	8076=	8077=	8078=	8079=
U 1740		6673=	6675=	6677=	6679=	6680=	6682=	6684=	6685=
U 1750		5582=	5583=	5711=	5713=	5714=	5715=	8086=	8088=
U 1760		5721=	5723=	5815=	5817=	5821=	5823=	6711=	6724=
U 1770		5824=	5825=	5925=	5926=	5996=	5997=	6298 	9722:

U 2000		7639=	6299 	7641=	7643=	7649=	7650=	7658=	7677=
U 2010		6000=	6002=	6088=	6089=	6096=	6097=	8299=	8300=
U 2020		6153=	6155=	6174=	6176=	7701=	7702=	6310 	7707=
U 2030		6282=	6284=	6287=	6288=	6300=	6301=	8301=	8302=
U 2040		6312=	6314=	7778=	7779=	6324=	6325=	7780=	7781=
U 2050		6326=	6327=	6316 	8350=	6398=	6399=	6338 	8351=
U 2060		6404=	6405=	6447=	6448=	7787=	7789=	7790=	7792=
U 2070		8389=	8390=	8391=	6387 	6388 	8399=	8393=	8401=

U 2100		6465=	6466=	8100=	8101=	6474=	6475=	8103=	8105=
U 2110		6501=	6502=	6504=	6505=	6510=	6511=	8408=	8409=
U 2120		8182=	8183=	8184=	8185=	8186=	8187=	8188=	8189=
U 2130		6539=	6540=	6541=	6542=	8549=	8438=	8550=	8439=
U 2140		6547=	6548=	6551=	6552=	6562=	6563=	8201=	8202=
U 2150		6586=	6588=	6610=	6611=	6614=	6616=	8559=	8560=
U 2160		6618=	6619=	6622=	6623=	8260=	8261=	8262=	8263=
U 2170		6667=	6669=	6694=	6696=	8972=	8620=	8973=	8621=

; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLU-4
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 2200		8269=	8270=	6699=	6701=	8272=	8273=	8274=	6390 
U 2210		6705=	6706=	6869=	6870=	9009=	8642=	9010=	8643=
U 2220		9363=	8704=	9364=	8705=	9380=	8706=	9382=	8707=
U 2230		9528=	8708=	9529=	8709=	9578=	8710=	9579=	8711=
U 2240		8327=	8328=	8329=	6392 	6401 	8938=	8331=	8939=
U 2250		6876=	6877=	6416 	8715=	6888=	6889=	6453 	8716=
U 2260		6890=	6891=	6928=	6929=	6931=	6932=	8440=	8441=
U 2270		6935=	6936=	6462 	8728=	6939=	6940=	6464 	8729=

U 2300		6958=	6959=	6961=	6964=	7074=	7075=	8448=	8449=
U 2310		7078=	7080=	6467 	8739=	7100=	7101=	6468 	8740=
U 2320		9628=	8841=	9629=	8842=	6469 	8843=	6471 	8844=
U 2330		6472 	8845=	6473 	8847=	6476 	8849=	6477 	8850=
U 2340		7136=	7138=	8537=	8538=	7160=	7161=	8539=	8540=
U 2350		9553=	9554=	9555=	9556=	9557=	9558=	9559=	9563=
U 2360		7162=	7164=	6479 	9572=	7168=	7170=	6488 	9573=
U 2370		7187=	6489 	6490 	9574=	7192=	7193=	6507 	9575=

U 2400		7205=	7206=	7220=	7222=	6508 	9582=	6509 	9583=
U 2410		7229=	7230=	7238=	7239=	6512 	9584=	6513 	9588=
U 2420		9595=	9596=	9597=	9598=	9599=	9600=	6514 	9601=
U 2430		7265=	7266=	7277=	7278=	7279=	7280=	6518 	9602=
U 2440		9610=	9611=	7291=	7292=	6519 	9612=	6520 	9613=
U 2450		7316=	7317=	7326=	7327=	7393=	7395=	6521 	9614=
U 2460		7484=	7485=	7592=	7593=	6524 	9617=	6525 	9618=
U 2470		7594=	7595=	7598=	7600=	6526 	9619=	6528 	9620=

U 2500		7603=	7604=	7606=	7624=	7635=	7636=	8578=	8579=
U 2510		7693=	7694=	7710=	7711=	6530 	9734=	9739=	9740=
U 2520		7739=	7741=	7745=	7746=	7749=	7750=	6543 	9763=
U 2530		7754=	7755=	7795=	7796=	7799=	7800=	6544 	9764=
U 2540		8585=	8588=	6545 	8590=	6549 	8592=	9403=	9404=
U 2550		7829=	7830=	7833=	7834=	7837=	7838=	9774=	9782=
U 2560		7841=	7842=	7951=	7952=	8636=	8637=	8640=	6553 
U 2570		7992=	7993=	7996=	7998=	6574 	9801=	6578 	9803=

U 2600		8676=	8677=	8678=	6580 	8004=	8005=	8679=	8680=
U 2610		8013=	8014=	8017=	8019=	6585 	9805=	6597 	9806=
U 2620		8047=	8050=	8055=	8056=	8719=	8720=	8721=	8722=
U 2630		9812=	9813=	6598 	9815=	6599 	9816=	6601 	9817=
U 2640		8063=	8064=	8108=	8109=	8759=	8761=	8762=	8763=
U 2650		9902=	9903=	6659 	9907=	6660 	9908=	6662 	9910=
U 2660		8166=	8167=	8217=	8218=	8794=	8795=	8796=	8797=
U 2670		8858=	8859=	8860=	8861=	8863=	8946=	6663 	8947=

U 2700		8220=	8221=	8255=	8256=	6664 	8948=	6687 	8953=
U 2710		8280=	8281=	8284=	8285=	6688 	8964=	6691 	8965=
U 2720		8974=	6693 	8976=	8977=	8978=	8979=	8980=	8981=
U 2730		8296=	8297=	8347=	8348=	9000=	9001=	9002=	9003=
U 2740		8362=	8363=	8385=	8387=	9011=	9012=	6703 	9013=
U 2750		9390=	6708 	9391=	6709 	8405=	8406=	9393=	9397=
U 2760		8416=	8417=	8464=	8465=	8480=	8482=	9409=	9410=
U 2770		9487=	9488=	9489=	9490=	9491=	6710 	9962=	9965=

; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLU-5
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 3000		8522=	8523=	9200:	9202:	6726 	8148:	8152:	6727 
U 3010		8153:	8154:	8156:	6728 	8555=	8556=	8615=	8617=
U 3020		8622=	8623=	8627=	8628=	6730 	8149:	8632=	8633=
U 3030		8693=	8695=	8157:	6732 	6733 	8150:	8755=	8756=
U 3040		9659=	9660=	6735 	9661=	9663=	9664=	6739 	9665=
U 3050		9669=	6740 	8764=	8765=	9670=	9672=	6742 	9673=
U 3060		8767=	8768=	8798=	8799=	8802=	8803=	8832=	8833=
U 3070		8854=	8855=	8870=	8871=	8883=	8884=	8886=	8887=

U 3100		8890=	8891=	8898=	8901=	8903=	8904=	8933=	8934=
U 3110		5023:	6920:	6921:	6922:	6913:	6743 	6912:	6756 
U 3120		8960=	8961=	9135=	9136=	9173=	9174=	9184=	9185=
U 3130		9187=	9188=	9205=	9206=	9313=	9314=	9328=	9334=
U 3140		9346=	9347=	9419=	9421=	9438=	9439=	9480=	9481=
U 3150		9504=	9506=	9589=	9590=	9751=	9752=	9829=	9830=
U 3160		9838=	9839=	9855=	9863=	9867=	9868=	9876=	9877=
U 3170		9891=	9892=	9925=	9929=	9934=	9936=	6758 	8097:

U 3200		9942=	9946=	9955=	9961=	9966=	9970=	9980=	9992=
U 3210		9998=	9999=	10004=	10005=	10020=	10021=	10024=	10026=
U 3220		10115=	10116=	10128=	10130=	10175=	10176=	6759 	6766 
U 3230		6886 	6892 	6925 	6926 	6927 	6933 	6934 	6937 
U 3240		6938 	6941 	6942 	6943 	6944 	6946 	6955 	6966 
U 3250		6967 	6968 	6969 	6970 	6971 	6972 	6973 	6974 
U 3260		6975 	6976 	6977 	6979 	7049 	7050 	7054 	7055 
U 3270		7057 	7058 	7059 	7063 	7064 	7065 	7066 	7067 

U 3300		7068 	7070 	7077 	7081 	7082 	7083 	7084 	7085 
U 3310		7086 	7097 	7139 	7140 	7141 	7176 	7199 	7227 
U 3320		7267 	7268 	7269 	7270 	7272 	7276 	7285 	7289 
U 3330		7296 	7308 	7315 	7328 	7329 	7330 	7331 	7333 
U 3340		7448 	7449 	7486 	7498 	7500 	7501 	7503 	7555 
U 3350		7556 	7605 	7626 	7633 	7695 	7751 	7752 	7756 
U 3360		7762 	7763 	7797 	7798 	7802 	7804 	7922 	7954 
U 3370		7968 	8020 	8022 	8023 	8028 	8029 	8058 	8062 

U 3400		8065 	8164 	8165 	8190 	8191 	8192 	8194 	8200 
U 3410		8209 	8213 	8215 	8219 	8222 	8227 	8228 	8229 
U 3420		8231 	8245 	8253 	8254 	8257 	8258 	8259 	8265 
U 3430		8266 	8267 	8278 	8283 	8304 	8305 	8360 	8361 
U 3440		8364 	8365 	8403 	8412 	8431 	8433 	8437 	8442 
U 3450		8444 	8445 	8446 	8447 	8450 	8451 	8467 	8468 
U 3460		8472 	8474 	8483 	8484 	8485 	8486 	8515 	8516 
U 3470		8517 	8519 	8542 	8551 	8552 	8554 	8558 	8562 

U 3500		8576 	8580 	8611 	8618 	8630 	8634 	8681 	8689 
U 3510		8691 	8727 	8731 	8732 	8778 	8792 	8800 	8811 
U 3520		8813 	8816 	8826 	8830 	8831 	8834 	8836 	8837 
U 3530		8838 	8868 	8873 	8878 	8888 	8892 	8894 	8909 
U 3540		8935 	8936 	8945 	8954 	8957 	8959 	8966 	8986 
U 3550		9005 	9006 	9008 	9015 	9134 	9177 	9190 	9194 
U 3560		9195 	9214 	9215 	9235 	9258 	9259 	9260 	9286 
U 3570		9306 	9308 	9309 	9310 	9326 	9348 	9359 	9360 

; KL10 Microcode--Copyright (C) Digital Equipment Corp., 29 May 1986  V2A(442)		MICRO %37(277)		Page LLU-6
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 3600		9361 	9366 	9383 	9384 	9385 	9417 	9440 	9448 
U 3610		9449 	9451 	9461 	9472 	9473 	9477 	9478 	9482 
U 3620		9485 	9492 	9493 	9495 	9496 	9497 	9498 	9502 
U 3630		9507 	9508 	9524 	9525 	9548 	9549 	9551 	9564 
U 3640		9565 	9568 	9570 	9609 	9622 	9623 	9624 	9625 
U 3650		9633 	9652 	9654 	9675 	9723 	9724 	9730 	9741 
U 3660		9753 	9754 	9755 	9756 	9757 	9765 	9766 	9767 
U 3670		9769 	9771 	9784 	9786 	9787 	9788 	9789 	9810 

U 3700		9823 	9824 	9825 	9826 	9828 	9844 	9845 	9852 
U 3710		9865 	9871 	9875 	9886 	9889 	9894 	9914 	9915 
U 3720		9916 	9922 	9940 	9951 	9953 	9996 	10001 	10006 
U 3730		10007 	10015 	10017 	10036 	10037 	10044 	10117 	10123 
U 3740		10124 	10126 	10127 	10132 	10133 	10134 	10135 	10142 
U 3750		10143 	10144 	10145 	10154 	10155 	10157 	10158 	10159 
U 3760		10161 	10162 	10163 	10164 	10166 	10168 	10169 	10170 
U 3770		10171 	10173 	10177 	10178 	10180 	10181 	10183 	9721:

No errors detected
End of microcode assembly
307 pages of listing
Used 58.67 in 01:31.86
  Memory used: 130P
  Symbol table: 42P
  Text strings: 14P
  Loc'n assignment: 18P
  Cross reference: 49P
