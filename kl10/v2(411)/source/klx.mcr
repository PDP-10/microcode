; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page TOC-1
; 							Table of Contents					

; 1		KLX.MIC[10,5351]	19:52 24-Jul-85
; 1	KL10 Microcode with KL Paging
; 19		EDHIS.MIC[10,5351]	19:59 24-Jul-85
; 36	REVISION HISTORY
; 1086		DEFINE.MIC[10,5351]	19:52 24-Jul-85
; 1087	CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS
; 1248	HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS
; 1451	MICROCODE LISTING TEMPLATE
; 1502	KL10 INSTRUCTION OPCODE MAP
; 1558	CONTROL RAM DEFINITIONS -- J, AD
; 1614	CONTROL RAM DEFINITIONS -- DATA PATH MIXERS
; 1687	CONTROL RAM DEFINITIONS -- 10-BIT LOGIC
; 1720	CONTROL RAM DEFINITIONS -- SHIFT, ARMM, VMA, TIME
; 1754	CONTROL RAM DEFINITIONS -- MEM SPECIAL FUNCTIONS
; 1783	CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS
; 1875	CONTROL RAM DEFINITIONS -- DISP/SPEC SPECIAL FUNCTIONS
; 1927	CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD
; 2246	DISPATCH RAM DEFINITIONS
; 2292		MACRO.MIC[10,5351]	19:52 24-Jul-85
; 2293	CRAM Macros--Miscellaneous and A
; 2343	CRAM Macros--AR
; 2576	CRAM Macros--AR Miscellaneous, ARL, and ARR
; 2652	CRAM Macros--ARX
; 2762	CRAM Macros--B, C, D
; 2869	CRAM Macros--E, F
; 2976	CRAM Macros--G, H, I, J, L
; 3093	CRAM Macros--M, N, O, P
; 3205	CRAM Macros--R
; 3266	CRAM Macros--S
; 3523	CRAM Macros--T, U, V, W, X
; 3633	DRAM Macros
; 3717		BASIC.MIC[10,5351]	19:52 24-Jul-85
; 3718	THE INSTRUCTION LOOP
; 3814	NEXT INSTRUCTION DISPATCH
; 3949	EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH
; 4015	WAIT FOR (E)
; 4083	TERMINATION
; 4142	MOVE GROUP, EXCH, BLT
; 4185	XMOVEI, XHLLI, MOVEM, EXCH, BLT
; 4216	HALFWORD GROUP
; 4363	DMOVE, DMOVN, DMOVEM, DMOVNM
; 4404	BOOLEAN GROUP
; 4561		SKPJMP.MIC[10,5351]	19:52 24-Jul-85
; 4562	TEST GROUP
; 4671	COMPARE -- CAI, CAM
; 4697	ARITHMETIC SKIPS -- AOS, SOS, SKIP
; 4746	CONDITIONAL JUMPS -- JUMP, AOJ, SOJ, AOBJ
; 4802	AC DECODE JUMPS -- JRST, JFCL
; 4942	HALT LOOP
; 4971	MAP, XCT
; 5000	STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ
; 5098	SUBROUTINE CALL/RETURN -- JSR, JSP, JSA, JRA
; 5148	UUO'S
; 5378	JSYS, ADJSP
; 5420	XCT, PXCT, SXCT
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page TOC-2
; 							Table of Contents					

; 5494		SHIFT.MIC[10,5351]	19:52 24-Jul-85
; 5495	ROTATES AND LOGICAL SHIFTS -- ROT, LSH, JFFO
; 5550	ROTATE AND LOGICAL SHIFT COMBINED -- ROTC, LSHC
; 5584	ARITHMETIC SHIFTS -- ASH, ASHC
; 5633		ARITH.MIC[10,5351]	19:52 24-Jul-85
; 5634	ADD, SUB
; 5659	MUL, IMUL
; 5710	MULTIPLY SUBROUTINE
; 5764	DIV, IDIV
; 5823	INTEGER DIVIDE SUBROUTINE
; 5863	BASIC DIVIDE LOOP
; 5912	DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV
; 6037		FP.MIC[10,5351]	19:52 24-Jul-85
; 6038	SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR
; 6112	SINGLE FLOATING MULTIPLY -- FMP, FMPR
; 6141	SINGLE FLOATING DIVIDE -- FDV, FDVR
; 6269	UFA, DFN, FSC
; 6320	FIX, FIXR, FLTR, EXTEND
; 6390	SINGLE PRECISION FLOATING NORMALIZATION
; 6531	DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV
; 6682	DOUBLE PRECISION NORMALIZATION
; 6744		EXTEXP.MIC[10,5351]	19:52 24-Jul-85
; 6745	GFLT DOUBLE PRECISION ARITHMETIC
; 6864	GFLT MULTIPLY
; 6911	GFLT DIVIDE
; 6954	GFLT NORMALIZATION
; 7097	GFLT TO INTEGER CONVERSION
; 7223	GFLT DATA CONVERSION INSTRUCTIONS
; 7451		BLT.MIC[10,5351]	19:52 24-Jul-85
; 7452	XBLT
; 7502	BLT
; 7590	EXTENDED ADDRESSING CODE FOR PXCT OF BLT
; 7627		BYTE.MIC[10,5351]	19:52 24-Jul-85
; 7628	Single Byte Instructions:  ILDB, LDB
; 7740	Single Byte Instructions:  DPB, IDPB
; 7847	Single Byte Instructions:  IBP, ADJBP
; 8054	Subroutines for Single Byte Instructions
; 8220		BYTSUB.MIC[10,5351]	19:52 24-Jul-85
; 8221	BYTE GROUP -- Some Old Style Subroutines
; 8231	INCREMENT BYTE POINTER SUBROUTINE
; 8283	BYTE EFFECTIVE ADDRESS EVALUATOR - XADDR
; 8331	LOAD BYTE SUBROUTINE
; 8354	DEPOSIT BYTE SUBROUTINE
; 8417		EIS.MIC[10,5351]	19:52 24-Jul-85
; 8418	EXTENDED INSTRUCTION SET DECODING
; 8587	ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND
; 8642	EIS -- STRING MOVE
; 8762	EIS -- STRING COMPARE
; 8835	EIS -- DECIMAL TO BINARY CONVERSION
; 8902	EIS -- BINARY TO DECIMAL CONVERSION
; 9060	EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE
; 9266	EIS -- EDIT FUNCTION
; 9525		IO.MIC[10,5351]	19:56 24-Jul-85
; 9526	I/O INSTRUCTIONS
; 9625	EXTERNAL DEVICE I/O INSTRUCTIONS
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page TOC-3
; 							Table of Contents					

; 9723	INTERNAL DEVICE FUNCTIONS -- APR, CCA
; 9777	INTERNAL DEVICE FUNCTIONS -- PI
; 9832	TRACKS SUPPORT
; 10089	INTERNAL DEVICE FUNCTIONS -- PAG
; 10212	INTERNAL DEVICE FUNCTIONS -- TIM & MTR
; 10317	PRIORITY INTERRUPT PROCESSING
; 10486	KL-MODE PAGE REFILL LOGIC
; 10876	KI-MODE PAGE FAIL HANDLING
; 11028	PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS
;	Cross Reference Index
;	DCODE Location / Line Number Index
;	UCODE Location / Line Number Index
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; KLX.MIC[10,5351]	19:52 24-Jul-85				KLX.MIC[10,5351]	19:52 24-Jul-85		

						; 1	.TOC	"KL10 Microcode with KL Paging"
						; 2	
						; 3	.SET/SNORM.OPT=1
						; 4	.SET/XADDR=1
						; 5	.SET/EPT540=1
						; 6	.SET/LONG.PC=1
						; 7	.SET/MODEL.B=1
						; 8	.SET/KLPAGE=1
						; 9	.SET/FPLONG=0
						; 10	.SET/BLT.PXCT=1
						; 11	.SET/SMP=0		;No SMP (DOES RPW instead of RW FOR DPB, IDPB)
						; 12	.SET/EXTEXP=1
						; 13	.SET/MULTI=1		;DOES NOT CACHE PAGE TABLE DATA
						; 14	.SET/NOCST=1		;DOES NOT DO AGE UPDATES, ETC. WITH CST = 0
						; 15	.SET/OWGBP=1		;ONE WORD GLOBAL BYTE POINTERS
						; 16	.SET/IPA20=1		;IPA20-L
						; 17	.SET/GFTCNV=0		;DO NOT DO GFLOAT CONVERSION INSTRUCTIONS [273]
						; 18				;SAVES 75 WORDS. MONITOR WILL TAKE CARE OF THEM.
						; 19	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; EDHIS.MIC[10,5351]	19:59 24-Jul-85				KLX.MIC[10,5351]	19:52 24-Jul-85		

; 20	.NOBIN
; 21	
; 22	;	THE INFORMATION IN THIS DOCUMENT IS SUBJECT TO CHANGE WITHOUT
; 23	; NOTICE AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY DIGITAL
; 24	; EQUIPMENT CORPORATION.  DIGITAL EQUIPMENT CORPORATION ASSUMES NO
; 25	; RESPONSIBITY FOR ANY ERRORS THAT MAY APPEAR IN THIS DOCUMENT.
; 26	;	THE SOFTWARE DESCRIBED IN THIS DOCUMENT IS FURNISHED TO THE
; 27	; PURCHASER UNDER A LICENSE FOR USE ON A SINGLE COMPUTER SYSTEM AND
; 28	; CAN BE COPIED (WITH INCLUSION OF DIGITAL'S COPYRIGHT NOTICE) ONLY
; 29	; FOR USE IN SUCH SYSTEM, EXCEPT AS MAY OTHERWISE BE PROVIDED IN WRITING
; 30	; BY DIGITAL.
; 31	;	DIGITAL EQUIPMENT CORPORATION ASSUMES NO RESPONSIBILITY FOR THE
; 32	; USE OR RELIABILITY OF ITS SOFTWARE ON EQUIPMENT THAT IS NOT SUPPLIED
; 33	; BY DIGITAL.
; 34	; COPYRIGHT (C) 1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985 DIGITAL EQUIPMENT CORPORATION
; 35	
; 36	.TOC	"REVISION HISTORY"
; 37	
; 38	;	The following collection of people have contributed to the
; 39	;	production and maintenance of this code.  In reverse chronological
; 40	;	order:
; 41	;
; 42	;	QQSV (Dick Wagman) -- beginning with edit 301
; 43	;	Sean Keenan
; 44	;	Don Dossa
; 45	;	Mike Newman
; 46	;	Jud Leonard
; 47	;
; 48	.TITLE	"KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985"
; 49	.VERSION/MAJOR=2/MINOR=0/EDIT=411/WHO=0
; 50	;REV	WHY
; 51	;
; 52	;411	24 July 85--Another try at the SMP fix.  PI cycle 7 must go to
; 53	;	memory for interlock to work, so delete use of the cache on the
; 54	;	PHYS REF.  This may have performance drawbacks for TOPS-20 and
; 55	;	TOPS-10 uniprocessor, so there may have to be two versions of
; 56	;	microcode (again!) to resolve this.
; 57	;410	11 July 85--Force PI functions 3 and 7 to use RPW cycles, so
; 58	;	SMP will work properly.  Save a couple words in the process.
; 59	;407	18 June 85--Change macro ARX_2 to ARX_2+MQ0 and fix related bug
; 60	;	in ADJBP by clearing MQ on entry to instruction.  This prevents
; 61	;	ADJBP from computing the wrong byte capacities for OWGs with
; 62	;	byte sizes of 6 and 18.  Also reverse AC1 and AC2 in DB2WD.
; 63	;	That was causing CVTDBx to reverse the byte pointer halves if
; 64	;	an OWG was used, ruining things entirely.
; 65	;406	11 Mar 85--Define R17 as HARDPFW, and save the hard page fail word
; 66	;	there for TOPS-10, thus protecting it from getting clobbered by a
; 67	;	later soft page fail.
; 68	;400	9 Aug 84--Initial first edit number for releasable version 2.0.
; 69	;377	9 Aug 84--All of these are reserved (somewhat paranoically, I think)
; 70	; .	for version 1 as well.  The likelihood of them actually being used
; 71	;360	is vanishingly small!
; 72	;357	9 Aug 84--Add the 136 location constraint (forgotten in 356).
; 73	;356	8 Aug 84--Make the # field of location 136 contain the major and
; 74	;	minor version numbers.  Grab a random instruction with no # field
; 75	;	in use to do this.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-1
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 76	;353	21 May 84--LDB and DPB in version 1 were leaving state register bit
; 77	;	3 set when the byte word was loaded, resulting in the page fault
; 78	;	handler treating it as if it were a string instruction and trying
; 79	;	to back up a byte pointer in AC1 when the reference page faulted.
; 80	;	Cure it by reseting the state register in GBYTE.  (Sure hope this
; 81	;	is the last bug in version 1!)
; 82	;352	4 Apr 84--It turns out that the string instructions had the same
; 83	;	problem as the byte instructions in 351!  Copy AR to ARX one
; 84	;	cycle earlier in both GRSC2 and IDST to fix it.  Also make sure
; 85	;	that all byte pointers default to PC section by initializing VMA
; 86	;	to PC on all calls to both of these routines.  This cleans up edit
; 87	;	300.
; 88	;351	12 Mar 84--When ILDB or IDPB incremented a one word local pointer
; 89	;	in such a way that the low half word changed sign, the section
; 90	;	computation for the byte address would get screwed up if the
; 91	;	index AC had a global address.  Fix this by copying the updated
; 92	;	pointer into ARX, thus forcing EA MOD DISP to look at the proper
; 93	;	bit in ARX18.
; 94	;350	15 Feb 84--Fix indexed indirection byte pointer effective address
; 95	;	calculations to load the indirect word into both AR and ARX.
; 96	;347	20 Jan 84--Rewrite the MVST and CMPS dispatches to test for illegal
; 97	;	bits in the lengths before BRX gets smashed.  UUO was reporting a
; 98	;	bogus op code in these situations.
; 99	;	Turn on BIG.PT by default, since it should work with both old and
; 100	;	new software and hardware.
; 101	;346	18 Jan 84--Fix the .IFNOT variation of BIG.PT to clear the Keep
; 102	;	bit if anybody sets it.  This was introduced in 343.
; 103	;	Add the DDT.BUG conditional.  Under it, rewrite APRID to move
; 104	;	bit 23 to bit 5 if it is set in the serial number.  This is a
; 105	;	piece of garbage which I hope can disappear soon (it seems EDDT
; 106	;	used the serial number to test for a KS-10!).
; 107	;	Fix the time field on the page map word type dispatch (the assembler
; 108	;	default was too high).  Also make the PAGCNT conditional hang on
; 109	;	to the original AR value after it counts the PFH entry (this would
; 110	;	only matter for an AR parity error).  Rename AR and ARX defaults to
; 111	;	MEM for the AR_MEM and ARX_MEM macros, respectively.
; 112	;345	6 Dec 83--Clean up all the pieces and integrate the new byte
; 113	;	instruction implementation into the rest of the microcode.  Also
; 114	;	add the FIN LOAD macro (more mnemonic than FIN XFER, its equivalent)
; 115	;	and include AR/AR on AR_MEM and ARX/ARX on ARX_MEM, as those macros
; 116	;	really don't work unless those fields take their default.  This
; 117	;	version marks the first time that major chunks of code have been
; 118	;	bodily replaced; accordingly, with this edit the major version has
; 119	;	been bumped to 2.
; 120	;344	1 Dec 83--Save CVTBDx fill character address, which was getting lost
; 121	;	if OWGBPs were in use, in a manner similar to that used in CMPSx
; 122	;	(see edit 310).  Also, fix some conditionals for EXPMSK constant
; 123	;	generation, so that OWGBPs will assemble with EXTEXP off.
; 124	;343	18 Nov 83--Install new code for IBP and ADJBP, saving time and
; 125	;	microwords.
; 126	;342	8 Nov 83--Change definition of CLR PT LINE to be consistent with
; 127	;	new paging board (see also 333).  Also, redefine bit 3 of effective
; 128	;	word to reverse keep sense (so unkept only pages are cleared when
; 129	;	bit 3 is set).
; 130	;341	28 Sep 83--Force the ARX to contain the first byte pointer word on
; 131	;	exit from INCRBP so that subsequent EA MOD DISP wil work.  Force; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-2
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 132	;	OWGs to explicitly wait for store to complete after increment
; 133	;	(unfortunately there is no implicit MB WAIT in MEM/EA CALC.  Sorry!).
; 134	;340	The OWG/OWL test for the byte instructions had the sense of the
; 135	;	test backwards in several places.  Rework LDBDSP, INCRBP, and
; 136	;	DPBDSP.  This makes it impossible for DPEA to test for a byte
; 137	;	off the top of a word on its return, so it has been desubroutinized.
; 138	;337	15 Sep 83--Start work on a complete rewrite of all byte and character
; 139	;	instructions.  Begin by installing initial versions of LDB, ILDB,
; 140	;	DPB, and IDPB.  All of these are designed to make one word global
; 141	;	byte pointers run faster by loading their shift counts from CRAM
; 142	;	dispatch tables.  Also, reduce time for DISP/SH0-3 to three ticks.
; 143	;	Move CDBLST into FP to allow EXTEXP conditional to be turned off.
; 144	;	Also, shuffle conditional placement to prevent EXTEXP shutoff from
; 145	;	turning off XBLT as well.
; 146	;336	9 Aug 83--Back off 330 for a bit, since TOPS-10 7.02 must be tested
; 147	;	and OWGs in section 0 fail for string instructions (they get converted
; 148	;	to TWGs, which are illegal in section 0).  For now, we will maintain
; 149	;	both sources.
; 150	;335	Force memory to be released for SMP case of DPB if P > 36 causes no
; 151	;	actual data to be stored.  Make an OWG reference to an address >
; 152	;	37,,777777 cause a page fail (GBYTE was stripping the excess bits).
; 153	;334	Fix conflict generated in CLRPT by 333 by creating new subroutine
; 154	;	ARSWAP which is just AR_AR SWAP.  Make several other routines call it,
; 155	;	thus saving a few words.
; 156	;333	Add new conditional BIG.PT.  Under it, add code to implement the "Keep
; 157	;	me" bit for paging as bit 5 of the page table, and to move it to page
; 158	;	map bit 23 during page refill.  Also make DATAO PAG not clear Kept
; 159	;	pages if bit 3 of the word is off.
; 160	;332	Redefine all bank 7 ACs as R0,...,R17, and all bank 6 ACs as P0,...,
; 161	;	P17.  Change all other alias definitions to refer to these.  This
; 162	;	gives us a uniform cross reference for all scratch register references.
; 163	;	Put all macro definitions into alphabetical order, making it easier
; 164	;	to look up a macro definition.  Split the edit history into its own
; 165	;	file.  There are no functional changes from 331.
; 166	;331	Allow XSFM anywhere.  Clean up the code a bit in the process.  There
; 167	;	still remain a number of references to XSFM or XPCW distinctions,
; 168	;	and these could almost certainly be cleaned up further.
; 169	;330	Allow one word global byte pointers in section zero.  This includes
; 170	;	changes in BYTE, EIS, and FP.  Change GBYTE and CNV2WD to return 2;
; 171	;	eliminate GTST as obsolete.  Also shuffle the calls to these routines
; 172	;	to conform to the new calling conventions, and put the OWG test at
; 173	;	the beginning of IBP, ILDB, IDBP, LDB, DPB, and ADJBP.
; 174	;327	Add PAGCNT conditional.  Under it, include control to count entry
; 175	;	into PFH code and DATAO PAG with bit 2 set.
; 176	;326	Change VMA restoration in INC2WD and CNV2WD (see edits 320 and 307)
; 177	;	to use RSTR VMA_MQ in order to keep the global/local sense of the
; 178	;	reference.  This was causing ILDBs of OWGs in shadow memory to
; 179	;	save the incremented byte pointer in the ACs instead of memory.
; 180	;325	Add VMA/LOAD to local indexed EA computation for EXTEND E1 to make
; 181	;	it read the section number from VMA instead of PCS (!) if the index
; 182	;	is section local.
; 183	;324	Force the XADDR conditional to use RPW type references for DPB and
; 184	;	IDPB if the SMP conditional is on, even if one word globals are not
; 185	;	active.
; 186	;323	Add missing constraint near NOT.WR, accidentally broken by 322.
; 187	;322	Generate the A(cessible) bit in a page fail word caused by a read; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-3
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 188	;	violation if the page is otherwise accessible and if no CST is present.
; 189	;	This could be fixed for the CST present case as well, but has been
; 190	;	deferred since we are tight on space and no one seems to need it
; 191	;	anyway.
; 192	;321	Prevent statistics microcode from losing traps by forcing NICOND
; 193	;	dispatch 11 to ignore the statistics and take the trap.
; 194	;320	Restore the VMA again in INC2WD (broken by 307), since the state
; 195	;	register bits may have changed in the interim.  This was causing
; 196	;	PXCT to do surprising things (mostly bad).
; 197	;317	Originally, this was an attempt to uncount multiply counted op
; 198	;	codes which resulted from interrupts during long instructions.
; 199	;	That project has been shelved for now.  Instead, the second
; 200	;	NICOND dispatch during op code counting has had its final constraint
; 201	;	fixed.
; 202	;316	Make counting only version compatible with time and counting by making
; 203	;	counting only version use TRX2 and TRX3, removing physical contiguity
; 204	;	requirement.
; 205	;315	Op code counting lives again!  The setup code activated by DATAO PI
; 206	;	was attempting to write the TRX registers with data fresh from memory,
; 207	;	resulting in parity checks when it was used (see edit 73, for example).
; 208	;	Juggle code to overlap next address calculation with parity wait.
; 209	;314	Add CST.WRITE conditional to facilitate assembly of microcode
; 210	;	without the CST writable bit (see edit 303).
; 211	;313	Put TIME/3T on XFERW, as the assembler was getting the wrong
; 212	;	value with both AR_MEM and ARX_MEM macros present.
; 213	;312	Fix definition of BYTE RPW to include a write test.  This was
; 214	;	causing the SMP version of DPB to hang when memory was readable
; 215	;	but not writable.
; 216	;311	Make all IOP function 7 style of references look in the cache.
; 217	;310	Improve the fix in 307 to save the computed E0+1 in FILL during
; 218	;	OWGBP conversion and to restore the VMA from there when done.
; 219	;	Also, make sure that the VMA is initialized to PC for all cases
; 220	;	when doing effective address calculations for two word globals
; 221	;	in string instructions.  307 was not enough to clean up the
; 222	;	CMPSx fill problem, since VMA HELD was never loaded.
; 223	;	Force EXT2WD to prereference AC4 and AC5 so that glitch discovered
; 224	;	for second edit 210 will not be activated.
; 225	;307	Restore VMA from MQ at end of CNV2WD (and remove it from INC2WD,
; 226	;	saving a word in the process).  This was causing CMPSx to load
; 227	;	a random fill word and MOVSLJ to store to a random place when the
; 228	;	source length was zero if one word globals were in use.
; 229	;	Force page fail code to look for ARX as well as AR parity errors
; 230	;	(now possible with BYTE RPW implemented).
; 231	;	Make sign extension of E1 go to right place in EXTEND decoding of
; 232	;	offset instructions (broken in 301).
; 233	;306	Add University of Essex code to statistics (TRACKS) code to make
; 234	;	it work with address break enabled.
; 235	;305	Fix CST write bit logic to not test bit 18 when reading.
; 236	;304	Switch byte read interlock from LDB to DPB (broken in 303).
; 237	;303	Implement bit 18 of a CST entry as a write enable bit in addition
; 238	;	to all the other write enable functions.
; 239	;	Knock one cycle out of byte deposit where the byte is being
; 240	;	deposited into the high order byte of a word.
; 241	;	Implement the SMP conditional for extended addressing by
; 242	;	replicating all the byte effective address calculation code for
; 243	;	DPB.  This is unfortunate, but necessary due to the huge dispatch; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-4
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 244	;	table that ends this subroutine.
; 245	;302	Move XFERW out of EIS (which no longer absolutely requires it
; 246	;	in line) into SKPJMP (more in the heart of things).  Also
; 247	;	juggle comment lines and code layout to reduce the listing
; 248	;	size a bit and to force some of the .TOC lines into the table
; 249	;	of contents (even though the code nearby may be suppressed).
; 250	;301	Fix ADJBP so that instructions which occur at the last word on
; 251	;	a page do not cause a page failure of some random type (one cycle
; 252	;	too many between I FETCH and NICOND).
; 253	;	Fix effective address calculation for EXTEND so that only offset
; 254	;	instructions (and not GSNGL, for example) will have E1 sign
; 255	;	smeared.
; 256	;	Implement XJRST.  Also force JSP and JSR to do full 30 bit
; 257	;	effective address calculations.
; 258	;300	ADD LOAD OF VMA FROM PC IN PUTDST TO GET THE SECTION ADDRESS
; 259	;	CORRECT ON THE STRING INSTRUCTIONS.
; 260	;277	Add EA CALC table for SMP configurations of extended addressing
; 261	;	for TOPS-10.  (TOPS-20 paging)
; 262	;276	Force global EA CALC for EXTEND instructions in PUTDST.
; 263	;275	FIX THE ERROR CODE IN STRING COMPARE FOR ILLEGAL BITS IN THE
; 264	;	LENGTH FIELD. WAS CAUSING AR PARITY ERRORS.
; 265	;274	SAVE THE API FUNCTION WORD ON AN IO PAGE FAIL INSTEAD OF THE
; 266	;	PAGE FAIL WORD. THIS TAKES PLACE IN BOTH THE AC BLK 7 AC 2
; 267	;	AND THE MONITOR.
; 268	;273	PUT CONDITIONALS AROUND 4 GFLOAT CONVERSION INSTRUCTIONS.
; 269	;	THEY WILL ACT AS MUUO'S AND MONITOR WILL TAKE CARE OF THEM.
; 270	;272	CONO APR 200000 AT TIMES WAS NOT GENERATING EBUS RESET OF A
; 271	;	SUFFICIENT LENGTH TO CLEAR DTE REGISTERS. ADDED ANOTHER
; 272	;	MICROWORD SO THAT CONO APR IS NOW UP FOR TWO FULL WORDS WHICH
; 273	;	GETS AROUND THE HARDWARE PROBLEM.
; 274	;271	ILLEGAL INDIRECT PAGE FAIL (24) WAS NOT ALLOWING USER TO BE SET.
; 275	;270	WHEN IN SECTIONS > 1, AN UPDATED OWGBP WOULD BE WRITTEN INTO
; 276	;	MEMORY INSTEAD OF THE AC'S.
; 277	;267	CHANGED TESTS FOR OWGBP TO TEST FOR PC SEC0 FIRST. SAVES 33 NS.
; 278	;266	CONDITIONALS ON FOR TOPS-20 DEVELOPMENT.
; 279	;265	REMOVED EDIT 244. SOFTWARE ENGINEERING WILL SUPPLY MONITOR
; 280	;	CODE TO TAKE CARE OF PROBLEM. CODE COSTS TOO MUCH TIME IN
; 281	;	THE INSTRUCTION EXECUTION.
; 282	;264	ADDED CONDITIONALS TO CODE FOR IPA20, OWGBP AND NO CST UPDATE IF
; 283	;	CBR IS ZERO. THIS IS FOR RELEASE 5 OF TOPS-20.
; 284	;263	IBP DID NOT CLEAR FPD ON EXIT.
; 285	;262	ALLOW XBLT TO BE VALID IN SECTION 0.
; 286	;261	FIX CODE AT END OF ADJBP CODE TO CLEAR STATE REG. IF ILDB
; 287	;	WITH 2 WD GLOBAL POINTER POINTING TO ADDRESS NOT IN CORE
; 288	;	CLEAN DISPATCHES TO WRONG CODE BECAUSE SR LEFT OVER FROM
; 289	;	ADJBP.
; 290	;260	FIX FM PARITY ERRORS AT MVF1: ADDED NULL CALL TO RET2:
; 291	;	AT MVST: TO TAKE CARE OF EXTRA TICK FOR PARITY.
; 292	;257	MAKE SURE THAT THE UPDATED ONE WORD GLOBAL BYTE POINTER IS
; 293	;	WRITTEN BACK INTO THE CORRECT CONTEXT.
; 294	;256	MAKE ANOTHER ATTEMPT TO FIX PXCT OF ONE WORD GLOBAL BYTE POINTERS.
; 295	;	THE GIBP CODE GETS THE SAME CHANGES AS EDIT 255.
; 296	;255	MAKE ONE WORD GLOBAL BYTE POINTERS WORK WITH PXCT. THE STATE
; 297	;	REGISTER BITS ON MCL4 (NOT TO BE CONFUSED WITH CON3), WERE NOT
; 298	;	BEING SET PROPERLY TO ALLOW PREVIOUS ENA AND USER ENA TO BE SET.
; 299	;	GUARANTEE THAT THESE SR BITS ARE SET PRIOR TO THE LOAD OF THE VMA.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-5
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 300	;254	FIX PROBLEM WITH OWGBP WHERE FPD DOES NOT EFFECT
; 301	;	INC OF POINTER AFTER PAGE FAIL
; 302	;253	FIXED ADDRESSING FOR SH DISP AT GADJL0:
; 303	;252	MOVE STRING INSTRUCTIONS DO NOT GET THE CORRECT DATA ON
; 304	;	LOCAL POINTERS IN NON 0 SECTIONS
; 305	;251	ADD CODE FOR ONE WORD GLOBAL BYTE POINTERS.
; 306	;	TOOK OUT EDITS 243 AND 250 TO GET ENOUGH SPACE IN CRAM
; 307	;	FOR THIS EDIT. OWGBP WITH EXTEND INSTRUCTIONS WILL NOT
; 308	;	RETURN A OWGBP. THEY WILL RETURN A TWO WORD GLOBAL BP.
; 309	;250	ALLOW SMP SWITCH TO EFFECT TOPS-20 MODEL B TO DO RPW IN
; 310	;	BYTE INSTRUCTIONS.
; 311	;247	DO NOT DO A CST UPDATE OR AGE UPDATE IF THE CBR IS ZERO.
; 312	;246	EXTEND OP CODE DECODE FOR MODEL A WAS ACCEPTING MODEL B
; 313	;	OP CODES (20-31). ADDED CONDITIONALS TO CODE TO FIX.
; 314	;245	FIX 2 WORD GLOBAL BYTE POINTER BUG WITH IBP INSTRUCTION
; 315	;	WITH EXTENDED ADDRESSING OUT OF SECTION 0
; 316	;244	FIX MOVST EXTEND INST. SO THAT ILLEGAL (> 36) S FIELD
; 317	;	DOES NOT CAUSE STOP CODE TO CRASH SYSTEM FOR TOPS-10 MODEL B.
; 318	;243	WRTIME TRIED TO DO MEM WRITE EVEN THOUGH THE INSTRUCTION
; 319	;	DOES NOT DO ANYTHING TO MEMORY. CAUSED PROBLEMS IF THE MEMORY
; 320	;	LOCATION WAS NOT WRITABLE.
; 321	;242	FIX CODE FROM EDIT 234 TO GET PF CODE OF 24.
; 322	;241	FIX DFAD AND DFMP FOR ROUNDING OCCURS PROPERLY. ADDED STICKY
; 323	;	BIT FOR LEAST SIGNIFICANT BITS OF THE RESULT.
; 324	;240	FIX GFLT INSTRUCTIONS GFIX AND DGFIX SO THEY WILL TRUNCATE NEGATIVE
; 325	;	NUMBERS IN THE CORRECT DIRECTION. THE MQ MUST BE ZERO BEFORE
; 326	;	THE ARX_2 MACRO IS INVOKED OR THE ARX MIGHT GET A 3 FROM MQ00.
; 327	;237	ADD OPTION BIT FOR PV CPU IN THE APRID WORD AS IT IS DOCUMENTED
; 328	;	IN ALL OF THE HARDWARE DOCUMENTATION. SET THE BIT ACCORDING
; 329	;	TO THE MODEL.B OPTION SWITCH. IT WILL BE MAGIC NUMBER BIT 3.
; 330	;236	ALLOW THE INTEGER DIVIDE OF THE LARGEST NEGATIVE NUMBER BY
; 331	;	PLUS ONE TO SUCCEED. THIS USED TO BE A DOCUMENTED RESTRICTION
; 332	;	THAT THIS OPERATION WOULD CAUSE AN OVERFLOW AND NO DIVIDE.
; 333	;235	FIX JRA SO IT DOESN'T FALL INTO SECTION ZERO FROM A NON-ZERO
; 334	;	SECTION EVERY TIME BY WRITING THE PC SECTION INTO THE VMAX.
; 335	;234	BUILD A PAGE FAIL CODE OF 24 WHEN AN ILLEGAL INDIRECT WORD
; 336	;	IS FOUND DURING THE EFFECTIVE ADDRESS CALCULATION IN
; 337	;	A NON-ZERO SECTION. THE PAGE FAIL CODE WAS PREVIOUSLY NOT
; 338	;	BEING REPORTED.
; 339	;233	SAVE THE IOP FUNCTION WORD THAT APPEARS ON THE EBUS WHEN AN
; 340	;	EXTERNAL DEVICE INTERRUPTS THE CPU. SAVE THIS INFORMATION
; 341	;	ON EVERY INTERRUPT IN AC BLOCK 7, AC 3. THE CONTENTS
; 342	;	OF THIS AC WILL BE PRESERVED UNTIL THE NEXT INTERRUPT.
; 343	;	OPERATING SYSTEMS SHOULD SAVE THIS INFORMATION AS SOON AS POSSIBLE
; 344	;	IF ITS CONTENTS ARE TO BE RELIABLE AND MEANINGFUL.
; 345	;232	ADDS 13 NEW INSRUCTIONS FOR SUPPORTING FORTRAN78 ON MODEL
; 346	;	B MACHINES. THESE INSTRUCTIONS ARE:
; 347	;	       OPCODE     SYMBOL
; 348	;	       ======     ======
; 349	;		102	GFAD AC,E
; 350	;		103	GFSB AC,E
; 351	;		106	GFMP AC,E
; 352	;		107	GFDV AC,E
; 353	;		EXTEND INSTRUCTIONS    EXTEND OPCODE
; 354	;		====== ============    ====== ======
; 355	;		EXTEND AC,[GSNGL  0,E]	    21; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-6
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 356	;		EXTEND AC,[GDBLE  0,E]	    22
; 357	;		EXTEND AC,[DGFIX  0,E]	    23
; 358	;		EXTEND AC,[GFIX   0,E]	    24
; 359	;		EXTEND AC,[DGFIXR 0,E]	    25
; 360	;		EXTEND AC,[GFIXR  0,E]	    26
; 361	;		EXTEND AC,[DGFLTR 0,E]	    27
; 362	;		EXTEND AC,[GFLTR  0,E]	    30
; 363	;		EXTEND AC,[GFSC   0,E]	    31
; 364	;231	FIX IN PROBLEM IN EDIT 215 TO XDPB THAT PREVENTED THE KL
; 365	;	FROM HANDLING INTERRUPTS WHILE EVALUTAING AN INDEXED INDIRECT CHAIN.
; 366	;	AN "=0" WAS MISSING BY BYTEIP.
; 367	;230	TO PRESERVE COMPATABILITY WITH THE KS10 AND BECAUSE OF SPACE
; 368	;	LIMITATIONS IN TOPS20 MODEL A, THE SPECIFICATION FOR THE
; 369	;	CVTDBX INSTRUCTIONS HAVE BEEN CHANGED TO ELIMINATE THE NEED
; 370	;	FOR AN OVERFLOW TEST DURING THE CONVERSION. THIS CHANGE
; 371	;	EFFECTIVELY REMOVES EDIT 221.
; 372	;227	DELETE EDIT 222 AND RETURN THE CVTBDX INSTRUCTIONS TO THEIR
; 373	;	OLD, BROKEN FUNCTIONALITY SINCE ANY ATTEMPT TO PREVENT THE
; 374	;	FLAGS FROM BEING CHANGED PREMATURELY HAS TO CONTEND WITH
; 375	;	INTERRUPTABILITY PROBLEMS. THE HARDWARE REFERENCE MANUAL
; 376	;	HAS A FOOTNOTE ABOUT THE FLAG PROBLEM SO THE CURRENT FUNCTIONALITY
; 377	;	IS DOCUMENTED FOR USERS.
; 378	;226	PREVENT AR PARITY ERRORS WHEN INCREMENTING BYTE POINTERS IN THE ACS.
; 379	;225	THE CODE TO SUPPORT THE MX20 VIA THE SBUS DIAG LOOP MECHANISM
; 380	;	DOES NOT TIME OUT CORRECTLY BECAUSE THE LOOP COUNTER IS BEING
; 381	;	REINITIALIZED EVERY TIME THROUGH THE LOOP. FIX THIS PROBLEM
; 382	;	EVEN THOUGH THE CODE IS NOT ASSEMBLED IN CURRENT RELEASES.
; 383	;224	FIX BUG IN EDIT 223 THAT CAUSED THE WRONG PAGE FAIL
; 384	;	WORD TO BE WRITTEN WHEN AN I/O PAGE FAIL OCCURS.
; 385	;223	WHEN A MEMORY PARITY ERROR OCCURRS AT PI LEVEL, AS EVIDENCED
; 386	;	BY AN AR DATA PARITY ERROR, THE DTE MAY BE WAITING FOR A
; 387	;	RESPONSE. IF IT IS, A DEX FAILURE WILL OCCUR UNLESS WE CAUSE
; 388	;	DEMAND TO WIGGLE.  WE CAN DO THIS BY FORCING THE DATA  IN THE
; 389	;	AR OVER THE EBUS.
; 390	;222	CVTBDX IS NOT SUPPOSED TO CHANGE THE CONTENTS OF THE ACS
; 391	;	OR MEMORY IF THE CONVERTED NUMBER WILL NOT FIT INTO THE
; 392	;	DESTINATION FIELD. IT WAS, HOWEVER, CHANGING THE FLAGS
; 393	;	BEFORE IT KNEW IF THE NUMBER WOULD FIT.
; 394	;221	THE CVTDBX WERE FAILING TO SET OV AND TRAP1 WHEN THE
; 395	;	CONVERTED DECIMAL NUMBER WOULD NOT FIT INTO A
; 396	;	DOUBLE WORD.
; 397	;220	THE TRANSLATE INSTRUCTIONS WERE USING A 15 BIT WIDE
; 398	;	FIELD FOR THE REPLACEMENT BYTE IN THE TRANSLATE TABLE
; 399	;	WHILE THE SPECIFICATION STATED THAT THE TRANSLATE
; 400	;	INSTRUCTIONS WOULD USE ONLY 12 BITS.
; 401	;217	PREVENT CRAM PARITY ERRORS CAUSED BY DISPATCHING TO LOCATION
; 402	;	3042 WHEN INDEXING IS SPECIFIED IN THE EFFECTIVE ADDDRESS
; 403	;	CALCULATION OF E1 WHEN THE EXTEDED OPCODE IS ZERO (ILLEGAL).
; 404	;	THE FIX IS TO PUT A JUMP TO UUO AT 3042.
; 405	;216	CHANGE THE DEFAULT VALUE FOR THE SMP SWITCH TO BE ONE. THIS
; 406	;	CAUSES THE MICROCODE TO INCLUDE SMP SUPPORT BY DEFAULT.
; 407	;215	CHANGES DPB INSTRUCTION TO R-P-W CYCLE ON DATA FETCH PORTION OF
; 408	;	INSTRUCTION TO SOLVE AN INTERACTION PROBLEM IN AN SMP OPERATING
; 409	;	SYSTEM.  THIS CHANGE ONLY APPLIES TO MICROCODES FOR TOPS-10
; 410	;	AND TOPS-20, MODEL A.
; 411	;214	ADDED CHANGES FOR XADR, RELEASE 4 AS FOLLOWS.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-7
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 412	;	STORE PREVIOUS CONTEXT SECTION (PCS) IN FLAGS WORD (BITS 31-35)
; 413	;	IF EXEC MODE AND XSFM OR XPCW INSTRUCTION,MUUO OR PAGE FAIL.
; 414	;	RESTORE PCS FROM FLAGS WORDS (BITS 31-35) WHEN XJRSTF OR XJEN
; 415	;	IS EXECUTED IN EXEC MODE AND THE NEW PC IS ALSO IN EXEC MODE.
; 416	;213	SET/FPLONG=0 PARAMETER ADDED TO TOPS-10 MICROCODE FOR KL MODEL
; 417	;	A AND MODEL B.
; 418	;212	CHANGE THE CODE AT LDIND: TO TEST FOR USER MODE IF USER MODE
; 419	;	TURN OFF SPECIAL CYCLE THAT MAY STILL BE ON. THE MICROCODE WILL DEPEND
; 420	;	ON KERNAL PROGRAMS TO NOT GET IN PAGE POINTER
; 421	;	LOOPS. INSTRUCTIONS EXECUTED FROM THE CONSOLE WILL NOT WORK.
; 422	;	PI INSTRUCTIONS GET A RESTRICTION TO NOT GET INDIRECT PAGE POINTERS
; 423	;	IN THEIR PAGING CHAIN AS DO EXAMINES AND DEPOSITS AND BYTE TRANSFERS.
; 424	;211	CHANGE THE TEST FOR INDIRECT POINTERS TO NOT HAPPEN ON SECTION
; 425	;	POINTERS AND JUST ON INDIRECT PAGE POINTERS. AT LDIND:+1 AND LDIMM:+2
; 426	;210	MAKE ALL AC+# MICROINSTRUCTIONS HAVE THE # FIELD THE SAME IN THE
; 427	;	PREVIOUS MICROINSTRUCTION TO SOLVE A TIMONG GLITCH IN THE HARDWARE.
; 428	;	MAKE EXCHANG MARK AND DESTINATION POINTERS UUO IF THEY DO NOT
; 429	;	HAVE BYTE POINTERS OF EQUAL LENGTH. CHANGES PERVASIVE IN EIS ALSO IN PF
; 430	;	RECOVERY IN IO.
; 431	;	MAKE THE LOAD OF AN INDIRECT POINTER CLEAR PI CYCLE IF SET.
; 432	;	THIS MEANS THAT THE MONITOR CANNOT USE KERNAL CYCLE, INSTR ABORT
; 433	;	INH PC+1 OR HALT IN A PI CYCLE IF AN INDIRECT POINTER CAN
; 434	;	BE A PART OF THE REFILL. ALSO NOTE THE POSSIBILITY OF GETTING AN
; 435	;	INTERUPT BEFOR THE PI INSTRUCTION COMPLETES. (NEVER CONTINUES PI
; 436	;	INSTRUCTION) CHANGES AT LDIND.
; 437	;207	CHANGE SBUS DIAG CODE FOR MOS PUT IT IN MOS CONDITIONAL /MOS=1
; 438	;	IF ON SBUS DIAG TRIES AT LEAST 8 TIMES TO GET A RESPONSE
; 439	;	OTHER THAN -1 IF IT GOT -1 ALL THOSE TIMES THE MICROCODE
; 440	;	GIVES UP AND RETURNS 0
; 441	;206	FINAL FIXES TO PUSHM AND POPM
; 442	;205	FIX BUG IN INDEX CALCULATION OF E1 FOR EXTENDED ADDRESSING.
; 443	;	INDEXING REQUIRED THAT AN AREAD BE PERFORMED IN ORDER TO LOAD
; 444	;	THE AR WITH A CORRECT FINAL RESULT. THE EFFECTIVE ADDRESS CALCULATION
; 445	;	AROUND EXTLA: GOT A NEW MACRO ADDED FOR INDEXING THAT DOES THE AREAD.
; 446	;	ABSOLUTE LOCATIONS IN THE RANGE 3040 GET USED AS TARGETS FOR THIS
; 447	;	AREAD THEN THE CODE REJOINS THE OLD CODE AT EXT2:
; 448	;	THE AREAD WAS NECESSARY FOR THE HARDWARE MAGIC TO LOAD PARTS OF THE
; 449	;	AR DEPENDING ON THE INDEX REGISTER AND OTHER EXTENDED ADRESSING
; 450	;	PARAMETERS.
; 451	;204	ADD AUTOMATIC VERSION NUMBER
; 452	;	ADD CODE TO DO SBUS DIAG TESTING REQUIRED BY MOS
; 453	;203	PUT THE BLKO PAG, CHANGE IN 201 IN A KLPAGING CONDITIONAL
; 454	;	KIPAGING GETS TANGLED IN AR PARITY ERRORS AND IN GENERAL DOES
; 455	;	THE WRONG THINGS
; 456	;202	TURN OFF IMULI OPTIMIZATION IT GETS THE SIGN BIT AND THE OVERFLOW
; 457	;	FOULED UP (TURNED OFF FOR MODEL B ONLY WAS OFF IN MODEL A)
; 458	;201	CHANGE BLKO PAG, TO INVALIDATE ONLY ONE ENTRY BY CLEARING IT
; 459	;	CHANGES AT PAGBO PAGBO+1 AND CLRPT+3 CLRPT+3 GETS SETUP THAT USED
; 460	;	TO BE AT PAGBO+1, PAGBO+1 NOW CLEARS ENTRY AND QUITS
; 461	;	KLPAGE ERROR CHECK FOR TOPS 10 MODEL A TO CAUSE ERROR
; 462	;	IF SWITCH SETTINGS ARE IN CONFLICT DIDDLED
; 463	;200	CHANGE ALL EXEC REF TRACKS FEATURES BACK TO PHYS REF
; 464	;	ON SUSPICION THAT PAGE FAULTS ARE NOT HANDLED PROPERLY
; 465	;	MAKE NON TRACKS INSTR STAT FEATURES GET FOUR PHYSICAL
; 466	;	PAGE NUMBERS FROM FIRST FOUR LOCATIONS IN THE PAGE PRESENTED
; 467	;	IN THE DATAO PI, THE CODE ALSO USES THAT PAGE FIRST; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-8
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 468	;	LOCATION TO PUT THE INITIAL JUNK INTO ON STARTUP
; 469	;177	FIX SOME BUGS IN OPCODE TIMING CODE AT OPTM0: AND BEYOND
; 470	;176	ADD TO THE TIME COUNTING CODE CODE THAT COUNTS FREQUENCY
; 471	;	OF EACH OPCODE IN PAGE+2 AND PAGE+3
; 472	;175	FIX TIME COUNTING CODE TO ACOUNT FOR EACH OPCODE IN THE
; 473	;	USER OR EXEC MODE IT WAS SEEN IN, EDGE COUNTS WERE DONE IN
; 474	;	WRONG MODE CHANGES UNDER OP.TIME CONDITONALS (PERVASIVE)
; 475	;174	CHANGE TRACKS AND TIME COUNTING TO USE EXEC VIRTUAL SPACE
; 476	;	INSTEAD OF PHYSICAL SPACE
; 477	;173	SEPERATE OUT THE DISMISS AT 626: BECAUSE OF SUSPECTED BUG
; 478	;172	THE FACT THAT XJEN DISMISSES BEFORE READING NEW PC WORDS CAUSES
; 479	;	A PROBLEM FOR TOPS 20. REHASH THE CODE AT 600: TO 637: TO MAKE
; 480	;	XJEN READ THE TWO WORDS FIRST AND THEN DISMISS.
; 481	;171	CAUSE IO PAGE FAIL FIX IN 170 TO SHIFT AT END GETTING CORRECT
; 482	;	PAGE FAIL WORD CHANGE AT IOPGF:
; 483	;170	MAKE CLRFPD: GO DIRECT TO FINI: INSTEAD OF THROUGH NOP: THIS WAS
; 484	;	COSTING 2 TICS IN BYTE INSTRUCTIONS
; 485	;	CHANGE IO PAGE FAIL TO SAVE A VIRTUAL ADDRESS IN THE AC BLOCK 7
; 486	;	LOCATION 2 INSTEAD OF THE DATA THAT WAS ON THE EBUS CHANGES AT
; 487	;	PGF4:+1 AND IOPGF:
; 488	;167	CHANGE DEFAULT ON ADB MIXER SELECTS. NO DEFAULT NOW SUBFIELD U23
; 489	;	IS DEFAULTED TO 1 TO AVOID SELECTING FM AND NEEDING TO WAIT FOR PARITY.
; 490	;	THIS LEAVES THE OTHER BIT OF THE FIELD AVAILABLE FOR PARITY
; 491	;	EPT MOVED TO 540 USING SWITCH IN KLX,KLL (KLA,KLB NOW DEFUNCT)
; 492	;166	CHANGE FIELD DEFINITION FORMAT CHANGE THE WAY THE OPTIONS FIELD
; 493	;	GETS ITS VALUES ASSIGNED. EACH BIT GETS A FIELD DEFINITION.
; 494	;165	BUG IN 161 TO 164 WAS MISSING AC0 AT POP2: PARITY BIT WAS PUT THERE
; 495	;	IN THE NEWER MICROCODES
; 496	;	INSTALL MANY THINGS TO MAKE WORD STRING MOVES WORK START AT
; 497	;	MOVWD1 AND UNTILL BMVWD1 ALSO ASSORTED MACROS ARE ADDED
; 498	;	THESE ARE INSTALLED IN A SEPERATED EIS FILE (WDEIS) FOR THE MOST PART
; 499	;	THERE ARE SOME NEW MACROS AND THE CLEAN+17 LOCATION IS USED FOR
; 500	;	THIS CASE UNDER MODEL B CONDITIONAL INTERRUPTS DO NOT WORK YET
; 501	;	IN THIS CODE BUT ALL DATA TRANSFERS ARE CORRECT. INTERRUPTS ARE
; 502	;	TAKEN SO SUSPECT THE PROBLEM IS IN THE CLEANUP CODE.
; 503	;164	LEAVE IN ONLY MAP FIX
; 504	;163	TAKE OUT MAP FIX LEAVING XHLLI IN AND JRSTF IN
; 505	;162	PUT XHLLI BACK IN TAKE OUT JRSTF ONLY IN SEC 0 CODE
; 506	;161	XHLLI OUT TO DEBUG ADD RSTF0: TO MAKE TEST FOR JRSTF IN NON
; 507	;	0 SECTIONS TEST IN ALL CASES
; 508	;157	INSTALL XHLLI MAKE JRSTF UUO ON NON ZERO SECTIONS
; 509	;	ALSO MAKE MAP DOING A REFILL PAGE FAIL RIGHT THIS MEANS THAT AFTER
; 510	;	CLEAN IT CANNOT DO ANYTHING INTERESTING IF AN INTERRUPT IS PENDING
; 511	;	CHANGES AT MAP2:
; 512	;156	REINSERT A SKP INTRPT IN THE PAGE FAULT HANDLER TO HAVE INDIRECT
; 513	;	POINTER CHAINS INTERRUPTABLE. AT PGRF6:+6
; 514	;155	ABORTIVE MAP FIX FIX REMOVED PROBLEM MUST BE FIXED IN HARDWARE.
; 515	;154	ADD TESTS FOR AC'S IN PHYSICAL REFERENCES FOR EXAMINES AND DEPOSITS
; 516	;	PHYS REFS GO TO MEMORY, NOT AC'S AFTER PROBLEM SHEET 1675
; 517	;	CHANGES AT PILD+3 PIFET+2 PSTOR PHYS1 PHYS2 PHYS3
; 518	;	ADD CHANGES IN TRACKS TO MAKE MODEL A WORK AT TRK2+2 AND +3
; 519	;153	ADD SPECIAL CODE FOR PXCT OF BLT THIS HOPEFULLY CAN GO AWAY
; 520	;	WHEN THE EXTENDED ADDRESSING MONITOR DOES NOT USE PXCT ANYMORE
; 521	;	IT IS UNDER .IF/BLT.PXCT CONDITIONAL AND COSTS 12 WORDS
; 522	;152	CHANGE WHAT BLT DOES TO MATCH THE SPEC SR_BLT(XXX) IS CHANGED TO
; 523	;	NOT FORCE GLOBAL ADDRESSING THE LOAD VMA(EA)_ARX+BR AND; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-9
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 524	;	STORE VMA(EA)_ARX MACROS ARE ADDED TO FORCE THE GLOBAL/LOCAL PARAMETERS
; 525	;	TO BE THE SAME AS THOSE OF THE EFFECTIVE ADDRESS
; 526	;151	PUT THE EPT AND UPT AT 540 UNDER SWITCH CONTROL .IF/EPT540
; 527	;150	VERSION NUMBER BACKED UP TO PRESERVE SPACE IN VERSION NUMBER FIELD
; 528	;304	EXTEND 0 WOULD GET A JUMP TO AN UNUSED MICROLOCATION IN MODEL.B
; 529	;	ONLY THIS WAS BECAUSE LOCATION 2002: IN MODEL.A SHOULD BE AT 3002:
; 530	;	IN MODEL.B 3002: AND 3003: PUT IN WHERE 2002: AND 2003: ARE UNDER
; 531	;	CONDITIONALS.
; 532	;303	CHANGE THE NUMBER FIELD OF THE SR_BLT(XXX) MACROS TO GIVE THE
; 533	;	BIT 0 OFF ALL THE TIME. THIS GIVES BLT MORE THE FORM OF THE OTHER
; 534	;	EXTENDED ADDRESSING STUFF IN HOW IT REFERS TO THE SHADOW AC'S.
; 535	;	IT IS STILL BELIEVED TO BE BROKEN BUT IS BETTER THAN IT WAS.
; 536	;302	ADD LONGER ADDRESS CONSTRAINTS FOR THE NEW MICROASSEMBLER. EVERY
; 537	;	LOCATION THAT THE DISPATCH RAM CAN JUMP TO IS EFFECTED. THE
; 538	;	CONSTRAINTS THATUSED TO LOOK LIKE =00**** MUST NOW LOOK LIKE
; 539	;	=0****00**** THIS IS BECAUSE THE MODEL B MACHINE CAN AND DID
; 540	;	REALLY SET THAT BIT. THE CHANGE MAKES THE MICROCODE INCOMPATIBLE
; 541	;	WITH THE OLD ASSEMBLER.
; 542	;301	HALT IS CLEARING THE RUN FLOP WITH HARDWARE MUST CHECK FOR
; 543	;	KERNAL MODE BEFOR THE HALT MACRO SO USER IOT MODE WILL
; 544	;	NOT BE ABLE TO HALT. THIS TAKES ONE MICROWORD AT 1017:
; 545	;	THE SENSE OF THE SKIP IS REVERSED AGAIN SO 1016: IS BACK TO
; 546	;	BEING THE UUO AND CHALT: IS NOW A SEPERATE WORD AFTER 1017:.
; 547	;300	REPLACE HALT CODE AGAIN BUT THIS TIME GET THE SENSE OF THE
; 548	;	SKIP RIGHT BY SWAPPING THE CONTENTS OF LOCATIONS 1016: AND 1017:
; 549	;	PUT THE 1: ADDRESS CONSTRAINT ON CONT:.
; 550	;277	PUT HALT BACK THE WAY IT WAS SKP USER HAS THE INVERSE SKIP SENSE
; 551	;	AND HENCE DOES THE WRONG THING. HALT TO BE FIXED LATER.
; 552	;276	YET ANOTHER TRY AT THE BLKO PROBLEM BLK1: SHOULD HAVE HAD A
; 553	;	J/BLK2.
; 554	;275	THE LONG PC CHANGES HAD XSFM1: BEFOR THE ADDRESS CONSTRAINT THUS
; 555	;	GIVEING THE WRONG ADDRESS. THE =0 IS PUT BEFOR THE LABEL.
; 556	;274	FIX THE DIAG.INST CONDITIONALS TO BEHAVE PROPERLY WITH THE
; 557	;	CONSTRAINTS OF DRAM LOCATIONS MAP DIED BECAUSE IT NEVER WAS
; 558	;	REACHED OUT OF A DISPATCH.
; 559	;273	INSERT THE DIAG.INST FEATURE FOR THE DIAGNOSTICS PEOPLE.
; 560	;	CHANGES AT DCODE 104:, 106: AND AT XCT: SHOULD NOT EFFECT OTHER
; 561	;	ASSEMBLIES.
; 562	;272	THE FIX TO THE GARBAGE IN THE LEFT HALF OF VMA IN 265 FORGOT TO
; 563	;	LOAD THE VMA IN BLK3:+1 PUT THAT IN. ALSO ON JUD'S RECOMENDATION
; 564	;	PUT A COPY OF THE NOP MICROINSTRUCTION AFTER CLRFPD: TO MAKE
; 565	;	ENOUGH TIME IN THE SKIP CASE. IT SEEMED TO WORK WITHOUT THIS
; 566	;	AND IF SPACE GETS TIGHT IT SOULD BE REMOVED.
; 567	;271	FIX IN 267 PGF4:+4 DOES NOT WORK, CANNOT PUT VMA_# THERE. POSSIBLY BECAUSE
; 568	;	VMA_# CONFLICTS IN SOME ESOTERIC WAY WITH STORE? THAT CHANGE
; 569	;	IS TAKEN OUT AND AT PGF1 THE VMA IS GIVEN 500 OR 501. THIS IS SLIGHTLY
; 570	;	LESS DESIREABLE AND FURTHER EFFORT COULD BE SPENT IN THE UCODE TO
; 571	;	MAKE PAGE FAILS LESS UNWEILDY FOR THE SOFTWARE ROUTINE THAT CONVERTS
; 572	;	THEM TO MODEL B FORM.
; 573	;270	CHANGE HALT TO CHECK FOR USER MODE INSTEAD OF IO LEGAL. A JOB
; 574	;	IN USER IOT SHOULD NOT BE ABLE TO HALT THE MACHINE.
; 575	;267	ADD NEW CONDITIONAL SHIFT.MUUO TO PROVIDE THE SHIFTED DOWN MUUO
; 576	;	DATA BLOCKS MORE SIMILAR TO THE XADDR TYPES. CONDITIONAL IS USED
; 577	;	AT 1003: AND PGF4:+4 TO PROVIDE A DIFFERENT STARTING ADDRESS.
; 578	;266	FIX PILD+3 TO LOAD THE VMA AT THE SAME TIME THUS ENABLING
; 579	;	THE MODEL HACK FIX TO LOAD THE LONG VMA.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-10
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 580	;265	HAIR UP THE ALREADY HAIRY BLKXX CODE TO CLOBBER THE LEFT HALF OF AR
; 581	;	BEFOR USING IT AS AN ADDRESS. CLOBBERED ARL AT BLK2 AND LOADED
; 582	;	VMA AT BLK3.
; 583	;264	ADD J/CLRFPD AT BFIN TO MAKE IT THE SAME AS IT WAS. BFIN GOT
; 584	;	MOVED TO A DIFFERENT PLACE IN THE LAST EDIT AND THIS J FIELD
; 585	;	WAS NOT FIXED.
; 586	;263	ADD THE MIT FIXES. IOTEND AND THE BLK1 TO BLK4 GROUP ARE CHANGED
; 587	;	EXTENSIVELY. CLRFPD IS PUT JUST BEFORE FINI CONSTRAINT ON IOFET
; 588	;	IS CHANGED.
; 589	;	ADD THE LONG PC FORMAT UNDER A NEW CONDITIONAL LONG.PC THE
; 590	;	CONDITIONAL IS TURNED ON BY XADDR. CONDITIONALS ARE ADDED TO THE
; 591	;	LONG PC CODE TO MAKE IT SMALLER WHEN ONLY SECTION 0 IS POSSIBLE.
; 592	;	ADD COMMENTS TO THE MICROCODE OPTIONS FIELD.
; 593	;	RESTORE SXCT CODE FROM VERSION 131. TO BE USED ONLY IN MODEL A
; 594	;	NON KLPAGING CODE.
; 595	;262	PUT WORD AT INDR1+1 UNDER SXCT CONDITIONAL SO WHEN SXCT IS OFF WE
; 596	;	GET AN ADDITIONAL SAVINGS OF ONE WORD.
; 597	;261	ADD PHYS REFS AT PGRF6+4 AND PIDISP+4 TO MAKE MODEL.A LOAD A LONG
; 598	;	VMA. PART OF THIS CODE IS NOT UNDER CONDITIONAL BECAUSE IT SHOULD NOT MATTER
; 599	;	TO A MODEL.B MACHINE. PIDISP+4 ALSO GETS THE LOAD OF THE SAME DATA
; 600	;	REPEATED SO THE PHYS REF HAS SOMETHING TO WORK ON.
; 601	;	FLUSH THE NOW USELESS CODE AT CHALT TO GENERATE THE LD AR.PHYS
; 602	;	CONSTANTS.
; 603	;	CURRENTLY THERE IS SORT OF A BUG IN THAT THE SBR AND THE CBR
; 604	;	CAN NOT BE ABOVE 256K IN A MODEL.A MACHINE. THIS DOES NOT BOTHER
; 605	;	THE CURRENT MONITORS AT ALL IN THAT THESE TABLES ARE IN VERY LOW CORE.
; 606	;	IF THAT CHANGES THE LOCATIONS SECIMM+3 SECIMM+7, LDIND, PGRF5, LDSHR
; 607	;	AND LDPT1+1 MUST ALL GET FIXED UP. THE GENERAL FIX IS TO GET A PHYS REF
; 608	;	IN THE MICROINSTRUCTION THAT LOADS THE VMA. THIS CAN BE DONE BY
; 609	;	POSTPONING THE LOAD OF THE VMA ONE MICROINSTRUCTION IN ALL OF THESE
; 610	;	PLACES, BUT, SINCE THAT CAUSES A PERFORMANCE DEGRADATION IT WAS NOT
; 611	;	DONE.
; 612	;260	DIVERGANT CHANGES TO MAKE KLPAGING PHYS REFS THE OLD WAY
; 613	;	CAUSE ALL CASES OF VMA_XXX+LD AR.PHYS TO GO BACK TO THE
; 614	;	OLD PHYS REF WAY
; 615	;257	IN MODEL B MACHINES AT LDPT+1 THE VMA IS GETTING GARBAGE IN THE
; 616	;	LEFT HALF BECAUSE IT ADDED IN JUNK THAT WAS IN AR LEFT. FIX IS TO
; 617	;	CLEAR ARL AFTER LDPT AND TO DO THE SHUFFLE PERFORMED THERE ONE
; 618	;	MICROINSTRUCTION LATER.
; 619	;******	A HACK FIX IS USED HERE THAT TAKES TWO WORDS. THIS WAS DONE BECAUSE
; 620	;	OF EXTREEM TIME PRESSURE TO DEBUG >256K MODEL B. THERE OUGHT TO BE
; 621	;	A WAY TO REDUCE THIS FIX TO ONLY ONE WORD IN SPACE AND TIME, OR
; 622	;	EVEN LESS.
; 623	;256	EDIT JUMPED TO RANDOMNESS WITH AN EXTRA RETURN. THIS HAPPENED
; 624	;	BECAUSE THERE WAS NO CALL AT EDSFLT IN THE MODEL B NON XADDR CODE
; 625	;	ADDED CALL TO EDSFLT.
; 626	;255	SAVE EDIT FROM GETTING AN EXTRA STORE CYCLE AT EDSSIG BY SENDING
; 627	;	IT ALWAYS TO THE EDFLT1 LOCATION INSTEAD OF EDFLT THIS ONLY
; 628	;	CHANGES WHAT HAPPENS IN MODEL B NON XADDR BECAUSE IN MODEL A
; 629	;	EDFLT AND EDFLT1 ARE THE SAME LOCATION ANYWAY
; 630	;254	CAUSE THE A INDRCT CHANGE IN 253 TO BE ONLY FOR NON EXTENDED
; 631	;	ADDRESSING MACHINES. THIS THROWS DOUBT ON THE WORD SAVINGS
; 632	;	THAT MIGHT HAVE BEEN POSSIBLE
; 633	;253	CHANGE A INDRCT TO LOAD BOTH THE AR AND ARX, IN THE EXTENDED
; 634	;	INSTRUCTION SET THIS HAPPENED TO BE DEPENDED ON AT EXT2+2 AND
; 635	;	EXT2+3. THE DEFINITION OF A IND IN EA CALC/ WAS FIXED TO; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-11
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 636	;	LOAD THE AR AND THE ARX
; 637	;	I THINK THIS PERMITS THE SAVINGS OF AN EXTRA WORD AND SOME
; 638	;	TIME ON ALL INDIRECTS. CHECK OUT FLUSHING INDR1 AND MAKING INDRCT
; 639	;	DO THE DISPATCH AND GO TO COMPEA
; 640	;	  FORCE ADB TO GENERATE AR*4 AS DEFAULT THIS DISABLES PARITY
; 641	;	CHECKING ON THE FM WHEN IT IS NOT BEING READ FIXED IN
; 642	;	DEFINITION OF ADB THIS WILL ALSO SPEED UP THE MACHINE BY SOME
; 643	;	BECAUSE THE ADB FIELD CAN NO LONGER FORCE 3 TICS WITHOUT REALLY
; 644	;	NEEDING THAT LONG
; 645	;252	SAVE A WORD AT IOPGF+1 BY MAKING IT PILD+3 THIS ADDS THE SET
; 646	;	ACCOUNT ENABLE TO AN UNDEFINED CASE.
; 647	;251	TURNING ON PAGING CAUSED A HANG THIS WAS BECAUSE OF A MISIMPLIMENTED
; 648	;	FIX IN 250. THE ATTEMPT TO PUT THAT FIX IN NO SPACE FAILED AND IT TOOK
; 649	;	ONE WORD. AT LDPT+1 ADD BR/AR AT GTCST1 RECOVER THE AR FROM THE BR
; 650	;	THIS SEEMS LIKE IT SHOULD BE ABLE TO BE BUMMED BUT I CANNOT
; 651	;	FIGURE OUT HOW
; 652	;	ALSO FIX A PLACE WHERE A PHYS REF WAS LEFT IN THE MODEL A CODE
; 653	;	AT PGRF6+4 MODEL B CONDITIONAL IS AS IT WAS MODEL A IS NEW TO USE
; 654	;	LD AR.PHYS MECHANISM
; 655	;250	LOADING HIGH ORDER GARBAGE TO THE VMA WITH THE FIX FOR
; 656	;	>256K CAUSES FUNNY THINGS TO HAPPEN. BITS GET CLOBBERED
; 657	;	WITH AR0-8_SCAD 14 LINES AFTER SECIMM. ACTUALLY IS MORE
; 658	;	HAIR BECAUSE OF CONFLICTING FIELDS. CODE ABOVE AND BELOW
; 659	;	THAT GOT REARRANGED TO SIMPLER MODEL A AND MODEL B CONDITIONALS
; 660	;	SINCE NOW ALL LINES ARE DIFFERENT. SHUFFLING OF FE IS DONE
; 661	;	TO PROVIDE ROOM FOR A CONSTANT ON THE CORRECT SIDE OF THE SCAD
; 662	;	AT LDPT A SIMILAR
; 663	;	RECODING IS NEEDED. 4 LINES OF CODE ARE REDONE IN MODEL
; 664	;	A CONDITIONAL AND CONDITIONALS ARE RESHUFFLED TO HAVE
; 665	;	SIMPLER FORMAT
; 666	;	NEW MACROS ARE ADDED GEN AR0-8, GEN FE AND AR0-8
; 667	;	VMA_AR+LD AR.PHYS AND ITS FRIENDS ARE TAKEN OUT OF KLPAGING
; 668	;	CONDITIONAL THEY ARE USED TO DO EXAMINES AND DEPOSITS NOW
; 669	;247	FIX ST AR.PHYS TO GIVE BIT 4 INSTEAD OF BIT 5 AT CHALT
; 670	;	AT PSTORE CHECK FOR AC REF AND IF SO WRITE FM MUST DO THIS
; 671	;	BECAUSE LOAD AD FUNC DOES NOT SET MCL STORE AR
; 672	;246	FIX MUUO, IN EXTENDED ADDRESSING, TO GET NEW PC BEFORE CLOBBERING
; 673	;	THE USER AND PUBLIC FLAGS THAT TELL WHERE TO GET IT.  FIX CONDITIONAL
; 674	;	ASSEMBLY AT INDRCT TO DO EA TYPE DISP IN MODEL A, NOT MODEL B.
; 675	;245	ADDITIONAL FIXES FOR THE 256K PROBLEM, TO MAKE EXAMINE AND
; 676	;	DEPOSIT WORK.  CHANGES AT CHALT TO CREATE CONSTANT "ST AR.PHYS",
; 677	;	AND EXTENSIVELY NEAR PICYC1, PIDATI, AND PIDATO.  CHANGES ARE ALL
; 678	;	UNDER MODEL B CONDITIONAL, BECAUSE MODEL B HARDWARE WORKS OK, AND
; 679	;	THE FIX IS REGARDED AS CROCKISH.
; 680	;244	WAIT FOR COMPLETION OF INDIRECT REFERENCE AT BYTEI+1 AND EXTI+1
; 681	;	EVEN THOUGH INTERRUPT REQUEST HAS BEEN SEEN, SO AS NOT TO CONFUSE MBOX.
; 682	;243	VARIOUS FIXES TO MAKE THESE SOURCES WITH MODEL.B SWITCH OFF
; 683	;	EQUIVALENT TO MODEL A SOURCES, SO WE CAN DISCARD MODEL A SOURCES
; 684	;	THE FIXES ARE:
; 685	;		1) SWITCH SNORM.OPT, TO SAVE SPACE IN SINGLE PRECISION
; 686	;		FLOATING NORMALIZATION.
; 687	;		2) CREATION OF LD AR.PHYS MAGIC CONSTANT, TO SOLVE HARDWARE
; 688	;		PROBLEMS GENERATING ADDRESSES ABOVE 256K.
; 689	;242	FIX AT SECPTR+1 TO PRESERVE AR LEFT UNTIL WE CAN CHECK
; 690	;	FOR BITS 12-17 NON ZERO CORRECT ADDRESS CONSTRAINTS AT
; 691	;	SECIMM+1 & +2 TO GET BRANCHING RIGHT FOR SHARED AND INDIRECT; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-12
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 692	;	SECTION POINTERS.  FIX AT LDIMM+1 TO CLEAR LH OF AR BEFORE
; 693	;	LOADING VMA WITH SPT ADDRESS, TO PREVENT PAGE FAULT ON SPT
; 694	;	REFERENCE.
; 695	;241	MORE FIXES AT START: AND NEWPC:, FOR SAME PROBLEM AS 240.
; 696	;	MUST LOAD FLAGS AND CLEAR VMAX, THEN LOAD VMA INTO PC TO CLEAR
; 697	;	PCX, THEN RELOAD VMA TO GET EFFECT OF NEW FLAGS AND CLEARED
; 698	;	PCX.  (MODEL A ONLY).
; 699	;240	FIXES AT START: AND NEWPC: TO LOAD 23-BIT ADDRESS RATHER
; 700	;	THAN 30-BIT, SINCE OTHER BITS ARE PC FLAGS.  AT SAME TIME AND
; 701	;	PLACE, FIX MODEL A CODE TO CLEAR PC SECTION NUMBER.
; 702	;237	CHANGE CONDITIONALS AROUND PUSH AND POP CODE FROM XADDR TO
; 703	;	MODEL.B. COULD SIMPLIFY IFNOT XADDR.
; 704	;236	FIX ADDRESS CONSTRAINTS ON USES OF EA MOD DISP IN MODEL
; 705	;	B MACHINE WITH EXTENDED ADDRESSING OFF.  PROBLEMS AT COMPEA,
; 706	;	BFETCH, AND EXT2.
; 707	;235	SLIGHTLY CLEANER FIXES FOR PROBLEMS IN 234 TO AVOID WASTING TIME
; 708	;	AND SPACE.  BYTE READ MACRO NEEDS TO SET VMA/LOAD, AND VMA_VMA
; 709	;	HELD MACRO DOESN'T USE MEM FIELD UNLESS MODEL B AND KL PAGING.
; 710	;	ALSO FIX CONDITIONAL ASSEMBLY STUFF TO AVOID SPURIOUS ERRORS.
; 711	;234	INSTALL FIXES FOR SOME PLACES WHERE MODEL B CODE CAUSES CONFLICT
; 712	;	WITH THE OLD NON KLPAGING NON EXTENDED ADDRESSING CODE
; 713	;	THESE ARE AT BFETCH, PGF3-1, PGF6, EXT1+2
; 714	;233	FIX THE FOLLOWING PROBLEMS:
; 715	;		KL PAGING SHOULD PRODUCE A PAGE FAILURE WHEN BITS
; 716	;		 12-17 OF A PRIVATE SECTION POINTER ARE NON 0
; 717	;		 FIXED AT SECPTR ETC.
; 718	;		EDIT DOES NOT ALLOW INTERUPTS
; 719	;		 FIXED AT EDNXT1 AND AFTER THAT
; 720	;		MAP SHOULD NOT BE LEGAL IN USER MODE
; 721	;		 FIXED AT MAP2 AND CLEAN+15
; 722	;		MOVMI IS SHORTENED BY MAKING IT THE SAME AS MOVEI
; 723	;		 AT DON LEWINES SUGGESTION THIS IS IN DCODE 215
; 724	;232	MERGE THE SECOND ORDER STATISTICS GATHERING CODE WITH THIS
; 725	;	CODE INTENT IS TO KEEP IT HERE
; 726	;231	CHANGE THE LOAD CCA DEFINITION TO REFLECT THE NEW HARDWARE
; 727	;	THIS IS ENABLED WHEN THE MODEL.B ASSEMBLY SWITCH IS ON
; 728	;230	THIS IS THE POINT WHERE MICHAEL NEWMAN TAKES OVER THE MICROCODE
; 729	;	MAINTENCE SEVERAL BUG FIXES GET EDITED INTO 126 AT THIS POINT
; 730	;	TWO SETS OF PARALLEL CODE WILL BE MAINTAINED FOR A WHILE.
; 731	;	FIX THE CMPS PARODY ERROR PROBLEM WHEN ILLEGAL BITS ARE FOUND IN
; 732	;	THE LENGTHS.
; 733	;227	FIX PIBYTE TO GET DTE# CORRECT ON TO-10 TRANSFERS.  FIX MTRREQ
; 734	;	CYCLES TO WAIT FOR STORE TO FINISH BEFORE RE-ENABLING ACCOUNT.
; 735	;	FIX ADJSP OF LONG STACK POINTERS TO FETCH NEXT INSTR.
; 736	;226	FIX EXMD TO LOAD AR, RATHER THAN ARX, WITH MARK POINTER, AS
; 737	;	EXPECTED BY THE HANDLER.  FIX EDIT, SEVERAL PLACES, TO IGNORE
; 738	;	LEFT HALF OF MARK & PATTERN ADDRESSES WHEN PC SECTION IS ZERO.
; 739	;	FIX EDIT TO MAKE EXTENDED REFERENCE FOR PATTERN BYTES.
; 740	;	FIX ADJSP TO BE MEANINGFUL WITH LONG STACK POINTERS
; 741	;225	FIX BYTEA NOT TO CLOBBER FE ON INDIRECTS, FIX EXMD TO BACK
; 742	;	UP VMA AFTER STORING DSTP2 AND BEFORE STORING DSTP.  FIX EDIT TO
; 743	;	COUNT THE WHOLE PATTERN ADDRESS IF PC SECTION NOT ZERO.
; 744	;224	FIX EXTEND ADDRESS CALCULATION TO RECOVER E0 FROM MQ, AND
; 745	;	FIX EXTEND OPCODE TEST TO DISALLOW OPS >20.
; 746	;	FIXES TO HANDLE NEW ENCODING OF AC-OP ON APR BOARD.
; 747	;223	COMPLETE 222.  P HAS TO GO TO SC AS WELL AS AR0-5.  CREATE; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-13
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 748	;	SUBROUTINE RESETP TO DO IT.  GET CODE IN SYNC WITH HARDWARE AND
; 749	;	MOST RECENT SPEC FOR MEANING OF PXCT AC BITS IN EXTEND.  THUS
; 750	;	UNDO COMMENT IN 221:  WE SHOULD LOOK AT PXCT B11.  ALSO FIX
; 751	;	EXTEND TO USE CORRECT ENCODING OF BITS 9, 11, AND 12 FOR PXCT
; 752	;	OF STRING OPERATIONS.  FIX DATAI PAG SO IT DOESN'T LOSE THE
; 753	;	PREVIOUS CONTEXT AC BLOCK WHEN LOADING PREVIOUS SECTION #.
; 754	;	INSERT CHANGE CLAIMED FOR EDIT 55, TO INHIBIT INTERRUPT DURING
; 755	;	PI CYCLES.
; 756	;222	FIX BYTE POINTER UPDATE ROUTINES GSRC & IDST IN EIS CODE
; 757	;	TO UPDATE P WHEN INCREMENTING SECOND WORD.  JUST FORGOT TO. TRY
; 758	;	AGAIN TO CONTROL EIS REFERENCES OFF E0, FOR EXTENDED OR NOT.
; 759	;221	COMPLETE FIX OF 220, TO KEEP SR CORRECT THROUGH RELOAD OF IR
; 760	;	IN EXTEND DECODING, AND TO CONTROL SR CORRECTLY FOR XBLT DST
; 761	;	REFERENCES.  (WE WERE LOOKING AT PXCT B11, SHOULD BE B12).
; 762	;220	FIXES SEVERAL PLACES TO USE "EA" IN DRAM A FIELD INSTEAD OF "I",
; 763	;	NOTABLY BLT, WHICH WAS USING WRONG SECTION.  FIX EXTEND TO
; 764	;	CONTROL VMA EXTENDED BEFORE FETCHING EXTEND-OP, SO AS NOT TO
; 765	;	LOOK "UNDER" THE AC'S.  FIX XBLT FOREWARD TO STOP WHEN AC GOES
; 766	;	TO ZERO, NOT -1.  ALSO CONTROL SR BEFORE INITIAL STORE TO GET
; 767	;	CORRECT CONTEXT.
; 768	;217	CODE CHANGES TO MAKE SECOND WORD OF BYTE POINTER WORK RIGHT
; 769	;	WHETHER EFIW OR IFIW, BY CONTROLLING CRY18 OR CRY6.
; 770	;216	RECODE EXTENDED INSTRUCTION SET DECODING & EFFECTIVE ADDRESS
; 771	;	CALCULATION.  FIX UUO CODE TO INCREMENT VMA AFTER STORING PC.
; 772	;	FIX ADJBP TO GET 36 BIT ADDRESS ADJUSTMENT IF B12 SET.
; 773	;215	REARRANGE CONDITIONAL ASSEMBLY DEFAULTS TO BE MORE LOGICAL
; 774	;	INSERT FORM FEEDS AND COMMENTS TO HELP BEAUTIFY THE LISTING.
; 775	;	REWORK THE NEW JRST'S, TO MAKE THEM SMALLER, FASTER, AND TEST
; 776	;	IO LEGAL BEFORE DISMISSING.  PUT IN XBLT.
; 777	;214	MODIFY ADJBP AND UUO'S FOR EXTENDED ADDRESSING. REWORK PARITY
; 778	;	ERROR HANDLING, IN A FRUITLESS ATTEMPT TO MAKE IT SMALLER,
; 779	;	BUT SUCCESSFULLY MAKING IT CLEARER.  FIX ASSEMBLY ERRORS IN EIS
; 780	;	DUE TO AC4 CHANGES, AND ADD CODE TO HANDLE LONG BYTE POINTERS
; 781	;	IN AC'S.  PUT IN CODE TO GIVE PAGE FAIL 24 ON ILLEGAL FORMAT
; 782	;	INDIRECT WORD.
; 783	;213	FIX LDB & DPB TO TEST POINTER BIT 12 ON CALL TO BYTEA.
; 784	;212	MODIFY JSP, JSR TO STORE FULL PC WITHOUT FLAGS IN NON-ZERO SEC
; 785	;	SEPARATE CONDITIONALS FOR "MODEL B" MACHINE FROM THOSE FOR
; 786	;	EXTENDED ADDRESSING MICROCODE.
; 787	;211	REMOVE UNNECESSARY DIDDLING OF VMA USER BIT DURING PAGE REFILL,
; 788	;	AND ELIMINATE SPECIAL CASE FOR MAP INSTRUCTION, WHEN EXTENDED
; 789	;	ADDRESSING HARDWARE EXISTS TO SOLVE THESE PROBLEMS.
; 790	;	FIX SEVERAL CASES OF SIGNS DISP WITH INADEQUATE CONSTRAINT.
; 791	;210	FIX DEFINITION OF "SKP LOCAL AC REF", WHICH CONFUSED "AC
; 792	;	REF" WITH "LOCAL AC REF".
; 793	;207	FIX JRSTF (AND ITS DERIVATIVES) TO LOAD FLAGS INTO AR AFTER
; 794	;	DOING EA MOD DISP, WHICH WOULD OTHERWISE CLOBBER THEM.  FIX
; 795	;	COMPEA CODE TO LET AREAD HARDWARE LOAD AR.  OTHERWISE GET SEC #.
; 796	;206	FIX PCTXT ROUTINE TO GET PREVIOUS CONTEXT SECTION.
; 797	;205	FIX POPJ TO LOAD HALFWORD OR FULLWORD PC ACCORDING TO PC SECT
; 798	;204	FIX CONDITIONALS AROUND LOC 47, WRONG IN 202.  FIX DEFINITION
; 799	;	OF A INDRCT, DOESN'T NEED #07.  FIX STACK INSTRUCTIONS FOR
; 800	;	EXTENDED ADDRESSING.  MUST NOT LOAD VMA FROM FULL AD.
; 801	;203	INCLUDE CODE AT NEXT+2 TO GENERATE ADDRESS MASK (LOW 23 BITS)
; 802	;	AT HALT TIME, AND CODE IN PICYCLE TO USE IT TO GET 23 BIT ADDR
; 803	;	OUT OF IOP FUNCTION WORD.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-14
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 804	;202	MOVE "40+A" LOCATIONS TO "A" UNDER EXTENDED ADDRESSING.  CHANGE
; 805	;	ALL CALL MACROS TO GENERATE CALL BIT INSTEAD OF SPECIAL FUNC'S.
; 806	;201	BEGIN EXTENDED ADDRESSING CHANGES IN EARNEST.  INTEGRATE NEW
; 807	;	EFFECTIVE ADDRESS COMPUTATION CODE, AND REVISE INSTRUCTION
; 808	;	ROUTINES AS NECESSARY.
; 809	;126	FIX STRAC3-2, WHERE COMMA GOT LEFT OFF WHEN IFETCH MOVED
; 810	;125	REMOVE NXT INSTR FROM STAC1, STRAC3, & STAC4, MAKING THEM JUMP
; 811	;	TO FINI INSTEAD.  PROBLEM INVOLVES A RACE IF PAGE FAIL OCCURS
; 812	;	WHILE WRITING FM.  IF FM ADDRESS CHANGES BEFORE COND/FM WRITE
; 813	;	GOES FALSE, APR BOARD MAY GRONK PARITY BIT OF SOME FM LOC'N.
; 814	;	THIS RESULTS IN SOME SOME PATHS FROM FETCH TO NICOND BECOMING
; 815	;	LONGER THAN 6 TICKS, SO THE FETCHES GOT SHUFFLED IN SOME PLACES.
; 816	;	MICROCODE PATCH ELIMINATES MOST PROBABLE CAUSE, WHICH IS PAGE
; 817	;	FAIL AT NICOND TIME WHILE WRITING AC OTHER THAN 0.  IT DOES NOT
; 818	;	TAKE CARE OF THE POSSIBILITY THAT COND/FM WRITE WILL GLITCH AT
; 819	;	INSTR 1777 TIME.
; 820	;124	FIXES IN SEVERAL PLACES TO SET AND CLEAR ACCOUNT ENABLE SO AS
; 821	;	TO GET REPEATABLE ACCOUNTING MEASURES OF USEFUL WORK DONE. THE
; 822	;	ENABLE IS NOW CLEARED FOR METER UPDATE CYCLES AND KL PAGE REFILL
; 823	;	CYCLES.  THE HARDWARE ALREADY TAKES CARE OF PI CYCLES.
; 824	;123	CORRECT 122 TO CONSTRAIN LOC "UNHALT", AND TO LOAD ARX FROM AR,
; 825	;	SO AS TO LET "SKP AR EQ" WORK.  PROBLEM AROSE BECAUSE MACRO ALSO
; 826	;	TESTS ARX00-01.  FIX EDIT, WHEN STORING DEST POINTER ON SELECT
; 827	;	SIGNIFICANCE START, TO ELIMINATE AMBIGUITY IN DEST P FIELD.
; 828	;122	SPEC CHANGE TO EXIT FROM HALT LOOP, SO THAT AR0-8=0 WITH AR9-35
; 829	;	NON-ZERO LOADS AR INTO PC TO START PROCESSOR.  THIS IS DIFFERENT
; 830	;	FROM EXECUTING JRST BECAUSE PC FLAGS ARE CLEARED.
; 831	;121	FIX TO 120 TO ALLOW A CYCLE BETWEEN FILLER FROM MEMORY AND
; 832	;	WRITING IT INTO FM (THUS PARITY CAN BE COMPUTED).  ALSO CLEAR
; 833	;	STATE REGISTER IN EDIT BEFORE GETTING NEXT PATTERN BYTE.
; 834	;120	FIX EIS TO TOLERATE PAGE FAIL ON READ OF FILL BYTE IN MOVSRJ
; 835	;	OR B2D CONVERSION.  REQUIRES GETTING FILLER BEFORE STORING DLEN
; 836	;	ALSO INTEGRATE OPCODE COUNTING/TIMING CODE UNDER CONDITIONALS
; 837	;117	FIX PARITY ERROR CODE TO WRITEBACK AR ON RPW ERROR.
; 838	;116	REWRITE OF DDIV, SO THAT THE NO-DIVIDE TEST IS ON THE MOST
; 839	;	SIGNIFICANT HALF OF THE MAGNITUDE OF THE DIVIDEND, RATHER THAN
; 840	;	THE MAGNITUDE OF THE MOST SIGNIFICANT HALF.  IN THE PROCESS,
; 841	;	SAVE TIME AND SPACE.  ALSO PUT IN CONDITIONAL ASSEMBLY VARIABLE
; 842	;	"WRTST" TO INHIBIT WRITE TEST CYCLE FOR INSTRUCTIONS WHICH
; 843	;	APPEAR NOT TO NEED IT, AND THUS TO SPEED THEM UP.
; 844	;115	FIX SBDIAG TO SET MCL REG FUNC TO INHIBIT EBOX MAY BE PAGED.
; 845	;114	RECODE STRING COMPARE TO SAVE SPACE AND TIME.  CHANGE DEFAULTS
; 846	;	FOR KLPAGING TO INCLUDE EIS, EXCLUDE TRACKS FEATURE.  CHANGE
; 847	;	KLPAGING (NEW SPEC) TO KEEP "LOGICALLY WRITABLE" IN SOFTWARE BIT
; 848	;113	RECODE KL PAGING TO ELIMINATE PROBLEM OF WRITING HARDWARE
; 849	;	PAGE TABLE BEFORE CHECKING FOR AGE TRAP, AND THEREFORE LEAVING
; 850	;	THE PAGE ACCESSIBLE AFTER THE TRAP.  THE RECODING ALSO IMPROVES
; 851	;	THE ALGORITHM IN THAT THE HARDWARE ENTRY INCLUDES THE W BIT SET
; 852	;	IF THE CORE TABLES ALLOWED WRITE AND THE CST INDICATES WRITTEN,
; 853	;	EVEN IF THE CURRENT REFERENCE WAS NOT A WRITE.
; 854	;	ALSO FIX CODE WHICH WRITES PT DIR, TO GET WRITE REF BIT FROM
; 855	;	VMA HELD INTO BIT 5 OF SAVED PAGE FAIL WORD.
; 856	;112	FIX PAGE FAIL CODE FOR USE WITH PROB SHEET 1396, WHICH LOADS
; 857	;	PC IF PAGE FAIL OCCURS ON NICOND.  THUS CODE NEEDN'T CHECK FOR
; 858	;	FETCH AT CLEAN, WHICH CAUSED OTHER PROBLEMS ON PARITY ERRORS.
; 859	;	CLEAR FE AND SC IN NXT INSTR MACRO (JUST CLEANLINESS).; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-15
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 860	;111	PATCH SEVERAL ROUTINES WITH THE FOLLOWING MACRO --
; 861	;	FETCH WAIT	"MEM/MB WAIT"
; 862	;	TO PREVENT SEQUENCES IN WHICH PAGE FAIL INFO CAN GET LOST
; 863	;	BECAUSE OF LONG TIME FROM REQUEST TO MB WAIT.  THESE PATCHES
; 864	;	SHOULD BE REMOVED AFTER AN ECO HAS BEEN INSTALLED TO FIX.
; 865	;	IN ADDITION, EBUSX SUBROUTINE HAS BEEN MODIFIED TO PREVENT RACE
; 866	;	CONDITION WHEN SETTING UP IO FUNCTION WITH COND/EBUS CTL AND
; 867	;	MAGIC # BIT 4.  MUST NOT CHANGE #5 THROUGH #8 ON NEXT CYCLE.
; 868	;	FIX KLPAGING CODE TO GO BACK TO AREAD ON MAP REF, BECAUSE
; 869	;	MEM/AD FUNC DOESN'T CORRECTLY RESTORE APR REG FUNC.  ALSO MAKE
; 870	;	THE CODE SMARTER ON NO MATCH CONDITION, SO REQUEST DOESN'T HAVE
; 871	;	TO BE RESTARTED AND IMMEDIATELY FAIL AGAIN.
; 872	;110	GIVE UP ON THE OLD STRING COMPARE CODE, INSTALLING MIKE NEWMAN'S
; 873	;	VERSION.  SOMEWHAT SLOWER, BUT GIVES THE RIGHT ANSWERS.
; 874	;	FIX LDB CODE TO WAIT FOR MEM WORD EVEN IF INTERRUPT REQUEST
; 875	;	SEEN, SO AS NOT TO GET CONFUSED WHEN IT ARRIVES OR PAGE FAILS.
; 876	;	ALSO IMPROVE CLRPT ROUTINE USED BY CONO AND DATAO PAG TO START
; 877	;	LOOP WITH VMA CLEARED AND PT WR SELECTION SETUP CORRECTLY.
; 878	;107	FIX STRING COMPARES TO CHECK FOR INTERRUPT.  THIS INVOLVED
; 879	;	CHECKING DURING GSRC ROUTINE, WHICH ELIMINATES NEED FOR CHECK
; 880	;	IN SRCMOD (WHICH CALLS GSRC).  IT ALSO REQUIRED CLEARING SFLGS
; 881	;	AT STARTUP, AND ADJUSTING DLEN UPDATE CODE IN DEST FILL TO GET
; 882	;	VALID LENGTH STORED ON INTERRUPT.
; 883	;106	ELIMINATE RACE IN DECODING OF # FIELD ON MTR BOARD BY HOLDING
; 884	;	LOW 3 BITS THROUGH NEXT MICROINSTRUCTION.
; 885	;	FIX LUUO AND MUUO TO ALLOW INTERRUPTS.
; 886	;	FIX B2D OFFSET TO SIGN-EXTEND E1 AFTER INTERRUPT.  FINISH 105,
; 887	;	TO GET ENTIRE AR LOADED WHILE CLEARING MQ (ARL WAS HOLDING).
; 888	;	FIX KL PAGING TO USE VMA/1 INSTEAD OF VMA/AD WHEN RESTORING VMA
; 889	;	FROM VMA HELD OR COPIES THEREOF.
; 890	;	FIX UFA NOT TO ALWAYS GET UNDERFLOW ON NEGATIVE RESULTS.
; 891	;	SAME FIX AS EDIT 103 OF BREADBOARD.  WHERE DID IT GET LOST?
; 892	;105	FIX KL PAGING AS REVISED BY EDIT 103 TO CORRECTLY RESTORE
; 893	;	BR ON NO-MATCH CONDITION
; 894	;	ANOTHER FIX TO B2D, TO CLEAR MQ ON ENTRY.  BUG INVOLVED GARBAGE
; 895	;	FROM MQ SHIFTING INTO ARX DURING DEVELOPMENT OF POWER OF TEN.
; 896	;104	FIX BINARY TO DECIMAL CONVERSION, WHICH WAS NOT GOING TO CLEAN
; 897	;	ON FINDING AN INTERRUPT, AND ON RESTART WITH FPD SET, WAS NOT
; 898	;	SETTING UP SLEN.  TSK, TSK.  CORRECT CLEANUP FOR DEST FILL IN
; 899	;	MOVSRJ, WHICH WAS INCREMENTING BOTH SLEN AND DLEN, SHOULD
; 900	;	HAVE BEEN NEITHER.  FIX JSR, BROKEN BY EDIT 103.  JUMP MUST BE
; 901	;	TO E+1, NOT E.
; 902	;103	CREATE CONDITIONAL ASSEMBLY FOR EXTENDED ADDRESSING. UNDER IT,
; 903	;	CREATE MEM FIELD DEFINITIONS, SUPPRESS SXCT.
; 904	;	SAVE A WORD IN JSR BY USING JSTAC IN COMMON WITH PUSHJ.
; 905	;	FORCE TIME FIELD IN CASES WHERE ASSEMBLER DEFAULT SCREWS UP.
; 906	;	ADD INTERRUPT TESTS IN KL PAGING CODE TO PREVENT HANGS, AND
; 907	;	REVISE PAGE FAIL WORD TO ELIMINATE THE NEW FAIL CODES.
; 908	;102	ATTEMPT ANOTHER FIX OF MOVSRJ, CVTBDX FILL.  EDIT 71 LOSES
; 909	;	DUE TO INCONSISTENCY -- DLEN UPDATE MUST NOT PRECEED CLEANUP.
; 910	;	CREATE CONDITIONAL ASSEMBLY SWITCHES TO CONTROL EXTENDED
; 911	;	INSTRUCTION SET, DOUBLE INTEGER ARITHMETIC, AND ADJBP.  CHANGE
; 912	;	DEFAULT OF IMULI.OPT, WHICH CAN GET SIGN WRONG ON OVERFLOW.
; 913	;101	FIX METER REQUEST CODE TO "ABORT INSTR" EVEN IF NOT SETTING
; 914	;	PI CYCLE.  THIS SHOULD FIX OCCASIONAL LOSS OF TRAPS PROBLEM.
; 915	;100	FIXES TO KL PAGING CODE TO PREVENT LOADING VMA FROM AD WHILE; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-16
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 916	;	REQUESTING PHYSICAL REF.  FIX JSR TO PREVENT FM PARITY STOP
; 917	;	ON STORE TO AC.  FIX 1777 TO FORCE RECIRCULATION OF AR/ARX,
; 918	;	EVEN IF MBOX RESP STILL TRUE.
; 919	;77	FIX DDIV TO GET MQ SHIFTED LEFT ONE PLACE, WITHOUT INTRODUCING
; 920	;	AN EXTRA BIT, AT DDVX1.  THIS INVOLVES INHIBITING ADA TO PREVENT
; 921	;	AD CRY0 FROM COMMING INTO MQ35.
; 922	;76	FIX UFA TO ALLOW AN EBOX CYCLE BETWEEN FETCH AND NICOND WHEN
; 923	;	FRACTION SUM IS ZERO, AT UFA3.
; 924	;75	PUT BACK INSTRUCTION "MBREL" REMOVED BY EDIT 64.  NECESSARY TO
; 925	;	ENSURE THAT EBOX REQUEST FOR FETCH DOESN'T COME UP WHILE
; 926	;	REGISTER FUNCTION IS IN PROGRESS, WHICH WOULD CONFUSE MBOX ON
; 927	;	STARTING THE FETCH.
; 928	;74	CHANGES TO EIS FOR NEW-SPEC AC USAGE.  CHANGES TO KL PAGING FOR
; 929	;	INDIRECT, IMMEDIATE SECTION POINTERS
; 930	;73	FIX JRA TO PREVENT WRITING AC WITH DATA FRESH FROM MEMORY (ALLOW
; 931	;	A CYCLE FOR PARITY CHECK).  FIX DPB CODE TAKE ONLY 3 TICKS ON
; 932	;	RETURN FROM BYTEA, SO THAT CACHE DATA DOESN'T ARRIVE INTO AR
; 933	;	AND ARX UNTIL DPB1, WHEN THE BYTE HAS GOTTEN OUT TO MQ.
; 934	;72	FIX DEFINITION OF SP MEM/UNPAGED TO INHIBIT VMA USER.  FIX
; 935	;	PAGE FAIL CODE TO CHECK FOR VMA FETCH BEFORE LOOKING AT
; 936	;	INTERRUPT REQUEST.  PROBLEM WAS INTERRUPT CONCURRENT WITH
; 937	;	PAGE FAIL ON JRSTF TO USER.  PC FLAGS GOT RESTORED, BUT VMA
; 938	;	NEVER COPIED TO PC BECAUSE PAGE FAIL INHIBITED NICOND, AND
; 939	;	INTERRUPT ABORTED PAGE FAIL HANDLING TO LOAD PC.
; 940	;71	DEFINE FMADR/AC4=6.  FIX MOVFIL ROUTINE TO PUT AWAY UPDATED
; 941	;	LENGTH DIFFERENCE WHEN INTERRUPTED, THUS AVOIDING RANDOMNESS
; 942	;	IN MOVSRJ, CVTBDX. FIX CVTBD CALL TO MOVFIL TO PRESERVE SR.
; 943	;	CHANGE STMAC AND PIDONE FROM "FIN XFER" TO "FIN STORE", BECAUSE
; 944	;	STORE WAS IN PROGRESS, WHICH CAUSED FM WRITE IF AC REF, AND
; 945	;	GOT A PARITY ERROR DUE TO ADB/FM.
; 946	;70	FIX PXCT 4,[POP ...], WHICH DIDN'T GET DEST CONTEXT SET FOR
; 947	;	STORE.  MUST USE SR_100 TO SET IT.
; 948	;67	FIX PROBLEM IN ADJBP BY WHICH BYTES/WORD WAS GETTING LOST
; 949	;	WHEN DIVIDE ROUTINE LOADED REMAINDER INTO BR.  SOLVED BY
; 950	;	SAVING BYTES/WORD IN T1.
; 951	;66	FIX KL PAGING TO RESTORE VMA ON TRAP, SAVE ADDRESS OF POINTER
; 952	;	CAUSING TRAP, AND NOT RESTORE ARX EXCEPT FOR BLT PAGE FAIL.
; 953	;	ALSO SET TIME PARAMETER ON ADB/FM TO ALLOW TIME FOR PARITY
; 954	;	CHECKING OF FM.
; 955	;65	FIX KL PAGING CODE TO DO MBWAIT AFTER DETERMINING THAT PARITY
; 956	;	ERROR HAS NOT OCCURRED, SO AS TO GET CORRECT VMA TO SAVE.
; 957	;	CREATE SYMBOLS FOR KL PAGE FAIL CODES.  PUT CONDITIONAL
; 958	;	ASSEMBLY AROUND IMULI OPTIMIZATION CODE, AND SXCT.  CREATE
; 959	;	SYMBOL "OPTIONS" IN # FIELD FOR MICROCODE OPTIONS.
; 960	;64	MICROCODE FOR KL10 PAGING (PAGE REFILL, MAP INSTR)...
; 961	;	REMOVE UNNECESSARY INSTRUCTION MBREL: FROM SWEEP AND APRBO
; 962	;	COSMETIC CHANGES TO KEEP COMMENTS & MACRO DEFINITIONS FROM
; 963	;	OVERFLOWING LINE OF LISTING, AND INSERTION OF CONDITIONAL
; 964	;	ASSEMBLY CONTROL OF LONG FLOATING POINT INSTRUCTIONS.
; 965	;63	IN MTR REQUEST ROUTINE, DON'T DISMISS WHEN PI CYCLE HASN'T
; 966	;	BEEN SET.
; 967	;62	FIX RDMTR CODE TO PUT 35 IN SC BEFORE GOING TO DMOVEM CODE.
; 968	;61	FIX PIIBP ROUTINE TO USE CALL.M INSTEAD OF SPEC/CALL,
; 969	;	WHICH GETS OVERRIDDEN BY P_P-S... IN MTR REQUEST SERVICE
; 970	;	ROUTINE, DON'T SET PI CYCLE UNLESS REQUEST IS FOR VECTOR.
; 971	;60	FIX DATAO PAG TO DO MB WAIT AFTER STORING EBOX ACCT AND; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-17
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 972	;	BEFORE CHANGING VMA.
; 973	;57	RE-CODE USES OF A@, B@ TO USE VMA/1, RATHER THAN VMA/AD,
; 974	;	IN ORDER TO GET CORRECT CONTEXT ON INDIRECT WORD. SEE MCL4
; 975	;56	FIX SECOND PART OF PICYCLE (TAG NEXT:) TO ENSURE THAT
; 976	;	PC+1 INH, KERNEL CYCLE, ETC REMAIN UP DURING 2ND PART.
; 977	;	ALSO CHANGE SPEC/FLAG CTL FOR ECO 1261, WHICH REQUIRES
; 978	;	#07 TO BE OPPOSITE OF #04 TO GENERATE SCD LEAVE USER.
; 979	;55	FIX SPEC INSTR/SET PI CYCLE TO INHIBIT INTERRUPTS
; 980	;	(IN PARTICULAR, METER UPDATE REQUESTS).  MAKE SURE VALID
; 981	;	DATA SAVED ON IO PAGE FAIL AND PARITY ERRORS. REMOVE
; 982	;	BACKWARDS BLT... IT BROKE TOO MANY PROGRAMS.
; 983	;54	FIX OVERFLOW CHECK IN IMULI OPTIMIZATION TO INH CRY 18
; 984	;	UPDATE TO USE CONDITIONAL ASSEMBLY IN MICRO VERS 20.
; 985	;53	FIX T1,T2 PARAMETERS ON BYTE DISP, SIGNS DISP
; 986	;52	CORRECT SHIFT AMOUNT FOR IMULI OPTIMIZATION, AND FIX MACRO
; 987	;	DEFINITIONS FOR SET SR?, WHICH WERE ALWAYS SETTING SR0.
; 988	;51	OPTIMIZE IMULI OF TWO POSITIVE OPERANDS (TO SPEED UP SUBSCRIPT
; 989	;	CALCULATIONS) BY TAKING ONLY 9 MULTIPLY STEPS AND STARTING
; 990	;	NEXT INSTRUCTION FETCH EARLIER.  OPTIMIZATION CAN BE REMOVED
; 991	;	BY COMMENTING OUT TWO INSTRUCTIONS AT IMULI, AND ONE FOLLOWING
; 992	;	IMUL.  ALSO FIX APRBI/UVERS TO KEEP SERIAL # OUT OF LH.
; 993	;50	INTRODUCE SKIP/FETCH AND CODE IN PAGE FAIL RECOVERY TO LOAD
; 994	;	PC FROM VMA IF PAGE FAIL OCCURED ON FETCH, BECAUSE NICOND
; 995	;	CYCLE, WHICH SHOULD HAVE LOADED PC, GETS INHIBITED BY INSTR 1777
; 996	;	ALSO INCLUDE EXTENDED INSTRUCTION SET.
; 997	;47	UNDO XCT CHANGES OF EDIT 46, WHICH BROKE XCT DUE TO INSUFFICIENT
; 998	;	TIME FOR DRAM HOLD BEFORE USING "A READ". ALSO FIX VECTOR
; 999	;	INTERRUPT CODE TO LOOK AT CORRECT BITS FOR CONTROLLER NUMBER.
; 1000	;46	FOLLOW-ON TO EDIT 45, SAVING 2 WORDS AND A CYCLE
; 1001	;	ALSO MOVE JRST TO 600, JFCL TO 700, UUO'S TO 100X AS PREPARATION
; 1002	;	FOR EXTENDED INSTRUCTION SET
; 1003	;45	FIX SXCT TO LOOK AT AC FIELD OF SXCT, NOT SUBJECT INSTRUCTION,
; 1004	;	WHEN DECIDING WHETHER TO USE BASE-TYPE ADDRESS CALCULATION.
; 1005	;44	FIX PAGE FAIL LOGIC TO WORK FOR EITHER PAGE FAIL OR PARITY
; 1006	;	ERROR.  EDITS 42 AND 43 BOTH WRONG.  ALSO CORRECT RACE IN
; 1007	;	WRITING PERFORMANCE ANALYSIS ENABLES TO PREVENT SPURIOUS COUNTS.
; 1008	;43	CORRECT USE OF PF DISP BY EDIT 42.  LOW BITS ARE INVERTED
; 1009	;42	FIX BUGS INTRODUCED BY EDIT 40, WHICH MADE FLTR OF 1B0 HANG
; 1010	;	TRYING TO NEGATE IT, AND FIX UP EXPONENT CORRECTION ON LONG
; 1011	;	SHIFT LEFT.  ALSO PUT IN CODE TO HANDLE PARITY ERROR PAGE
; 1012	;	FAILURES, AND SET TIME CONTROLS ON 43-47.
; 1013	;41	REWRITE OF VECTOR INTERRUPT PROCESSING TO MAKE DTE VECTORS
; 1014	;	GO TO 142+8N, WHERE N IS DTE#.  RH20 GO TO PROGRAMMED ADDRESS
; 1015	;	IN EPT, EXTERNAL DEVICES USE EXEC VIRTUAL ADDRESSES.
; 1016	;40	IMPROVEMENTS TO FLOATING NORMALIZATION TO MAKE LONG SHIFTS
; 1017	;	FASTER, PRIMARILY TO HELP FLTR
; 1018	;37	FIX FLOATING DIVIDE SO THAT THE TRUNCATED FORM OF A NEGATIVE
; 1019	;	QUOTIENT IS EQUAL TO THE HIGH-ORDER PART OF THE INFINITE-
; 1020	;	PRECISION QUOTIENT.  SEE COMMENTS IN THE CODE.  ALSO BUM
; 1021	;	A CYCLE OUT OF FLOATING DIVIDE BY STARTING THE NORMALIZE
; 1022	;	WHILE MOVING THE QUOTIENT INTO AR.
; 1023	;	SEVERAL CHANGES TO MAKE TRACKS FEATURE WORK
; 1024	;36	FIX CONO MTR TO PUT DATA ON BOTH HALVES, SO PI CAN SEE PIA
; 1025	;35	FIX CONI PI TO READ BACK WRITE EVEN PARITY ENABLES
; 1026	;34	FIX BLT USE OF SR, SO NO CORRECTION OF ARX NECESSARY
; 1027	;33	FIX PAGE TABLE REFERENCES TO FORCE UNPAGED REF.  FIX TRAP; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-18
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 1028	;	TO SET PC+1 INHIBIT.
; 1029	;32	CORRECT SETTING OF SC FOR SHIFTING METER COUNTERS, TO GET
; 1030	;	12 BITS UNUSED AT RIGHT WHEN IT GETS TO CORE.
; 1031	;31	RECODE ASH AND ASHC TO SAVE SPACE
; 1032	;30	FIX JFFO TO SHIFT AR CORRECTLY AT JFFO2.  BUM ADJSP TO USE
; 1033	;	STMAC FOR UPDATING PDL POINTER.
; 1034	;27	FIX CONI PAG TO READ EBUS.  CORRECT DEFINITIONS OF MBOX
; 1035	;	REGISTER FUNCTIONS, WHICH HAD BITS 0 AND 3 INVERTED.
; 1036	;26	FIX DEFINITIONS OF DIAG FUNC CONO MTR AND CONO TIM, WHICH
; 1037	;	WERE REVERSED
; 1038	;25	FIX DECODING OF PHYSICAL DEVICE NUMBER IN PI FUNCTION CODE
; 1039	;	AND RE-CODE JFCL FOR FEWER MICROWORDS
; 1040	;24	FIX JFFO TO SHIFT ON FIRST 6-BIT TEST STEP, AND JRSTF TO
; 1041	;	KEEP E AND XR DISTINCT.  ALSO SET LOAD-ENABLE BITS IN
; 1042	;	DATAI PAG, WORD.
; 1043	;23	FIX CONO PI, TO HOLD AR ONTO EBUS THRU REL EBUS, BECAUSE
; 1044	;	PI BOARD DELAYS CONO PI TO GET CONO SET EQUIVALENT.
; 1045	;22	MORE JFCL FIXES.  MUST USE FLAG CTL/JFCL WHILE CLEARING BITS,
; 1046	;	AS WELL AS WHILE TESTING THEM.  BUM A WORD OUT OF JFFO BY
; 1047	;	MAKING THE SIXBIT COUNT NEGATIVE.  CHANGES SO SHIFT SUBR
; 1048	;	RETURNS 2, BYTEA 1.  FIX SETMB TO STORE BACK AND FETCH.
; 1049	;21	RE-WRITE JFCL TO KEEP LOW OPCODE BITS OUT OF AR0-1, BECAUSE
; 1050	;	PC00 GETS PROPAGATED LEFT TO ADA -1 AND -2.
; 1051	;20	FIX BLT TO LOAD BR WITH SRC-DST ADDR
; 1052	;	ALSO SET TIME PARAMETERS ON CONDITIONAL FETCH FUNCTIONS
; 1053	;17	CHANGE SWEEP ONE PAGE TO PUT PAGE # IN E, RATHER THAN ADDR.
; 1054	;	ALSO CHANGE COND/FM WRITE TO MATCH ECO #1068.
; 1055	;16	FIX JUMP FETCH MACRO TO LOAD VMA FROM PC+1 (TEST SATISFIED
; 1056	;	OVERRIDES THIS TO HOLD VMA).  ALSO BUM ONE MICROWORD FROM MUUO.
; 1057	;15	INCLUDE PAGE FAIL DISP IN DISP/ FIELD
; 1058	;	ALSO MAKE MUUO STORE PROCESS CONTEXT WORD AT 426, AND SETUP
; 1059	;	PCS FROM PC EXTENSION, CWSX FROM SXCT
; 1060	;14	FIX DEFINITIONS OF SKIP/IO LEGAL, AC#0, SC0, EVEN PAR
; 1061	;	ALSO FIX DATAO PAG, TO SEND LH DATA ON BOTH HALVES OF EBUS
; 1062	;13	ALIGN SETEBR SO CALL TO SHIFT RETURNS CORRECTLY
; 1063	;12	MAKE SURE AD COPIES AR DURING DATAO, CONO, AND CLEAR AR AT
; 1064	;	SET DATAI TIME.
; 1065	;11	FIXES TO CONTINUE CODE SO CONSOLE WORKS, AND CORRECTIONS TO
; 1066	;	PROTECTED DEP/EXAM SO PROTECTION PROTECTS.
; 1067	;10	FIX A READ MACRO TO VMA/PC+1.  AD OVERRIDES UNLESS DRAM A=1
; 1068	;07	RE-WRITE OF PI CYCLE CODE TO RECOGNIZE NEW EBUS SPEC.
; 1069	;06	FIX DEFINITIONS OF SKIPS 40-57 BY COMPLEMENTING 3 LOW ORDER BITS
; 1070	;	FIX MULSUB TO CORRESPOND TO NEW CRA LOGIC
; 1071	;05	FIX EBUS CTL DEFINITIONS TO GET F01 CORRECT.  CORRECT FLAG CTL
; 1072	;	DEFINITIONS TO PREVENT LEAVE USER WHEN NOT WANTED, AND FIX
; 1073	;	JRST/JFCL TO HAVE FLAGS IN AR WHEN NEEDED.
; 1074	;04	FIX RETURNS FROM MULSUB, PUT BETTER COMMENTS ON SNORM CODE,
; 1075	;	IMPROVE SNORM ALGORITHM TO MINIMIZE WORST-CASE TIME.
; 1076	;03	FIX DISPATCH ADDRESS PROBLEMS, MOSTLY JRST/JFCL AND UUO'S.
; 1077	;02	CHANGES PER INSTRUCTION SET REVIEW -- DELETE USE OF BIT12 OF
; 1078	;	BYTE POINTERS, CHANGE BLT TO PUT FINAL SRC,DST ADDRESSES IN AC,
; 1079	;	MAKE TRUNCATE FORM FLOATING POINT REALLY TRUNCATE, ELIMINATE
; 1080	;	LOCAL JSYS SUPPORT, DELETE PXCT OPCODE (XCT W/ NON-ZERO AC IN
; 1081	;	EXEC MODE), LUUO'S GO TO 40/41 OF CURRENT SPACE.
; 1082	;01	UPDATES FOR .TITLE AND .TOC PSEUDO OPS,
; 1083	;	AND VARIOUS CHANGES FOR PROTO HARDWARE; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-19
; EDHIS.MIC[10,5351]	19:59 24-Jul-85			REVISION HISTORY					

; 1084	;00	CREATION, BASED ON BREADBOARD AS OF EDIT 66
						; 1085	.BIN
						; 1086	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS		

						; 1087	.TOC	"CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS"
; 1088	.NOBIN
; 1089	
; 1090	; [COST ESTIMATES IN BRACKETS INDICATE NUMBER OF ADDITIONAL
; 1091	; MICROINSTRUCTIONS REQUIRED BY TURNING ON THE FEATURE SWITCH]
; 1092	
; 1093	.DEFAULT/TRACKS=0	;1 ENABLES STORING PC AFTER EVERY INSTRUCTION,
; 1094				; & CREATES DATAI/O PI TO READ/SETUP PC BUFFER
; 1095				;ADDRESS. [COST = 21 WDS]
; 1096	
; 1097	.DEFAULT/OP.CNT=0	;1 ENABLES CODE TO BUILD A HISTOGRAM IN CORE
; 1098				; COUNTING USES OF EACH OPCODE IN USER & EXEC
; 1099	
; 1100	.DEFAULT/OP.TIME=0	;1 ENABLES CODE TO ACCUMULATE TIME SPENT BY
; 1101				; EACH OPCODE
; 1102	
; 1103	.DEFAULT/SO.CNT=0	;SECOND ORDER COUNTING IN 128K STARTING AT LOC
; 1104				; 400000 NOT DEBUGED [COST = 28 WDS]
; 1105	
; 1106	.DEFAULT/SO2.CNT=0	;SECOND ORDER COUNTING IN 128K STARTING AT LOC
; 1107				; PRESENTED AT START DOES ONE MORE ADD THAN
; 1108				; SO.CNT AND HENCE AN INSTRUCTION TAKES
; 1109				; 120 NS LONGER THAN SO.CNT [COST = 28 WDS]
; 1110	
; 1111	.DEFAULT/PAGCNT=0	;Enable code to count entries into the PFH and
; 1112				; number of DATAO PAGs with bit 2 set.  [Cost =
; 1113				; 6 words] [327]
; 1114	
; 1115	.DEFAULT/FPLONG=1	;1 ENABLES KA-STYLE DOUBLE PRECISION FLOATING
; 1116				;POINT INSTRUCTIONS: FADL, FSBL, FMPL, FDVL,
; 1117				; UFA, DFN. [COST = 49 WDS]
; 1118	
; 1119	.DEFAULT/MULTI=0	;1 IF MULTIPROCESSOR SYSTEM, TO SUPPRESS CACHE
; 1120				;ON UNPAGED REF'S.  PAGED REF'S ARE UP TO EXEC.
; 1121	
; 1122	.DEFAULT/KLPAGE=0	;1 ENABLES KL-MODE PAGING. [COST = 85 WDS]
; 1123	
; 1124	.DEFAULT/SHIFT.MUUO=0	;ENABLES A DIFFERENT MUUO FORMAT FOR MODEL A
; 1125				;THAT IS SLIGHTLY CLOSER TO THE XADDR FORMAT
; 1126				;EXPECTED TO BE USED IN CONJUNCTION WITH LONG.PC
; 1127				;BUT THEY DO NOT DEPEND ON EACH OTHER
; 1128	
; 1129	.DEFAULT/MODEL.B=0	;1 INDICATES EXTENDED ADDRESSING HARDWARE,
; 1130				;PRIMARILY 2K (RATHER THAN 1280) CONTROL RAM,
; 1131				;NEW MCL, CTL, AND APR BOARDS.
; 1132	
; 1133	.DEFAULT/BLT.PXCT=0	;1ENABLES SPECIAL BLT CODE FOR EXTENDED ADDRESSING
; 1134				;THIS IS SUPPOSED TO GO AWAY IN THE FUTURE
; 1135				;WHEN PXCT OF BLT IS NO LONGER USED BY TOPS-20
; 1136				;THIS SHOULD ONLY BE USED BY KLX XADDR MICROCODE
; 1137				;[COST 12 WORDS]
; 1138	
; 1139		.IF/KLPAGE
;;1140		.IFNOT/MODEL.B
;;1141		.SET/XADDR=0	;CAN'T DO EXTENDED ADDRESSING WITHOUT MODEL B
;;1142		.set/extexp=0	;No room in TOPS20 Model A machine for extended exp.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-1
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS		

; 1143	.ENDIF/MODEL.B
; 1144	
;;1145		.IFNOT/KLPAGE
;;1146	.SET/XADDR=0		;CAN'T HAVE EXTENDED ADDRESSING WITHOUT KL PAGE
; 1147		.ENDIF/KLPAGE
; 1148	
; 1149	.DEFAULT/IMULI.OPT=0	;1 ENABLES OPTIMIZATION OF IMULI TO TAKE ONLY
; 1150				;NINE MULTIPLY STEPS [COST = 3 WDS]
; 1151	
; 1152		.IF/MODEL.B	; [COST = 19 WDS]
; 1153	.SET/SXCT=0		;DONT NEED SXCT WITH EXTENDED ADDRESSING
; 1154				;CAN'T DO IT IN MODEL B HARDWARE
; 1155		.ENDIF/MODEL.B
; 1156	.DEFAULT/SXCT=0		;1 ENABLES SPECIAL XCT INSTR, WHICH ALLOWS
; 1157				; DIAGNOSTICS TO GENERATE LARGE ADDRESSES.
; 1158	
; 1159	
; 1160	.DEFAULT/SNORM.OPT=0	;1 ENABLES FASTER NORMALIZATION OF SINGLE-
; 1161				; PRECISION RESULTS WHICH HAVE SEVERE LOSS OF
; 1162				; SIGNIFICANCE [COST = 4 WDS]
; 1163	
;;1164	.IFNOT/MODEL.B
;;1165		.SET/PUSHM=0	;CODE ONLY WORKS FOR MODEL B
; 1166	.ENDIF/MODEL.B
; 1167	
; 1168	.DEFAULT/PUSHM=0	;ENABLES THE PUSHM AND POPM INSTRUCTIONS
; 1169				; [COST = ??? WDS]
; 1170	.DEFAULT/EXTEND=1	;1 ENABLES EXTENDED INSTRUCTION SET
; 1171				; [COST = 290 WDS]
; 1172	
; 1173	.DEFAULT/DBL.INT=1	;1 ENABLES DOUBLE INTEGER INSTRUCTIONS
; 1174				; [COST = 59 WDS]
; 1175	
; 1176	.DEFAULT/ADJBP=1	;1 ENABLES ADJUST BYTE POINTER
; 1177				; [COST = 24 WDS]
; 1178	
; 1179	.DEFAULT/RPW=1		;1 ENABLES READ-PAUSE-WRITE CYCLES FOR
; 1180				;NON-CACHED REFERENCES BY CERTAIN INSTRUCTIONS.
; 1181				; [COST = 0]
; 1182	
; 1183	.DEFAULT/WRTST=0	;1 ENABLES WRITE-TEST CYCLES AT AREAD TIME FOR
; 1184				;INSTRUCTIONS LIKE MOVEM AND SETZM.  [COST = 0]
; 1185	
; 1186	.DEFAULT/BACK.BLT=0	;1 ENABLES BLT TO DECREMENT ADDRESSES ON EACH
; 1187				;STEP IF E < RH(AC).  BREAKS MANY PROGRAMS.
; 1188				; [COST = 9 WDS]
; 1189	
;;1190	.IF/TRACKS		;SETUP CONTROL FOR COMMON CODE
;;1191		.SET/INSTR.STAT=1
; 1192	.ENDIF/TRACKS
; 1193	
;;1194	.IF/OP.CNT
;;1195		.SET/INSTR.STAT=1	;ENABLE COMMON CODE, ERROR IF TRACKS TOO
; 1196	.ENDIF/OP.CNT
; 1197	
;;1198	.IF/OP.TIME; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-2
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONDITIONAL ASSEMBLY VARIABLE DEFINITIONS		

;;1199		.SET/INSTR.STAT=1	;ERROR IF TRACKS OR OP.CNT ALSO SET
; 1200	.ENDIF/OP.TIME
; 1201	
;;1202	.IF/SO.CNT
;;1203		.SET/INSTR.STAT=1
; 1204	.ENDIF/SO.CNT
; 1205	
;;1206	.IF/SO2.CNT
;;1207		.SET/INSTR.STAT=1
; 1208	.ENDIF/SO2.CNT
; 1209	
; 1210	.DEFAULT/INSTR.STAT=0		;IF NO STATISTICS, TURN OFF COMMON CODE
; 1211	
;;1212	.IF/INSTR.STAT
;;1213		.SET/NONSTD=1		;STATISTICS CODE IS NONSTANDARD
;;1214		.SET/TRXDEF=1		;Make sure TRX registers get defined [327]
; 1215	.ENDIF/INSTR.STAT
; 1216	
;;1217	.IF/PAGCNT
;;1218		.SET/NONSTD=1		;All statistics are nonstandard
;;1219		.SET/TRXDEF=1		;We need the TRX registers
; 1220	.ENDIF/PAGCNT
; 1221	
; 1222	.DEFAULT/TRXDEF=0		;Normally no TRX registers needed
; 1223	
; 1224	.DEFAULT/LONG.PC=0		;LONG PC FORMAT [COST 9 WORDS 11 WORDS IF XADDR]
; 1225	
; 1226	.DEFAULT/EPT540=0		;PUT EPT AND UPT SECTION TABLES AT 540 IF ON
; 1227					;  440 IF OFF
; 1228	
; 1229	.DEFAULT/DIAG.INST=0		;UNSUPPORTED DIAGNOSTIC MICROCODE
; 1230	
;;1231	.IF/DIAG.INST
;;1232		.SET/NONSTD=1		;NONSTANDARD MICROCODE
; 1233	.ENDIF/DIAG.INST
; 1234	
; 1235	.DEFAULT/NONSTD=0		;NONSTANDARD MICROCODE IS NORMALLY OFF
; 1236	.DEFAULT/SMP=1			;[216]1 IF SYMMETRIC MULTIPROCESSOR 
; 1237					;SYSTEM.
; 1238					;TO ENABLE RPW ON DPB INSTRUCTION.
; 1239					;[COST=9 WORDS if not XADDR, more if XADDR]
; 1240	.DEFAULT/OWGBP=0		;[264]
; 1241	.DEFAULT/IPA20=0		;[264]
; 1242	.DEFAULT/NOCST=0		;[264]
; 1243	.DEFAULT/CST.WRITE=1		;[314] Enable CST writable bit
; 1244	.DEFAULT/BIG.PT=1		;[333][347] Special code for big page table and Keep bit
; 1245	.DEFAULT/DDT.BUG=0		;[346] If on, enable APRID hack to move bit 23
; 1246	.DEFAULT/GFTCNV=1		;[273] GFLOAT CONVERSION INST.
; 1247	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1248	.TOC	"HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS"
; 1249	
; 1250	;(1)	FIELD DEFINITIONS
; 1251	;	THESE OCCUR AT THE BEGINNING OF THE LISTING, IN THE SOURCE FILE
; 1252	; DEFINE.MIC (CONTROL AND DISPATCH RAM DEFINITIONS).
; 1253	; THEY HAVE THE FORM:
; 1254	;	SYMBOL/=<L:R>M,J
; 1255	;ANOTHER FORM ACCEPTED BY THE ASSEMBLER (FOR HISTORIC REASONS) IS:
; 1256	;	SYMBOL/=J,K,R,M		;THIS FORM HAS BEEN REMOVED FROM THIS CODE
; 1257	;	THE PARAMETER (J) IS MEANINGFUL ONLY WHEN "D" IS SPECIFIED
; 1258	; AS THE DEFAULT MECHANISM, AND IN THAT CASE, GIVES THE DEFAULT VALUE OF
; 1259	; THE FIELD IN OCTAL.
; 1260	;	THE PARAMETER (K) GIVES THE FIELD SIZE IN (DECIMAL) NUMBER
; 1261	; OF BITS. THIS IS USED ONLY IN THE OUTDATED FORMAT.
; 1262	;	THE PARAMETER (L) GIVES THE BIT POSITION OF THE LEFTMOST BIT
; 1263	;IN THE FIELD. THE SAME METHOD IS USED AS FOR (R) BELOW.
; 1264	;	THE PARAMETER (R) GIVES THE FIELD POSITION IN DECIMAL
; 1265	; AS THE BIT NUMBER OF THE RIGHTMOST BIT OF THE FIELD.  BITS ARE NUMBERED
; 1266	; FROM 0 ON THE LEFT.  NOTE THAT THE POSITION OF BITS IN THE MICROWORD
; 1267	; SHOWN IN THE LISTING BEARS NO RELATION TO THE ORDERING OF BITS IN THE
; 1268	; HARDWARE MICROWORD, WHERE FIELDS ARE OFTEN BROKEN UP AND SCATTERED.
; 1269	;	THE PARAMETER (M) IS OPTIONAL, AND SELECTS A DEFAULT
; 1270	; MECHANISM FOR THE FIELD.  THE LEGAL VALUES OF THIS PARAMETER ARE THE
; 1271	; CHARACTERS "D", "T", "P", OR "+".
; 1272	;	  "D" MEANS (J) IS THE DEFAULT VALUE OF THE FIELD IF NO EXPLICIT
; 1273	;	VALUE IS SPECIFIED.
; 1274	;	  "T" IS USED ON THE TIME FIELD TO SPECIFY THAT THE VALUE OF THE
; 1275	;	FIELD DEPENDS ON THE TIME PARAMETERS SELECTED FOR OTHER FIELDS.
; 1276	;	THE VALUE OF A FIELD WITH THIS SPECIFICATION DEFAULTS TO THE
; 1277	;	MAX OF <SUM OF THE T1 PARAMETERS DEFINED FOR FIELD/VALUES
; 1278	;	SPECIFIED IN THIS MICROINSTRUCTION>, <SUM OF THE T2 PARAMETERS
; 1279	;	FOR THIS MICROINSTRUCTION>, <J PARAMETER OF THIS FIELD>.
; 1280	;	WITHIN THE KL10 MICROCODE, T1 PARAMETERS ARE USED TO SPECIFY
; 1281	;	FUNCTIONS WHICH DEPEND ON THE ADDER SETUP TIME, AND T2 PARAMETERS
; 1282	;	ARE USED FOR FUNCTIONS WHICH REQUIRE ADDITIONAL TIME FOR CORRECT
; 1283	;	SELECTION OF THE NEXT MICROINSTRUCTION ADDRESS.
; 1284	;	  "P" IS USED ON THE PARITY FIELD TO SPECIFY THAT THE VALUE OF THE
; 1285	;	FIELD SHOULD DEFAULT SUCH THAT PARITY OF THE ENTIRE WORD
; 1286	;	IS ODD.  IF THIS OPTION IS SELECTED ON A FIELD WHOSE SIZE (K) IS
; 1287	;	ZERO, THE MICRO ASSEMBLER WILL ATTEMPT TO FIND A BIT SOMEWHERE
; 1288	;	IN THE WORD FOR WHICH NO VALUE IS SPECIFIED OR DEFAULTED.
; 1289	;	  "+" IS USED ON THE JUMP ADDRESS FIELD TO SPECIFY THAT THE DEFAULT
; 1290	;	JUMP ADDRESS IS THE ADDRESS OF THE NEXT INSTRUCTION ASSEMBLED (NOT,
; 1291	;	IN GENERAL, THE CURRENT LOCATION +1).
; 1292	;	IN GENERAL, A FIELD CORRESPONDS TO THE SET OF BITS WHICH PROVIDE
; 1293	; SELECT INPUTS FOR MIXERS OR DECODERS, OR CONTROLS FOR ALU'S.
; 1294	; EXAMPLES:
; 1295	;	AR/=<24:26>D,0	OR	AR/=0,3,26,D
; 1296	;	THE MICROCODE FIELD WHICH CONTROLS THE AR MIXER (AND THEREFORE
; 1297	; THE DATA TO BE LOADED INTO AR ON EACH EBOX CLOCK) IS THREE BITS WIDE
; 1298	; AND THE RIGHTMOST BIT IS SHOWN IN THE LISTING AS BIT 26 OF THE
; 1299	; MICROINSTRUCTION.  IF NO VALUE IS SPECIFICALLY REQUESTED FOR THE FIELD,
; 1300	; THE MICROASSEMBLER WILL ENSURE THAT THE FIELD IS 0.
; 1301	;	AD/=<12:17>	OR	AD/=0,6,17
; 1302	;	THE FIELD WHICH CONTROLS THE AD IS 6 BITS WIDE, ENDING ON
; 1303	; BIT 17.  THE FOURTH PARAMETER OF THE FIELD IS OMITTED, SO THE FIELD; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-1
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1304	; IS AVAILABLE TO THE MICROASSEMBLER (IF NO VALUE IS EXPLICITLY
; 1305	; CALLED OUT FOR THE FIELD) FOR MODIFICATION TO ENSURE ODD PARITY IN THE
; 1306	; ENTIRE WORD.
; 1307	;
; 1308	;(2)	VALUE DEFINITIONS
; 1309	;	FOLLOWING A FIELD DEFINITION, SYMBOLS MAY BE CREATED IN THAT
; 1310	; FIELD TO CORRESPOND TO VALUES OF THE FIELD.  THE FORM IS:
; 1311	;	SYMBOL=N,T1,T2
; 1312	;	"N" IS, IN OCTAL, THE VALUE OF SYMBOL WHEN USED IN THE FIELD.
; 1313	; T1 AND T2 ARE OPTIONAL, AND SPECIFY PARAMETERS IN THE TIME FIELD
; 1314	; CALCULATION FOR MICROINSTRUCTIONS IN WHICH THIS FIELD/SYMBOL IS USED.
; 1315	; THE MICROASSEMBLER COMPUTES THE SUMS OF ALL THE T1'S AND ALL THE T2'S
; 1316	; SPECIFIED FOR FIELD/SYMBOL SPECIFICATIONS IN A WORD, AND USES THE MAX
; 1317	; OF THE TWO SUMS AS THE DEFAULT VALUE FOR THE FIELD WHOSE DEFAULT
; 1318	; MECHANISM IS "T".  EXAMPLES:
; 1319	;	AD/=<12:17>	;FIELD DEFINITION IN WHICH FOLLOWING SYMBOLS EXIST
; 1320	;	XOR=31
; 1321	;	A+B=6,1
; 1322	;	HERE THE SYMBOLS "XOR" AND "A+B" ARE DEFINED FOR THE "AD" FIELD.
; 1323	; TO THE ASSEMBLER, THEREFORE, WRITING "AD/XOR" MEANS PUT THE VALUE 31
; 1324	; INTO THE 6-BIT FIELD ENDING ON BIT 17 OF THE MICROWORD.  THE SYMBOLS
; 1325	; ARE CHOSEN FOR MNEMONIC SIGNIFICANCE, OF COURSE, SO ONE READING
; 1326	; THE MICROCODE WOULD INTERPRET "AD/XOR" AS "THE OUTPUT OF AD SHALL BE THE
; 1327	; EXCLUSIVE OR OF ITS A AND B INPUTS".  SIMILIARLY, "AD/A+B" IS READ AS
; 1328	; "AD PRODUCES THE SUM OF A AND B".  THE SECOND PARAMETER IN THE DEFINITION
; 1329	; OF "A+B" IS A CONTROL TO THE MICRO ASSEMBLER'S TIME-FIELD CALCULATION,
; 1330	; WHICH TELLS THE ASSEMBLER THAT THIS OPERATION TAKES LONGER THAN THE
; 1331	; BASIC CYCLE, AND THEREFORE THAT THE TIME FIELD SHOULD BE INCREASED.
; 1332	;	AR/=<24:26>D,0	;FIELD DEFINITION FOR FOLLOWING SYMBOLS
; 1333	;	AR=0
; 1334	;	AD=2
; 1335	;	HERE THE SYMBOLS "AR" AND "AD" ARE DEFINED FOR THE FIELD NAMED
; 1336	; "AR", WHICH CONTROLS THE AR MIXER.  WE COULD WRITE AR/AR TO MEAN THAT
; 1337	; THE AR MIXER SELECT INPUTS WOULD BE 0, WHICH IN THE 
; 1338	; HARDWARE SELECTS THE AR OUTPUT FOR RECIRCULATION TO THE REGISTER.  IN
; 1339	; PRACTICE, HOWEVER, WE WANT THIS TO BE THE DEFAULT CASE, SO THAT AR
; 1340	; DOES NOT CHANGE UNLESS WE SPECIFICALLY REQUEST IT, SO THE FIELD
; 1341	; DEFINITION SPECIFIES 0 AS THE DEFAULT VALUE OF THE FIELD.  IF WE
; 1342	; WANT AR LOADED FROM THE AD OUTPUT, WE WRITE "AR/AD" TO SET THE
; 1343	; MIXER SELECTS TO PASS THE AD OUTPUT INTO THE AR.
; 1344	;
; 1345	;(3)	LABEL DEFINITIONS
; 1346	;	A MICRO INSTRUCTION MAY BE LABELLED BY A SYMBOL FOLLOWED BY COLON
; 1347	; PRECEDING THE MICROINSTRUCTION DEFINITION.  THE ADDRESS OF THE
; 1348	; MICROINSTRUCTION BECOMES THE VALUE OF THE SYMBOL IN THE FIELD NAMED "J".
; 1349	; EXAMPLE:
; 1350	;	FOO:	J/FOO
; 1351	;	THIS IS A MICROINSTRUCTION WHOSE "J" FIELD (JUMP ADDRESS) CONTAINS
; 1352	; THE VALUE "FOO".  IT ALSO DEFINES THE SYMBOL "FOO" TO BE THE ADDRESS
; 1353	; OF ITSELF.  THEREFORE, IF EXECUTED BY THE MICROPROCESSOR, IT WOULD
; 1354	; LOOP ON ITSELF.
; 1355	;
; 1356	;(4)	COMMENTS
; 1357	;	A SEMICOLON ANYWHERE ON A LINE CAUSES THE REST OF THE LINE
; 1358	; TO BE IGNORED BY THE ASSEMBLER.  THIS TEXT IS AN EXAMPLE OF COMMENTS.
; 1359	;; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-2
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1360	;(5)	MICROINSTRUCTION DEFINITION
; 1361	;	A WORD OF MICROCODE IS DEFINED BY SPECIFYING A FIELD NAME,
; 1362	; FOLLOWED BY SLASH (/), FOLLOWED BY A VALUE.  THE VALUE MAY BE A
; 1363	; SYMBOL DEFINED FOR THAT FIELD, AN OCTAL DIGIT STRING, OR A DECIMAL
; 1364	; DIGIT STRING (DISTINGUISHED BY THE FACT THAT IT CONTAINS "8" AND/OR
; 1365	; "9" AND/OR IS TERMINATED BY A PERIOD). SEVERAL FIELDS MAY BE SPECIFIED
; 1366	; IN ONE MICROINSTRUCTION BY SEPARATING FIELD/VALUE SPECIFICATIONS WITH
; 1367	; COMMAS.  EXAMPLE:
; 1368	;	ADB/BR,ADA/AR,AD/A+B,AR/AD
; 1369	;	THE FIELD NAMED "ADB" IS GIVEN THE VALUE NAMED "BR" (TO
; 1370	; CAUSE THE MIXER ON THE B SIDE OF AD TO SELECT BR), FIELD "ADA" HAS VALUE
; 1371	; "AR", FIELD "AD" HAS VALUE "A+B", AND FIELD "AR" HAS VALUE "AD".
; 1372	;
; 1373	;(6)	CONTINUATION
; 1374	;	THE DEFINITION OF A MICROINSTRUCTION MAY CONTINUED ONTO TWO OR
; 1375	; MORE LINES BY BREAKING IT AFTER ANY COMMA.  IN OTHER WORDS, IF THE
; 1376	; LAST NON-BLANK, NON-COMMENT CHARACTER ON A LINE IS A COMMA, THE
; 1377	; INSTRUCTION SPECIFICATION IS CONTINUED ON THE FOLLOWING LINE.
; 1378	; EXAMPLE:
; 1379	;	ADB/BR,ADA/AR,		;SELECT AR & BR AS AD INPUTS
; 1380	;		AD/A+B,AR/AD	;TAKE THE SUM INTO AR
; 1381	; BY CONVENTION, CONTINUATION LINES ARE INDENTED AN EXTRA TAB.
; 1382	;
; 1383	;(7)	MACROS
; 1384	;	A MACRO IS A SYMBOL WHOSE VALUE IS ONE OR MORE FIELD/VALUE
; 1385	; SPECIFICATIONS AND/OR MACROS.  A MACRO DEFINITION IS A LINE CONTAINING
; 1386	; THE MACRO NAME FOLLOWED BY A QUOTED STRING WHICH IS THE VALUE OF THE
; 1387	; MACRO.  EXAMPLE:
; 1388	;	AR_AR+BR	"ADB/BR,ADA/AR,AD/A+B,AR/AD"
; 1389	; THE APPEARANCE OF A MACRO IN A MICROINSTRUCTION DEFINITION IS EQUIVALENT
; 1390	; TO THE APPEARANCE OF ITS VALUE.  MACROS FOR VARIOUS FUNCTIONS
; 1391	; ARE DEFINED IN "MACRO.MIC".
; 1392	;
; 1393	;(8)	PSEUDO OPS
; 1394	;	THE MICRO ASSEMBLER HAS 10 PSEUDO-OPERATORS:
; 1395	;.DCODE AND .UCODE SELECT THE RAM INTO WHICH SUBSEQUENT MICROCODE WILL
; 1396	;BE LOADED, AND THEREFORE THE FIELD DEFINITIONS AND MACROS WHICH ARE
; 1397	;MEANINGFUL IN SUBSEQUENT MICROCODE
; 1398	;.TITLE DEFINES A STRING OF TEXT TO APPEAR IN THE PAGE HEADER, AND
; 1399	;.TOC DEFINES AN ENTRY FOR THE TABLE OF CONTENTS AT THE BEGINNING.
; 1400	;.SET DEFINES THE VALUE OF A CONDITIONAL ASSEMBLY PARAMETER,
; 1401	;.CHANGE REDEFINES A CONDITIONAL ASSEMBLY PARAMETER,
; 1402	;.DEFAULT ASSIGNS A VALUE TO AN UNDEFINED PARAMETER.
; 1403	;.IF ENABLES ASSEMBLY IF THE VALUE OF THE PARAMETER IS NOT ZERO,
; 1404	;.IFNOT ENABLES ASSEMBLY IF THE PARAMETER VALUE IS ZERO, AND
; 1405	;.ENDIF RE-ENABLES ASSEMBLY IF SUPPRESSED BY THE PARAMETER NAMED.
; 1406	;
; 1407	;(9)	LOCATION CONTROL
; 1408	;	A MICROINSTRUCTION "LABELLED" WITH A NUMBER IS ASSIGNED TO THAT
; 1409	; ADDRESS.
; 1410	;	THE CHARACTER "=" AT THE BEGINNING OF A LINE, FOLLOWED BY
; 1411	; A STRING OF 0'S, 1'S, AND/OR *'S, SPECIFIES A CONSTRAINT ON THE
; 1412	; ADDRESS OF FOLLOWING MICROINSTRUCTIONS.  THE NUMBER OF CHARACTERS
; 1413	; IN THE CONSTRAINT STRING (EXCLUDING THE "=") IS THE NUMBER OF LOW-ORDER
; 1414	; BITS CONSTRAINED IN THE ADDRESS.  THE MICROASSEMBLER ATTEMPTS TO FIND
; 1415	; AN UNUSED LOCATION WHOSE ADDRESS HAS 0 BITS IN THE POSITIONS; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-3
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			HOW TO READ THE MICROCODE -- FORMATS & CONSTRUCTS	

; 1416	; CORRESPONDING TO 0'S IN THE CONSTRAINT STRING AND 1 BITS WHERE THE
; 1417	; CONSTRAINT HAS 1'S.  ASTERISKS DENOTE "DON'T CARE" BIT POSITIONS.
; 1418	;	IF THERE ARE ANY 0'S IN THE CONSTRAINT STRING, THE CONSTRAINT
; 1419	; IMPLIES A BLOCK OF <2**N> MICROWORDS, WHERE N IS THE NUMBER OF 0'S
; 1420	; IN THE STRING.  ALL LOCATIONS IN THE BLOCK WILL HAVE 1'S IN THE ADDRESS
; 1421	; BITS CORRESPONDING TO 1'S IN THE STRING, AND BIT POSITIONS DENOTED BY *'S
; 1422	; WILL BE THE SAME IN ALL LOCATIONS OF THE BLOCK.
; 1423	;	IN SUCH A CONSTRAINT BLOCK, THE DEFAULT ADDRESS PROGRESSION IS
; 1424	; COUNTING IN THE "0" POSITIONS OF THE CONSTRAINT STRING, BUT A NEW
; 1425	; CONSTRAINT STRING OCCURING WITHIN A BLOCK MAY FORCE SKIPPING OVER
; 1426	; SOME LOCATIONS OF THE BLOCK.  WITHIN A BLOCK, A NEW CONSTRAINT
; 1427	; STRING DOES NOT CHANGE THE PATTERN OF DEFAULT ADDRESS PROGRESSION, IT
; 1428	; MERELY ADVANCES THE LOCATION COUNTER OVER THOSE LOCATIONS.  THE
; 1429	; MICROASSEMBLER WILL LATER FILL THEM IN.
; 1430	;	A NULL CONSTRAINT STRING ("=" FOLLOWED BY ANYTHING BUT "0",
; 1431	; "1", OR "*") SERVES TO TERMINATE A CONSTRAINT BLOCK.
; 1432	; EXAMPLES:
; 1433	;	=0	
; 1434	;	THIS SPECIFIES THAT THE LOW-ORDER ADDRESS BIT MUST BE ZERO--
; 1435	; THE MICROASSEMBLER FINDS AN EVEN-ODD PAIR OF LOCATIONS, AND PUTS
; 1436	; THE NEXT TWO MICROINSTRUCTIONS INTO THEM.
; 1437	;	=11
; 1438	;	THIS SPECIFIES THAT THE TWO LOW-ORDER BITS OF THE ADDRESS MUST
; 1439	; BOTH BE ONES.  SINCE THERE ARE NO 0'S IN THIS CONSTRAINT, THE
; 1440	; ASSEMBLER FINDS ONLY ONE LOCATION MEETING THE CONSTRAINT.
; 1441	;	=0*****
; 1442	;	THIS SPECIFIES AN ADDRESS IN WHICH THE "40" BIT IS ZERO.  DUE
; 1443	; TO THE IMPLEMENTATION OF THIS FEATURE IN THE ASSEMBLER,  THE DEFAULT
; 1444	; ADDRESS PROGRESSION APPLIES ONLY TO THE LOW-ORDER 5 BITS, SO THIS
; 1445	; CONSTRAINT FINDS ONE WORD IN WHICH THE "40" BIT IS ZERO, AND DOES
; 1446	; NOT ATTEMPT TO FIND ONE IN WHICH THAT BIT IS A ONE.
; 1447	;THIS LIMITATION HAS BEEN CHANGED WITH NEWER ASSEMBLER VERSIONS.
; 1448	;HOWEVER NONE OF THE LOCATIONS IN THE MICROCODE REQUIRE ANYTHING BUT THE
; 1449	;CONSTRAINT MENTIONED ABOVE.
; 1450	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			MICROCODE LISTING TEMPLATE				

; 1451	.TOC	"MICROCODE LISTING TEMPLATE"
; 1452	;HERE IS A TEMPLATE WHICH CAN BE USED WITH THE MICROCODE
; 1453	; LISTING TO IDENTIFY FIELDS IN THE OUTPUT --
; 1454	
; 1455	
; 1456	; ----  ---- ---- ---- ---- ---- ---- ----
; 1457	; [--]  [--] []!! !!!! !!!! !![] [][] ![-]
; 1458	;   !     !   !!! !!!! !!!! !! !  ! ! ! + # = MAGIC NUMBERS
; 1459	;   !     !   !!! !!!! !!!! !! !  ! ! + MARK = SCOPE SYNC
; 1460	;   !     !   !!! !!!! !!!! !! !  ! !
; 1461	;   !     !   !!! !!!! !!!! !! !  ! + CALL, DISP/SPEC = SPEC FUNCTIONS
; 1462	;   !     !   !!! !!!! !!!! !! !  + SKIP/COND = SPECIAL FUNCTIONS
; 1463	;   !     !   !!! !!!! !!!! !! !
; 1464	;   !     !   !!! !!!! !!!! !! + TIME, MEM = UINST TIME & MEM FUNCTION
; 1465	;   !     !   !!! !!!! !!!! !+ VMA = VMA INPUT SELECT
; 1466	;   !     !   !!! !!!! !!!! + SH/ARMM = SH FUNCTION / ARMM SELECT
; 1467	;   !     !   !!! !!!! !!!!
; 1468	;   !     !   !!! !!!! !!!+ SC, FE = SC INPUT SELECT & FE LOAD
; 1469	;   !     !   !!! !!!! !!+ SCADB = SELECT FOR SCAD "B" INPUT
; 1470	;   !     !   !!! !!!! !+ SCADA = ENABLE AND SELECT FOR SCAD "A" INPUT
; 1471	;   !     !   !!! !!!! + SCAD = SC/FE ADDER FUNCTION
; 1472	;   !     !   !!! !!!!
; 1473	;   !     !   !!! !!!+ FM ADR = FAST MEMORY ADDRESS SELECT
; 1474	;   !     !   !!! !!+ BR, BRX, MQ = LOAD BR & BRX, SEL FOR MQ
; 1475	;   !     !   !!! !+ ARX = SELECT FOR ARX INPUT
; 1476	;   !     !   !!! + AR = SELECT FOR AR INPUT
; 1477	;   !     !   !!!
; 1478	;   !     !   !!+ ADB = SELECT FOR ADDER "B" INPUT
; 1479	;   !     !   !+ ADA = SELECT AND ENABLE FOR ADDER "A" INPUT
; 1480	;   !     !   + AD = OPERATION IN ADDER AND ADDER EXTENSION
; 1481	;   !     !
; 1482	;   !     + J = BASE ADDRESS TO WHICH THIS MICROINSTRUCTION JUMPS
; 1483	;   !
; 1484	;   + LOCATION IN CRAM INTO WHICH THIS WORD IS LOADED
; 1485	;
; 1486	; U/V = MICRO INSTRUCTION FOR CRAM
; 1487	
; 1488	;*******************************************************************
; 1489	
; 1490	; D = WORD FOR DRAM
; 1491	;
; 1492	;   + LOCATION IN DRAM INTO WHICH THIS WORD IS LOADED
; 1493	;   !
; 1494	;   !   + A = OPERAND ACCESS CONTROL
; 1495	;   !   !+ B = INSTRUCTION "MODE"
; 1496	;   !   !! + P = PARITY FOR THIS WORD
; 1497	;   !   !! !
; 1498	;   !   !! !   + J = ADDRESS OF HANDLER FOR THIS INSTRUCTION
; 1499	; [--]  !! ! [--]
; 1500	; ----  ---- ----
; 1501	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			KL10 INSTRUCTION OPCODE MAP				

; 1502	.TOC	"KL10 INSTRUCTION OPCODE MAP"
; 1503	
; 1504	;	0	1	2	3	4	5	6	7
; 1505	;100	UUO	UUO	EFAD	EFSB	JSYS	ADJSP	EFMP	EFDV
; 1506	;110	DFAD	DFSB	DFMP	DFDV	DADD	DSUB	DMUL	DDIV
; 1507	;120	DMOVE	DMOVN	FIX	EXTEND	DMOVEM	DMOVNM	FIXR	FLTR
; 1508	;130	UFA	DFN	FSC	IBP	ILDB	LDB	IDPB	DPB
; 1509	;140	FAD	FADL	FADM	FADB	FADR	FADRI	FADRM	FADRB
; 1510	;150	FSB	FSBL	FSBM	FSBB	FSBR	FSBRI	FSBRM	FSBRB
; 1511	;160	FMP	FMPL	FMPM	FMPB	FMPR	FMPRI	FMPRM	FMPRB
; 1512	;170	FDV	FDVL	FDVM	FDVB	FDVR	FDVRI	FDVRM	FDVRB
; 1513	;	0	1	2	3	4	5	6	7
; 1514	;200	MOVE	MOVEI	MOVEM	MOVES	MOVS	MOVSI	MOVSM	MOVSS
; 1515	;210	MOVN	MOVNI	MOVNM	MOVNS	MOVM	MOVMI	MOVMM	MOVMS
; 1516	;220	IMUL	IMULI	IMULM	IMULB	MUL	MULI	MULM	MULB
; 1517	;230	IDIV	IDIVI	IDIVM	IDIVB	DIV	DIVI	DIVM	DIVB
; 1518	;240	ASH	ROT	LSH	JFFO	ASHC	ROTC	LSHC	UUO
; 1519	;250	EXCH	BLT	AOBJP	AOBJN	JRST	JFCL	XCT	MAP
; 1520	;260	PUSHJ	PUSH	POP	POPJ	JSR	JSP	JSA	JRA
; 1521	;270	ADD	ADDI	ADDM	ADDB	SUB	SUBI	SUBM	SUBB
; 1522	;	0	1	2	3	4	5	6	7
; 1523	;300	CAI	CAIL	CAIE	CAILE	CAIA	CAIGE	CAIN	CAIG
; 1524	;310	CAM	CAML	CAME	CAMLE	CAMA	CAMGE	CAMN	CAMG
; 1525	;320	JUMP	JUMPL	JUMPE	JUMPLE	JUMPA	JUMPGE	JUMPN	JUMPG
; 1526	;330	SKIP	SKIPL	SKIPE	SKIPLE	SKIPA	SKIPGE	SKIPN	SKIPG
; 1527	;340	AOJ	AOJL	AOJE	AOJLE	AOJA	AOJGE	AOJN	AOJG
; 1528	;350	AOS	AOSL	AOSE	AOSLE	AOSA	AOSGE	AOSN	AOSG
; 1529	;360	SOJ	SOJL	SOJE	SOJLE	SOJA	SOJGE	SOJN	SOJG
; 1530	;370	SOS	SOSL	SOSE	SOSLE	SOSA	SOSGE	SOSN	SOSG
; 1531	;	0	1	2	3	4	5	6	7
; 1532	;400	SETZ	SETZI	SETZM	SETZB	AND	ANDI	ANDM	ANDB
; 1533	;410	ANDCA	ANDCAI	ANDCAM	ANDCAB	SETM	SETMI	SETMM	SETMB
; 1534	;420	ANDCM	ANDCMI	ANDCMM	ANDCMB	SETA	SETAI	SETAM	SETAB
; 1535	;430	XOR	XORI	XORM	XORB	IOR	IORI	IORM	IORB
; 1536	;440	ANDCB	ANDCBI	ANDCBM	ANDCBB	EQV	EQVI	EQVM	EQVB
; 1537	;450	SETCA	SETCAI	SETCAM	SETCAB	ORCA	ORCAI	ORCAM	ORCAB
; 1538	;460	SETCM	SETCMI	SETCMM	SETCMB	ORCM	ORCMI	ORCMM	ORCMB
; 1539	;470	ORCB	ORCBI	ORCBM	ORCBB	SETO	SETOI	SETOM	SETOB
; 1540	;	0	1	2	3	4	5	6	7
; 1541	;500	HLL	HLLI	HLLM	HLLS	HRL	HRLI	HRLM	HRLS
; 1542	;510	HLLZ	HLLZI	HLLZM	HLLZS	HRLZ	HRLZI	HRLZM	HRLZS
; 1543	;520	HLLO	HLLOI	HLLOM	HLLOS	HRLO	HRLOI	HRLOM	HRLOS
; 1544	;530	HLLE	HLLEI	HLLEM	HLLES	HRLE	HRLEI	HRLEM	HRLES
; 1545	;540	HRR	HRRI	HRRM	HRRS	HLR	HLRI	HLRM	HLRS
; 1546	;550	HRRZ	HRRZI	HRRZM	HRRZS	HLRZ	HLRZI	HLRZM	HLRZS
; 1547	;560	HRRO	HRROI	HRROM	HRROS	HLRO	HLROI	HLROM	HLROS
; 1548	;570	HRRE	HRREI	HRREM	HRRES	HLRE	HLREI	HLREM	HLRES
; 1549	;	0	1	2	3	4	5	6	7
; 1550	;600	TRN	TLN	TRNE	TLNE	TRNA	TLNA	TRNN	TLNN
; 1551	;610	TDN	TSN	TDNE	TSNE	TDNA	TSNA	TDNN	TSNN
; 1552	;620	TRZ	TLZ	TRZE	TLZE	TRZA	TLZA	TRZN	TLZN
; 1553	;630	TDZ	TSZ	TDZE	TSZE	TDZA	TSZA	TDZN	TSZN
; 1554	;640	TRC	TLC	TRCE	TLCE	TRCA	TLCA	TRCN	TLCN
; 1555	;650	TDC	TSC	TDCE	TSCE	TDCA	TSCA	TDCN	TSCN
; 1556	;660	TRO	TLO	TROE	TLOE	TROA	TLOA	TRON	TLON
; 1557	;670	TDO	TSO	TDOE	TSOE	TDOA	TSOA	TDON	TSON; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- J, AD			

; 1558	.TOC	"CONTROL RAM DEFINITIONS -- J, AD"
; 1559	;FIELDS ARRANGED FOR READABILITY, NOT COMPACTNESS
; 1560	; IN THE PROCESSOR, BITS ARE SCATTERED IN ANOTHER ORDER
; 1561	
; 1562	U0/=<0:0>D,0	;BIT 0 UNUSED
; 1563	J/=<1:11>+	;SYMBOLS WILL BE DEFINED BY TAGS (CRA1&CRA2)
; 1564	
; 1565	;MAIN ADDER CONTROLS.  Bit 0 = carry in, bit 1 = boolean operation
; 1566	; Bits 2-5 are S8-S1 of the 10181 ALU chip.  For normal arithmetic,
; 1567	; the AD and ADX are separated unless SPEC/AD LONG or equivalent is given.
; 1568	
; 1569	
; 1570	AD/=<12:17>	; (EDP3, EXCEPT CARRY IN, ON CTL1)
; 1571		A+1=40,1
; 1572		A+XCRY=00,1
; 1573	;	A+ANDCB=01,1
; 1574	;	A+AND=02,1
; 1575		A*2=03,1
; 1576		A*2+1=43,1
; 1577	;	OR+1=44,1
; 1578	;	OR+ANDCB=05,1
; 1579		A+B=06,1
; 1580		A+B+1=46,1
; 1581	;	A+OR=07,1
; 1582		ORCB+1=50,1
; 1583		A-B-1=11,1
; 1584		A-B=51,1
; 1585	;	AND+ORCB=52,1
; 1586	;	A+ORCB=53,1
; 1587		XCRY-1=54,1
; 1588	;	ANDCB-1=15,1
; 1589	;	AND-1=16,1
; 1590		A-1=17,1
; 1591			;ADDER LOGICAL FUNCTIONS
; 1592		SETCA=20
; 1593		ORC=21		;NAND
; 1594		ORCA=22
; 1595		1S=23
; 1596		ANDC=24		;NOR
; 1597		NOR=24
; 1598		SETCB=25
; 1599		EQV=26
; 1600		ORCB=27
; 1601		ANDCA=30
; 1602		XOR=31
; 1603		B=32
; 1604		OR=33
; 1605		0S=34
; 1606		ANDCB=35
; 1607		AND=36
; 1608		A=37
; 1609			;BOOLEAN FUNCTIONS FOR WHICH CRY0 IS INTERESTING
; 1610		CRY A EQ -1=60,1	;GENERATE CRY0 IF A=1S, AD=SETCA
; 1611		CRY A.B#0=36,1		;CRY 0 IF A&B NON-ZERO, AD=AND
; 1612		CRY A#0=37,1		;GENERATE CRY0 IF A .NE. 0, AD=A
; 1613		CRY A GE B=71,1		;CRY0 IF A .GE. B, UNSIGNED; AD=XOR; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- DATA PATH MIXERS		

; 1614	.TOC	"CONTROL RAM DEFINITIONS -- DATA PATH MIXERS"
; 1615	
; 1616	ADA/=<18:20>		; (EDP3)
; 1617		AR=0
; 1618		ARX=1
; 1619		MQ=2
; 1620		PC=3
; 1621	ADA EN/=<18:18>		;ADA ENABLE ALSO ENABLES ADXA (EDP3)
; 1622		EN=0
; 1623		0S=1
; 1624	U21/=<21:21>D,0		;BIT 21 UNUSED
; 1625	ADB/=<22:23>		;CONTROLS ADB AND ADXB (EDP3)
; 1626		FM=0,,1		;MUST HAVE TIME FOR PARITY CHECK
; 1627		BR*2=1
; 1628		BR=2
; 1629		AR*4=3
; 1630	U23/=<23:23>D,1		;PREVENT DEFAULT SELECTION OF FM
; 1631				;FORCE IT TO TAKE ONE OF THE SHORTER
; 1632				;PATHS IF FM NOT NEEDED ALSO DISABLES
; 1633				;PARITY CHECKING LOGIC
; 1634	
; 1635	;REGISTER INPUTS
; 1636	
; 1637	AR/=<24:26>D,0		; (EDP1)
; 1638		AR=0
; 1639		ARMM=0		;REQUIRES SPECIAL FUNCTION
; 1640		MEM=0		;[346] MB WAIT will poke to 1 (CACHE) or 2 (AD)
; 1641		CACHE=1		;ORDINARILY SELECTED BY HWARE
; 1642		AD=2
; 1643		EBUS=3
; 1644		SH=4
; 1645		AD*2=5		;Low bit from ADX0
; 1646		ADX=6
; 1647		AD*.25=7
; 1648	ARX/=<27:29>D,0		; (EDP2)
; 1649		ARX=0		;[345] BY DEFAULT
; 1650		MEM=0		;[346] Gets poked by MB WAIT to 1 or 2
; 1651		CACHE=1		;ORDINARILY BY MBOX RESP
; 1652		AD=2
; 1653		MQ=3
; 1654		SH=4
; 1655		ADX*2=5		;Low bit from MQ0
; 1656		ADX=6
; 1657		ADX*.25=7
; 1658	BR/=<30:30>D,0		;DEFAULT TO RECIRCULATE (EDP4)
; 1659		AR=1
; 1660	BRX/=<31:31>D,0		;DEFAULT TO RECIRCULATE (EDP4)
; 1661		ARX=1
; 1662	MQ/=<32:32>D,0		;DEFAULT TO RECIRCULATE (EDP2)
; 1663		SH=1		;LOAD FROM SHIFT MATRIX
; 1664		MQ*2=0		;With SPEC/MQ SHIFT--Low bit from AD CRY -2
; 1665		MQ*.25=1	;With SPEC/MQ SHIFT--High bits from ADX34, ADX35
; 1666		MQ SEL=0	;WITH COND/REG CTL
; 1667		MQM SEL=1	;WITH COND/REG CTL
; 1668	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- DATA PATH MIXERS		

; 1669	;FMADR SELECTS THE SOURCE OF THE FAST MEMORY ADDRESS,
; 1670	; RATHER THAN PROVIDING THE ADDRESS ITSELF
; 1671	
; 1672	FMADR/=<33:35>		; (APR4&APR5)
; 1673		AC0=0		;IR 9-12
; 1674		AC1=1		;<IR 9-12>+1 MOD 16
; 1675		XR=2		;ARX 14-17
; 1676		VMA=3		;VMA 32-35
; 1677		AC2=4		;<IR 9-12>+2 MOD 16
; 1678		AC3=5		;<IR 9-12>+3 MOD 16
;;1679	.IFNOT/MODEL.B
;;1680		AC4=6		;CURRENT BLOCK, AC+4
;;1681		ac5=7		;current block, ac+5
; 1682	.IF/MODEL.B
; 1683		AC+#=6		;CURRENT BLOCK, AC+ MAGIC #
; 1684	.ENDIF/MODEL.B
; 1685		#B#=7		;BLOCK AND AC SELECTED BY # FIELD
; 1686	
; 1687	.TOC	"CONTROL RAM DEFINITIONS -- 10-BIT LOGIC"
; 1688	
; 1689	SCAD/=<36:38>		; (SCD1)
; 1690		A=0
; 1691		A-B-1=1
; 1692		A+B=2
; 1693		A-1=3
; 1694		A+1=4
; 1695		A-B=5
; 1696		OR=6
; 1697		AND=7
; 1698	SCADA/=<39:41>		; (SCD1)
; 1699		FE=0
; 1700		AR0-5=1		;BYTE POINTER P FIELD
; 1701		AR EXP=2	;<AR 01-08> XOR <AR 00>
; 1702		#=3		;SIGN EXTENDED WITH #00
; 1703	SCADA EN/=<39:39>	; (SCD1)
; 1704		0S=1
; 1705	U42/=<42:42>D,0	;BIT 42 UNUSED
; 1706	SCADB/=<43:44>		; (SCD1)
; 1707		SC=0
; 1708		AR6-11=1	;BYTE POINTER S FIELD
; 1709		AR0-8=2
; 1710		#=3		;NO SIGN EXTENSION
; 1711	U45/=<45:45>D,0		;BIT 45 UNUSED
; 1712	SC/=<46:46>D,0		;RECIRCULATE BY DEFAULT (SCD2)
; 1713		FE=0		;WITH SCM ALT
; 1714		SCAD=1
; 1715		AR SHIFT=1	;WITH SCM ALT ;AR 18, 28-35
; 1716	FE/=<47:47>D,0		;RECIRCULATE BY DEFAULT (SCD2)
; 1717		SCAD=1
; 1718	U48/=<48:48>D,0		;BIT 48 UNUSED
; 1719	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- SHIFT, ARMM, VMA, TIME	

; 1720	.TOC	"CONTROL RAM DEFINITIONS -- SHIFT, ARMM, VMA, TIME"
; 1721	
; 1722	SH/=<49:50>		; (SH1)
; 1723		SHIFT AR!ARX=0	;LEFT BY (SC)
; 1724		AR=1
; 1725		ARX=2
; 1726		AR SWAP=3	;HALVES SWAPPED
; 1727	ARMM/=<49:50>		;SAME BITS AS SH CONTROL (SCD3)
; 1728		#=0		;MAGIC # 0-8 TO AR 0-8
; 1729		EXP_SIGN=1	;AR1-8 _ AR0
; 1730		SCAD EXP=2	;AR0-8_SCAD
; 1731		SCAD POS=3	;AR0-5_SCAD
; 1732	.IF/MODEL.B
; 1733	VMAX/=<49:50>		;SAME BITS AS SH CONTROL (VMA4)
; 1734		VMAX=0		;VMA SECTION #
; 1735		PC SEC=1	;PC SECTION #
; 1736		PREV SEC=2	;PREVIOUS CONTEXT SECT
; 1737		AD12-17=3
; 1738	.ENDIF/MODEL.B
; 1739	U51/=<51:51>D,0		;BIT 51 UNUSED
; 1740	VMA/=<52:53>D,0		;ALSO CONTROLLED BY SPECIAL FUNCTIONS
; 1741		VMA=0		;BY DEFAULT
; 1742		PC=1		;MAY BE OVERRIDDEN BY MCL LOGIC	TO LOAD FROM AD
; 1743		LOAD=1		; IF WE KNOW IT WILL BE OVERRIDDEN, USE THIS
; 1744		PC+1=2
; 1745		AD=3		;ENTIRE VMA, INCLUDING SECTION
; 1746	TIME/=<54:55>T		;CONTROLS MINIMUM MICROINSTRUCTION EXECUTION
; 1747				; TIME, COUNTING MBOX CLOCK TICKS (CLK)
; 1748				;ASSEMBLER GENERALLY TAKES CARE OF THIS
; 1749		2T=0		;2 TICKS
; 1750		3T=1		;3 TICKS
; 1751		4T=2		;4 TICKS
; 1752		5T=3		;5 TICKS (COND/DIAG FUNC & #00, --> .5 USEC)
; 1753	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MEM SPECIAL FUNCTIONS	

; 1754	.TOC	"CONTROL RAM DEFINITIONS -- MEM SPECIAL FUNCTIONS"
; 1755	
; 1756	MEM/=<56:59>D,0		; (MCL1)
; 1757	;	NOP=0		;DEFAULT
; 1758		ARL IND=1	;CONTROL AR LEFT MUX FROM # FIELD
; 1759		MB WAIT=2	;WAIT FOR MBOX RESP IF PENDING
; 1760		A RD=4		;OPERAND READ and load PXCT bits (model B)
; 1761		B WRITE=5	;CONDITIONAL WRITE ON DRAM B 01
; 1762		FETCH=6		;LOAD NEXT INSTR TO ARX (CONTROL BY #)
; 1763		REG FUNC=7	;MBOX REGISTER FUNCTIONS
; 1764		LOAD AR=12
; 1765		LOAD ARX=13
; 1766		WRITE=16	;FROM AR TO MEMORY
; 1767	.IF/MODEL.B
; 1768		RESTORE VMA=3	;AD FUNC WITHOUT GENERATING A REQUEST
; 1769		AD FUNC=10	;FUNCTION LOADED FROM AD LEFT
; 1770		EA CALC=11	;FUNCTION DECODED FROM # FIELD
; 1771		RW=14		;READ, TEST WRITABILITY
; 1772		RPW=15		;READ-PAUSE-WRITE
; 1773		IFET=17		;UNCONDITIONAL FETCH
;;1774	.IFNOT/MODEL.B		;OLD-STYLE MCL BOARD
;;1775		SEC 0=3		;CLEAR VMAX
;;1776		A IND=10	;A-TYPE INDIRECT
;;1777		BYTE IND=11	;BYTE-TYPE INDIRECT
;;1778		AD FUNC=14	;FUNCTION FROM AD LEFT
;;1779		BYTE RD=15	;BYTE READ TO BOTH AR AND ARX
;;1780		RPW=17		;LOAD AR WITH RPW CYCLE
; 1781	.ENDIF/MODEL.B
; 1782	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS

; 1783	.TOC	"CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS"
; 1784	
; 1785	SKIP/=<60:65>D,0	;MICRO-PROGRAM SKIPS
; 1786				; 40-57 DECODED ON (CRA2)
; 1787	;	SPARE=40
; 1788		EVEN PAR=41,,1	;AR PARITY IS EVEN
; 1789		BR0=42		;BR BIT 00
; 1790		ARX0=43		;ARX BIT 00
; 1791		AR18=44		;AR BIT 18
; 1792		AR0=45		;AR BIT 00
; 1793		AC#0=46		;IR9-12 .EQ. 0
; 1794		SC0=47		;SC BIT 00
;;1795	.IFNOT/MODEL.B
;;1796		SC .LT. 36=50
; 1797	.IF/MODEL.B
; 1798		PC SEC0=50
; 1799	.ENDIF/MODEL.B
; 1800		SCAD0=51,,1	;SIGN OF SCAD OUTPUT
; 1801		SCAD#0=52,,1	;SCAD OUTPUT IS NON-ZERO
; 1802		ADX0=53,1	;ADDER EXTENSION BIT 00
; 1803		AD CRY0=54,1	;CARRY OUT OF AD BIT -2 (BOOLE IGNORED)
; 1804		AD0=55,1	;ADDER BIT 00
; 1805		AD#0=56,1	;AD BITS 00-35 CONTAIN SOME ONES
; 1806	.IF/MODEL.B
; 1807		-LOCAL AC ADDR=57	;VMA18-31 =0 ON LOCAL REF IN SEC >1
; 1808	.ENDIF/MODEL.B
; 1809				; 60-77 DECODED ON (CON2)
; 1810		FETCH=60	;VMA FETCH (LAST CYCLE WAS A FETCH)
; 1811		KERNEL=61	;PC IS IN KERNEL MODE
; 1812		USER=62		;PC IS IN USER MODE
; 1813		PUBLIC=63	;PC IS PUBLIC (INCLUDING SUPER)
; 1814		RPW REF=64	;MIDDLE OF READ-PAUSE-WRITE CYCLE
; 1815		PI CYCLE=65	;PI CYCLE IN PROGRESS
; 1816		-EBUS GRANT=66	;PI HASN'T RELEASED BUS FOR CPU USE
; 1817		-EBUS XFER=67	;NO TRANSFER RECIEVED FROM DEVICE
; 1818		INTRPT=70	;AN INTERRUPT REQUEST WAITING FOR SERVICE
; 1819		-START=71	;NO CONTINUE BUTTON
; 1820		RUN=72		;PROCESSOR NOT HALTED
; 1821		IO LEGAL=73	;KERNEL, PI CYCLE, USER IOT, OR DEVICE .GE. 740
; 1822		P!S XCT=74	;PXCT OR SXCT
; 1823	.IF/MODEL.B
; 1824		-VMA SEC0=75	;VMA SECTION NUMBER (13-17) IS NOT ZERO
; 1825	.ENDIF/MODEL.B
; 1826		AC REF=76,,1	;VMA .LT.20 ON READ OR WRITE
; 1827		-MTR REQ=77	;INTERRUPT REQUEST NOT DUE TO METER
; 1828	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- SKIP/COND SPECIAL FUNCTIONS

; 1829	;SKIP/COND FIELD CONTINUED
; 1830	
; 1831	COND/=<60:65>D,0	;NON-SKIP SPECIAL FUNCTIONS
; 1832				;0-7 DECODED ON (CTL2)
; 1833	;	NOP=0		;BY DEFAULT
; 1834		LD AR0-8=1
; 1835		LD AR9-17=2	;Gates VMAX into ARMM (see VMA4)
; 1836		LD AR18-35=3
; 1837		AR CLR=4
; 1838		ARX CLR=5
; 1839		ARL IND=6	;CONTROL AR LEFT, CALL, AND CLEAR BITS FROM #
; 1840		REG CTL=7	;CONTROL AR LOAD, EXP TST, AND MQ FROM #
; 1841				; 10-37 DECODED ON (CON1)
; 1842		FM WRITE=10	;WRITE AR INTO CURRENTLY ADDRESSED FM LOC
; 1843		PCF_#=11	;SET PC FLAGS FROM # FIELD
; 1844		FE SHRT=12	;SHIFT FE RIGHT 1
; 1845		AD FLAGS=13	;SET PC CRY0, CRY1, OVRFLO, TRAP1 AS APPROPRIATE
; 1846		LOAD IR=14	;LATCH AD OR CACHE DATA INTO IR, load PXCT bits
; 1847		SPEC INSTR=15	;SET/CLR SXCT, PXCT, PICYC, TRAP INSTR FLAGS
; 1848		SR_#=16		;CONTROL FOR STATE REGISTER and PXCT bits (CON3, MCL4)
; 1849		SEL VMA=17	;READ VMA THROUGH ADA/PC
; 1850		DIAG FUNC=20	;SELECT DIAGNOSTIC INFO ONTO EBUS
; 1851		EBOX STATE=21	;SET STATE FLOPS
; 1852		EBUS CTL=22	;I/O FUNCTIONS
; 1853		MBOX CTL=23
; 1854	;	SPARE=24
; 1855	.IF/MODEL.B
; 1856		LONG EN=25	;THIS WORD CAN BE INTERPRETED AS LONG INDIRECT
; 1857	.ENDIF/MODEL.B
; 1858	;	SPARE=26
; 1859	;	SPARE=27
; 1860		VMA_#=30
; 1861		VMA_#+TRAP=31
; 1862		VMA_#+MODE=32
; 1863		VMA_#+AR32-35=33
; 1864		VMA_#+PI*2=34
; 1865		VMA DEC=35	;VMA_VMA-1
; 1866		VMA INC=36	;VMA_VMA+1
; 1867		LD VMA HELD=37	;HOLD VMA ON SIDE
;;1868	.IFNOT/MODEL.B
;;1869	U66/=<66:66>D,0		;BIT 66 UNUSED
; 1870	.IF/MODEL.B
; 1871	CALL/=<66:66>D,0	;CALL FUNCTION
; 1872		CALL=1		;GOOD TO 15 LEVELS IN MODEL B
; 1873	.ENDIF/MODEL.B
; 1874	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- DISP/SPEC SPECIAL FUNCTIONS

; 1875	.TOC	"CONTROL RAM DEFINITIONS -- DISP/SPEC SPECIAL FUNCTIONS"
; 1876	
; 1877	DISP/=<67:71>D,10	;0-7 AND 30-37 ARE DISPATCHES (CRA1&CRA2)
; 1878		DIAG=0
; 1879		DRAM J=1
; 1880		DRAM A RD=2	;IMPLIES INH CRY18
; 1881		RETURN=3	;POPJ RETURN
; 1882		PG FAIL=4	;PAGE FAIL TYPE DISP
; 1883		SR=5		;16 WAYS ON STATE REGISTER
; 1884		NICOND=6	;NEXT INSTRUCTION CONDITION (see NEXT for detail)
; 1885		SH0-3=7,,1	;[337] 16 WAYS ON HIGH-ORDER BITS OF SHIFTER
; 1886		MUL=30		;FE0*4 + MQ34*2 + MQ35; implies MQ SHIFT, AD LONG
; 1887		DIV=31,,1	;FE0*4 + BR0*2 + AD CRY0; implies MQ SHIFT, AD LONG
; 1888		SIGNS=32,1	;ARX0*8 + AR0*4 + BR0*2 + AD0
; 1889		DRAM B=33	;8 WAYS ON DRAM B FIELD
; 1890		BYTE=34,,1	;FPD*4 + AR12*2 + SCAD0
; 1891		NORM=35,2	;See normalization for details. Implies AD LONG
; 1892		EA MOD=36	;(ARX0 or -LONG EN)*8 + -(LONG EN and ARX1)*4 +
; 1893				;ARX13*2 + (ARX2-5) or (ARX14-17) non zero; enable
; 1894				;is (ARX0 or -LONG EN) for second case.  If ARX18
; 1895				;is 0, clear AR left; otherwise, poke ARL select
; 1896				;to set bit 2 (usually gates AD left into ARL)
;;1897	.IFNOT/MODEL.B
;;1898		EA TYPE=37
; 1899	.ENDIF/MODEL.B
; 1900	
; 1901	SPEC/=<67:71>D,10	;NON-DISPATCH SPECIAL FUNCTIONS (CTL1)
; 1902	;	NOP=10		;DEFAULT
; 1903		INH CRY18=11
; 1904		MQ SHIFT=12	;ENABLE MQ*2, MQ SHRT2
; 1905		SCM ALT=13	;ENABLE FE, ARSHIFT
; 1906		CLR FPD=14
; 1907		LOAD PC=15
; 1908		XCRY AR0=16	;CARRY INTO AD IS XOR'D WITH AR00
; 1909		GEN CRY18=17
;;1910	.IFNOT/MODEL.B
;;1911		SEC HOLD=20	;INHIBIT LOADING VMAX
;;1912		CALL=21		;MAX DEPTH 4, INCLUDING PAGE REFILL
; 1913	.IF/MODEL.B
; 1914		STACK UPDATE=20	;CONTROL CRY18 IF LOCAL STACK
; 1915	.ENDIF/MODEL.B
; 1916		ARL IND=22	;# SPECIFIES ARL MIX, ENABLES, & CALL
; 1917		MTR CTL=23	;# CONTROLS METERS
; 1918		FLAG CTL=24	;FUNCTION ENCODED IN # FIELD
; 1919		SAVE FLAGS=25	;TELLS PI CYCLE TO HOLD INTRPT
; 1920		SP MEM CYCLE=26	;MEM REQUEST IS MODIFIED BY #
; 1921		AD LONG=27	;AD BECOMES 72 BIT ALU
; 1922	
; 1923	U73/=<72:73>D,0		;BITS 72-73 UNUSED
; 1924	
; 1925	MARK/=<74:74>D,0	;FIELD SERVICE "MARK" BIT
; 1926	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 1927	.TOC	"CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD"
; 1928	
; 1929	#/=<75:83>D,0		;THE INFAMOUS "MAGIC NUMBERS"
; 1930	
; 1931	MAJVER/=<75:80>		;[356] Major version number
; 1932	MINVER/=<81:83>		;[356] Minor version number
; 1933	
; 1934		;THE OPTIONS DESIGNATE CERTAIN ASSEMBLIES FROM THE SAME
; 1935			;MICROCODE SOURCES
; 1936			;# BIT 0 INDICATES KLPAGING
; 1937			;# BIT 1 INDICATES EXTENDED ADDRESSING
; 1938			;# BIT 2 INDICATES NONSTANDARD MICROCODE
; 1939			;# BIT 3 INDICATES A CPU WITH THE PV KIT. (MODEL B)
; 1940			;# BIT 8 INDICATES INSTRUCTION STATISTICS GATHERING
; 1941			;	(I.E. TRACKS)
; 1942		;EACH OPTION BIT IS GIVEN A SEPARATE FIELD DEFINITION
; 1943	
; 1944	KLPAGE/=<75:75>			;KLPAGING
; 1945	.IF/KLPAGE
; 1946		OPTIONS=1
;;1947	.IFNOT/KLPAGE
;;1948		OPTIONS=0
; 1949	.ENDIF/KLPAGE
; 1950	
; 1951	LONGPC/=<76:76>			;LONG PC FORMAT AS IN EXTENDED ADDRESSING
; 1952	.IF/LONG.PC			; THIS IS A SLIGHTLY BASTARDIZED FORMAT IN
; 1953		OPTIONS=1		; MODEL A MACHINES DUE TO SPACE LIMITATIONS
;;1954	.IFNOT/LONG.PC
;;1955		OPTIONS=0
; 1956	.ENDIF/LONG.PC
; 1957	
; 1958	NONSTD/=<77:77>			;NONSTANDARD MICROCODE (EG DIAGNOSTIC MICROCODE)
;;1959	.IF/NONSTD
;;1960		OPTIONS=1
; 1961	.IFNOT/NONSTD
; 1962		OPTIONS=0
; 1963	.ENDIF/NONSTD
; 1964	
; 1965	PV/=<78:78>			;MODEL B - PV CPU
; 1966	.IF/MODEL.B
; 1967		OPTIONS=1
;;1968	.IFNOT/MODEL.B
;;1969		OPTIONS=0
; 1970	.ENDIF/MODEL.B
; 1971	
; 1972	ISTAT/=<83:83>			;STATISTICS GATHERING CODE (IE TRACKS)
;;1973	.IF/INSTR.STAT
;;1974		OPTIONS=1
; 1975	.IFNOT/INSTR.STAT
; 1976		OPTIONS=0
; 1977	.ENDIF/INSTR.STAT
; 1978	
; 1979	ACB/=<77:79>		;AC block number. Used with FMADR/#B#
; 1980		PAGB=6		;AC block used for KL paging registers
; 1981		MICROB=7	;AC block for general microcode scratch
; 1982	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13-1
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 1983	AC#/=<80:83>		;AC number used with ACB or AC-OP (below)
; 1984	
; 1985	.IF/MODEL.B
; 1986	PXCT/=<75:77>		;(MCL4) Loaded by CON/SR_#, CON/LOAD IR, and MEM/A RD
; 1987				;Bit 0 enables the VMAX to not come from the AD when
; 1988				; VMA/AD (allowing local AC refs, for example).  Bits
; 1989				; 1 and 2 select which PXCT bits a memory reference
; 1990				; will select for possible previous context.
; 1991	;
; 1992	; WARNING !!! BECAUSE OF A TIMING PROBLEM IN THE HARDWARE ALL AC-OPS
; 1993	;		MUST HAVE THE NUMBER FIELD THE SAME IN THE PREVIOUS
; 1994	;		MICROINSTRUCTION. THE SYMPTOM WILL BE GARBAGE WRITTEN IN A
; 1995	;		DIFFERENT AC AS THE ADDRESS LINES DON'T MAKE IT IN TIME
; 1996	;		FOR THE WRITE PULSE.
; 1997	;
; 1998	AC-OP/=<75:79>		;CONTROLS OPERATION ON AC AND AC#
; 1999		AC+#=6
; 2000		#=32		;JUST AC#
; 2001		OR=33		;AC <OR> AC#
; 2002				;ALL AD/ FUNCTIONS <40 WORK
; 2003	.ENDIF/MODEL.B
; 2004	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 14
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2005	;VARIOUS SPECIAL FUNCTIONS ENABLE SPECIAL DECODING OF THE
; 2006	; "MAGIC #" FIELD, AS FOLLOWS:
; 2007	
; 2008	;SPECIAL DATA PATH CONTROLS
; 2009	
;;2010	.IFNOT/MODEL.B
;;2011	CALL/=<75:75>		;ENABLED BY ARL IND (CTL2)
;;2012		CALL=1
; 2013	.ENDIF/MODEL.B
; 2014	AR0-8/=<76:76>		;ENABLED BY ARL IND (CTL2)
; 2015		LOAD=1
; 2016	CLR/=<77:80>		;ENABLED BY ARL IND (CTL2)
; 2017		MQ=10
; 2018		ARX=4
; 2019		ARL=2
; 2020		ARR=1
; 2021		AR=3
; 2022		AR+ARX=7
; 2023		AR+MQ=13
; 2024		ARX+MQ=14
; 2025		AR+ARX+MQ=17
; 2026		ARL+ARX=6
; 2027		ARL+ARX+MQ=16
; 2028		ARR+MQ=11
; 2029	ARL/=<81:83>		;ENABLED BY ARL IND (CTL2)
; 2030		ARL=0
; 2031		ARMM=0		;REQUIRES SPECIAL FUNCTION
; 2032		CACHE=1		;ORDINARILY SELECTED BY HWARE
; 2033		AD=2
; 2034		EBUS=3
; 2035		SH=4
; 2036		AD*2=5
; 2037		ADX=6
; 2038		AD*.25=7
; 2039	AR CTL/=<75:77>		;ENABLED BY COND/REG CTL (CTL2)
; 2040		AR0-8 LOAD=4
; 2041		AR9-17 LOAD=2	;Gates VMAX into ARMM (see VMA4)
; 2042		ARR LOAD=1
; 2043		ARL LOAD=6
; 2044	EXP TST/=<80:80>	;ENABLED BY COND/REG CTL (CTL1)
; 2045		AR_EXP=1
; 2046	MQ CTL/=<82:83>		;ENABLED BY COND/REG CTL (CTL2)
; 2047	;	MQ=0		;WITH MQ/MQ SEL
; 2048		MQ*2=1		;WITH MQ/MQ SEL--Low bit is ADX0
; 2049	;	MQ*.5=2		; " (DROPS BITS 0,6,12,18,24,30)
; 2050		0S=3		; "
; 2051		SH=0		;WITH MQ/MQM SEL
; 2052		MQ*.25=1	;WITH MQ/MQM SEL--High bits are ADX34, ADX35
; 2053		1S=2		; "
; 2054		AD=3		; "
; 2055	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2056	;SPECIAL CONTROL OF EBOX FLAGS & FUNCTIONS
; 2057	
; 2058	PC FLAGS/=<75:83>	;ENABLED BY COND/PCF_# (SCD4)
; 2059		AROV=420	;SET ARITH OVFLO & TRAP1
; 2060		FLOV=620	;SAME, PLUS FLOATING OVFLO
; 2061		FPD=100		;SET FIRST PART DONE
; 2062		TRAP2=40	;SET TRAP2 (PDL OVFLO)
; 2063		TRAP1=20	;SET TRAP1 (ARITH OVFLO)
; 2064		FXU=630		;FLOV + EXP UNDERFLOW
; 2065		DIV CHK=424	;NO DIVIDE + AROV
; 2066		FDV CHK=624	;FLOATING NO DIVIDE
; 2067	FLAG CTL/=<75:83>	;ENABLED BY SPEC/FLAG CTL (SCD5)
; 2068		RSTR FLAGS=420	;AS IN JRSTF
; 2069		JFCL=602	;FORCE PC 00 = AROV
; 2070		JFCL+LD=622	;SECOND PART OF JFCL -- CLEAR TESTED FLAGS
; 2071		DISMISS=502	;CLEAR PI CYCLE IF SET (CON5)
; 2072				; ELSE DISMISS HIGHEST PI HOLD
; 2073		DISMISS+LD=522	;LOAD FLAGS AND DISMISS
; 2074		HALT=442	;STOP PROCESSOR IF LEGAL (CON2)
; 2075		SET FLAGS=20	;AS IN MUUO
; 2076		PORTAL=412	;CLEAR PUBLIC IF PRIVATE INSTR
; 2077	SPEC INSTR/=<75:83>	;ENABLED BY COND/SPEC INSTR
; 2078		SET PI CYCLE=714; (CON5)
; 2079		KERNEL CYCLE=200;MAKE IO LEGAL, EXEC ADDR SPACE (CON4)
; 2080		INH PC+1=100	;TO MAKE JSR WORK IN TRAP, INTRPT (CON4)
; 2081		SXCT=40		;START SECTION XCT (MCL4)
; 2082		PXCT=20		;START PREV CONTXT XCT (MCL4)
; 2083		INTRPT INH=10	;INHIBIT INTERRUPTS (CON4)
; 2084		INSTR ABORT=4	; (CON2)
; 2085		HALTED=302	;TELL CONSOLE WE'RE HALTED (CON4)
; 2086		CONS XCT=310	;FLAGS FOR INSTR XCT'D FROM CONSOLE
; 2087		CONT=0		;RESTORE NORMAL STATE FOR CONTINUE
; 2088	FETCH/=<75:83>		;ENABLED BY MEM/FETCH
; 2089		UNCOND=400
; 2090				;LOW 2 BITS DECODED ON (IR3)
; 2091		COMP=201,2	;DEPENDING ON AD AND DRAM B
; 2092		SKIP=202,2
; 2093		TEST=203,1
; 2094		JUMP=502,2	;AS IN JUMPX, ON AD AND DRAM B
; 2095		JFCL=503,1	;JUMP ON TEST CONDITION
; 2096	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2097	;SPECIAL MEMORY REQUEST FUNCTIONS
; 2098	
; 2099	.IF/MODEL.B
; 2100	EA CALC/=<75:83>	;SPECIFIC CONTROLS FOR MEM/EA CALC
; 2101	;	LOAD AR=400
; 2102	;	LOAD ARX=200
; 2103	;	PAUSE=100	;Freeze memory--always use with 040
; 2104	;	WRITE=040	;SET VMA WRITE
; 2105	;	PREV EN=20	;PREV CONTXT SELECTED BY SR AND PXCT
; 2106	;	INDIRECT=10	;PREV CONTXT FOR EA CALC
; 2107	;	EA=2		;RESTORATION OF ORIGINAL EA CONDITIONS
; 2108	;	STACK=1		;PREV CONTXT SELECTED BY PXCT B12
; 2109	.IF/XADDR		;JUST TO ARX FOR EXTENDED ADDRESSING
; 2110		A IND=230	;INDIRECT AT FIRST EA CALC TIME
;;2111	.IFNOT/XADDR		;TO BOTH AR AND ARX AS IN MODEL A
;;2112		A IND=630	;INDIRECT AT FIRST EA CALC TIME
; 2113	.ENDIF/XADDR
; 2114		BYTE LD=420	;Read byte data to AR only [337]
; 2115		BYTE RD=620	;READ BYTE DATA TO AR & ARX
; 2116		BYTE RD PC=621	;READ BYTE DATA TO AR & ARX WITH PC SECTION
; 2117		BYTE RPW=760	;Read byte data to AR, ARX, write test, pause [312]
; 2118		BYTE IND=610	;INDIRECT AT BYTE EA CALC TIME
; 2119		PUSH=041	;STORE TO STACK
; 2120		POP AR=421	;READ FROM STACK TO AR
; 2121		POP ARX=221	;READ FROM STACK TO ARX
; 2122		POP AR-ARX=621	;POP TO BOTH
; 2123		WRITE(E)=042
; 2124		LD AR(EA)=402	;LOAD AR GLOBAL/LOCAL AS IN EA
; 2125		LD AR+WR=440	;LOAD AR, TEST WRITABILITY
; 2126		LD ARX+WR=240	;LOAD ARX, TEST WRITABILITY
; 2127	.ENDIF/MODEL.B
; 2128	
; 2129	SP MEM/=<75:83>		;ENABLED BY SPEC/SP MEM CYCLE
; 2130		FETCH=400	;LOAD IR WHEN DATA ARRIVES (MCL5)
; 2131		USER=200	;FORCE USER OR UPT (MCL2)
; 2132		EXEC=100	;FORCE EXEC OR EPT (MCL3)
; 2133		SEC 0=40	;CLEAR VMAX (MCL4)
; 2134		UPT EN=20	;UPT IF USER EN (MCL3)
; 2135		EPT EN=10	;EPT IF NOT USER EN (MCL3)
; 2136		CACHE INH=2	; (MCL6)
; 2137		UNCSH+UNPAGE=103;UNCACHED AND UNPAGED
; 2138		UNPAGED+CACHED=101	;physical reference with cache enabled.
;;2139	.IFNOT/MULTI
;;2140		UNPAGED=101	; (MCL6)
;;2141		EPT=111
;;2142		EPT CACHE=111	;[260]
;;2143		EPT FETCH=511
;;2144		UPT=221
;;2145		UPT FETCH=621
;;2146		PT=31
;;2147		PT FETCH=431
; 2148	.IF/MULTI
; 2149		UNPAGED=103	; (MCL6)
; 2150		EPT=113
; 2151		EPT CACHE=111	;[260]
; 2152		EPT FETCH=513; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16-1
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2153		UPT=223
; 2154		UPT FETCH=623
; 2155		PT=33
; 2156		PT FETCH=433
; 2157	.ENDIF/MULTI
; 2158	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 17
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2159	;MBOX CONTROLS
; 2160	
; 2161	MREG FNC/=<75:83>	;ENABLED BY MEM/REG FUNC (APR6)
; 2162		SBUS DIAG=407	;PERFORM SBUS DIAGNOSTIC CYCLE
; 2163		READ UBR=502	;ASK MBOX TO LOAD UBR INTO EBUS REG
; 2164		READ EBR=503	;PUT EBR INTO EBUS REG
; 2165		READ ERA=504
; 2166		WR REFILL RAM=505	;DISGUISED AS A "READ REG" FUNCTION
; 2167	.IF/MODEL.B		;THIS GOT CHANGED IN THE GENERAL SPEEDUP (APR6)
; 2168		LOAD CCA=606	;START A SWEEP
;;2169	.IFNOT/MODEL.B		;HERE IS WHAT IT USED TO BE
;;2170		LOAD CCA=601	;START A SWEEP
; 2171	.ENDIF/MODEL.B
; 2172		LOAD UBR=602	;SETUP UBR FROM VMA
; 2173		LOAD EBR=603	;SETUP EBR FROM VMA
; 2174		MAP=140		;GET PHYS ADDR CORRESPONDING TO VMA (MCL6)
; 2175	MBOX CTL/=<75:83>	;ENABLED BY COND/MBOX CTL (APR5)
; 2176		SET PAGE FAIL=200
; 2177		SET IO PF ERR=100
; 2178		CLR PT LINE(NK)=61,,1;[333] Clear valid if no Keep bit set
; 2179		PT DIR CLR(NK)=41;Enable clear of PT DIR for non keep entries
; 2180		CLR PT LINE=31,,1;CLEAR VALID FOR 4 ENTRIES (new pager board) [342]
; 2181		PT DIR WR=20,1	;WRITE PAGE TABLE DIRECTORY
; 2182		PT WR=10,1	;WRITE PAGE TABLE ENTRY SELECTED BY VMA
; 2183		PT DIR CLR=1	;SELECT FOR CLEARING PT DIR (PAG3)
; 2184		NORMAL=0	;RESET PT WR SELECTION
; 2185	MTR CTL/=<81:83>	;FUNCTION DECODING FOR METERS (MTR3)
; 2186		CLR TIME=0		; USUALLY USED WITH DIAG FUNC
; 2187		CLR PERF=1
; 2188		CLR E CNT=2
; 2189		CLR M CNT=3
; 2190		LD PA LH=4
; 2191		LD PA RH=5
; 2192		CONO MTR=6
; 2193		CONO TIM=7
; 2194	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 18
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			CONTROL RAM DEFINITIONS -- MAGIC NUMBER FIELD		

; 2195	;I/O FUNCTIONS
; 2196	
; 2197	EBUS CTL/=<75:83>	;ENABLED BY COND/EBUS CTL (APR3)
; 2198		GRAB EEBUS=400	;"EBUS RETURN" TAKES ECL EBUS FOR EBOX
; 2199		REQ EBUS=200
; 2200		REL EBUS=100	; (CON3)
; 2201		EBUS DEMAND=60	;ASSERT DEMAND, KEEP CS, FUNC
; 2202		EBUS NODEMAND=20;DROP DEMAND, KEEP CS, FUNC
; 2203	;	CTL_IR=10	;SELECT F01 & F02 FROM IR
; 2204	;	DISABLE CS=4	;TURN OFF CONTROLLER SELECT
; 2205	;	DATAIO=2	;0 FOR CONI/O
; 2206	;	INPUT=1		;0 FOR OUTPUT
; 2207		IO INIT=30	;ENABLE IR3-9 TO EBUS CONTROLLER SELECT,
; 2208				; IR10-12 (DECODED) TO FUNCTION
; 2209				; AND AR ONTO EBUS IF FUNCTION IS OUTPUT
; 2210		DATAO=26	;0'S TO CS, DATAO TO FCN, AND AR TO EBUS
; 2211		DATAI=27	;0'S TO CS, DATAI TO FCN
; 2212		REL EEBUS=0	;LEGGO
; 2213	DIAG FUNC/=<75:83>	;ENABLED BY COND/DIAG FUNC (CTL3)
; 2214		.5 USEC=400,3		;STRETCH CLOCK TO LET EBUS SETTLE (CON?)
; 2215		LD PA LEFT=404,3	;LH PERF ANAL CONTROLS FROM RH (MTR)
; 2216		LD PA RIGHT=405,3	;RH PA CONTROLS FROM RH (MTR)
; 2217		CONO MTR=406,3		;ACCOUNTING CONTROLS (MTR)
; 2218		CONO TIM=407,3		;INTERVAL TIMER CONTROLS (MTR)
; 2219		CONO APR=414,3		; (CON3)
; 2220		CONO PI=415,3		; (CON3)
; 2221		CONO PAG=416,3		;CACHE & PAGING CTL (CON3)
; 2222		DATAO APR=417,3		;ADDRESS BREAK (CON3)
; 2223		DATAO PAG=620,3		;AC BLOCKS & PREV CONTXT (CON3)
; 2224		LD AC BLKS=425,3	;FORCE LOADING AC BLOCKS
; 2225		LD PCS+CWSX=426,3	;FORCE LOADING PREV CONTXT SEC, CWSX
; 2226		CONI PI(R)=500,3	;PI HOLD & ACTIVE TO LH (PI)
; 2227		CONI PI(L)=501,3	;PI GEN TO LH (PI)
; 2228		CONI APR(R)=510,3	;APR INTERRUPT & PIA TO LH (APR6)
; 2229		RD TIME=510,3		;TIME BASE TO RH (MTR5)
; 2230		DATAI PAG(L)=511,3	;AC BLOCKS, PREV CONTXT TO LH (APR6)
; 2231		RD PERF CNT=511,3	;PERFORMANCE COUNT TO RH (MTR5)
; 2232		CONI APR(L)=512,3	;APR INTERRUPT ENABLES TO LH (APR6)
; 2233		RD EBOX CNT=512,3	;EBOX COUNT TO RH (MTR5)
; 2234		DATAI APR=513,3		;ADDR BREAK CONDITIONS TO LH (APR6)
; 2235		RD CACHE CNT=513,3	;CACHE COUNT TO RH (MTR5)
; 2236		RD INTRVL=514,3		;INTERVAL TIMER TO RH (MTR5)
; 2237		RD PERIOD=515,3		;PERIOD REGISTER TO RH (MTR5)
; 2238		CONI MTR=516,3		;CONTROLS & PIA TO RH (MTR5)
; 2239		RD MTR REQ=517,3	;ENCODED UPDATE REQUEST TO 20-22 (MTR5)
; 2240		CONI PI(PAR)=530,3	;WRITE EVEN PARITY ENABLES TO RH (CON1)
; 2241		CONI PAG=531,3		;CACHE & TRAP CTL TO RH (CON1)
; 2242		RD EBUS REG=567,3	;EBUS REGISTER IN MBOX (MBZ1 & MBC1)
; 2243	
; 2244	PARITY/=0,0,0,P		;USE ANY AVAILABLE FIELD FOR PARITY
; 2245	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 19
; DEFINE.MIC[10,5351]	19:52 24-Jul-85			DISPATCH RAM DEFINITIONS				

; 2246	.TOC	"DISPATCH RAM DEFINITIONS"
; 2247	;FIELDS ARE ARRANGED FOR EASY READING, NOT COMPACTNESS
; 2248	
; 2249		.DCODE
; 2250	A/=<0:2>		;OPERAND FETCH MODE
; 2251		IMMED=0		;IMMEDIATE
; 2252		IMMED-PF=1	;IMMEDIATE, START PREFETCH
; 2253	.IF/MODEL.B
; 2254		ADDR=2		;FULL EFFECTIVE ADDRESS
; 2255	.ENDIF/MODEL.B
; 2256		WR-TST=3	;TEST WRITABILITY
; 2257		READ=4		;READ ONLY
; 2258		READ-PF=5	;READ, THEN PREFETCH
; 2259		RD-WR=6		;READ WRITE (SEPARATE CYCLES)
; 2260		RD-P-WR=7	;READ PAUSE WRITE
; 2261	
; 2262	B/=<3:5>		;STORE RESULTS AT--
; 2263		DBL AC=1	;DOUBLE RESULT TO AC & AC+1
; 2264		DBL BOTH=2	;MULB, DIVB, ETC
; 2265		SELF=3		;SELF MODE INSTRUCTIONS
; 2266		AC=5		;SINGLE RESULT TO AC, PREFETCH IN PROG
; 2267		MEM=6		;RESULT TO MEMORY
; 2268		BOTH=7		;SINGLE RESULT TO MEMORY AND AC
; 2269	
; 2270		SJC-=3		;SKIP JUMP COMPARE CONTROLS
; 2271		SJCL=2
; 2272		SJCE=1
; 2273		SJCLE=0
; 2274		SJCA=7
; 2275		SJCGE=6
; 2276		SJCN=5
; 2277		SJCG=4
; 2278	B0/=<3:3>		;INVERTS VARIOUS TEST, SKIP, AND JUMP CONTROLS
; 2279		CRY0(0)=0	;TEST TST CAUSES PC SKIP IF CRY0=0
; 2280		CRY0(1)=1	; SAME IF CRY0=1
; 2281	B1-2/=<4:5>		;FLOATING RESULT STORE MODE
; 2282		AC=1	;RESULT TO AC
; 2283		MEM=2	;RESULT JUST TO MEM
; 2284		BOTH=3	;RESULT TO BOTH
; 2285	
; 2286	PARITY/=<11:11>P
; 2287	
; 2288	J/=<14:23>		;EXECUTOR (40&20-BITS ALWAYS 0)
; 2289		.UCODE
; 2290	
						; 2291	.BIN
						; 2292	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--Miscellaneous and A			

						; 2293	.TOC	"CRAM Macros--Miscellaneous and A"
; 2294	.NOBIN
; 2295	;
; 2296	;	All the CRAM macros have been alphabetized for easy reference.  We have
; 2297	;	defined "_" to be alphabetically lower than the alphabet (although its
; 2298	;	ASCII representation makes it higher) so that macros such as AR_AR+1
; 2299	;	will precede ARX_AR+1, for example (this seems more intuitive).
; 2300	;
; 2301	[]_[]*[]	"@1/AD, ADA/@2, ADB/@3"
; 2302	[]_[]*FM[]	"@3, ADA/@2, ADB/FM, @1/AD"
; 2303	[]_[]-FM[]	"@3, ADA/@2, ADB/FM, @1/AD, AD/A-B"
; 2304	[]_#[]		"@1_#,#/@2"
; 2305	[]_ADA[]	"@1/AD, ADA/@2, AD/A"
; 2306	[]_ADB[]	"@1/AD, ADA EN/0S, ADB/@2, AD/B"
; 2307	[]_FM[]		"@1/AD, ADA EN/0S, ADB/FM, @2, AD/B"
; 2308	
; 2309	(AR+ARX+MQ)*.25	"ADA/AR,AD/A,AR/AD*.25,ARX/ADX*.25,(MQ)*.25"
; 2310	(AR+ARX+MQ)*2	"ADA/AR,AD/A,AR/AD*2,ARX/ADX*2,(MQ)*2"
; 2311	(MQ)*.25	"COND/REG CTL,MQ/MQM SEL,MQ CTL/MQ*.25"
; 2312	(MQ)*2		"COND/REG CTL,MQ/MQ SEL,MQ CTL/MQ*2"
; 2313	
; 2314	.IF/MODEL.B
; 2315	A INDRCT	"MEM/EA CALC,EA CALC/A IND,VMA/LOAD"
; 2316	A READ		"VMA/PC+1,DISP/DRAM A RD,MEM/A RD,#/300,J/0"
;;2317	.IFNOT/MODEL.B
;;2318	A INDRCT	"MEM/A IND,VMA/LOAD"
;;2319	A READ		"VMA/PC+1,DISP/DRAM A RD,MEM/A RD,#/0,J/0"
; 2320	.ENDIF/MODEL.B
; 2321	ABORT INSTR	"COND/SPEC INSTR,SPEC INSTR/INSTR ABORT"
; 2322	AC0		"FMADR/AC0"
; 2323	AC0_AR		"FMADR/AC0,COND/FM WRITE"
; 2324	AC1_AR		"FMADR/AC1,COND/FM WRITE"
; 2325	AC2_AR		"FMADR/AC2,COND/FM WRITE"
; 2326	AC3_AR		"FMADR/AC3,COND/FM WRITE"
; 2327	.IF/MODEL.B
; 2328	AC4		"FMADR/AC+#,AC-OP/AC+#,AC#/4"
;;2329	.IFNOT/MODEL.B
;;2330	AC4		"FMADR/AC4"
; 2331	.ENDIF/MODEL.B
; 2332	AC4_AR		"AC4,COND/FM WRITE"
; 2333	.IF/MODEL.B
; 2334	AC5		"FMADR/AC+#,AC-OP/AC+#,AC#/5"
;;2335	.IFNOT/MODEL.B
;;2336	AC5		"FMADR/AC5"
; 2337	.ENDIF/MODEL.B
; 2338	AC5_AR		"AC5,COND/FM WRITE"
; 2339	AD FLAGS	"COND/AD FLAGS"
; 2340	AD LONG		"SPEC/AD LONG"
; 2341	ADMSK		"R15"		;23 ONES
; 2342	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR						

; 2343	.TOC	"CRAM Macros--AR"
; 2344	
; 2345	AR_[] AND FM[]	"ADA/@1,ADB/FM,@2,AD/AND,AR/AD"
; 2346	AR_(AR+2BR)*.25	"ADA/AR,ADB/BR*2,AD/A+B,AR/AD*.25"
; 2347	AR_(AR+BR)*.25	"ADA/AR,ADB/BR,AD/A+B,AR/AD*.25"
; 2348	AR_(AR-2BR)*.25	"ADA/AR,ADB/BR*2,AD/A-B,AR/AD*.25"
; 2349	AR_(AR-BR)*.25	"ADA/AR,ADB/BR,AD/A-B,AR/AD*.25"
; 2350	AR_(ARX OR AR*4)*.25	"ADA/ARX,ADB/AR*4,AD/OR,AR/AD*.25"
; 2351	AR_-AC0		"FMADR/AC0,ADB/FM,ADA EN/0S,AD/A-B,AR/AD"
; 2352	AR_-AR		"ADA EN/0S,ADB/AR*4,AD/A-B,AR/AD*.25"
; 2353	AR_-AR LONG	"GEN -AR LONG,AR_AD*.25 LONG"
; 2354	AR_-BR		"ADB/BR,ADA EN/0S,AD/A-B,AR/AD"
; 2355	AR_-BR LONG	"ADA EN/0S,ADB/BR,AD/A-B,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2356	AR_-BR*2 LONG	"ADA EN/0S,ADB/BR*2,AD/A-B,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2357	AR_-BRX		"ADB/BR,ADA EN/0S,AD/A-B,AR/ADX"
; 2358	AR_-DLEN	"DLEN,ADB/FM,ADA EN/0S,AD/A-B,AR/AD"
; 2359	AR_-FM[]	"ADA EN/0S,ADB/FM,@1,AD/A-B,AR/AD"
; 2360	AR_-SLEN	"SLEN,ADB/FM,ADA EN/0S,AD/A-B,AR/AD"
; 2361	AR_0.C		"COND/ARL IND,CLR/AR"
; 2362	AR_0.M		"MEM/ARL IND,CLR/AR"
; 2363	AR_0.S		"SPEC/ARL IND,CLR/AR"
; 2364	AR_0S		"AD/0S,AR/AD"
; 2365	AR_1		"ADA EN/0S,AD/A+1,AR/AD"
; 2366	AR_1 LONG	"ADA EN/0S,AD/A+1,AR/AD*.25,ARX/ADX"
; 2367	AR_1S		"AD/1S,AR/AD"
; 2368	AR_2		"ADA EN/0S,AD/A+1,AR/AD*2"
; 2369	AR_2(AR*BR)	"ADA/AR,ADB/BR,AR/AD*2"
; 2370	AR_2(AR+1)	"ADA/AR,AD/A+1,AR/AD*2"
; 2371	AR_2(AR+BR)	"AR_2(AR*BR),AD/A+B"
; 2372	AR_2(AR+BR) LONG "AR_2(AR*BR),AD/A+B,ARX/ADX*2,SPEC/AD LONG"
; 2373	AR_2(AR-BR)	"AR_2(AR*BR),AD/A-B"
; 2374	
; 2375	AR_AC0		"FMADR/AC0,ADB/FM,AD/B,AR/AD"
; 2376	AR_AC0 COMP	"FMADR/AC0,ADB/FM,AD/SETCB,AR/AD"
; 2377	AR_AC0+1	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A+B+1,AR/AD"
; 2378	AR_AC1		"FMADR/AC1,ADB/FM,AD/B,AR/AD"
; 2379	AR_AC1 COMP	"FMADR/AC1,ADB/FM,AD/SETCB,AR/AD"
; 2380	AR_AC1*2	"FMADR/AC1,ADB/FM,AD/B,AR/AD*2"
; 2381	AR_AC2		"FMADR/AC2,ADB/FM,AD/B,AR/AD"
; 2382	AR_AC3		"FMADR/AC3,ADB/FM,AD/B,AR/AD"
; 2383	AR_AC3*2	"FMADR/AC3,ADB/FM,AD/B,AR/AD*2"
; 2384	AR_AC4		"AC4,ADB/FM,AD/B,AR/AD"
; 2385	AR_AD*.25 LONG	"AR/AD*.25,ARX/ADX*.25,SPEC/AD LONG"
; 2386	AR_ADMSK AND VMA HELD	"COND/SEL VMA,ADA/PC,ADB/FM,ADMSK,AD/AND,AR/AD"
; 2387	
; 2388	AR_AR AND ADMSK	 "ADMSK,ADB/FM,ADA/AR,AD/AND,AR/AD"
; 2389	.IF/KLPAGE
; 2390	AR_AR AND CSMSK	"CSMSK,ADB/FM,ADA/AR,AD/AND,AR/AD"
; 2391	AR_AR OR PUR	"PUR,ADB/FM,ADA/AR,AD/OR,AR/AD"
; 2392	.ENDIF/KLPAGE
; 2393	AR_AR SWAP	"SH/AR SWAP,AR/SH"
; 2394	
; 2395	AR_AR*.25	"ADA/AR,AD/A,AR/AD*.25"
; 2396	AR_AR*.25 LONG	"ADA/AR,AD/A,AR/AD*.25,ARX/ADX*.25"
; 2397	AR_AR*.5	"ADA/AR,AD/A*2,AR/AD*.25"
; 2398	AR_AR*.5 LONG	"ADA/AR,AD/A*2,SPEC/AD LONG,AR/AD*.25,ARX/ADX*.25"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR						

; 2399	AR_AR*1.25 LONG	"ADA/AR,ADB/AR*4,AD/A+B,AR_AD*.25 LONG"
; 2400	AR_AR*10	"ADA/AR,ADB/AR*4,AD/A+B,AR/AD*2"
; 2401	AR_AR*10 LONG	"ADA/AR,ADB/AR*4,AD/A+B,AR/AD*2,ARX/ADX*2,SPEC/AD LONG"
; 2402	AR_AR*2		"ADA/AR,AD/A,AR/AD*2"
; 2403	AR_AR*2 LONG	"ADA/AR,AD/A,AR/AD*2,ARX/ADX*2"
; 2404	AR_AR*4		"ADB/AR*4,AD/B,AR/AD"
; 2405	AR_AR*4 LONG	"ADB/AR*4,AD/B,AR/AD,ARX/ADX"
; 2406	AR_AR*5 LONG	"ADA/AR,ADB/AR*4,AD/A+B,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2407	AR_AR*8		"ADB/AR*4,AD/B,AR/AD*2"
; 2408	AR_AR*8 LONG	"ADB/AR*4,AD/B,AR/AD*2,ARX/ADX*2"
; 2409	AR_AR*AC0	"FMADR/AC0,ADB/FM,ADA/AR,AR/AD"	;GENERAL BINARY OPERATION
; 2410	AR_AR*AC1	"FMADR/AC1,ADB/FM,ADA/AR,AR/AD"
; 2411	AR_AR*BR	"ADA/AR,ADB/BR,AR/AD"
; 2412	AR_AR*EXPMSK	"EXPMSK,ADB/FM,ADA/AR,AR/AD"	;[224]
; 2413	AR_AR*MSK	"MSK,ADB/FM,ADA/AR,AR/AD"
; 2414	AR_AR*SFLGS	"SFLGS,ADB/FM,ADA/AR,AR/AD"
; 2415	AR_AR*SLEN	"SLEN,ADB/FM,ADA/AR,AR/AD"
; 2416	AR_AR*T0	"T0,ADB/FM,ADA/AR,AR/AD"
; 2417	
; 2418	AR_AR+1		"ADA/AR,AD/A+1,AR/AD"
; 2419	AR_AR+1 LONG	"AR_AR+1,ARX/ADX,SPEC/AD LONG"
; 2420	AR_AR+1-AR0	"ADA/AR,AD/A+1,AR/AD,SPEC/XCRY AR0"
; 2421	AR_AR+BR	"ADA/AR,ADB/BR,AD/A+B,AR/AD"
; 2422	AR_AR+BR LONG	"AR_AR+BR,ARX/ADX,SPEC/AD LONG"
; 2423	AR_AR+E1	"E1,ADB/FM,ADA/AR,AD/A+B,AR/AD"
; 2424	AR_AR+FM[]	"ADA/AR,ADB/FM,@1,AD/A+B,AR/AD";[343]
; 2425	.IF/KLPAGE
; 2426	AR_AR+SBR	"SBR,ADB/FM,ADA/AR,AD/A+B,AR/AD"
; 2427	.ENDIF/KLPAGE
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
; 2448	.IF/KLPAGE
; 2449	AR_ARX OR PUR	"PUR,ADB/FM,ADA/ARX,AD/OR,AR/AD"
; 2450	.ENDIF/KLPAGE
; 2451	AR_ARX*.25	"ADA/ARX,AD/A,AR/AD*.25"
; 2452	AR_ARX*.25-AR-1	"ADB/AR*4,ADA/ARX,AD/A-B-1,AR/AD*.25"
; 2453	AR_ARX*2	"ADA/ARX,AD/A,AR/AD*2"
; 2454	AR_ARX*4	"ADB/AR*4,AD/B,AR/ADX"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-2
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR						

; 2455	AR_ARX*4 COMP	"ADB/AR*4,AD/SETCB,AR/ADX"
; 2456	AR_ARX*AC1	"FMADR/AC1,ADB/FM,ADA/ARX,AR/AD"
; 2457	AR_ARX*BR	"ADA/ARX,ADB/BR,AR/AD"
; 2458	AR_ARX*BRX	"ADA/AR,ADB/BR,AR/ADX"
; 2459	AR_ARX*E1	"E1,ADB/FM,ADA/ARX,AR/AD"
; 2460	AR_ARX+1	"ADA EN/EN,AD/A+1,AR/ADX"
; 2461	AR_ARX+1 (AD)	"ADA/ARX,AD/A+1,AR/AD"
; 2462	AR_ARX+AC0	"ADA/ARX,ADB/FM,FMADR/AC0,AD/A+B,AR/AD"
; 2463	AR_ARX+AR*4	"ADA/ARX,ADB/AR*4,AD/A+B,AR/AD"
; 2464	AR_ARX+BR	"ADA/ARX,ADB/BR,AD/A+B,AR/AD"
; 2465	AR_ARX+BRX	"ADA EN/EN,ADB/BR,AD/A+B,AR/ADX"
; 2466	AR_ARX+BRX+1	"ADA EN/EN,ADB/BR,AD/A+B+1,AR/ADX"	;[343]
; 2467	AR_ARX+FM[]	"ADA/ARX,ADB/FM,@1,AD/A+B,AR/AD"
; 2468	AR_ARX+XR	"GEN ARX+XR,AR/AD"
; 2469	AR_ARX-1	"ADA EN/EN,AD/A-1,AR/ADX"
; 2470	AR_ARX-AC3	"ADA/ARX,ADB/FM,FMADR/AC3,AD/A-B,AR/AD"
; 2471	AR_ARX-BR	"ADA/ARX,ADB/BR,AD/A-B,AR/AD"
; 2472	
; 2473	AR_BR		"ADB/BR,AD/B,AR/AD"
; 2474	AR_BR COMP	"ADB/BR,AD/SETCB,AR/AD"
; 2475	AR_BR COMP LONG	"ADB/BR,AD/SETCB,AR/AD,ARX/ADX"
; 2476	AR_BR LONG	"ADB/BR,AD/B,AR/AD,ARX/ADX"
; 2477	AR_BR OR ARX	"ADA/ARX,ADB/BR,AD/OR,AR/AD"
; 2478	AR_BR*.5	"ADB/BR*2,AD/B,AR/AD*.25"
; 2479	AR_BR*.5 LONG	"ADB/BR*2,AD/B,AR/AD*.25,ARX/ADX*.25"
; 2480	AR_BR*2		"ADB/BR*2,AD/B,AR/AD"
; 2481	AR_BR*2 LONG	"ADB/BR*2,AD/B,AR/AD,ARX/ADX"
; 2482	AR_BR*4		"ADB/BR*2,AD/B,AR/AD*2"			;[230]
; 2483	AR_BR*4 LONG	"ADB/BR*2,AD/B,AR/AD*2,ARX/ADX*2"
; 2484	AR_BR+1		"ADB/BR,ADA EN/0S,AD/A+B+1,AR/AD"
; 2485	AR_BR+1 LONG	"ADA EN/0S,ADB/BR,AD/A+B+1,AR/AD,ARX/ADX,SPEC/AD LONG"
; 2486	AR_BRX		"ADB/BR,AD/B,AR/ADX"
; 2487	AR_BRX+1	"ADA EN/0S,ADB/BR,AD/A+B+1,AR/ADX"
; 2488	
; 2489	AR_CACHE CNT	"DIAG IN,DIAG FUNC/RD CACHE CNT"
; 2490	AR_DLEN		"DLEN,AR_FM"
; 2491	AR_DLEN COMP	"DLEN,ADB/FM,AD/SETCB,AR/AD"
; 2492	AR_DLEN+1	"DLEN,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2493	AR_DSTP		"DSTP,AR_FM"
; 2494	AR_DSTP+1	"DSTP,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2495	AR_DSTP2	"DSTP2,AR_FM"
; 2496	AR_DSTP2+1	"DSTP2,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2497	AR_DSTW		"DSTW,AR_FM"
; 2498	AR_E0		"E0,AR_FM"
; 2499	AR_E1		"E1,AR_FM"
; 2500	AR_EBOX CNT	"DIAG IN,DIAG FUNC/RD EBOX CNT"
; 2501	AR_EBUS		"AR/EBUS,TIME/5T"
; 2502	AR_EBUS REG	"DIAG IN,DIAG FUNC/RD EBUS REG"
; 2503	AR_FILL		"FILL,AR_FM"
; 2504	AR_FM		"ADB/FM,AD/B,AR/AD"
; 2505	AR_FM[]		"AR/AD, AD/B, ADB/FM, @1"	;[274]
; 2506	AR_FM[]+1	"ADA EN/0S,ADB/FM,@1,AD/A+B+1,AR/AD"
; 2507	.IF/MODEL.B
; 2508	AR_FM(#)	"FMADR/AC+#,AC-OP/AC+#,ADB/FM,AD/B,AR/AD"
; 2509	.ENDIF/MODEL.B
; 2510	AR_FM(VMA)	"FMADR/VMA,ADB/FM,AD/B,AR/AD"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-3
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR						

; 2511	AR_INTERVAL	"DIAG IN,DIAG FUNC/RD INTRVL"
; 2512	
; 2513	AR_MEM		"MEM/MB WAIT,FMADR/VMA,ADB/FM,AD/B,AR/MEM"
; 2514	AR_MQ		"ADA/MQ,AD/A,AR/AD"
; 2515	AR_MQ COMP	"ADA/MQ,AD/SETCA,AR/AD"
; 2516	AR_MQ*.25	"ADA/MQ,AD/A,AR/AD*.25"
; 2517	AR_MQ*2		"ADA/MQ,AD/A,AR/AD*2"
; 2518	AR_MQ*4		"ADA/MQ,AD/A*2,AR/AD*2"
; 2519	AR_MQ*AC1	"FMADR/AC1,ADB/FM,ADA/MQ,AR/AD"
; 2520	AR_MQ*AC2	"FMADR/AC2,ADB/FM,ADA/MQ,AR/AD"
; 2521	AR_MQ*AC3	"FMADR/AC3,ADB/FM,ADA/MQ,AR/AD"
; 2522	AR_MQ+1		"ADA/MQ,AD/A+1,AR/AD"
; 2523	AR_MQ+AC0	"FMADR/AC0,ADB/FM,ADA/MQ,AD/A+B,AR/AD"
; 2524	AR_MQ+BR	"ADA/MQ,ADB/BR,AD/A+B,AR/AD"		;[343]
; 2525	AR_MQ-1		"ADA/MQ,AD/A-1,AR/AD"
; 2526	AR_MQ-AC3	"ADA/MQ,ADB/FM,FMADR/AC3,AD/A-B,AR/AD"
; 2527	AR_MQ-BR	"ADA/MQ,ADB/BR,AD/A-B,AR/AD"
; 2528	AR_MTR REQ	"DIAG IN,DIAG FUNC/RD MTR REQ"
; 2529	AR_PC		"ADA/PC,AD/A,AR/AD"
; 2530	AR_PC FLAGS	"ADMSK,ADB/FM,ADA/PC,AD/ANDCB,AR/AD"
; 2531	AR_PC+1		"ADA/PC,AD/A+1,AR/AD,SPEC/SAVE FLAGS"
; 2532	AR_PERF CNT	"DIAG IN,DIAG FUNC/RD PERF CNT"
; 2533	AR_PERIOD	"DIAG IN,DIAG FUNC/RD PERIOD"
; 2534	.IF/KLPAGE
; 2535	AR_PUR+AR0	"PUR,ADB/FM,ADA EN/0S,AD/A+B,SPEC/XCRY AR0,AR/AD"
; 2536	.ENDIF/KLPAGE
; 2537	
; 2538	AR_SERIAL	"AR/ARMM,COND/REG CTL,AR CTL/ARR LOAD"
; 2539	AR_SFLGS	"SFLGS,AR_FM"
; 2540	AR_SHIFT	"SH/SHIFT AR!ARX,AR/SH"
; 2541	AR_SIGN		"AD/XCRY-1,SPEC/XCRY AR0,AR/AD"
; 2542	AR_SLEN		"SLEN,AR_FM"
; 2543	AR_SLEN COMP	"SLEN,ADB/FM,AD/SETCB,AR/AD"
; 2544	AR_SLEN+1	"SLEN,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2545	AR_SRCP		"SRCP,AR_FM"
; 2546	AR_SRCP+1	"SRCP,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2547	AR_SRCP2	"SRCP2,AR_FM"
; 2548	AR_SRCP2+1	"SRCP2,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2549	AR_SV.AR	"SV.AR,AR_FM"
; 2550	.IF/KLPAGE
; 2551	AR_SV.ARX	"SV.ARX,AR_FM"
; 2552	AR_SV.BR	"SV.BR,AR_FM"
; 2553	AR_SV.PFW	"SV.PFW,AR_FM"
; 2554	AR_SV.SC	"SV.SC,AR_FM"
; 2555	AR_SV.VMA	"SV.VMA,AR_FM"
; 2556	.ENDIF/KLPAGE
; 2557	AR_SWD		"SWD,AR_FM"
; 2558	
; 2559	AR_T0		"T0,AR_FM"
; 2560	AR_T1		"T1,AR_FM"
; 2561	AR_T2		"T2,AR_FM"
; 2562	AR_TIME BASE	"DIAG IN,DIAG FUNC/RD TIME"
;;2563	.IF/TRXDEF
;;2564	AR_TRB		"TRB,AR_FM"
;;2565	AR_TRX		"TRX,AR_FM"
;;2566	AR_TRX+1	"TRX,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-4
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR						

;;2567	AR_TRX1		"TRX1,AR_FM"
;;2568	AR_TRX2		"TRX2,AR_FM"
;;2569	AR_TRX2+1	"TRX2,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
;;2570	AR_TRX3		"TRX3,AR_FM"
;;2571	AR_TRX3+1	"TRX3,ADB/FM,ADA EN/0S,AD/A+B+1,AR/AD"
; 2572	.ENDIF/TRXDEF
; 2573	AR_VMA HELD	"COND/SEL VMA,AR_PC"
; 2574	AR_XR		"FMADR/XR,ADB/FM,AD/B,AR/AD"
; 2575	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR Miscellaneous, ARL, and ARR		

; 2576	.TOC	"CRAM Macros--AR Miscellaneous, ARL, and ARR"
; 2577	
; 2578	AR+ARX+MQ_0.M	"MEM/ARL IND,CLR/AR+ARX+MQ"
; 2579	AR+MQ_0.M	"MEM/ARL IND,CLR/AR+MQ"
; 2580	AR+MQ_0.S	"SPEC/ARL IND,CLR/AR+MQ"
; 2581	
; 2582	AR0-3 DISP	"SH/AR,DISP/SH0-3"
; 2583	AR0-8_#		"COND/LD AR0-8,AR/ARMM,ARMM/#"
; 2584	AR0-8_# AND AR0-8 "SCADA/#,SCADB/AR0-8,SCAD/AND,AR0-8_SCAD#"
; 2585	AR0-8_# OR AR0-8 "SCADA/#,SCADB/AR0-8,SCAD/OR,AR0-8_SCAD#"
; 2586	AR0-8_#+SC	"SCADA/#,SCADB/SC,SCAD/A+B,AR0-8_SCAD#"
; 2587	AR0-8_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,AR0-8_SCAD"
; 2588	AR0-8_FE	"SCADA/FE,SCAD/A,AR0-8_SCAD"
; 2589	AR0-8_FE OR #	"SCADA/FE,SCADB/#,SCAD/OR,AR0-8_SCAD#"
; 2590	AR0-8_FE OR SC	"SCADA/FE,SCADB/SC,SCAD/OR,AR0-8_SCAD.M"
; 2591	AR0-8_FE#	"SCADA/FE,SCAD/A,ARMM/SCAD EXP,AR/ARMM,COND/LD AR0-8"
; 2592	AR0-8_FE+#	"SCADA/FE,SCADB/#,SCAD/A+B,AR0-8_SCAD#"
; 2593	AR0-8_FE+1	"SCADA/FE,SCAD/A+1,AR0-8_SCAD"
; 2594	AR0-8_FE+SC	"SCADA/FE,SCADB/SC,SCAD/A+B,AR0-8_SCAD.M"
; 2595	AR0-8_FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B,AR0-8_SCAD.M"
; 2596	AR0-8_FE.M	"SCADA/FE,SCAD/A,AR0-8_SCAD.M"
; 2597	AR0-8_FE.R	"GEN FE,AR0-8_SCAD.R"
; 2598	AR0-8_SC	"SCADA EN/0S,SCADB/SC,SCAD/A+B,AR0-8_SCAD"
; 2599	AR0-8_SCAD	"SPEC/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD EXP"
; 2600	AR0-8_SCAD#	"ARMM/SCAD EXP,AR/ARMM,COND/LD AR0-8"
; 2601	AR0-8_SCAD.M	"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD EXP"
; 2602	AR0-8_SCAD.R	"ARMM/SCAD EXP,AR/ARMM,COND/REG CTL,AR CTL/AR0-8 LOAD"
; 2603	AR12-17_PC SEC	"AR/ARMM,VMAX/PC SEC,COND/LD AR9-17"
; 2604	AR12-17_PREV SEC	"AR/ARMM,VMAX/PREV SEC,COND/LD AR9-17"
; 2605	AR18-21 DISP	"SH/AR SWAP,DISP/SH0-3"
; 2606	
; 2607	ARL_0.C		"COND/ARL IND,CLR/ARL"
; 2608	ARL_0.M		"MEM/ARL IND,CLR/ARL"
; 2609	ARL_0.S		"SPEC/ARL IND,CLR/ARL"
; 2610	ARL_0S		"COND/ARL IND,CLR/ARL"
; 2611	ARL_1.M		"ADA EN/0S,AD/A+1,SPEC/GEN CRY18,MEM/ARL IND,ARL/AD"
; 2612	ARL_1S		"AD/1S,COND/ARL IND,ARL/AD"
; 2613	ARL_1S.M	"AD/1S,MEM/ARL IND,ARL/AD"
; 2614	ARL_AC0		"FMADR/AC0,ADB/FM,AD/B,COND/ARL IND,ARL/AD"
; 2615	ARL_ARL		"COND/ARL IND,ARL/ARL"
; 2616	ARL_ARL.M	"MEM/ARL IND,ARL/ARL"
; 2617	ARL_ARL.S	"SPEC/ARL IND,ARL/ARL"
; 2618	ARL_ARR		"COND/ARL IND,ARL/SH,SH/AR SWAP"
; 2619	ARL_ARR.M	"MEM/ARL IND,ARL/SH,SH/AR SWAP"
; 2620	ARL_ARR.S	"SPEC/ARL IND,ARL/SH,SH/AR SWAP"
; 2621	ARL_ARX (ADX)	"ADA EN/EN,AD/A,MEM/ARL IND,ARL/ADX"
; 2622	ARL_ARXL	"SPEC/ARL IND,SH/ARX,ARL/SH"
; 2623	ARL_ARXL.M	"MEM/ARL IND,SH/ARX,ARL/SH"
; 2624	ARL_BRL		"ADB/BR,AD/B,COND/ARL IND,ARL/AD"
; 2625	ARL_BRL.M	"ADB/BR,AD/B,MEM/ARL IND,ARL/AD"
; 2626	ARL_BRL.S	"ADB/BR,AD/B,SPEC/ARL IND,ARL/AD"
; 2627	ARL_SHIFT	"MEM/ARL IND,SH/SHIFT AR!ARX,ARL/SH"
; 2628	ARL_SIGN	"AD/XCRY-1,SPEC/XCRY AR0,COND/ARL IND,ARL/AD"
; 2629	ARL+ARX+MQ_0.M	"MEM/ARL IND,CLR/ARL+ARX+MQ"
; 2630	ARL+ARX_0.M	"MEM/ARL IND,CLR/ARL+ARX"
; 2631	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--AR Miscellaneous, ARL, and ARR		

; 2632	ARR_0.C		"COND/ARL IND,CLR/ARR"
; 2633	ARR_0.M		"MEM/ARL IND,CLR/ARR"
; 2634	ARR_0.S		"SPEC/ARL IND,CLR/ARR"
; 2635	ARR_0S		"AR_0S"
; 2636	ARR_1S		"AR_1S"
; 2637	ARR_AC0		"AR_AC0"
; 2638	ARR_AC0.S	"SPEC/ARL IND,FMADR/AC0,ADB/FM,AD/B,AR/AD"
; 2639	ARR_AR+1	"AR_AR+1"				;[343]
; 2640	ARR_AR+BR	"AR_AR+BR"				;[343]
; 2641	ARR_ARL		"SH/AR SWAP,AR/SH"
; 2642	ARR_ARR		"AR/AR"
; 2643	ARR_ARX		"AR_ARX"
; 2644	ARR_ARX+1	"AR_ARX+1"
; 2645	ARR_ARX+BRX	"AR_ARX+BRX"
; 2646	ARR_ARX+BR	"AR_ARX+BR"				;[343]
; 2647	ARR_ARX-1	"AR_ARX-1"
; 2648	ARR_BR		"ADB/BR,AD/B,COND/ARL IND,AR/AD"	;[252]
; 2649	ARR_PC+1	"ADA/PC,AD/A+1,AR/AD"
; 2650	ARR+MQ_0.S	"SPEC/ARL IND,CLR/ARR+MQ"
; 2651	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--ARX					

; 2652	.TOC	"CRAM Macros--ARX"
; 2653	
; 2654	ARX_-2+MQ0	"AD/1S,ARX/ADX*2"			;[343] -2 if MQ0 = 0
; 2655	ARX_-AC0	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A-B,ARX/AD"
; 2656	ARX_-BRX	"ADB/BR,ADA EN/0S,AD/A-B,ARX/ADX"
; 2657	ARX_-FM[]	"ADA EN/0S,ADB/FM,@1,AD/A-B,ARX/AD"
; 2658	ARX_-SLEN	"SLEN,ADB/FM,ADA EN/0S,AD/A-B,ARX/AD"
; 2659	ARX_0.C		"COND/ARL IND,CLR/ARX"
; 2660	ARX_0.M		"MEM/ARL IND,CLR/ARX"
; 2661	ARX_0.S		"SPEC/ARL IND,CLR/ARX"
; 2662	ARX_0S		"AD/0S,ARX/AD"
; 2663	ARX_1		"ADA EN/0S,AD/A+1,ARX/AD"
; 2664	ARX_1B1		"ADA EN/0S,AD/A+1,ARX/ADX*.25"
; 2665	ARX_1B17-1	"ADA EN/0S,AD/A-1,SPEC/GEN CRY18,ARX/AD"
; 2666	ARX_1S		"AD/1S,ARX/AD"
; 2667	ARX_2+MQ0	"ADA EN/0S,AD/A+1,ARX/ADX*2"
; 2668	
; 2669	ARX_AC0		"FMADR/AC0,ADB/FM,AD/B,ARX/AD"
; 2670	ARX_AC0 COMP	"ADB/FM,FMADR/AC0,AD/SETCB,ARX/AD"
; 2671	ARX_AC0+1	"ADA EN/0S,ADB/FM,FMADR/AC0,AD/A+B+1,ARX/AD"
; 2672	ARX_AC1		"FMADR/AC1,ADB/FM,AD/B,ARX/AD"
; 2673	ARX_AC2		"FMADR/AC2,ADB/FM,AD/B,ARX/AD"
; 2674	ARX_AC3		"FMADR/AC3,ADB/FM,AD/B,ARX/AD"
; 2675	ARX_AC4		"AC4,ADB/FM,AD/B,ARX/AD"
; 2676	
; 2677	ARX_AR		"SH/AR,ARX/SH"
; 2678	ARX_AR (AD)	"ADA/AR,AD/A,ARX/AD"
; 2679	ARX_AR AND ADMSK "ADMSK,ADB/FM,ADA/AR,AD/AND,ARX/AD"
; 2680	ARX_AR ANDCA BR	"ADA/AR,ADB/BR,AD/ANDCA,ARX/AD"
; 2681	ARX_AR SIGN	"AD/XCRY-1,SPEC/XCRY AR0,ARX/AD"
; 2682	ARX_AR SWAP	"SH/AR SWAP,ARX/SH"
; 2683	ARX_AR*2	"ADA/AR,AD/A*2,ARX/AD"			;[343]
; 2684	ARX_AR*4 COMP	"ADB/AR*4,AD/SETCB,ARX/AD"
; 2685	ARX_AR*MSK	"MSK,ADB/FM,ADA/AR,ARX/AD"
; 2686	ARX_AR+1	"ADA/AR,AD/A+1,ARX/AD"
; 2687	.IF/KLPAGE
; 2688	ARX_AR+CBR	"CBR,ADB/FM,ADA/AR,AD/A+B,ARX/AD"
; 2689	.ENDIF/KLPAGE
; 2690	ARX_AR+FM[]	"ADA/AR,ADB/FM,@1,AD/A+B,ARX/AD"
; 2691	ARX_AR-1	"ADA/AR,AD/A-1,ARX/AD"
; 2692	ARX_AR-BR	"ADA/AR,ADB/BR,AD/A-B,ARX/AD"		;[224]
; 2693	ARX_AR-FM[]	"ADA/AR,ADB/FM,@1,AD/A-B,ARX/AD"
; 2694	ARX_AR-FM[]-1	"ADA/AR,ADB/FM,@1,AD/A-B-1,ARX/AD"
; 2695	
; 2696	ARX_ARX AND ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/AND,ARX/AD"
; 2697	ARX_ARX ANDC ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/ANDCB,ARX/AD"
; 2698	ARX_ARX*-6	"ADA EN/EN,ADB/AR*4,AD/A-B,ARX/ADX*2"
; 2699	ARX_ARX*.25	"ADA EN/EN,AD/A,ARX/ADX*.25"
; 2700	ARX_ARX*.5	"ADA EN/EN,AD/A*2,ARX/ADX*.25"
; 2701	ARX_ARX*2	"ADA EN/EN,AD/A,ARX/ADX*2"
; 2702	ARX_ARX*2 COMP	"ADA EN/EN,AD/SETCA,ARX/ADX*2"
; 2703	ARX_ARX*4	"ADB/AR*4,AD/B,ARX/ADX"
; 2704	ARX_ARX*4 COMP	"ADB/AR*4,AD/SETCB,ARX/ADX"
; 2705	ARX_ARX*8	"ADB/AR*4,AD/B,ARX/ADX*2"
; 2706	ARX_ARX*BRX	"ADA/AR,ADB/BR,ARX/ADX"
; 2707	ARX_ARX*EXPMSK	"EXPMSK,ADB/FM,ADA/ARX,ARX/AD"		;[224]; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--ARX					

; 2708	ARX_ARX+1	"ADA EN/EN,AD/A+1,ARX/ADX"
; 2709	ARX_ARX+AC0	"ADA/ARX,ADB/FM,FMADR/AC0,AD/A+B,ARX/AD"
; 2710	.IF/KLPAGE
; 2711	ARX_ARX+CBR	"CBR,ADB/FM,ADA/ARX,AD/A+B,ARX/AD"
; 2712	.ENDIF/KLPAGE
; 2713	ARX_ARX+FM[]	"ADA/ARX,ADB/FM,@1,AD/A+B,ARX/AD"	;[343]
; 2714	ARX_ARX-1	"ADA EN/EN,AD/A-1,ARX/ADX"
; 2715	ARX_ARX-1 (AD)	"ADA/ARX,AD/A-1,ARX/AD"
; 2716	ARX_ARX-AR*4	"ADA/ARX,ADB/AR*4,AD/A-B,ARX/AD"	;[343]
; 2717	ARX_ARX-FM[]	"ADA/ARX,ADB/FM,@1,AD/A-B,ARX/AD"
; 2718	ARX_ARX-FM[]-1	"ADA/ARX,ADB/FM,@1,AD/A-B-1,ARX/AD"
; 2719	
; 2720	ARX_BR		"ADB/BR,AD/B,ARX/AD"
; 2721	ARX_BR*2	"ADB/BR*2,AD/B,ARX/AD"
; 2722	ARX_BR+1	"ADB/BR,ADA EN/0S,AD/A+B+1,ARX/AD"
; 2723	ARX_BRX		"ADB/BR,AD/B,ARX/ADX"
; 2724	ARX_BRX COMP	"ADB/BR,AD/SETCB,ARX/ADX"
; 2725	ARX_BRX+1	"ADA EN/0S,ADB/BR,AD/A+B+1,ARX/ADX"
; 2726	ARX_DSTP	"DSTP,ARX_FM"
; 2727	ARX_DSTP2	"DSTP2,ARX_FM"
; 2728	ARX_E1		"E1,ARX_FM"
; 2729	ARX_FILL	"FILL,ARX_FM"				;[310]
; 2730	ARX_FM		"ADB/FM,AD/B,ARX/AD"
; 2731	ARX_FM[]	"ADB/FM,@1,AD/B,ARX/AD"			;[343]
; 2732	ARX_FM[]+1	"ADA EN/0S,ADB/FM,@1,AD/A+B+1,ARX/AD"
; 2733	ARX_FM(VMA)	"FMADR/VMA,ADB/FM,AD/B,ARX/AD"
; 2734	ARX_MEM		"MEM/MB WAIT,FMADR/VMA,ADB/FM,AD/B,ARX/MEM"
; 2735	ARX_MQ-1	"ADA/MQ,AD/A-1,ARX/AD"			;[343]
; 2736	ARX_MQ-FM[]	"ADA/MQ,ADB/FM,@1,AD/A-B,ARX/AD"
; 2737	ARX_MQ-FM[]-1	"ADA/MQ,ADB/FM,@1,AD/A-B-1,ARX/AD"
; 2738	
; 2739	ARX_PC		"ADA/PC,AD/A,ARX/AD"
; 2740	ARX_PC+1	"ADA/PC,AD/A+1,ARX/AD,SPEC/SAVE FLAGS"
; 2741	ARX_SHIFT	"SH/SHIFT AR!ARX,ARX/SH"
; 2742	ARX_SRCP	"SRCP,ARX_FM"
; 2743	ARX_SRCP2	"SRCP2,ARX_FM"
; 2744	.IF/KLPAGE
; 2745	ARX_SV.AR	"SV.AR,ARX_FM"
; 2746	ARX_SV.ARX	"SV.ARX,ARX_FM"
; 2747	ARX_SV.BR	"SV.BR,ARX_FM"
; 2748	ARX_SV.VMA	"SV.VMA,ARX_FM"
; 2749	.ENDIF/KLPAGE
; 2750	ARX_T0		"T0,ARX_FM"
; 2751	ARX_T2		"T2,ARX_FM"
;;2752	.IF/TRXDEF
;;2753	ARX_TRB		"TRB,ARX_FM"
; 2754	.ENDIF/TRXDEF
; 2755	ARX_VMA HELD	"COND/SEL VMA,ARX_PC"
; 2756	ARX+MQ_0.M	"MEM/ARL IND,CLR/ARX+MQ"
; 2757	ARX+MQ_0.S	"SPEC/ARL IND,CLR/ARX+MQ"
; 2758	ARX0_AR35	"ADA/AR,AD/A*2+1,ARX/ADX*.25"	;[337]
; 2759	ARX0_MQ35	"ADA/MQ,AD/A*2+1,ARX/ADX*.25"
; 2760	ARX0-3 DISP	"SH/ARX,DISP/SH0-3"
; 2761	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--B, C, D					

; 2762	.TOC	"CRAM Macros--B, C, D"
; 2763	
; 2764	B DISP		"DISP/DRAM B"
; 2765	B WRITE		"DISP/DRAM B,MEM/B WRITE"
; 2766	BLKO TIM(L)	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/LD PA LEFT"
; 2767	BLKO TIM(R)	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/LD PA RIGHT"
; 2768	BR_AR LONG	"BR/AR,BRX/ARX"
; 2769	BYTE DISP	"DISP/BYTE"
;;2770	.IFNOT/MODEL.B
;;2771	BYTE INDRCT	"MEM/BYTE IND,VMA/LOAD"
; 2772	.IF/MODEL.B
; 2773	BYTE INDRCT	"MEM/EA CALC,EA CALC/BYTE IND,VMA/LOAD"
; 2774	BYTE LOAD	"MEM/EA CALC,EA CALC/BYTE LD,VMA/LOAD";[337]
; 2775	.ENDIF/MODEL.B
; 2776	BYTE PREV & CLR SR3	"COND/SR_#,#/640"
; 2777	BYTE PREV & SET SR2	"COND/SR_#,#/622"
; 2778	BYTE PREV & SET SR3	"COND/SR_#,#/641"
; 2779	.IF/MODEL.B
; 2780	BYTE READ	"MEM/EA CALC,EA CALC/BYTE RD,VMA/LOAD"
; 2781	BYTE READ PC	"MEM/EA CALC,EA CALC/BYTE RD PC,VMA/LOAD"
; 2782	BYTE RPW	"MEM/EA CALC,EA CALC/BYTE RPW,VMA/LOAD"
;;2783	.IFNOT/MODEL.B
;;2784	BYTE READ	"MEM/BYTE RD,VMA/LOAD"
;;2785	BYTE RPW	"MEM/RPW,VMA/AD"
; 2786	.ENDIF/MODEL.B
; 2787	
; 2788	.IF/MODEL.B
; 2789	CALL		"CALL/CALL"
;;2790	.IFNOT/MODEL.B
;;2791	CALL		"SPEC/CALL"
; 2792	.ENDIF/MODEL.B
; 2793	CALL []		"CALL, J/@1"
; 2794	CALL[]		"CALL, J/@1"
; 2795	.IF/MODEL.B
; 2796	CALL.C		"CALL/CALL"
; 2797	CALL.M		"CALL/CALL"
; 2798	CALL.S		"CALL/CALL"
;;2799	.IFNOT/MODEL.B
;;2800	CALL.C		"COND/ARL IND,CALL/CALL"
;;2801	CALL.M		"MEM/ARL IND,CALL/CALL"
;;2802	CALL.S		"SPEC/ARL IND,CALL/CALL"
; 2803	.ENDIF/MODEL.B
; 2804	.IF/KLPAGE
; 2805	CBR		"P2"
; 2806	.ENDIF/KLPAGE
; 2807	CLR ACC+SET UCODE	"COND/EBOX STATE,#/245"
; 2808	CLR ACCOUNT EN	"COND/EBOX STATE,#/145"
; 2809	CLR AR		"COND/AR CLR"
; 2810	CLR ARX		"COND/ARX CLR"
; 2811	CLR EBUS DEMAND	"COND/EBUS CTL,EBUS CTL/EBUS NODEMAND"
; 2812	CLR EXP		"SCADA EN/0S,SCAD/A,EXP_SCAD"
; 2813	CLR FE		"SCADA EN/0S,SCAD/A,FE/SCAD"
; 2814	CLR FPD		"SPEC/CLR FPD"
; 2815	CLR MQ		"COND/REG CTL,MQ/MQ SEL,MQ CTL/0S"
; 2816	CLR MTR PA EN	"COND/EBOX STATE,#/025"
; 2817	CLR P		"SCADA EN/0S,SCAD/A,P_SCAD"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--B, C, D					

; 2818	CLR PT LINE	"COND/MBOX CTL,MBOX CTL/CLR PT LINE"
; 2819	CLR PT LINE (KEEP) "COND/MBOX CTL,MBOX CTL/CLR PT LINE(NK)"
; 2820	CLR SC		"SCADA EN/0S,SCAD/A,SC/SCAD"
; 2821	CLR SPECIAL CYCLE	"COND/SPEC INSTR,SPEC INSTR/0"
; 2822	CLR SR2		"COND/SR_#,#/20"
; 2823	CLR SR3		"COND/SR_#,#/40"
; 2824	CLR TRACKS EN	"COND/EBOX STATE,#/121"
; 2825	CLR TRK+PA EN	"COND/EBOX STATE,#/021"
; 2826	CMS FETCH	"VMA/PC+1,MEM/FETCH,FETCH/SKIP"
; 2827	COMP FETCH	"AD/XOR,VMA/PC+1,MEM/FETCH,FETCH/COMP"
; 2828	CONI APR(L)	"DIAG IN,DIAG FUNC/CONI APR(L)"
; 2829	CONI APR(R)	"DIAG IN,DIAG FUNC/CONI APR(R)"
; 2830	CONI MTR	"DIAG IN,DIAG FUNC/CONI MTR"
; 2831	CONI PAG	"DIAG IN,DIAG FUNC/CONI PAG"
; 2832	CONI PI(L)	"DIAG IN,DIAG FUNC/CONI PI(L)"
; 2833	CONI PI(PAR)	"DIAG IN,DIAG FUNC/CONI PI(PAR)"
; 2834	CONI PI(R)	"DIAG IN,DIAG FUNC/CONI PI(R)"
; 2835	CONO APR	"DIAG OUT,DIAG FUNC/CONO APR"
; 2836	CONO MTR	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/CONO MTR"
; 2837	CONO PAG	"DIAG OUT,DIAG FUNC/CONO PAG"
; 2838	CONO PI		"DIAG OUT,DIAG FUNC/CONO PI"
; 2839	CONO TIM	"SPEC/MTR CTL,DIAG OUT,DIAG FUNC/CONO TIM"
; 2840	CONTINUE	"COND/SPEC INSTR,SPEC INSTR/CONT"
; 2841	.IF/KLPAGE
; 2842	CSMSK		"P0"
; 2843	.ENDIF/KLPAGE
; 2844	
; 2845	DATAI APR(L)	"DIAG IN,DIAG FUNC/DATAI APR"
; 2846	DATAI PAG(L)	"DIAG IN,DIAG FUNC/DATAI PAG(L)"
; 2847	DATAO APR	"DIAG OUT,DIAG FUNC/DATAO APR"
; 2848	DATAO PAG(L)	"DIAG OUT,DIAG FUNC/DATAO PAG"
; 2849	DIAG IN		"COND/DIAG FUNC,TIME/5T,AR/EBUS"
; 2850	DIAG OUT	"COND/DIAG FUNC,TIME/5T,ADA/AR,AD/A"
; 2851	DISMISS		"SPEC/FLAG CTL,FLAG CTL/DISMISS"
; 2852	DIVIDE		"FE_FE-1,DISP/DIV,MQ/MQ*2"
; 2853	DLEN		"FMADR/AC3"
; 2854	DLEN_AR		"DLEN,FM_AR"
; 2855	DROP EBUS REQ	"COND/EBUS CTL,EBUS CTL/0"
; 2856	.IF/MODEL.B
; 2857	DSTP		"FMADR/AC+#,AC-OP/AC+#,AC#/4"
;;2858	.IFNOT/MODEL.B
;;2859	DSTP		"FMADR/AC4"
; 2860	.ENDIF/MODEL.B
; 2861	DSTP_AR		"DSTP,FM_AR"
; 2862	.IF/MODEL.B
; 2863	DSTP2		"FMADR/AC+#,AC-OP/AC+#,AC#/5"
; 2864	.ENDIF/MODEL.B
; 2865	DSTP2_AR	"DSTP2,FM_AR"
; 2866	DSTW		"R14"
; 2867	DSTW_AR		"DSTW,FM_AR"
; 2868	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--E, F					

; 2869	.TOC	"CRAM Macros--E, F"
; 2870	
; 2871	E0		"R16"
; 2872	E0_AR		"E0,FM_AR"
; 2873	E1		"R5"
; 2874	E1_AR		"E1,FM_AR"
; 2875	.IF/XADDR
; 2876	EA MOD DISP	"DISP/EA MOD,AD/1S"
;;2877	.IFNOT/XADDR
;;2878	EA MOD DISP	"DISP/EA MOD"
;;2879	EA TYPE DISP	"DISP/EA TYPE"
; 2880	.ENDIF/XADDR
; 2881	EPT FETCH	"MEM/LOAD ARX,SPEC/SP MEM CYCLE,SP MEM/EPT FETCH"
; 2882	EPT REF		"SPEC/SP MEM CYCLE,SP MEM/EPT"
; 2883	EPT REF	CACHE	"SPEC/SP MEM CYCLE,SP MEM/EPT CACHE"
; 2884	EXEC REF	"SPEC/SP MEM CYCLE,SP MEM/EXEC"
; 2885	EXIT		"DISP/DRAM B,MEM/B WRITE,J/ST0"
; 2886	EXIT DBL	"MB WAIT,J/ST2AC"	;"I FETCH,J/DSTAC" WHEN TIMING FIXED
; 2887	EXP TEST	"COND/REG CTL,EXP TST/AR_EXP"
; 2888	EXP TST		"COND/REG CTL,EXP TST/AR_EXP"
; 2889	EXPMSK		"R4"		;[224][233]
; 2890	EXP_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,EXP_SCAD"
; 2891	EXP_-SC-1 TST	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,EXP_SCAD.C,EXP TST"
; 2892	EXP_1		"SCADA EN/0S,SCAD/A+1,EXP_SCAD"
; 2893	EXP_FE TST	"SCADA/FE,SCAD/A,EXP_SCAD.C,EXP TST"
; 2894	EXP_SC		"SCADA EN/0S,SCADB/SC,SCAD/A+B,EXP_SCAD"
; 2895	EXP_SC.MS	"MEM/ARL IND,ARL/ARMM,COND/LD AR0-8,ARMM/SCAD EXP,EXP_SCAD.MS";[224]
; 2896	EXP_SCAD	"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD EXP"
; 2897	EXP_SCAD.C	"COND/REG CTL,AR CTL/AR0-8 LOAD,AR/ARMM,ARMM/SCAD EXP"
; 2898	EXP_SCAD.MS	"SCADA EN/0S,SCADB/SC,SCAD/A+B"
; 2899	EXP_SIGN	"SPEC/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/EXP_SIGN"
; 2900	EXP_SIGN.C	"COND/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/EXP_SIGN"
; 2901	EXP_SIGN.M	"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/EXP_SIGN"
; 2902	EXP_SIGN.MS	"MEM/ARL IND,ARL/ARMM,COND/LD AR0-8,ARMM/EXP_SIGN";[224]
; 2903	.IF/MODEL.B
; 2904	EXT ADDR	"MEM/A RD,#/400,DISP/DRAM B"
; 2905	EXT BYTE READ	"MEM/EA CALC,EA CALC/BYTE RD,VMA/LOAD,GLOBAL"
; 2906	EXT BYTE RPW	"MEM/EA CALC,EA CALC/BYTE RPW,VMA/LOAD,GLOBAL";[337]
; 2907	EXT INDEX	"MEM/A RD,#/400,DISP/DRAM A RD"
; 2908	EXT INDRCT	"MEM/EA CALC,EA CALC/A IND,VMA/LOAD"
; 2909	.ENDIF/MODEL.B
; 2910	
; 2911	FE_#		"SCADA/#,SCAD/A,FE/SCAD"
; 2912	FE_# AND S	"SCADA/#,SCADB/AR6-11,SCAD/AND,FE/SCAD"
; 2913	FE_#+AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/A+B,FE/SCAD"
; 2914	FE_#+SC		"SCADA/#,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2915	FE_#-SC		"SCADA/#,SCADB/SC,SCAD/A-B,FE/SCAD"
; 2916	FE_+#		"SCADA EN/0S,SCADB/#,SCAD/A+B,FE/SCAD"
; 2917	FE_-1		"SCADA EN/0S,SCAD/A-1,FE/SCAD"
; 2918	FE_-S		"SCADA EN/0S,SCADB/AR6-11,SCAD/A-B,FE/SCAD";[337]
; 2919	FE_-SC		"SCADA EN/0S,SCADB/SC,SCAD/A-B,FE/SCAD"
; 2920	FE_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,FE/SCAD"
; 2921	FE_1		"SCADA EN/0S,SCAD/A+1,FE/SCAD"
; 2922	FE_AR0-8	"SCADA EN/0S,SCADB/AR0-8,SCAD/A+B,FE/SCAD"
; 2923	FE_AR0-8 AND #	"SCADA/#,SCADB/AR0-8,SCAD/AND,FE/SCAD"
; 2924	FE_AR0-8 COMP	"SCADA EN/0S,SCADB/AR0-8,SCAD/A-B-1,FE/SCAD"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--E, F					

; 2925	FE_EXP		"SCADA/AR EXP,SCAD/A,FE/SCAD"
; 2926	FE_EXP+1	"SCADA/AR EXP,SCAD/A+1,FE/SCAD"
; 2927	FE_EXP+SC	"SCADA/AR EXP,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2928	FE_EXP-#	"SCADA/AR EXP,SCADB/#,SCAD/A-B,FE/SCAD"
; 2929	FE_EXP-1	"SCADA/AR EXP,SCAD/A-1,FE/SCAD"
; 2930	FE_FE AND #	"SCADA/FE,SCADB/#,SCAD/AND,FE/SCAD"
; 2931	FE_FE AND AR0-8	"SCADA/FE,SCADB/AR0-8,SCAD/AND,FE/SCAD"
; 2932	FE_FE OR #	"SCADA/FE,SCADB/#,SCAD/OR,FE/SCAD"
; 2933	FE_FE OR AR0-8	"SCADA/FE,SCADB/AR0-8,SCAD/OR,FE/SCAD"
; 2934	FE_FE SHRT	"COND/FE SHRT,FE/0"
; 2935	FE_FE+#		"SCADA/FE,SCADB/#,SCAD/A+B,FE/SCAD"
; 2936	FE_FE+1		"SCADA/FE,SCAD/A+1,FE/SCAD"
; 2937	FE_FE+S		"SCADA/FE,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2938	FE_FE+SC	"SCADA/FE,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2939	FE_FE-#		"SCADA/FE,SCADB/#,SCAD/A-B,FE/SCAD"
; 2940	FE_FE-1		"SCADA/FE,SCAD/A-1,FE/SCAD"
; 2941	FE_FE-S		"SCADA/FE,SCADB/AR6-11,SCAD/A-B,FE/SCAD"
; 2942	FE_FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B,FE/SCAD"
; 2943	FE_FE-SC-1	"SCADA/FE,SCADB/SC,SCAD/A-B-1,FE/SCAD"	;[343]
; 2944	FE_P		"SCADA/AR0-5,SCAD/A,FE/SCAD"
; 2945	FE_P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND,FE/SCAD"
; 2946	FE_P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND,FE/SCAD"
; 2947	FE_P OR #	"SCADA/AR0-5,SCADB/#,SCAD/OR,FE/SCAD"
; 2948	FE_P+1		"SCADA/AR0-5,SCAD/A+1,FE/SCAD"		;[343]
; 2949	FE_P+SC		"SCADA/AR0-5,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2950	FE_P-S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A-B,FE/SCAD"
; 2951	FE_S		"SCADA EN/0S,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2952	FE_S+#		"SCADA/#,SCADB/AR6-11,SCAD/A+B,FE/SCAD"
; 2953	FE_SC		"SCADA EN/0S,SCADB/SC,SCAD/A+B,FE/SCAD"
; 2954	
; 2955	.IF/MODEL.B
; 2956	FETCH		"MEM/IFET"
; 2957	FETCH+1		"COND/VMA INC,MEM/IFET"
;;2958	.IFNOT/MODEL.B
;;2959	FETCH		"MEM/FETCH,FETCH/UNCOND"
;;2960	FETCH+1		"COND/VMA INC,MEM/FETCH,FETCH/UNCOND"
; 2961	.ENDIF/MODEL.B
; 2962	FETCH WAIT	"MEM/MB WAIT"		;See edit 111
; 2963	FILL		"R13"
; 2964	FILL_AR		"FILL,FM_AR"
; 2965	FIN LOAD	"ADB/FM,FMADR/VMA,AD/B"	;Finish load of AR or ARX, start new op
; 2966	FIN STORE	"FMADR/VMA"		;FINISH STORE, start new operation
; 2967	FIN XFER	"FMADR/VMA,ADB/FM,AD/B"	;Same as FIN LOAD
; 2968	FINISH		"J/FINI"	;USE INSTEAD OF NXT INSTR IF FM WRITE
; 2969	FM_AR		"COND/FM WRITE"
; 2970	FM[]_AR		"@1, FM_AR"
; 2971	.IF/MODEL.B
; 2972	FM(#)_AR	"FMADR/AC+#,AC-OP/AC+#,COND/FM WRITE"
; 2973	.ENDIF/MODEL.B
; 2974	FORCE AR-ARX	"ADB/AR*4,AD/B,AR/AD*.25,ARX/ADX*.25"
; 2975	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--G, H, I, J, L				

; 2976	.TOC	"CRAM Macros--G, H, I, J, L"
; 2977	
; 2978	GEN # AND AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/AND"
; 2979	GEN #+AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/A+B"
; 2980	GEN #+SC	"SCADA/#,SCADB/SC,SCAD/A+B"
; 2981	GEN #-SC	"SCADA/#,SCADB/SC,SCAD/A-B"
; 2982	GEN -AR LONG	"ADB/AR*4,ADA EN/0S,AD/A-B,SPEC/AD LONG"
; 2983	GEN -AR*4	"ADA EN/0S,ADB/AR*4,AD/A-B"
; 2984	GEN -SC		"SCADB/SC,SCADA EN/0S,SCAD/A-B"
; 2985	GEN -SC-1	"SCADB/SC,SCADA EN/0S,SCAD/A-B-1"
; 2986	GEN 0S		"AD/0S"
; 2987	GEN 2AR		"ADA/AR, AD/A*2"
; 2988	
; 2989	GEN AC0		"FMADR/AC0,ADB/FM,AD/B"
; 2990	GEN AC0+1	"FMADR/AC0,ADB/FM,ADA EN/0S,AD/A+B+1"
; 2991	GEN AR		"ADA/AR,AD/A"
; 2992	GEN AR*AC0	"FMADR/AC0,ADB/FM,ADA/AR"
; 2993	GEN AR*BR	"ADA/AR,ADB/BR"
; 2994	GEN AR*T0	"T0,ADB/FM,ADA/AR"
; 2995	GEN AR+1	"ADA/AR,AD/A+1"
; 2996	GEN AR+2BR	"ADA/AR,ADB/BR*2,AD/A+B"
; 2997	GEN AR+BR	"ADA/AR,ADB/BR,AD/A+B"
; 2998	GEN AR+E1	"E1,ADB/FM,ADA/AR,AD/A+B"
; 2999	GEN AR+FM[]	"ADA/AR,ADB/FM,@1,AD/A+B"
; 3000	GEN AR+XR	"FMADR/XR,ADB/FM,ADA/AR,AD/A+B"
; 3001	GEN AR-2BR	"ADA/AR,ADB/BR*2,AD/A-B"
; 3002	GEN AR-AC3	"FMADR/AC3,ADB/FM,ADA/AR,AD/A-B"
; 3003	GEN AR-BR	"ADA/AR,ADB/BR,AD/A-B"
; 3004	GEN AR-BR-1	"GEN AR*BR,AD/A-B-1"
; 3005	GEN AR-FM[]	"ADA/AR,ADB/FM,@1,AD/A-B"
; 3006	GEN AR0-8	"SCADA EN/0S,SCADB/AR0-8,SCAD/OR"
; 3007	GEN ARX		"ADA/ARX,AD/A"
; 3008	GEN ARX COMP	"ADA/ARX,AD/SETCA"
; 3009	GEN ARX*BR	"ADA/ARX,ADB/BR"		;[224]
; 3010	GEN ARX*BRX	"ADA EN/EN,ADB/BR"
; 3011	GEN ARX+1	"ADA/ARX,AD/A+1"
; 3012	GEN ARX+XR	"FMADR/XR,ADB/FM,ADA/ARX,AD/A+B"
; 3013	GEN ARX-1	"ADA/ARX,AD/A-1"
; 3014	
; 3015	GEN BR		"ADB/BR,AD/B"
; 3016	GEN BR*2	"ADB/BR*2,AD/B"
; 3017	GEN BR+ARX	"ADA/ARX,ADB/BR,AD/A+B"		;[230]
; 3018	GEN BRX+1	"ADA EN/0S,ADB/BR,AD/A+B+1"
; 3019	GEN CRY18	"SPEC/GEN CRY18"
; 3020	GEN E1		"E1, ADB/FM, AD/B"
; 3021	GEN FE		"SCADA/FE,SCAD/A"
; 3022	GEN FE AND #	"SCADA/FE,SCADB/#,SCAD/AND"
; 3023	GEN FE AND AR0-8 "SCADA/FE,SCADB/AR0-8,SCAD/AND"
; 3024	GEN FE AND S	"SCADA/FE,SCADB/AR6-11,SCAD/AND"
; 3025	GEN FE AND SC	"SCADA/FE,SCADB/SC,SCAD/AND"
; 3026	GEN FE OR AR0-8	"SCADA/FE,SCADB/AR0-8,SCAD/OR"	;[347]
; 3027	GEN FE+#	"SCADA/FE,SCADB/#,SCAD/A+B"
; 3028	GEN FE-#	"SCADA/FE,SCADB/#,SCAD/A-B"
; 3029	GEN FE-1	"SCADA/FE,SCAD/A-1"
; 3030	GEN FE-S	"SCADA/FE,SCADB/AR6-11,SCAD/A-B"
; 3031	GEN FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--G, H, I, J, L				

; 3032	GEN FE-SC-1	"SCADA/FE,SCADB/SC,SCAD/A-B-1"	; [303] For DPB to top byte
; 3033	GEN MQ		"ADA/MQ,AD/A"
; 3034	GEN MQ-AR	"ADA/MQ,ADB/AR,AD/A-B"
; 3035	GEN P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND"
; 3036	GEN P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND"
; 3037	GEN P+SC	"SCADA/AR0-5,SCADB/SC,SCAD/A+B"
; 3038	GEN P-#		"SCADA/AR0-5,SCADB/#,SCAD/A-B"		;[337]
; 3039	GEN P-S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A-B"
; 3040	GEN P-SC	"SCADA/AR0-5,SCADB/SC,SCAD/A-B"
; 3041	GEN SC		"SCADB/SC,SCADA EN/0S,SCAD/A+B"
; 3042	GEN SCAD 0S	"SCADA EN/0S,SCAD/A"
; 3043	GEN T1		"T1,ADB/FM,AD/B"
; 3044	GEN T2		"T2,ADB/FM,AD/B"
; 3045	GET ECL EBUS	"COND/EBUS CTL,EBUS CTL/GRAB EEBUS"
; 3046	GLOBAL		"SH/1"
; 3047	
; 3048	HALT		"SPEC/FLAG CTL,FLAG CTL/HALT"
; 3049	HARDPFW		"R17"				;Hard page fail word
; 3050	
; 3051	.IF/MODEL.B
; 3052	I FETCH		"VMA/PC+1,MEM/IFET"
;;3053	.IFNOT/MODEL.B
;;3054	I FETCH		"VMA/PC+1,MEM/FETCH,FETCH/UNCOND"
; 3055	.ENDIF/MODEL.B
; 3056	INDEXED		"SH/2"
; 3057	INH CRY18	"SPEC/INH CRY18"
; 3058	IO INIT		"COND/EBUS CTL,EBUS CTL/IO INIT"
; 3059	IR DISP		"DISP/DRAM J"
; 3060	
; 3061	JFCL FETCH	"VMA/PC+1,MEM/FETCH,FETCH/JFCL"
; 3062	JFCL S		"SPEC/FLAG CTL,FLAG CTL/JFCL+LD"
; 3063	JFCL T		"SPEC/FLAG CTL,FLAG CTL/JFCL"
; 3064	JUMP FETCH	"VMA/PC+1,MEM/FETCH,FETCH/JUMP"
; 3065	
; 3066	LD PCS		"COND/DIAG FUNC,TIME/5T,DIAG FUNC/LD PCS+CWSX,ADA/ARX,AD/A"
; 3067	LD PREV CTXT	"COND/DIAG FUNC,TIME/5T,DIAG FUNC/LD PCS+CWSX,ADA/PC,AD/A"
; 3068	LOAD AR		"MEM/LOAD AR"
; 3069	.IF/MODEL.B
; 3070	LOAD AR (RPW)	"MEM/RPW"
; 3071	LOAD AR (WR TST)	"MEM/EA CALC,EA CALC/LD AR+WR"
; 3072	.ENDIF/MODEL.B
; 3073	LOAD ARX	"MEM/LOAD ARX"
; 3074	.IF/MODEL.B
; 3075	LOAD ARX (WR TST)	"MEM/EA CALC,EA CALC/LD ARX+WR"
; 3076	.ENDIF/MODEL.B
; 3077	LOAD EBR	"MEM/REG FUNC,MREG FNC/LOAD EBR"
; 3078	.IF/MODEL.B
; 3079	LOAD IR		"COND/LOAD IR,PXCT/0"
;;3080	.IFNOT/MODEL.B
;;3081	LOAD IR		"COND/LOAD IR"
; 3082	.ENDIF/MODEL.B
; 3083	LOAD UBR	"MEM/REG FUNC,MREG FNC/LOAD UBR"
; 3084	.IF/BLT.PXCT
; 3085	LOAD VMA(EA)_ARX+BR "VMA/LOAD,MEM/EA CALC,EA CALC/LD AR(EA),ADA/ARX,ADB/BR,AD/A+B"
;;3086	.IFNOT/BLT.PXCT
;;3087	LOAD VMA(EA)_ARX+BR "ADA/ARX,ADB/BR,AD/A+B,VMA/AD,LOAD AR"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7-2
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--G, H, I, J, L				

; 3088	.ENDIF/BLT.PXCT
; 3089	.IF/MODEL.B
; 3090	LONG EN		"COND/LONG EN"
; 3091	.ENDIF/MODEL.B
; 3092	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--M, N, O, P					

; 3093	.TOC	"CRAM Macros--M, N, O, P"
; 3094	
; 3095	MAP		"MEM/REG FUNC,MREG FNC/MAP"
; 3096	MB WAIT		"MEM/MB WAIT"
; 3097	MEM_AR		"FMADR/VMA,MEM/MB WAIT"
; 3098	
; 3099	MQ_0.C		"COND/ARL IND,CLR/MQ"
; 3100	MQ_0.M		"MEM/ARL IND,CLR/MQ"
; 3101	MQ_0.S		"SPEC/ARL IND,CLR/MQ"
; 3102	MQ_1		"ADA EN/0S,AD/A+1,MQ_AD"
; 3103	MQ_1S		"COND/REG CTL,MQ/MQM SEL,MQ CTL/1S"
; 3104	MQ_AD		"COND/REG CTL,MQ/MQM SEL,MQ CTL/AD"
; 3105	MQ_AR		"SH/AR,MQ/SH"
; 3106	MQ_AR (AD)	"ADA/AR,AD/A,MQ_AD"
; 3107	MQ_AR COMP	"ADA/AR,AD/SETCA,MQ_AD"
; 3108	MQ_AR SWAP	"SH/AR SWAP,MQ/SH"
; 3109	MQ_ARX		"SH/ARX,MQ/SH"
; 3110	MQ_ARX COMP	"ADA/ARX,AD/SETCA,MQ_AD"
; 3111	MQ_BR		"ADB/BR,AD/B,MQ_AD"
; 3112	MQ_BR COMP	"ADB/BR,AD/SETCB,MQ_AD"
; 3113	MQ_FM[]		"ADB/FM,@1,AD/B,MQ_AD"			;[343]
; 3114	MQ_MQ*.25	"SPEC/MQ SHIFT,MQ/MQ*.25"
; 3115	MQ_MQ*2		"SPEC/MQ SHIFT,MQ/MQ*2"
; 3116	MQ_MQ*BR	"ADA/MQ, ADB/BR, MQ_AD"
; 3117	MQ_MQ-1		"ADA/MQ,AD/A-1,MQ_AD"			;[343]
; 3118	MQ_SHIFT	"SH/SHIFT AR!ARX,MQ/SH"
; 3119	
; 3120	MSK		"R7"
; 3121	MSK_AR		"MSK,FM_AR"
; 3122	MUL		"FE_FE+1,DISP/MUL,MQ/MQ*.25"
; 3123	
; 3124	
; 3125	NO CRY		"AD/SETCA"
; 3126	NORM		"DISP/NORM"
; 3127	NORM -AR	"ADA EN/0S,ADB/AR*4,AD/A-B,AR/AD*.25,ARX/ADX*.25,DISP/NORM"
; 3128	NORM AR		"ADB/AR*4,AD/B,DISP/NORM"
; 3129	NXT INSTR	"MEM/MB WAIT,DISP/NICOND,#/0,CLR SC,CLR FE,J/NEXT"
; 3130	
; 3131	OPTIONS		"ISTAT/OPTIONS,KLPAGE/OPTIONS,LONGPC/OPTIONS,NONSTD/OPTIONS,PV/OPTIONS"
; 3132	
; 3133	P_#		"SCADA/#,SCAD/A,P_SCAD#"
; 3134	P_#-S		"SCADA/#,SCADB/AR6-11,SCAD/A-B,P_SCAD#"
; 3135	P_#-SC		"SCADA/#,SCADB/SC,SCAD/A-B,P_SCAD#"	;[343]
; 3136	P_-SC		"SCADA EN/0S,SCADB/SC,SCAD/A-B,P_SCAD"
; 3137	P_0		"SCADA EN/0S,SCAD/A,P_SCAD"
; 3138	P_1S		"SCADA EN/0S,SCAD/A-1,P_SCAD"
; 3139	P_FE		"SCADA/FE,SCAD/A,P_SCAD"
; 3140	P_FE OR SC	"SCADA/FE,SCADB/SC,SCAD/OR,P_SCAD"
; 3141	P_FE+SC		"SCADA/FE,SCADB/SC,SCAD/A+B,P_SCAD.C"
; 3142	P_FE-S		"SCADA/FE,SCADB/AR6-11,SCAD/A-B,P_SCAD.C"
; 3143	P_FE-S.S	"SCADA/FE,SCADB/AR6-11,SCAD/A-B,P_SCAD.S"
; 3144	P_FE.C		"SCADA/FE,SCAD/A,P_SCAD#"
; 3145	P_P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND,P_SCAD#"
; 3146	P_P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND,P_SCAD"
; 3147	P_P OR #	"SCADA/AR0-5,SCADB/#,SCAD/OR,P_SCAD#"
; 3148	P_P OR SC	"SCADA/AR0-5,SCADB/SC,SCAD/OR,P_SCAD"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--M, N, O, P					

; 3149	P_P OR SC#	"SCADA/AR0-5,SCADB/SC,SCAD/OR,P_SCAD#"
; 3150	P_P+#		"SCADA/AR0-5,SCADB/#,SCAD/A+B,P_SCAD#"
; 3151	P_P+1		"SCADA/AR0-5,SCAD/A+1,P_SCAD#"		;[337]
; 3152	P_P+S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,P_SCAD"
; 3153	P_P+S.C		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,P_SCAD#"
; 3154	P_P-S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A-B,P_SCAD"
; 3155	P_SC		"SCADA EN/0S,SCADB/SC,SCAD/A+B,P_SCAD"
; 3156	P_SC#		"SCADA EN/0S,SCADB/SC,SCAD/A+B,P_SCAD#"
; 3157	P_SCAD		"MEM/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD POS"
; 3158	P_SCAD#		"COND/LD AR0-8,AR/ARMM,ARMM/SCAD POS"
; 3159	P_SCAD.C	"COND/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD POS"
; 3160	P_SCAD.S	"SPEC/ARL IND,ARL/ARMM,AR0-8/LOAD,ARMM/SCAD POS"
; 3161	
; 3162	P0		"FMADR/#B#,ACB/PAGB,AC#/0"	Paging AC 0
; 3163	P1		"FMADR/#B#,ACB/PAGB,AC#/1"
; 3164	P10		"FMADR/#B#,ACB/PAGB,AC#/10"
; 3165	P11		"FMADR/#B#,ACB/PAGB,AC#/11"
; 3166	P12		"FMADR/#B#,ACB/PAGB,AC#/12"
; 3167	P13		"FMADR/#B#,ACB/PAGB,AC#/13"
; 3168	P14		"FMADR/#B#,ACB/PAGB,AC#/14"
; 3169	P15		"FMADR/#B#,ACB/PAGB,AC#/15"
; 3170	P16		"FMADR/#B#,ACB/PAGB,AC#/16"
; 3171	P17		"FMADR/#B#,ACB/PAGB,AC#/17"
; 3172	P2		"FMADR/#B#,ACB/PAGB,AC#/2"
; 3173	P3		"FMADR/#B#,ACB/PAGB,AC#/3"
; 3174	P4		"FMADR/#B#,ACB/PAGB,AC#/4"
; 3175	P5		"FMADR/#B#,ACB/PAGB,AC#/5"
; 3176	P6		"FMADR/#B#,ACB/PAGB,AC#/6"
; 3177	P7		"FMADR/#B#,ACB/PAGB,AC#/7"
; 3178	
; 3179	PC_VMA		"SPEC/LOAD PC"
; 3180	PF DISP		"DISP/PG FAIL"
; 3181	.IF/KLPAGE
; 3182	PFA		"P4"
; 3183	PFA_AR		"PFA,COND/FM WRITE"
; 3184	.ENDIF/KLPAGE
; 3185	PHYS REF	"SPEC/SP MEM CYCLE,SP MEM/UNPAGED"
; 3186	PHYS REF CACHE	"SPEC/SP MEM CYCLE,SP MEM/UNPAGED+CACHED"
; 3187	.IF/MODEL.B
; 3188	POP AR		"MEM/EA CALC,EA CALC/POP AR,VMA/LOAD"
; 3189	POP AR-ARX	"MEM/EA CALC,EA CALC/POP AR-ARX,VMA/LOAD"
; 3190	POP ARX		"MEM/EA CALC,EA CALC/POP ARX,VMA/LOAD"
; 3191	.ENDIF/MODEL.B
; 3192	PORTAL		"SPEC/FLAG CTL,FLAG CTL/PORTAL"
; 3193	PT FETCH	"MEM/LOAD ARX,SPEC/SP MEM CYCLE,SP MEM/PT FETCH"
; 3194	PT REF		"SPEC/SP MEM CYCLE,SP MEM/PT"
; 3195	PT SEL_INVAL	"COND/MBOX CTL,MBOX CTL/PT DIR CLR"
; 3196	PT SEL_INVAL (KEEP) "COND/MBOX CTL,MBOX CTL/PT DIR CLR(NK)"
; 3197	PT SEL_NORMAL	"COND/MBOX CTL,MBOX CTL/NORMAL"
; 3198	.IF/KLPAGE
; 3199	PUR		"P1"
; 3200	.ENDIF/KLPAGE
; 3201	.IF/MODEL.B
; 3202	PUSH		"MEM/EA CALC,EA CALC/PUSH,VMA/LOAD,SPEC/STACK UPDATE"
; 3203	.ENDIF/MODEL.B
; 3204	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--R						

; 3205	.TOC	"CRAM Macros--R"
; 3206	
; 3207	R0		"FMADR/#B#,ACB/MICROB,AC#/0"	Scratch register 0
; 3208	R1		"FMADR/#B#,ACB/MICROB,AC#/1"	Scratch register 1
; 3209	R10		"FMADR/#B#,ACB/MICROB,AC#/10"	Scratch register 10
; 3210	R11		"FMADR/#B#,ACB/MICROB,AC#/11"	Scratch register 11
; 3211	R12		"FMADR/#B#,ACB/MICROB,AC#/12"	Scratch register 12
; 3212	R13		"FMADR/#B#,ACB/MICROB,AC#/13"	Scratch register 13
; 3213	R14		"FMADR/#B#,ACB/MICROB,AC#/14"	Scratch register 14
; 3214	R15		"FMADR/#B#,ACB/MICROB,AC#/15"	Scratch register 15
; 3215	R16		"FMADR/#B#,ACB/MICROB,AC#/16"	Scratch register 16
; 3216	R17		"FMADR/#B#,ACB/MICROB,AC#/17"	Scratch register 17
; 3217	R2		"FMADR/#B#,ACB/MICROB,AC#/2"	Scratch register 2
; 3218	R3		"FMADR/#B#,ACB/MICROB,AC#/3"	Scratch register 3
; 3219	R4		"FMADR/#B#,ACB/MICROB,AC#/4"	Scratch register 4
; 3220	R5		"FMADR/#B#,ACB/MICROB,AC#/5"	Scratch register 5
; 3221	R6		"FMADR/#B#,ACB/MICROB,AC#/6"	Scratch register 6
; 3222	R7		"FMADR/#B#,ACB/MICROB,AC#/7"	Scratch register 7
; 3223	
; 3224	RD+CLR C CNT	"SPEC/MTR CTL,AR_CACHE CNT"
; 3225	RD+CLR E CNT	"SPEC/MTR CTL,AR_EBOX CNT"
; 3226	RD+CLR PA	"SPEC/MTR CTL,AR_PERF CNT"
; 3227	RD+CLR TB	"SPEC/MTR CTL,AR_TIME BASE"
; 3228	.IF/MODEL.B
; 3229	READ BP2	"MEM/EA CALC,EA CALC/BYTE IND,VMA/VMA,COND/VMA INC"
; 3230	.ENDIF/MODEL.B
; 3231	READ EBR	"MEM/REG FUNC,MREG FNC/READ EBR"
; 3232	READ ERA	"MEM/REG FUNC,MREG FNC/READ ERA"
; 3233	READ UBR	"MEM/REG FUNC,MREG FNC/READ UBR"
; 3234	REL EBUS	"COND/EBUS CTL,EBUS CTL/REL EBUS"
; 3235	REL ECL EBUS	"COND/EBUS CTL,EBUS CTL/REL EEBUS"
; 3236	REQ EBUS	"COND/EBUS CTL,EBUS CTL/REQ EBUS"
; 3237	.IF/KLPAGE
; 3238	REQ SV.VMA	"SV.VMA,ADB/FM,AD/B,VMA/1,MEM/AD FUNC"
; 3239	REQ VMA HELD	"COND/SEL VMA,ADA/PC,AD/A,VMA/1,MEM/AD FUNC"
; 3240	.ENDIF/KLPAGE
; 3241	
; 3242	RET[]		"DISP/RETURN,J/@1"
; 3243	RETURN []	"DISP/RETURN,J/@1"
; 3244	RETURN0		"DISP/RETURN,J/0"
; 3245	RETURN1		"DISP/RETURN,J/1"
; 3246	RETURN10	"DISP/RETURN,J/10"
; 3247	RETURN12	"DISP/RETURN,J/12"
; 3248	RETURN15	"DISP/RETURN,J/15"			;[343]
; 3249	RETURN16	"DISP/RETURN,J/16"			;[337]
; 3250	RETURN17	"DISP/RETURN,J/17"			;[337]
; 3251	RETURN2		"DISP/RETURN,J/2"
; 3252	RETURN20	"DISP/RETURN,J/20"
; 3253	RETURN3		"DISP/RETURN,J/3"
; 3254	RETURN30	"DISP/RETURN,J/30"
; 3255	RETURN37	"DISP/RETURN,J/37"
; 3256	RETURN4		"DISP/RETURN,J/4"
; 3257	RETURN5		"DISP/RETURN,J/5"
; 3258	RETURN6		"DISP/RETURN,J/6"
; 3259	RETURN7		"DISP/RETURN,J/7"
; 3260	RSTR FLAGS_AR	"SPEC/FLAG CTL,FLAG CTL/RSTR FLAGS"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--R						

; 3261	.IF/KLPAGE
; 3262	RSTR VMA_ARX	"ADA/ARX,AD/A,VMA/LOAD,MEM/RESTORE VMA"
; 3263	RSTR VMA_MQ	"ADA/MQ,AD/A,VMA/LOAD,MEM/RESTORE VMA"
; 3264	RSTR VMA_SV.VMA	"SV.VMA,ADB/FM,AD/B,VMA/LOAD,MEM/RESTORE VMA"
; 3265	.ENDIF/KLPAGE; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--S						

; 3266	.TOC	"CRAM Macros--S"
; 3267	
; 3268	.IF/KLPAGE
; 3269	SBR		"P3"
; 3270	.ENDIF/KLPAGE
; 3271	SBUS DIAG	"MEM/REG FUNC,MREG FNC/SBUS DIAG"
; 3272	
; 3273	SC_#		"SCADA/#,SCAD/A,SC/SCAD"
; 3274	SC_# AND AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/AND,SC/SCAD"
; 3275	SC_# AND S	"SCADA/#,SCADB/AR6-11,SCAD/AND,SC/SCAD"
; 3276	SC_# OR SC	"SCADA/#,SCADB/SC,SCAD/OR,SC/SCAD"
; 3277	SC_#+AR0-8	"SCADA/#,SCADB/AR0-8,SCAD/A+B,SC/SCAD"
; 3278	SC_#+SC		"SCADA/#,SCADB/SC,SCAD/A+B,SC/SCAD"
; 3279	SC_#-S		"SCADA/#,SCADB/AR6-11,SCAD/A-B,SC/SCAD"
; 3280	SC_#-SC		"SCADA/#,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3281	SC_#-SC-1	"SCADA/#,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3282	SC_-S		"SCADA EN/0S,SCADB/AR6-11,SCAD/A-B,SC/SCAD";[343]
; 3283	SC_-SC		"SCADA EN/0S,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3284	SC_-SC-1	"SCADA EN/0S,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3285	SC_0		"SCADA EN/0S,SCAD/A,SC/SCAD"
; 3286	SC_1		"SCADA EN/0S,SCAD/A+1,SC/SCAD"
; 3287	SC_1S		"SCADA EN/0S,SCAD/A-1,SC/SCAD"
; 3288	SC_AR0-8	"SCADA EN/0S,SCADB/AR0-8,SCAD/A+B,SC/SCAD" ;[231]
; 3289	SC_EA		"SPEC/SCM ALT,SC/AR SHIFT"
; 3290	SC_EXP		"SCADA/AR EXP,SCAD/A,SC/SCAD"
; 3291	SC_EXP+1	"SCADA/AR EXP,SCAD/A+1,SC/SCAD"
; 3292	SC_EXP+SC	"SCADA/AR EXP,SCADB/SC,SCAD/A+B,SC/SCAD"
; 3293	SC_EXP-#	"SCADA/AR EXP,SCADB/#,SCAD/A-B,SC/SCAD"
; 3294	SC_EXP-1	"SCADA/AR EXP,SCAD/A-1,SC/SCAD"
; 3295	SC_EXP-SC	"SCADA/AR EXP,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3296	SC_FE		"SPEC/SCM ALT,SC/FE"
; 3297	SC_FE AND #	"SCADA/FE,SCADB/#,SCAD/AND,SC/SCAD"
; 3298	SC_FE#		"SCADA/FE,SCAD/A,SC/SCAD"	  ;[337] If SPEC is in conflict
; 3299	SC_FE+#		"SCADA/FE,SCADB/#,SCAD/A+B,SC/SCAD"
; 3300	SC_FE+1		"SCADA/FE,SCAD/A+1,SC/SCAD"
; 3301	SC_FE+S		"SCADA/FE,SCADB/AR6-11,SCAD/A+B,SC/SCAD"
; 3302	SC_FE+SC	"SCADA/FE,SCADB/SC,SCAD/A+B,SC/SCAD"
; 3303	SC_FE-#		"SCADA/FE,SCADB/#,SCAD/A-B,SC/SCAD"
; 3304	SC_FE-1		"SCADA/FE,SCAD/A-1,SC/SCAD"
; 3305	SC_FE-SC	"SCADA/FE,SCADB/SC,SCAD/A-B,SC/SCAD"
; 3306	SC_FE-SC-1	"SCADA/FE,SCADB/SC,SCAD/A-B-1,SC/SCAD"
; 3307	SC_P		"SCADA/AR0-5,SCAD/A,SC/SCAD"
; 3308	SC_P AND #	"SCADA/AR0-5,SCADB/#,SCAD/AND,SC/SCAD"
; 3309	SC_P AND SC	"SCADA/AR0-5,SCADB/SC,SCAD/AND,SC/SCAD"
; 3310	SC_P+1		"SCADA/AR0-5,SCAD/A+1,SC/SCAD"		;[337]
; 3311	SC_P+S		"SCADA/AR0-5,SCADB/AR6-11,SCAD/A+B,SC/SCAD";[343]
; 3312	SC_P-#		"SCADA/AR0-5,SCADB/#,SCAD/A-B,SC/SCAD"
; 3313	SC_S		"SCADB/AR6-11,SCADA EN/0S,SCAD/A+B,SC/SCAD"
; 3314	SC_SC AND #	"SCADA/#,SCADB/SC,SCAD/AND,SC/SCAD"
; 3315	
; 3316	.IF/MODEL.B
; 3317	SEL AC4		"AC-OP/AC+#,AC#/4"
; 3318	SEL DSTP	"AC-OP/AC+#,AC#/4"
; 3319	SEL DSTP2	"AC-OP/AC+#,AC#/5"
; 3320	.ENDIF/MODEL.B
; 3321	SET ACC+CLR UCODE	"COND/EBOX STATE,#/005"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--S						

; 3322	SET ACCOUNT EN	"COND/EBOX STATE,#/105"
; 3323	SET AROV	"COND/PCF_#,PC FLAGS/AROV"
; 3324	SET CONS XCT	"COND/SPEC INSTR,SPEC INSTR/CONS XCT"
; 3325	SET DATAI	"COND/EBUS CTL,EBUS CTL/DATAI,AD/0S,AR/AD"
; 3326	SET DATAO	"COND/EBUS CTL,EBUS CTL/DATAO"
; 3327	SET EBUS DEMAND	"COND/EBUS CTL,EBUS CTL/EBUS DEMAND"
; 3328	SET FL NO DIV	"COND/PCF_#,PC FLAGS/FDV CHK"
; 3329	SET FLAGS_AR	"SPEC/FLAG CTL,FLAG CTL/SET FLAGS"
; 3330	SET FLOV	"COND/PCF_#,PC FLAGS/FLOV"
; 3331	SET FPD		"COND/PCF_#,PC FLAGS/FPD"
; 3332	SET FXU		"COND/PCF_#,PC FLAGS/FXU"	;[224]
; 3333	SET HALTED	"COND/SPEC INSTR,SPEC INSTR/HALTED"
; 3334	SET IO PF	"COND/MBOX CTL,MBOX CTL/SET IO PF ERR"
; 3335	SET MTR PA EN	"COND/EBOX STATE,#/225"
; 3336	SET NO DIVIDE	"COND/PCF_#,PC FLAGS/DIV CHK"
; 3337	SET PC+1 INH	"COND/SPEC INSTR,SPEC INSTR/INH PC+1"
; 3338	SET PI CYCLE	"COND/SPEC INSTR,SPEC INSTR/SET PI CYCLE"
; 3339	SET PXCT	"COND/SPEC INSTR,SPEC INSTR/PXCT"
; 3340	SET SR1		"COND/SR_#,#/64"
; 3341	SET SR2		"COND/SR_#,#/62"
; 3342	SET SR3		"COND/SR_#,#/61"
; 3343	SET SXCT	"COND/SPEC INSTR,SPEC INSTR/SXCT"
; 3344	SET TRACKS EN	"COND/EBOX STATE,#/131"
; 3345	SET TRK+PA EN	"COND/EBOX STATE,#/231"
; 3346	SFLGS		"FMADR/AC0"
; 3347	SFLGS_AR	"SFLGS,FM_AR"
; 3348	SH DISP		"SH/SHIFT AR!ARX,DISP/SH0-3"
; 3349	SIGNS DISP	"DISP/SIGNS"
; 3350	SKIP FETCH	"ADA/AR,AD/A,VMA/PC+1,MEM/FETCH,FETCH/SKIP"
; 3351	
; 3352	SKP -EBUS GRANT	"SKIP/-EBUS GRANT"
; 3353	SKP -EBUS XFER	"SKIP/-EBUS XFER"
; 3354	.IF/MODEL.B
; 3355	SKP -LOCAL AC ADDR	"SKIP/-LOCAL AC ADDR"
; 3356	.ENDIF/MODEL.B
; 3357	SKP -START	"SKIP/-START"
; 3358	.IF/MODEL.B
; 3359	SKP -VMA SEC0	"SKIP/-VMA SEC0"
; 3360	.ENDIF/MODEL.B
; 3361	SKP AC EQ 0	"SKIP/AC#0"			;[343] More mnemonic than AC#0
; 3362	SKP AC REF	"SKIP/AC REF"
; 3363	SKP AC#0	"SKIP/AC#0"
; 3364	SKP AC0+	"FMADR/AC0,ADB/FM,AD/SETCB,SKIP/AD0"
; 3365	SKP AC0-	"FMADR/AC0,ADB/FM,AD/B,SKIP/AD0"
; 3366	SKP AD NE	"SKIP/AD#0"
; 3367	SKP AD NZ	"SKIP/AD#0"			;Mnemonic synonym
; 3368	SKP AD0		"SKIP/AD0"
; 3369	SKP ADX0	"SKIP/ADX0"
; 3370	SKP AR EQ	"ADA EN/0S,ADB/AR*4,AD/ORCB+1,SKIP/AD CRY0"
; 3371	SKP AR EQ -1	"ADA/AR,AD/CRY A EQ -1,SKIP/AD CRY0"
; 3372	SKP AR GT BR	"ADA/AR,ADB/BR,AD/XOR,SKIP/AD CRY0"
; 3373	SKP AR NE	"ADA/AR,AD/CRY A#0,SKIP/AD CRY0"
; 3374	SKP AR NE BR	"ADA/AR,ADB/BR,AD/XOR,SKIP/AD#0"
; 3375	SKP AR NZ	"ADA/AR,AD/A,SKIP/AD#0"			;[343]
; 3376	SKP AR SIG	"ADA/AR,AD/A+XCRY,SPEC/XCRY AR0,SKIP/AD#0"
; 3377	SKP AR0		"SKIP/AR0"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10-2
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--S						

; 3378	SKP AR1		"ADA/AR,AD/A*2,SKIP/AD0"
; 3379	SKP AR18	"SKIP/AR18"
; 3380	SKP AR2		"ADB/AR*4,AD/B,SKIP/AD0"
; 3381	SKP AR6		"SCADB/AR6-11,SCADA/#,#/40,SCAD/AND,SKIP/SCAD#0"
; 3382	SKP ARX LE BRX	"ADA EN/EN,ADB/BR,AD/A-B-1,SKIP/ADX0"
; 3383	SKP ARX LT BRX	"ADA EN/EN,ADB/BR,AD/A-B,SKIP/ADX0"
; 3384	SKP ARX NE	"ADA/ARX,AD/CRY A#0,SKIP/AD CRY0"
; 3385	SKP ARX NZ	"ADA/ARX,AD/A,SKIP/AD#0"		;[343]
; 3386	SKP ARX+MQ NE	"ADA/MQ,AD/CRY A#0,SPEC/AD LONG,SKIP/AD CRY0"
; 3387	SKP ARX0	"SKIP/ARX0"
; 3388	SKP ARX2	"ADB/AR*4,AD/B,SKIP/ADX0"
; 3389	
; 3390	SKP BR EQ	"ADA EN/0S,ADB/BR,AD/CRY A GE B,SKIP/AD CRY0"
; 3391	SKP BR EQ -1	"ADA EN/0S,ADB/BR,AD/A+B+1,SKIP/AD CRY0"
; 3392	SKP BR0		"SKIP/BR0"
; 3393	SKP CRY0	"SKIP/AD CRY0"
; 3394	SKP EVEN PAR	"SKIP/EVEN PAR"
; 3395	SKP EXP NE	"SCADA/AR EXP,SCAD/A,SKIP/SCAD#0"
; 3396	SKP FE NZ	"SCADA/FE,SCAD/A,SKIP/SCAD#0"
; 3397	SKP FE0		"SCADA/FE,SCAD/A,SKIP/SCAD0"
; 3398	SKP FETCH	"SKIP/FETCH"
; 3399	SKP INTRPT	"SKIP/INTRPT"
; 3400	SKP IO LEGAL	"SKIP/IO LEGAL"
; 3401	SKP KERNEL	"SKIP/KERNEL"
; 3402	SKP MQ EQ -1	"ADA/MQ,AD/CRY A EQ -1,SKIP/AD CRY0"
; 3403	SKP MQ NE	"ADA/MQ,AD/CRY A#0,SKIP/AD CRY0"
; 3404	SKP P NE	"SCADA/AR0-5,SCAD/A,SKIP/SCAD#0"
; 3405	SKP P!S XCT	"SKIP/P!S XCT"
; 3406	.IF/MODEL.B
; 3407	SKP PC SEC0	"SKIP/PC SEC0"
; 3408	.ENDIF/MODEL.B
; 3409	SKP PI CYCLE	"SKIP/PI CYCLE"
; 3410	SKP RPW		"SKIP/RPW REF"
; 3411	SKP RUN		"SKIP/RUN"
;;3412	.IFNOT/MODEL.B
;;3413	SKP SC LE 36	"SCADB/SC,SCADA/#,#/-36.,SCAD/A+B,SKIP/SCAD0"
; 3414	.ENDIF/MODEL.B
; 3415	SKP SC NE	"SCADB/SC,SCADA EN/0S,SCAD/A+B,SKIP/SCAD#0"
; 3416	SKP SC NZ	"SCADA EN/0S,SCADB/SC,SCAD/A+B,SKIP/SCAD#0"
; 3417	SKP SC0		"SKIP/SC0"
; 3418	SKP SCAD NE	"SKIP/SCAD#0"
; 3419	SKP SCAD NZ	"SKIP/SCAD#0"			;[347]
; 3420	SKP SCAD0	"SKIP/SCAD0"
; 3421	SKP USER	"SKIP/USER"
; 3422	
; 3423	SLEN		"R10"		;MUST BE 170
; 3424	SLEN_AR		"SLEN,FM_AR"
; 3425	SR DISP		"DISP/SR"
; 3426	SR_#		"COND/SR_#"		;USED FOR NON-PAGE-FAIL APPLICATIONS
; 3427	SR_0		"COND/SR_#,#/0"
; 3428	SR_1		"COND/SR_#,#/1"
; 3429	SR_2		"COND/SR_#,#/2"		;[224]
; 3430	.IF/MODEL.B
; 3431	SR_BDD		"COND/SR_#,#/206"	;B2D AFTER UPDATING DST PTR
; 3432	SR_BDF		"COND/SR_#,#/203"	;B2D STORING FILLERS
; 3433	SR_BDT		"COND/SR_#,#/010"	;B2D IN TRANSLATION; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10-3
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--S						

; 3434	SR_BLT(DST)	"COND/SR_#,#/507"
; 3435	.IF/BLT.PXCT
; 3436	SR_BLT(PXCT DST)"COND/SR_#,#/107"
; 3437	SR_BLT(PXCT SRC)"COND/SR_#,#/307"	;SPECIAL FOR PXCT
; 3438	.ENDIF/BLT.PXCT
; 3439	SR_BLT(SRC)	"COND/SR_#,#/707"
; 3440	SR_DB		"COND/SR_#,#/102"	;D2B ANYWHERE
; 3441	SR_DST		"COND/SR_#,#/212"
; 3442	SR_DSTF		"COND/SR_#,#/214"
; 3443	SR_ED(+D)	"COND/SR_#,#/224"
; 3444	SR_ED(PAT)	"COND/SR_#,#/0"	;PATTERN REF IS EXTENDED
; 3445	SR_ED(S)	"COND/SR_#,#/101"
;;3446	.IFNOT/MODEL.B
;;3447	SR_BDD		"COND/SR_#,#/6"		;B2D AFTER UPDATING DST PTR
;;3448	SR_BDF		"COND/SR_#,#/3"		;B2D STORING FILLERS
;;3449	SR_BDT		"COND/SR_#,#/10"	;B2D IN TRANSLATION
;;3450	SR_BLT(DST)	"COND/SR_#,#/107"	;  BY PXCT 10,4
;;3451	SR_BLT(SRC)	"COND/SR_#,#/607"	;CONTEXT CONTROLLED BY PXCT 2,1
;;3452	SR_DB		"COND/SR_#,#/2"		;D2B ANYWHERE
;;3453	SR_DST		"COND/SR_#,#/12"
;;3454	SR_DSTF		"COND/SR_#,#/14"
;;3455	SR_ED(+D)	"COND/SR_#,#/24"
;;3456	SR_ED(PAT)	"COND/SR_#,#/0"
;;3457	SR_ED(S)	"COND/SR_#,#/1"
; 3458	.ENDIF/MODEL.B
; 3459	.IF/KLPAGE
; 3460	SR_MAP		"COND/SR_#,#/15"	;CATCH MAP PAGE FAILURES
; 3461	.ENDIF/KLPAGE
; 3462	.IF/MODEL.B
; 3463	SR_SRC		"COND/SR_#,#/111"
; 3464	SR_SRC+DST	"COND/SR_#,#/213"
;;3465	.IFNOT/MODEL.B
;;3466	SR_SRC		"COND/SR_#,#/11"
;;3467	SR_SRC+DST	"COND/SR_#,#/13"
; 3468	.ENDIF/MODEL.B
; 3469	SR_WORD		"COND/SR_#,#/17"	;WORD MOVES OF STRING INSTRS
; 3470	.IF/MODEL.B
; 3471	.IF/XADDR
; 3472	SR_XBLT(DST)	"COND/SR_#,#/316"
; 3473	SR_XBLT(SRC)	"COND/SR_#,#/216"
; 3474	.ENDIF/XADDR
; 3475	.ENDIF/MODEL.B
; 3476	SRCP		"FMADR/AC1"
; 3477	SRCP_AR		"SRCP,FM_AR"
; 3478	SRCP2		"FMADR/AC2"
; 3479	SRCP2_AR	"SRCP2,FM_AR"
; 3480	
; 3481	.IF/MODEL.B
; 3482	STACK UPDATE	"SPEC/STACK UPDATE"
; 3483	.ENDIF/MODEL.B
; 3484	STORE		"MEM/WRITE"
; 3485	.IF/BLT.PXCT
; 3486	STORE VMA(EA)_ARX	"VMA/LOAD,MEM/EA CALC,EA CALC/WRITE(E),ADA/ARX,AD/A"
;;3487	.IFNOT/BLT.PXCT
;;3488	STORE VMA(EA)_ARX		"ADA/ARX,AD/A,VMA/AD,STORE"
; 3489	.ENDIF/BLT.PXCT; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10-4
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--S						

; 3490	.IF/KLPAGE
; 3491	SV.AR		"P16"		;156 REQUIRED FOR PF.PAR HACK
;;3492	.IFNOT/KLPAGE
;;3493	SV.AR		"R0"
; 3494	.ENDIF/KLPAGE
; 3495	SV.AR_AR	"SV.AR,COND/FM WRITE"
; 3496	.IF/KLPAGE
; 3497	SV.ARX		"P17"		;157 REQUIRED FOR PF.PAR HACK
;;3498	.IFNOT/KLPAGE
;;3499	SV.ARX		"R1"
; 3500	.ENDIF/KLPAGE
; 3501	SV.ARX_AR	"SV.ARX,COND/FM WRITE"
; 3502	.IF/KLPAGE
; 3503	SV.BR		"P10"
; 3504	SV.BR_AR	"SV.BR,COND/FM WRITE"
; 3505	.ENDIF/KLPAGE
; 3506	SV.IOP		"R3"		;[233]
; 3507	SV.IOPF		"R2"
; 3508	SV.IOPF_AR	"SV.IOPF,COND/FM WRITE"	;IO PAGE FAIL WORD
; 3509	.IF/KLPAGE
; 3510	SV.PAR		"R0"		;Note not in PAGB block
; 3511	SV.PAR_AR	"SV.PAR,COND/FM WRITE"
; 3512	SV.PFW		"P12"
; 3513	SV.PFW_AR	"SV.PFW,COND/FM WRITE"
; 3514	SV.SC		"P11"
; 3515	SV.SC_AR	"SV.SC,COND/FM WRITE"
; 3516	SV.VMA		"P5"
; 3517	SV.VMA_AR	"SV.VMA,COND/FM WRITE"
; 3518	.ENDIF/KLPAGE
; 3519	SWD		"R1"		;BUFFER FOR SOURCE BYTE WORD
; 3520	SWD_AR		"SWD,FM_AR"
; 3521	SWEEP CACHE	"MEM/REG FUNC,MREG FNC/LOAD CCA"
; 3522	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--T, U, V, W, X				

; 3523	.TOC	"CRAM Macros--T, U, V, W, X"
; 3524	
; 3525	T0		"R6"
; 3526	T0_AR		"T0,FM_AR"
; 3527	T1		"R11"
; 3528	T1_AR		"T1,FM_AR"
; 3529	T2		"R12"
; 3530	T2_AR		"T2,FM_AR"
; 3531	TAKE INTRPT	"SKIP/-MTR REQ,J/MTRINT"
; 3532	TEST AR		"ADA/AR,AD/CRY A#0"
; 3533	TEST AR.AC0	"FMADR/AC0,ADB/FM,ADA/AR,AD/CRY A.B#0"
; 3534	TEST AR.BR	"ADB/BR,ADA/AR,AD/CRY A.B#0"
; 3535	TEST AR.MSK	"MSK,ADB/FM,ADA/AR,AD/CRY A.B#0"
; 3536	TEST ARX	"ADA/ARX,AD/CRY A#0"
; 3537	TEST ARX.AR*4	"ADA/ARX,ADB/AR*4,AD/CRY A.B#0"
; 3538	TEST BRL	"ADA EN/0S,ADB/BR,AD/ORCB+1,GEN CRY18"
; 3539	.IF/KLPAGE
; 3540	TEST CBR	"CBR,ADB/FM,AD/B,SKP AD NE"	;[247]
; 3541	.ENDIF/KLPAGE
; 3542	TEST FETCH	"VMA/PC+1,MEM/FETCH,FETCH/TEST"
; 3543	TRAP1		"COND/PCF_#,PC FLAGS/TRAP1"
; 3544	TRAP2		"COND/PCF_#,PC FLAGS/TRAP2"
; 3545	TRAP3		"COND/PCF_#,PC FLAGS/TRAP3"
;;3546	.IF/TRXDEF
;;3547	TRB		"E0"		;same as E0.
;;3548	TRB_AR		"TRB,FM_AR"
;;3549	TRX		"R17"
;;3550	TRX_AR		"TRX,FM_AR"
;;3551	TRX1		"R2"
;;3552	TRX1_AR		"TRX1,FM_AR"
;;3553	TRX2		"R1"
;;3554	TRX2_AR		"TRX2,FM_AR"
;;3555	TRX3		"R14"
;;3556	TRX3_AR		"TRX3,FM_AR"
; 3557	.ENDIF/TRXDEF
; 3558	
; 3559	UNCSH PHYS REF	"SPEC/SP MEM CYCLE,SP MEM/UNCSH+UNPAGE"
; 3560	UPT FETCH	"MEM/LOAD ARX,SPEC/SP MEM CYCLE,SP MEM/UPT FETCH"
; 3561	UPT REF		"SPEC/SP MEM CYCLE,SP MEM/UPT"
; 3562	USER REF	"SPEC/SP MEM CYCLE,SP MEM/USER"
; 3563	
; 3564	VMA_#		"VMA/LOAD,COND/VMA_#"
; 3565	VMA_#+AR32-35	"VMA/LOAD,COND/VMA_#+AR32-35"
; 3566	VMA_40		"VMA/LOAD,COND/VMA_#,#/40"
; 3567	VMA_40+PI*2	"VMA/LOAD,COND/VMA_#+PI*2,#/40"
; 3568	VMA_41		"VMA/LOAD,COND/VMA_#,#/41"
; 3569	VMA_41+PI*2	"VMA/LOAD,COND/VMA_#+PI*2,#/41"
; 3570	VMA_420+TRAP	"VMA/LOAD,COND/VMA_#+TRAP,#/420"
; 3571	VMA_430+MODE	"VMA/LOAD,COND/VMA_#+MODE,#/430"
; 3572	VMA_AC3		"FMADR/AC3,ADB/FM,AD/B,VMA/AD"
; 3573	VMA_AR		"ADA/AR,AD/A,VMA/AD"
; 3574	VMA_AR AND ADMSK "ADMSK,ADB/FM,ADA/AR,AD/AND,VMA/AD"
; 3575	VMA_AR+1	"ADA/AR,AD/A+1,VMA/AD"
; 3576	VMA_AR+BR	"ADA/AR,ADB/BR,AD/A+B,VMA/AD"
; 3577	.IF/KLPAGE
; 3578	VMA_AR+CBR	"CBR,ADB/FM,ADA/AR,AD/A+B,VMA/AD"; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11-1
; MACRO.MIC[10,5351]	19:52 24-Jul-85			CRAM Macros--T, U, V, W, X				

; 3579	.ENDIF/KLPAGE
; 3580	VMA_AR+E0	"E0,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3581	VMA_AR+E0+1	"E0,ADB/FM,ADA/AR,AD/A+B+1,VMA/AD"
; 3582	VMA_AR+E1	"E1,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3583	.IF/KLPAGE
; 3584	VMA_AR+SBR	"SBR,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3585	.ENDIF/KLPAGE
;;3586	.IF/TRXDEF
;;3587	VMA_AR+TRB	"TRB,ADB/FM,ADA/AR,AD/A+B,VMA/AD"
; 3588	.ENDIF/TRXDEF
; 3589	VMA_AR+XR	"GEN AR+XR,VMA/AD"
; 3590	VMA_AR-1	"ADA/AR,AD/A-1,VMA/AD"
; 3591	VMA_ARX		"ADA/ARX,AD/A,VMA/AD"
; 3592	VMA_ARX AND ADMSK "ADMSK,ADB/FM,ADA/ARX,AD/AND,VMA/AD"
; 3593	VMA_ARX+1	"ADA/ARX,AD/A+1,VMA/AD"
; 3594	VMA_ARX+BR	"ADA/ARX,ADB/BR,AD/A+B,VMA/AD"
; 3595	.IF/KLPAGE
; 3596	VMA_ARX+CBR	"CBR,ADB/FM,ADA/ARX,AD/A+B,VMA/AD"
; 3597	.ENDIF/KLPAGE
; 3598	VMA_ARX+XR	"GEN ARX+XR,VMA/AD"
; 3599	
; 3600	VMA_BR		"ADB/BR,AD/B,VMA/AD"
; 3601	VMA_E0+1	"E0,ADB/FM,ADA EN/0S,AD/A+B+1,VMA/AD"
; 3602	VMA_FM[]	"ADA EN/0S,ADB/FM,@1,AD/B,VMA/AD";[344]
; 3603	VMA_MQ		"ADA/MQ,AD/A,VMA/AD"
; 3604	VMA_MQ+1	"ADA/MQ,AD/A+1,VMA/AD"		;[310]
; 3605	VMA_PC		"VMA/PC"			;[252]
; 3606	VMA_PC+1	"VMA/PC+1"
; 3607	.IF/KLPAGE
; 3608	VMA_SV.VMA	"SV.VMA,ADB/FM,AD/B,VMA/AD"
; 3609	.ENDIF/KLPAGE
;;3610	.IF/TRXDEF
;;3611	VMA_TRB		"TRB,ADB/FM,AD/B,VMA/AD"
; 3612	.ENDIF/TRXDEF
;;3613	.IFNOT/MODEL.B
;;3614	VMA_VMA HELD	"COND/SEL VMA,ADA/PC,AD/A,VMA/AD"
; 3615	.IF/MODEL.B
;;3616	.IFNOT/KLPAGE
;;3617	VMA_VMA HELD	"COND/SEL VMA,ADA/PC,AD/A,VMA/AD"
; 3618	.IF/KLPAGE
; 3619	VMA_VMA HELD	"COND/SEL VMA,ADA/PC,AD/A,VMA/AD,MEM/RESTORE VMA"
; 3620	.ENDIF/KLPAGE
; 3621	.ENDIF/MODEL.B
; 3622	VMA_VMA+1	"VMA/VMA,COND/VMA INC"
; 3623	VMA_VMA-1	"VMA/VMA,COND/VMA DEC"
; 3624	
; 3625	WR PT ENTRY	"COND/MBOX CTL,MBOX CTL/PT WR"
; 3626	WR REFILL RAM	"MEM/REG FUNC,MREG FNC/WR REFILL RAM"
; 3627	.IF/MODEL.B
; 3628	WRITE (E)	"MEM/EA CALC,EA CALC/WRITE(E),VMA/LOAD"
; 3629	.ENDIF/MODEL.B
; 3630	
; 3631	XR		"FMADR/XR"
; 3632	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; MACRO.MIC[10,5351]	19:52 24-Jul-85			DRAM Macros						

; 3633	.TOC	"DRAM Macros"
; 3634	
; 3635		.DCODE
; 3636	;
; 3637	;	These macros have not been sorted alphabetically, as (1) there are
; 3638	;	too few to bother, and (2) no one ever looks at the DRAM anyway!
; 3639	;
; 3640	;"A FIELD" MACROS
; 3641	; DECODED TO TELL WHAT TO DO WITH EFFECTIVE ADDRESS
; 3642	; AND WHETHER TO PREFETCH FROM PC+1
; 3643	
; 3644	I	"A/IMMED"
; 3645	I-PF	"A/IMMED-PF"
;;3646	.IFNOT/XADDR
;;3647	EA	"A/IMMED"
; 3648	.IF/XADDR
; 3649	EA	"A/ADDR"
; 3650	.ENDIF/XADDR
; 3651	W	"A/WR-TST"
; 3652	R	"A/READ"
; 3653	R-PF	"A/READ-PF"
; 3654	RW	"A/RD-WR"
; 3655	.IF/RPW
; 3656	RPW	"A/RD-P-WR"
;;3657	.IFNOT/RPW
;;3658	RPW	"A/RD-WR"
; 3659	.ENDIF/RPW
;;3660	.IF/WRTST
;;3661	IW	"A/WR-TST"
; 3662	.IFNOT/WRTST
; 3663	IW	"A/IMMED"
; 3664	.ENDIF/WRTST
; 3665	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; MACRO.MIC[10,5351]	19:52 24-Jul-85			DRAM Macros						

; 3666	;"B FIELD" MACROS
; 3667	; DECODED BY MOST INSTRUCTIONS TO TELL WHERE TO STORE RESULTS,
; 3668	; BUT USED BY OTHERS TO HOLD VARIOUS "MODE" INFORMATION
; 3669	
; 3670	AC	"B/AC"
; 3671	M	"B/MEM"
; 3672	S	"B/SELF"
; 3673	B	"B/BOTH"
; 3674	DBL AC	"B/DBL AC"
; 3675	DBL B	"B/DBL BOTH"
; 3676	FL-AC	"B1-2/AC"
; 3677	FL-MEM	"B1-2/MEM"
; 3678	FL-BOTH	"B1-2/BOTH"
; 3679	
; 3680	TN-	"B0/CRY0(1),B1-2/0"
; 3681	TNE	"B0/CRY0(0),B1-2/0"
; 3682	TNA	"B0/CRY0(0),B1-2/0"
; 3683	TNN	"B0/CRY0(1),B1-2/0"
; 3684	TZ-	"B0/CRY0(1),B1-2/1"
; 3685	TZE	"B0/CRY0(0),B1-2/1"
; 3686	TZA	"B0/CRY0(0),B1-2/1"
; 3687	TZN	"B0/CRY0(1),B1-2/1"
; 3688	TC-	"B0/CRY0(1),B1-2/2"
; 3689	TCE	"B0/CRY0(0),B1-2/2"
; 3690	TCA	"B0/CRY0(0),B1-2/2"
; 3691	TCN	"B0/CRY0(1),B1-2/2"
; 3692	TO-	"B0/CRY0(1),B1-2/3"
; 3693	TOE	"B0/CRY0(0),B1-2/3"
; 3694	TOA	"B0/CRY0(0),B1-2/3"
; 3695	TON	"B0/CRY0(1),B1-2/3"
; 3696	
; 3697	SJC-	"B/SJC-"
; 3698	SJCL	"B/SJCL"
; 3699	SJCE	"B/SJCE"
; 3700	SJCLE	"B/SJCLE"
; 3701	SJCA	"B/SJCA"
; 3702	SJCGE	"B/SJCGE"
; 3703	SJCN	"B/SJCN"
; 3704	SJCG	"B/SJCG"
; 3705	
; 3706	BLKI	"B0/CRY0(0),B1-2/2"
; 3707	BLKO	"B0/CRY0(0),B1-2/0"
; 3708	DATAI	"B/6"
; 3709	DATAO	"B/4"
; 3710	CONI	"B/6"
; 3711	CONO	"B/4"
; 3712	CONSO	"B0/CRY0(1),B1-2/1"
; 3713	CONSZ	"B0/CRY0(0),B1-2/1"
; 3714	
						; 3715	.BIN
						; 3716		.UCODE
						; 3717	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; BASIC.MIC[10,5351]	19:52 24-Jul-85			THE INSTRUCTION LOOP					

						; 3718	.TOC	"THE INSTRUCTION LOOP"
						; 3719	
						; 3720	;	Comments updated [302][344]
						; 3721	;
						; 3722	;INSTRUCTION DECODE, EA COMPUTATION, AND OPERAND FETCH
						; 3723	;
						; 3724	;	IN GENERAL, AN INSTRUCTION IS STARTED AT XCTGO.
						; 3725	; AT THIS TIME THE INSTRUCTION IS IN ARX AND IR, AND PC HAS ITS ADDRESS.
						; 3726	; THE DRAM OUTPUTS AND "AC" BITS WILL SETTLE DURING THIS
						; 3727	; MICROINSTRUCTION, AND WILL BE LATCHED BY THE CLOCK WHICH ENDS
						; 3728	; THE CYCLE.  XCTGO DISPATCHES ON THE STATE OF THE
						; 3729	; INDIRECT AND INDEX BITS OF THE ARX (EA MOD DISP) TO COMPEA OR
						; 3730	; ONE OF THE THREE LOCATIONS FOLLOWING IT.
						; 3731	;	IF INDIRECT IS SPECIFIED, THE INDIRECT POINTER IS FETCHED (AT
						; 3732	; COMPEA+2 OR +3 DEPENDING ON WHETHER INDEXING IS ALSO SPECIFIED).
						; 3733	; WE WAIT FOR IT AT INDRCT, AND THEN LOOP BACK TO COMPEA.  WHEN NO
						; 3734	; INDIRECT IS CALLED FOR, WE COMPUTE THE INSTRUCTION'S EFFECTIVE ADDRESS
						; 3735	; (EA) AT COMPEA OR COMPEA+1 (DEPENDING ON WHETHER INDEXING IS CALLED
						; 3736	; FOR), AND PERFORM THE FUNCTION "A READ", WHOSE OPERATION DEPENDS
						; 3737	; ON THE DRAM A FIELD, AS FOLLOWS:
						; 3738	;
						; 3739	; MACRO	 A-FLD	MEM FUNCTION	VMA	DISPATCH
						; 3740	;  I	  0	NONE		AD(=EA)  DRAM J
						; 3741	; I-PF	  1	FETCH		PC+1	 DRAM J
						; 3742	; EA	  2	30 BIT EA CALC	AD	 DRAM J
						; 3743	;  W	  3	WR TST		AD	   3 (MODEL B)	43 (MODEL A)
						; 3744	;  R	  4	READ		AD	   4		44
						; 3745	; R-PF	  5	READ		AD	   5		45
						; 3746	; RW	  6	READ/WR TST	AD	   6		46
						; 3747	; RPW	  7	RD-PSE/WR TST	AD	   7		47
						; 3748	;
						; 3749	;	A FIELD VALUES 0 AND 1 ARE USED FOR INSTRUCTIONS WHICH NEITHER
						; 3750	; READ NOR WRITE THE CONTENTS OF EA (IMMEDIATE-MODE INSTRUCTIONS,
						; 3751	; JUMPS, ETC).  THESE DISPATCH FROM "A READ" DIRECTLY TO THE MICROCODE
						; 3752	; WHICH HANDLES THE INSTRUCTION.  IF THE A FIELD CONTAINS 1, "A READ"
						; 3753	; CAUSES A PREFETCH (FROM PC+1), SO THAT THE MBOX CAN WORK ON GETTING
						; 3754	; THE NEXT INSTRUCTION INTO ARX WHILE THE EBOX PERFORMS THIS ONE.
						; 3755	;	IF THE A FIELD CONTAINS A 2, THE EA CALCULATION WILL PROVIDE
						; 3756	; A FULL 30 BIT EFFECTIVE ADDRESS TO THE AD AT THE END.  THIS WAS
						; 3757	; INTRODUCED WITH EXTENDED ADDRESSING, TO ALLOW SUCH INSTRUCTIONS AS
						; 3758	; XMOVEI AND XHLLI TO COMPUTE A COMPLETE ADDRESS WITHOUT ACTUALLY
						; 3759	; REFERENCING IT.  OTHERWISE, THIS IS SIMILAR TO AN A FIELD OF 0.  NOTE
						; 3760	; THAT AN A FIELD OF 0 WILL STILL PROVIDE A 30 BIT ADDRESS TO THE VMA;
						; 3761	; ONLY THE RESULT IN THE AD WILL BE DIFFERENT.
						; 3762	;	IF THE A FIELD CONTAINS 3, THE MBOX PERFORMS A PAGING CHECK ON
						; 3763	; EA, AND CAUSES A PAGE FAIL IF THAT LOCATION IS NOT WRITABLE.
						; 3764	; THE MICROCODE GOES TO 3 TO WAIT FOR COMPLETION OF THE PAGE CHECK,
						; 3765	; AND AT THAT LOCATION LOADS AC INTO AR.  THE WRITABILITY OF EA IS
						; 3766	; VERIFIED AT THIS TIME TO PREVENT INCORRECTLY SETTING FLAGS OR
						; 3767	; THE PROCESSOR STATE IF THE INSTRUCTION WILL BE ABORTED BY PAGE
						; 3768	; FAILURE.  LOCATION 3 THEN DISPATCHES TO THE HANDLER FOR THE
						; 3769	; CURRENT INSTRUCTION.
						; 3770	;	A FIELD VALUES 4 TO 7 PERFORM READS FROM EA.  6 AND 7 ALSO TEST
						; 3771	; THE WRITABILITY OF THE LOCATION, AND 7 PERFORMS THE FIRST HALF OF
						; 3772	; A READ-PAUSE-WRITE CYCLE IF EA IS AN UN-CACHED ADDRESS.  THE DISPATCH
						; 3773	; IS TO A, WHERE WE WAIT FOR MEMORY DATA TO ARRIVE IN AR.  IF THE A; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-1
; BASIC.MIC[10,5351]	19:52 24-Jul-85			THE INSTRUCTION LOOP					

						; 3774	; FIELD WAS 5, WE PREFETCH FROM PC+1 AS SOON AS THE DATA ARRIVES.
						; 3775	; IN ANY CASE, WE DISPATCH ACCORDING TO THE DRAM J FIELD TO THE
						; 3776	; HANDLER FOR THE INSTRUCTION.
						; 3777	;	IF A PAGE FAIL OCCURS AT ANY TIME (EITHER IN THIS CODE OR DURING
						; 3778	; INSTRUCTION EXECUTION) THE MICROPROCESSOR TRAPS TO CRAM LOCATION
						; 3779	; 1777 OR 3777, WHERE IT CAUSES A PAGE FAIL TRAP.
						; 3780	;
						; 3781	;	MOST INSTRUCTIONS (THE MOVE, HALFWORD, AND BOOLEAN GROUPS,
						; 3782	; PLUS ADD AND SUB) ARE PERFORMED BY HANDLERS CONSISTING OF ONE OR
						; 3783	; TWO MICROINSTRUCTIONS WHICH LEAVE THE RESULT IN AR, AND COMPLETE
						; 3784	; BY INVOKING THE "EXIT" MACRO.  EXIT USES THE MEM/B WRITE FUNCTION
						; 3785	; TO BEGIN A STORE TO MEMORY FOR THOSE MODES IN WHICH THE RESULT
						; 3786	; GOES TO MEMORY, AND DISP/DRAM B TO GET TO ONE OF THE MICROINSTRUCTIONS
						; 3787	; FOLLOWING ST0.  THIS CODE DEPENDS ON A CERTAIN AMOUNT OF CORRELATION
						; 3788	; BETWEEN THE DRAM A AND B FIELDS.  IN PARTICULAR, STAC (STORE AC)
						; 3789	; ASSUMES THAT A PREFETCH HAS OCCURRED, WHILE THE OTHERS ASSUME THAT
						; 3790	; NO PREFETCH HAS OCCURED.  THUS NORMAL AND IMMEDIATE MODES, WHOSE
						; 3791	; RESULTS GO ONLY TO AC, MUST PREFETCH IN THE DRAM A FIELD, WHILE
						; 3792	; MEM, BOTH, AND SELF MODES, WHOSE RESULTS GO TO MEMORY, MUST NOT.
						; 3793	; (THIS RESTRICTION IS AVOIDED FOR THOSE INSTRUCTIONS WHICH NEVER
						; 3794	; PREFETCH -- IN MUL, DIV, AND IDIV BY USE OF THE EXIT TO ST2AC,
						; 3795	; AND IN IMUL AND THE SINGLE PRECISION FLOATING POINT
						; 3796	; INSTRUCTIONS BY A RESTRICTED EXIT TO ST6.)
						; 3797	;	ANOTHER LARGE SET OF INSTRUCTIONS (SKIP, AOS, SOS, JUMP, AOJ,
						; 3798	; SOJ, AOBJ, CAI, CAM, AND THE TEST GROUP) KNOWS WHERE TO PUT THE
						; 3799	; RESULTS WITHOUT MODE INFORMATION, AND THEY USE THE DRAM B FIELD TO
						; 3800	; DETERMINE WHETHER TO SKIP OR JUMP, AS A FUNCTION OF THEIR OPERANDS.
						; 3801	; SKIP, AOS, AND SOS ARE CONSIDERED SELF-MODE INSTRUCTIONS,
						; 3802	; AND AFTER MAKING THE FETCH DECISION (AND RE-WRITING MEMORY, IN
						; 3803	; THE CASE OF AOS OR SOS), JUMP TO STSELF TO DECIDE WHETHER OR NOT
						; 3804	; TO PUT THE RESULT ALSO IN AC.  THE OTHER INSTRUCTIONS OF THIS SET
						; 3805	; JUMP TO STORAC OR NOP AFTER MAKING THE FETCH DECISION, DEPENDING
						; 3806	; ON WHETHER OR NOT THE OPCODE DEFINITION REQUIRES MODIFICATION OF AC.
						; 3807	; (NOTE THE DIFFERENCE BETWEEN STAC AND FINI ON THE ONE HAND,
						; 3808	; AND STORAC AND NOP ON THE OTHER -- STORAC AND NOP MUST BE USED WHEN
						; 3809	; THE NEXT INSTRUCTION FETCH OCCURS ON THE PRECEDING EBOX CYCLE, BECAUSE
						; 3810	; NICOND MUST NOT IMMEDIATELY FOLLOW A FETCH (ONE CYCLE REQUIRED FOR
						; 3811	; VMA AC REF TO MAKE IT THROUGH THE NICOND LOGIC), STAC AND FINI ARE
						; 3812	; USED WHEN THERE HAS BEEN AN INTERVENING CYCLE.)
						; 3813	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; BASIC.MIC[10,5351]	19:52 24-Jul-85			NEXT INSTRUCTION DISPATCH				

						; 3814	.TOC	"NEXT INSTRUCTION DISPATCH"
						; 3815	
						; 3816	;START BY PUTTING PC WORD IN AR, JUMP HERE
						; 3817	0:
						;;3818	.IFNOT/MODEL.B
						;;3819	START:	SET FLAGS_AR,VMA_AR,MEM/SEC 0,	;LOAD FLAGS, CLEAR VMAX
						;;3820			BR/AR,J/SETPC		;THEN JUMP TO ADDR IN AR
						; 3821	.IF/MODEL.B
U 0000, 1232,0001,0000,0000,0000,0024,0020	; 3822	START:	SET FLAGS_AR,J/SETPC		;LOAD FLAGS, USE REST AS ADDR
						; 3823	.ENDIF/MODEL.B
						; 3824	1:					;MUST BE AT START+1
U 0001, 0075,4001,0000,0000,0117,0010,0000	; 3825	CONT:	VMA/PC,FETCH,J/XCTW		;HERE TO CONTINUE FROM PC
						; 3826	;
						; 3827	;	Comments updated [321].
						; 3828	;
						; 3829	;	DISP/NICOND (THE "NXT INSTR" MACRO) BRINGS US TO ONE OF THE
						; 3830	; LOCATIONS FOLLOWING "NEXT".  PC HAS BEEN UPDATED TO ADDRESS THE NEXT
						; 3831	; INSTRUCTION IN THE NORMAL FLOW, AND IF IT IS FROM MEMORY
						; 3832	; (AS OPPOSED TO AC'S), THE INSTRUCTION IS IN ARX AND IR.
						; 3833	;	THE NICOND DISPATCH IS PRIORITY ENCODED, AS FOLLOWS:
						; 3834	; [FOR FULL DETAILS, SEE PRINT CON2]
						; 3835	;(1)	IF PI CYCLE IS TRUE, GO TO NEXT FOR SECOND HALF
						; 3836	; OF STANDARD OR VECTOR INTERRUPT.
						; 3837	;(2)	IF THE RUN FLOP (CON RUN) IS OFF, GO TO NEXT+2, FROM WHICH THE
						; 3838	; MICROCODE WILL ENTER THE HALT LOOP TO WAIT FOR THE CONSOLE TO RESTART
						; 3839	; INSTRUCTION PROCESSING.
						; 3840	;(3)	IF THE METER HAS A REQUEST, GO TO NEXT+4 (MTRINT) TO SERVE IT.
						; 3841	;(4)	IF THE PI SYSTEM HAS A REQUEST READY, GO TO NEXT+6 (INTRPT)
						; 3842	; TO START A PI CYCLE.
						; 3843	;(5)	IF CON UCODE STATE 05 (TRACK EN) IS SET, GO TO NEXT+10 OR 11.
						; 3844	; Normally NEXT+10 will be used; if a trap flag was set by the previous
						; 3845	; instruction, however, NEXT+11 will be reached.  This is the only
						; 3846	; way the trap will ever be detected, so be cautious of ignoring it.
						; 3847	; THIS FLOP IS ENTIRELY UNDER CONTROL OF THE MICROCODE, AND IS ONLY
						; 3848	; USED FOR THE SPECIAL STATISTICS-GATHERING MICROCODE.
						; 3849	;(6)	IF THE LAST INSTRUCTION SET A TRAP FLAG, GO TO NEXT+13 OR +17,
						; 3850	; IT DOESN'T MATTER WHICH.  (NEXT+17 will be reached if VMA contains
						; 3851	; an AC address, probably irrelevant when a trap flag was set.)
						; 3852	;(7)	IF VMA CONTAINS AN AC ADDRESS, IMPLYING THAT THE NEXT
						; 3853	; INSTRUCTION IS TO COME OUT OF FAST MEMORY, GO TO NEXT+16 TO GET IT.
						; 3854	;(10)	--NORMAL CASE-- THE INSTRUCTION IS IN ARX, READY TO GO, GO
						; 3855	; TO NEXT+12 (XCTGO).
						; 3856	;
						; 3857	;	The NICOND dispatch yields the following:
						; 3858	;
						; 3859	;	+0	CON PI CYCLE
						; 3860	;	+1	Unused
						; 3861	;	+2	-CON RUN (i.e. halt)
						; 3862	;	+3	Unused
						; 3863	;	+4	CON MTR INT REQ (meter interrupt)
						; 3864	;	+5	Unused
						; 3865	;	+6	CON INT REQ (interrupt)
						; 3866	;	+7	Unused
						; 3867	;	+10	CON UCODE STATE 05 (tracks enable)
						; 3868	;	+11	CON UCODE STATE 05+TRAP REQ (tracks enable+trap)
						; 3869	;	+12	-VM AC REF (normal instruction); KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-1
; BASIC.MIC[10,5351]	19:52 24-Jul-85			NEXT INSTRUCTION DISPATCH				

						; 3870	;	+13	-VM AC REF+TRAP REQ (normal instruction+trap)
						; 3871	;	+14	Unused
						; 3872	;	+15	Unused
						; 3873	;	+16	-CON PI CYCLE (AC ref)
						; 3874	;	+17	-CON PI CYCLE+TRAP REQ (AC ref+trap)
						; 3875	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; BASIC.MIC[10,5351]	19:52 24-Jul-85			NEXT INSTRUCTION DISPATCH				

						; 3876	;NICOND (NXT INSTR) DISPATCH BLOCK
						; 3877	
						; 3878	=11*0000				;USE LOC'NS INACCESSIBLE TO DRAM
						; 3879	NEXT:	AR_MQ+1,VMA/AD,			;2ND PART OF INTERRUPT
U 0140, 3633,4023,2000,0000,0320,1510,0714	; 3880			SET PI CYCLE,J/PICYC2	;CONTINUE WITH 41+2N
						; 3881	=0010	CLR AR,ARX_1S,SC_#,#/23.,	;HERE IF RUN FLOP OFF
U 0142, 1454,2341,0200,0302,0000,0450,0027	; 3882			CALL,J/ROTS		;BUILD ADDR MASK
						;;3883	.IFNOT/EXTEXP				;[230]
						;;3884	.IFNOT/OWGBP				;[344]
						;;3885		ADMSK,FM_AR,AR_AR+1,J/CHALT	;SAVE MASK, GO HALT
						;;3886	.IF/OWGBP				;[344]
						;;3887		FM[ADMSK]_AR,AR_AR*2,J/EXMSK	;[230] AR HAS 77,,777776
						;;3888	.ENDIF/OWGBP				;[344]
						; 3889	.IF/EXTEXP				;[230]
U 0143, 0030,3701,5007,0000,0000,1010,0175	; 3890		FM[ADMSK]_AR,AR_AR*2,J/EXMSK	;[230] AR HAS 77,,777776
						; 3891	.ENDIF/EXTEXP				;[230]
						; 3892	=0100
U 0144, 3607,0001,0000,0000,0000,2110,0145	; 3893	MTRINT:	CLR ACCOUNT EN,J/MTRREQ		;HERE IF METER REQUEST UP
U 0145, 3626,0001,3000,0302,0060,0010,0002	; 3894		AR_EBUS,SC_#,#/2,J/PICYC1	;HERE IF TAKE INTRPT DOESNT FIND
						; 3895	=0110					; A METER REQUEST
U 0146, 3626,0001,3000,0302,0060,0010,0002	; 3896	INTRPT:	AR_EBUS,SC_#,#/2,J/PICYC1	;HERE IF INTERRUPT PENDING
						; 3897	;
						; 3898	;	[321] Even if statistics are enabled, traps should not be lost,
						; 3899	;	so override TRACKS and friends when TRAP is set.
						; 3900	;
						;;3901	.IF/TRACKS
						;;3902	=1000	AR_TRX+1,GEN CRY18,SKP CRY0,J/TRK1;STORE PC BEFORE EXECUTING INSTR
						;;3903		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3904	.ENDIF/TRACKS
						;;3905	.IF/OP.CNT
						;;3906	=1000	SC_#,#/9.,SKP USER,J/OPCT1	;COUNT THIS INSTR
						;;3907		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3908	.ENDIF/OP.CNT
						;;3909	.IF/OP.TIME
						;;3910	=1000	AR_2,CLR TRK+PA EN,J/OPTM0	;TIME THIS INSTR
						;;3911		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3912	.ENDIF/OP.TIME
						;;3913	.IF/SO.CNT
						;;3914	=1000
						;;3915	TRK0:	ARX_TRB,BRX/ARX,SKP AC REF,J/TRK1;GET PREV INSTR HOLD THIS INSTR
						;;3916		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3917	.ENDIF/SO.CNT
						;;3918	.IF/SO2.CNT
						;;3919	=1000
						;;3920	TRK0:	ARX_TRB,BRX/ARX,SKP AC REF,J/TRK1;GET PREV INSTR HOLD THIS INSTR
						;;3921		VMA_420+TRAP,J/TRAPX		;[321] Don't lose traps!
						; 3922	.ENDIF/SO2.CNT
						; 3923	=1010
						; 3924	XCTGO:	BRX/ARX,AR_ARX,SET ACCOUNT EN,	;SAVE INSTR, SIGN EXTEND Y,
U 0152, 0174,2341,4022,0000,2000,2136,0105	; 3925			XR,EA MOD DISP,J/COMPEA	; GO CALCULATE EA
U 0153, 0060,4001,0000,0000,0100,3110,0420	; 3926	TRAP:	VMA_420+TRAP,J/TRAPX		;HERE IF TRAP BITS SET
U 0156, 0152,3240,0203,0000,0020,1410,0000	; 3927	=1110	ARX_FM(VMA),TIME/3T,LOAD IR,J/XCTGO	;HERE IF INSTR IS IN FM
U 0157, 0060,4001,0000,0000,0100,3110,0420	; 3928		VMA_420+TRAP,J/TRAPX		;HERE IF TRAP BITS SET
						; 3929	
						; 3930	;HERE ON TRAPS, VMA SETUP WITH 420+TRAP CODE
						; 3931	=11****; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3-1
; BASIC.MIC[10,5351]	19:52 24-Jul-85			NEXT INSTRUCTION DISPATCH				

U 0060, 0074,0001,0000,0000,0013,0026,0033	; 3932	TRAPX:	LOAD ARX,PT REF			;GET AND XCT TRAP INSTR
U 0074, 0075,0001,0000,0000,0000,1510,0100	; 3933	=11****	SET PC+1 INH			;DON'T INCREMENT PC FOR THIS INSTR
						; 3934	
						; 3935	;HERE AFTER FETCHING INSTR TO BE EXECUTED
						; 3936	
						; 3937	=11****
U 0075, 0152,3240,0003,0000,0022,1410,0000	; 3938	XCTW:	ARX_MEM,LOAD IR,J/XCTGO		;GET INSTR TO XCT
						; 3939	.IF/EXTEXP				;[230]
U 0030, 0124,4003,2000,0000,0020,0010,0000	; 3940	EXMSK:	AR_AR+1				;[230] GIVES 77,,777777
U 0124, 0752,4001,0007,0000,0000,1010,0164	; 3941		FM[EXPMSK]_AR,J/CHALT		;[230] MASK FOR FORTRAN EXT EXP
						;;3942	.IFNOT/EXTEXP				;[344]
						;;3943	.IF/OWGBP				;[344]
						;;3944	EXMSK:	AR_AR+1				;[230] GIVES 77,,777777
						;;3945		FM[EXPMSK]_AR,J/CHALT		;[230] MASK FOR FORTRAN EXT EXP
						;;3946	.ENDIF/OWGBP				;[344]
						; 3947	.ENDIF/EXTEXP				;[230]
						; 3948	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; BASIC.MIC[10,5351]	19:52 24-Jul-85			EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH		

						; 3949	.TOC	"EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH"
						; 3950	
						; 3951	.IF/XADDR
						; 3952	=11*0000
						; 3953	EFIW:	AR_ARX (AD),GLOBAL,A INDRCT,	;LONG EXTENDED ADDR
U 0160, 0070,3713,2000,0000,1111,7010,0230	; 3954			SKP INTRPT,J/INDRCT	;WITH INDIRECT SET
						; 3955		AR_ARX+XR,GLOBAL,A INDRCT,
U 0161, 0070,0610,2002,4000,1131,7010,0230	; 3956			SKP INTRPT,J/INDRCT
						; 3957		AR_ARX (AD),GLOBAL,A INDRCT,
U 0162, 0070,3713,2000,0000,1111,7010,0230	; 3958			SKP INTRPT,J/INDRCT
						; 3959		AR_ARX+XR,GLOBAL,A INDRCT,
U 0163, 0070,0610,2002,4000,1131,7010,0230	; 3960			SKP INTRPT,J/INDRCT
						; 3961	
U 0164, 0000,3713,2000,0000,1204,0002,0300	; 3962		AR_ARX (AD),GLOBAL,A READ	;LONG EXTENDED ADDR
U 0165, 0000,0610,2002,4000,1224,0002,0300	; 3963		AR_ARX+XR,GLOBAL,A READ		; WITH INDEXING IN 2-5
U 0166, 0000,3713,2000,0000,1204,0002,0300	; 3964		AR_ARX (AD),GLOBAL,A READ
U 0167, 0000,0610,2002,4000,1224,0002,0300	; 3965		AR_ARX+XR,GLOBAL,A READ
						; 3966	
U 0170, 0131,0001,0000,0000,0000,2210,0400	; 3967	PF24:	GET ECL EBUS,J/ILLIND		;[234]ARX BITS 0,1 = 11
U 0171, 0131,0001,0000,0000,0000,2210,0400	; 3968		GET ECL EBUS,J/ILLIND
U 0172, 0131,0001,0000,0000,0000,2210,0400	; 3969		GET ECL EBUS,J/ILLIND
U 0173, 0131,0001,0000,0000,0000,2210,0400	; 3970		GET ECL EBUS,J/ILLIND
						; 3971	
						; 3972	.ENDIF/XADDR
						; 3973	
						; 3974	=11*1100
U 0174, 0000,3701,0000,0000,0204,0002,0300	; 3975	COMPEA:	GEN AR,A READ			;LOCAL
U 0175, 0000,0600,0002,4000,2224,0002,0300	; 3976		GEN AR+XR,INDEXED,A READ	;LOCAL UNLESS XR>0
						; 3977		GEN AR,A INDRCT,
U 0176, 0070,3701,0000,0000,0111,7010,0230	; 3978			SKP INTRPT,J/INDRCT
						; 3979		GEN AR+XR,INDEXED,A INDRCT,
U 0177, 0070,0600,0002,4000,2131,7010,0230	; 3980			SKP INTRPT,J/INDRCT
						;;3981	.IFNOT/MODEL.B
						;;3982	=11***0
						;;3983	INDRCT:	ARX_MEM,EA TYPE DISP,J/INDR1
						;;3984	TAKINT:	ARX_MEM,TAKE INTRPT
						;;3985	=11**01
						;;3986	INDR1:	AR_ARX,XR,EA MOD DISP,J/COMPEA
						;;3987	.IF/SXCT
						;;3988		AR_ARX (AD),A READ		;HERE IF SXCT 0,
						;;3989	.IFNOT/SXCT
						;;3990	=
						;;3991	.ENDIF/SXCT
						; 3992	.IF/MODEL.B
						;;3993	.IFNOT/XADDR
						;;3994	=11***0
						;;3995	INDRCT:	ARX_MEM,J/INDR1
						;;3996	TAKINT:	ARX_MEM,TAKE INTRPT
						;;3997	
						;;3998	=11****
						;;3999	INDR1:	AR_ARX,EA MOD DISP,J/COMPEA
						; 4000	.IF/XADDR
						; 4001	=11***0
U 0070, 0460,3240,0003,0000,0022,2510,0000	; 4002	INDRCT:	ARX_MEM,LONG EN,J/INDR1
U 0071, 0144,3200,0003,0000,0022,7710,0000	; 4003	TAKINT:	ARX_MEM,TAKE INTRPT
						; 4004	=11****; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4-1
; BASIC.MIC[10,5351]	19:52 24-Jul-85			EFFECTIVE ADDRESS COMPUTATION AND OPERAND FETCH		

						; 4005	INDR1:	AR_ARX,XR,EA MOD DISP,
U 0460, 0160,2341,4002,0301,2020,0036,0024	; 4006			FE_#,#/24,TIME/3T,J/EFIW
						; 4007	
U 0131, 0141,3731,0000,0301,0303,1710,0024	; 4008	ILLIND:	VMA_VMA HELD, FE_#, #/24	;[234]ILLEGAL INDIRECT PF
U 0141, 0147,4001,0000,0302,0000,0010,0140	; 4009		SC_#, #/140			;[234][242]
						; 4010						;[271]MASK TO INSERT PF CODE.
U 0147, 3653,0001,0000,0000,0007,0010,0140	; 4011		MAP, J/PFPAR2			;[234]GET MAP INFO, JOIN PF ROUTINE
						; 4012	.ENDIF/XADDR
						; 4013	.ENDIF/MODEL.B
						; 4014	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; BASIC.MIC[10,5351]	19:52 24-Jul-85			WAIT FOR (E)						

						; 4015	.TOC	"WAIT FOR (E)"
						; 4016	
						; 4017	;THE EXECUTE CODE FOR EACH INSTRUCTION IS ENTERED WITH
						; 4018	; THE OPCODE AND AC # IN BRX AND IR, THE LAST INDIRECT WORD
						; 4019	; IN ARX, AND AR AND VMA SETUP AS A FUNCTION OF THE A
						; 4020	; FIELD OF THE DISPATCH RAM. A PREFETCH IS IN PROGRESS IF THE
						; 4021	; DRAM A FIELD WAS 1 OR 5 (OR IF IR CONTAINS "JRST 0,").
						; 4022	
						; 4023	;ON "A READ", THE HARDWARE DISPATCHES TO THE EXECUTE CODE FOR
						; 4024	; THE INSTRUCTION IF THE DRAM A FIELD IS 0-2.  IF THE A FIELD
						; 4025	; CONTAINS 3-7, THE HARDWARE DISPATCHES TO A (MODEL B), OR 40+A
						; 4026	; (MODEL A), BELOW:
						; 4027	
						; 4028	;COME HERE ON "A READ" FUNCTION IF DRAM A FIELD IS 3
						; 4029	; A "WRITE TST" IS IN PROGRESS
						; 4030	
						; 4031	.IF/MODEL.B
						; 4032	3:
						;;4033	.IFNOT/MODEL.B
						;;4034	43:
						; 4035	.ENDIF/MODEL.B
						; 4036		BR/AR,AR_AC0,MB WAIT,		;WAIT FOR PERMISSION TO WRITE
U 0003, 0000,3240,2040,0000,0022,0001,0000	; 4037			TIME/3T,IR DISP,J/0	;AND GO TO EXECUTE CODE
						; 4038	
						; 4039	;HERE ON "A READ" FUNCTION IF DRAM A FIELD IS 4
						; 4040	; A "LOAD AR" IS IN PROGRESS.  We load FE with 2 for LDB and DPB. [337]
						; 4041	
						; 4042	.IF/MODEL.B
						; 4043	4:
						;;4044	.IFNOT/MODEL.B
						;;4045	44:
						; 4046	.ENDIF/MODEL.B
						; 4047		BR/AR,AR_MEM,TIME/3T,FE_#,#/2,	;GET OPERAND, set up FE for byte
U 0004, 0000,3200,0043,0301,0022,0001,0002	; 4048			IR DISP,J/0		; instructions [337], and go
						; 4049	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; BASIC.MIC[10,5351]	19:52 24-Jul-85			WAIT FOR (E)						

						; 4050	;HERE ON "A READ" IF A FIELD IS 5
						; 4051	; A "LOAD AR" IS IN PROGRESS, AND WE MUST PREFETCH WHEN IT COMPLETES
						; 4052	
						; 4053	.IF/MODEL.B
						; 4054	5:
						;;4055	.IFNOT/MODEL.B
						;;4056	45:
						; 4057	.ENDIF/MODEL.B
						; 4058		BR/AR,FIN XFER,I FETCH,		;GET OPERAND, PREFETCH,
U 0005, 0000,3200,0043,0000,0237,0001,0000	; 4059			TIME/3T,IR DISP,J/0	; & START EXECUTE
						; 4060	
						; 4061	;HERE ON "A READ" IF A FIELD IS 6
						; 4062	; A "LOAD AR" IS IN PROGRESS, BUT PAGING IS TESTING WRITABILITY
						; 4063	; We load FE with 2 for ILDB and IDPB. [337]
						; 4064	.IF/MODEL.B
						; 4065	6:
						;;4066	.IFNOT/MODEL.B
						;;4067	46:
						; 4068	.ENDIF/MODEL.B
						; 4069		BR/AR,AR_MEM,TIME/3T,FE_#,#/2,	;GET OPERAND, load FE for byte
U 0006, 0000,3200,0043,0301,0022,0001,0002	; 4070			IR DISP,J/0		; instructions [337], and go
						; 4071	
						; 4072	;HERE ON "A READ" IF A FIELD IS 7
						; 4073	; A "READ-PAUSE-WRITE" IS IN PROGRESS
						; 4074	
						; 4075	.IF/MODEL.B
						; 4076	7:
						;;4077	.IFNOT/MODEL.B
						;;4078	47:
						; 4079	.ENDIF/MODEL.B
						; 4080		BR/AR,AR_MEM,TIME/3T,		;GET OPERAND
U 0007, 0000,3200,0043,0000,0022,0001,0000	; 4081			IR DISP,J/0		; START EXECUTE
						; 4082	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; BASIC.MIC[10,5351]	19:52 24-Jul-85			TERMINATION						

						; 4083	.TOC	"TERMINATION"
						; 4084	
						; 4085	;DISPATCH HERE WITH THE "EXIT" MACRO,
						; 4086	; OR JUMP DIRECTLY TO ONE OF THESE LOCATIONS.
						; 4087	
						; 4088	=11*000
						; 4089	ST0:					;BASE FOR B DISP IN EXIT MACRO
						; 4090	=001
U 0061, 0241,5441,2000,0000,0237,1016,0000	; 4091	ST2AC:	AC0_AR,AR_SIGN,I FETCH,J/STD1	;HERE TO STORE AC0 & AC1
U 0062, 0061,4001,0003,0000,0002,0010,0000	; 4092		FIN STORE,EXIT DBL		;MULB, DIVB, ETC ...
						; 4093		FIN STORE,I FETCH,		;SELF MODE
U 0063, 0072,4001,0003,0000,0217,4610,0000	; 4094			SKP AC#0,J/STSELF	; RESULT TO AC TOO?
U 0064, 0065,0001,4000,0000,0000,0010,0000	; 4095	SHFLOD:	AR_SHIFT			;[337] Shift byte load result
U 0065, 0140,4001,0000,0403,0002,1006,0000	; 4096	STAC:	AC0_AR,NXT INSTR		;NORMAL AND IMMEDIATE MODES
						; 4097	ST6:
						; 4098	IFNOP:
U 0066, 0073,4001,0003,0000,0217,0010,0000	; 4099	STMEM:	FIN STORE,I FETCH,J/NOP		;MEM MODE
						; 4100	IFSTAC:
U 0067, 0072,0001,0003,0000,0217,0010,0000	; 4101	STBOTH:	FIN STORE,I FETCH,J/STORAC	;BOTH MODE
						; 4102	=
						; 4103	;HERE TO FINISH, AFTER FETCHING NEXT INSTRUCTION.
						; 4104	; WE MUST GUARANTEE AT LEAST ONE EBOX CYCLE BETWEEN FETCH AND NICOND,
						; 4105	; TO ALLOW VMA AC REF TO MAKE IT THROUGH THE NICOND LOGIC.
						; 4106	=11***0
						; 4107	STSELF:					;SKIP, AOS, SOS COME HERE
U 0072, 0065,0001,0000,0000,0000,1610,0000	; 4108	STORAC:	SR_0,J/STAC			;STORE AC, TOO
U 0073, 0221,4001,0000,0000,0000,1610,0000	; 4109	NOP:	SR_0,J/FINI
						; 4110	
						; 4111	=0
U 0020, 0221,4001,0000,0000,0000,0014,0000	; 4112	CLRFPD:	CLR FPD,J/FINI			;CAN'T DO THIS UNTIL STORE COMPLETE
U 0021, 0221,4001,0000,0000,0000,1610,0000	; 4113	NOP2:	SR_0,J/FINI			;THE CODE SEEMS TO WORK WITHOUT THIS
						; 4114						; BUT THE TIMING IS VERY HAIRY AND
						; 4115						; THE HARDWARE PROBABLY ISN'T
						; 4116						; SUPPOSED TO
						; 4117	
U 0221, 0140,0001,0000,0403,0002,0006,0000	; 4118	FINI:	NXT INSTR			;GET NEXT INSTR IN ARX & IR,
						; 4119						; LOAD PC, TEST PI CYCLE, RUN,
						; 4120						; PI READY, TRAPS
						; 4121	
						; 4122	;HERE TO STORE ARITHMETIC DOUBLE RESULTS
						; 4123	
U 0232, 0241,5401,2000,0000,0020,1016,0000	; 4124	DSTAC:	AC0_AR,AR_SIGN			;HERE WITH FETCH STARTED
U 0241, 0136,0001,4000,0000,0000,1610,0000	; 4125	STD1:	AR_SHIFT,SR_0			;BRING IN LOW PART
						; 4126	;
						; 4127	;	[356] The next two locations are fixed at 136 and 137.  We don't
						; 4128	;	really care where they are, but the front end looks at the # field
						; 4129	;	of these to get the microcode major version (# bits 0-5 of 136),
						; 4130	;	minor version (# bits 6-8 of 136), and edit number (# bits 0-8 of
						; 4131	;	137).
						; 4132	;	
						; 4133	136:					;[357]
						; 4134	STAC1:	AC1_AR,FINISH,			;GO DO NEXT INSTRUCTION
U 0136, 0221,4001,0001,0000,0000,1010,0020	; 4135			MAJVER/MAJOR,MINVER/MINOR;[356]
						; 4136	;
						; 4137	;HERE TO GET MICRO-CODE VERSION #.  FIXED LOC'N SO SOFTWARE CAN FIND IT
						; 4138	;; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7-1
; BASIC.MIC[10,5351]	19:52 24-Jul-85			TERMINATION						

						; 4139	137:
U 0137, 3640,4001,0040,0000,0000,0110,0411	; 4140	UVERS:	BR/AR,AR0-8_#,#/EDIT,J/GTAR08	;COPY VERSION TO AR
						; 4141	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; BASIC.MIC[10,5351]	19:52 24-Jul-85			MOVE GROUP, EXCH, BLT					

						; 4142	.TOC	"MOVE GROUP, EXCH, BLT"
						; 4143	
						; 4144		.DCODE
D 0200, 5500,0100				; 4145	200:	R-PF,	AC,	J/MOVE	;BASIC MOVE
D 0201, 1501,0100				; 4146		I-PF,	AC,	J/MOVE
						;;4147	.IF/WRTST
						;;4148		W,	M,	J/MOVE
						; 4149	.IFNOT/WRTST
D 0202, 0101,0103				; 4150		I,	B/1,	J/MOVEM
						; 4151	.ENDIF/WRTST
D 0203, 7301,0100				; 4152		RPW,	S,	J/MOVE
						; 4153	
D 0204, 5500,0002				; 4154	204:	R-PF,	AC,	J/MOVS
D 0205, 1501,0002				; 4155		I-PF,	AC,	J/MOVS
D 0206, 3600,0002				; 4156		W,	M,	J/MOVS
D 0207, 7301,0002				; 4157		RPW,	S,	J/MOVS
						; 4158	
D 0210, 5501,0201				; 4159	210:	R-PF,	AC,	J/MOVN
D 0211, 1500,0201				; 4160		I-PF,	AC,	J/MOVN
D 0212, 3601,0201				; 4161		W,	M,	J/MOVN
D 0213, 7300,0201				; 4162		RPW,	S,	J/MOVN
						; 4163	
D 0214, 5500,0112				; 4164	214:	R-PF,	AC,	J/MOVM
D 0215, 1501,0112				; 4165		I-PF,	AC,	J/MOVM
D 0216, 3600,0112				; 4166		W,	M,	J/MOVM
D 0217, 7301,0112				; 4167		RPW,	S,	J/MOVM
						; 4168		.UCODE
						; 4169	
						; 4170	; ENTER WITH 0,E, (E), OR (AC) IN AR
						; 4171	=0****00****
U 0002, 0060,4001,4000,0000,3005,0033,0000	; 4172	MOVS:	AR_AR SWAP,EXIT			;ALSO USED BY HALFWORD GROUP
						; 4173	=
						; 4174	=0****00****
U 0112, 0100,0001,0040,0000,0000,4510,0000	; 4175	MOVM:	BR/AR,SKP AR0,J/MOVE		;FORCE POSITIVE
						; 4176	=
						; 4177	=0****00****
U 0201, 0101,0001,0040,0000,0000,0010,0000	; 4178	MOVN:	BR/AR,J/MOVNEG			;GET NEGATIVE
						; 4179	=
						; 4180	100:
U 0100, 0060,0001,0000,0000,0005,0033,0000	; 4181	MOVE:	EXIT				;STORE AS IS FROM AR
						; 4182	101:
U 0101, 0100,5142,2000,0000,0022,1310,0000	; 4183	MOVNEG:	AR_-BR,AD FLAGS,FETCH WAIT,J/MOVE
						; 4184	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; BASIC.MIC[10,5351]	19:52 24-Jul-85			XMOVEI, XHLLI, MOVEM, EXCH, BLT				

						; 4185	.TOC	"XMOVEI, XHLLI, MOVEM, EXCH, BLT"
						; 4186	
						; 4187	;HERE FOR XMOVEI (=SETMI), WITH 36-BIT ADDRESS IN VMA AND AR
						; 4188	
						; 4189	.IF/XADDR
						; 4190	102:
U 0102, 0224,0001,0000,0000,0217,5710,0000	; 4191	XMOVEI:	SKP -LOCAL AC ADDR,I FETCH
						; 4192	=0
U 0224, 0065,4061,0000,0000,0021,0017,0002	; 4193	XMOVEI1:ARL_1.M,ARR_ARR,J/STAC		;AC IN NON-ZERO SECTION
U 0225, 0065,0001,0000,0400,3001,0010,0200	; 4194		CLR P,J/STAC			;RETURN 30-BIT ADDRESS
						; 4195	
						; 4196	;HERE FOR XHLLI (=HLLI), WITH 36-BIT ADDRESS IN VMA AND AR
						; 4197	107:
						; 4198	XHLLI:	SKP -LOCAL AC ADDR,I FETCH,
U 0107, 0224,3240,2000,0000,0237,5722,0000	; 4199			ARR_AC0.S,ARL_ARL.S,J/XMOVEI1
						; 4200	.ENDIF/XADDR
						; 4201	
						; 4202	;EXCH, BLT
						; 4203	
						; 4204		.DCODE
D 0250, 7001,0103				; 4205	250:	RPW,	B/0,	J/EXCH
D 0251, 2000,0104				; 4206		EA,		J/BLT
						; 4207		.UCODE
						; 4208	
						; 4209	103:
						; 4210	MOVEM:					;LIKE EXCH, EXCEPT NO STORE AC
U 0103, 1160,3200,2400,0000,1036,0010,0000	; 4211	EXCH:	ARX_AR,AR_AC0,STORE,J/STMAC	;PUT AC AT E, THEN STORE AC
						; 4212	104:
						; 4213	BLT:	MQ_AR,ARX_AR,			;END ADDR TO MQ & ARX
U 0104, 3317,3200,2410,0000,1020,0610,0000	; 4214			ARR_AC0,ARL_ARL,J/BLT1	;FIRST DEST ADDR TO AR
						; 4215	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; BASIC.MIC[10,5351]	19:52 24-Jul-85			HALFWORD GROUP						

						; 4216	.TOC	"HALFWORD GROUP"
						; 4217	;	DESTINATION LEFT HALF
						; 4218	
						; 4219		.DCODE
D 0500, 5500,0106				; 4220	500:	R-PF,	AC,	J/HLL
						; 4221	.IF/XADDR
D 0501, 2500,0107				; 4222		EA,	AC,	J/XHLLI		;GET 36 BIT ADDRESS IN AR AND VMA
						;;4223	.IFNOT/XADDR
						;;4224		I-PF,	AC,	J/HLL
						; 4225	.ENDIF/XADDR
D 0502, 7601,0105				; 4226		RPW,	M,	J/HRR		;HLLM = HRR EXCEPT FOR STORE
D 0503, 7301,0100				; 4227		RPW,	S,	J/MOVE		;HLLS = MOVES
						; 4228	
D 0504, 5500,0205				; 4229		R-PF,	AC,	J/HRL
D 0505, 1501,0205				; 4230		I-PF,	AC,	J/HRL
D 0506, 7601,0114				; 4231		RPW,	M,	J/HRLM
D 0507, 7300,0115				; 4232		RPW,	S,	J/HRLS
						; 4233	
D 0510, 5501,0702				; 4234	510:	R-PF,	AC,	J/HLLZ
D 0511, 1500,0702				; 4235		I-PF,	AC,	J/HLLZ
D 0512, 3601,0702				; 4236		W,	M,	J/HLLZ
D 0513, 7300,0702				; 4237		RPW,	S,	J/HLLZ
						; 4238	
D 0514, 5500,0504				; 4239		R-PF,	AC,	J/HRLZ
D 0515, 1501,0504				; 4240		I-PF,	AC,	J/HRLZ
D 0516, 3600,0504				; 4241		W,	M,	J/HRLZ
D 0517, 7301,0504				; 4242		RPW,	S,	J/HRLZ
						; 4243	
D 0520, 5500,0703				; 4244	520:	R-PF,	AC,	J/HLLO
D 0521, 1501,0703				; 4245		I-PF,	AC,	J/HLLO
D 0522, 3600,0703				; 4246		W,	M,	J/HLLO
D 0523, 7301,0703				; 4247		RPW,	S,	J/HLLO
						; 4248	
D 0524, 5501,0505				; 4249		R-PF,	AC,	J/HRLO
D 0525, 1500,0505				; 4250		I-PF,	AC,	J/HRLO
D 0526, 3601,0505				; 4251		W,	M,	J/HRLO
D 0527, 7300,0505				; 4252		RPW,	S,	J/HRLO
						; 4253	
D 0530, 5501,0503				; 4254	530:	R-PF,	AC,	J/HLLE
D 0531, 1500,0503				; 4255		I-PF,	AC,	J/HLLE
D 0532, 3601,0503				; 4256		W,	M,	J/HLLE
D 0533, 7300,0503				; 4257		RPW,	S,	J/HLLE
						; 4258	
D 0534, 5501,0401				; 4259		R-PF,	AC,	J/HRLE
D 0535, 1500,0401				; 4260		I-PF,	AC,	J/HRLE
D 0536, 3601,0401				; 4261		W,	M,	J/HRLE
D 0537, 7300,0401				; 4262		RPW,	S,	J/HRLE
						; 4263	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; BASIC.MIC[10,5351]	19:52 24-Jul-85			HALFWORD GROUP						

						; 4264	;	DESTINATION RIGHT HALF
						; 4265	
D 0540, 5500,0105				; 4266	540:	R-PF,	AC,	J/HRR
D 0541, 1501,0105				; 4267		I-PF,	AC,	J/HRR
D 0542, 7601,0106				; 4268		RPW,	M,	J/HLL		;HRRM = HLL EXCEPT FOR STORE
D 0543, 7301,0100				; 4269		RPW,	S,	J/MOVE		;HRRS = MOVES
						; 4270	
D 0544, 5500,0206				; 4271		R-PF,	AC,	J/HLR
D 0545, 1501,0206				; 4272		I-PF,	AC,	J/HLR
D 0546, 7600,0402				; 4273		RPW,	M,	J/HLRM
D 0547, 7301,0403				; 4274		RPW,	S,	J/HLRS
						; 4275	
D 0550, 5500,0414				; 4276	550:	R-PF,	AC,	J/HRRZ
D 0551, 1501,0414				; 4277		I-PF,	AC,	J/HRRZ
D 0552, 3600,0414				; 4278		W,	M,	J/HRRZ
D 0553, 7301,0414				; 4279		RPW,	S,	J/HRRZ
						; 4280	
D 0554, 5501,0514				; 4281		R-PF,	AC,	J/HLRZ
D 0555, 1500,0514				; 4282		I-PF,	AC,	J/HLRZ
D 0556, 3601,0514				; 4283		W,	M,	J/HLRZ
D 0557, 7300,0514				; 4284		RPW,	S,	J/HLRZ
						; 4285	
D 0560, 5501,0415				; 4286	560:	R-PF,	AC,	J/HRRO
D 0561, 1500,0415				; 4287		I-PF,	AC,	J/HRRO
D 0562, 3601,0415				; 4288		W,	M,	J/HRRO
D 0563, 7300,0415				; 4289		RPW,	S,	J/HRRO
						; 4290	
D 0564, 5500,0515				; 4291		R-PF,	AC,	J/HLRO
D 0565, 1501,0515				; 4292		I-PF,	AC,	J/HLRO
D 0566, 3600,0515				; 4293		W,	M,	J/HLRO
D 0567, 7301,0515				; 4294		RPW,	S,	J/HLRO
						; 4295	
D 0570, 5500,0212				; 4296	570:	R-PF,	AC,	J/HRRE
D 0571, 1501,0212				; 4297		I-PF,	AC,	J/HRRE
D 0572, 3600,0212				; 4298		W,	M,	J/HRRE
D 0573, 7301,0212				; 4299		RPW,	S,	J/HRRE
						; 4300	
D 0574, 5500,0405				; 4301		R-PF,	AC,	J/HLRE
D 0575, 1501,0405				; 4302		I-PF,	AC,	J/HLRE
D 0576, 3600,0405				; 4303		W,	M,	J/HLRE
D 0577, 7301,0405				; 4304		RPW,	S,	J/HLRE
						; 4305	
						; 4306		.UCODE
						; 4307	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; BASIC.MIC[10,5351]	19:52 24-Jul-85			HALFWORD GROUP						

						; 4308	;FIRST, THE 16 OPS WHICH DO NOT AFFECT THE "OTHER" HALF.
						; 4309	;THESE MUST BE TREATED SEPARATELY, BECAUSE THEY COMBINE MEMORY DATA
						; 4310	;IN AR WITH DATA FROM THE FM.  ENTER WITH 0,E OR (E) IN AR.
						; 4311	
						; 4312	105:
U 0105, 0060,3200,0000,0000,0025,0633,0002	; 4313	HRR:	ARL_AC0,ARR_ARR,EXIT		;HRR, HRRI, HLLM
						; 4314	106:
U 0106, 0060,3200,2000,0000,0025,0633,0000	; 4315	HLL:	ARR_AC0,ARL_ARL,EXIT		;HLL, HLLI, HRRM
						; 4316				;HRRS, HLLS ARE BOTH EQUIVALENT TO MOVES
						; 4317	=0****00****
U 0205, 0060,3240,2000,0000,3025,0633,0004	; 4318	HRL:	ARL_ARR,ARR_AC0,EXIT		;HRL, HRLI
						; 4319	=
						; 4320	=0****00****
U 0206, 0060,3240,4000,0000,3025,0633,0002	; 4321	HLR:	ARR_ARL,ARL_AC0,EXIT		;HLR, HLRI
						; 4322	=
						; 4323	=0****00***0
U 0114, 0002,3240,2000,0000,3020,0610,0004	; 4324	HRLM:	ARL_ARR,ARR_AC0,J/MOVS		;HRLM
U 0115, 0060,4001,0000,0000,3005,0633,0004	; 4325	HRLS:	ARL_ARR,ARR_ARR,EXIT		;HRLS
						; 4326	=
						; 4327	=0****00***0
U 0402, 0002,3240,4000,0000,3020,0610,0002	; 4328	HLRM:	ARR_ARL,ARL_AC0,J/MOVS		;HLRM
U 0403, 0060,4001,4000,0000,3005,0633,0000	; 4329	HLRS:	ARR_ARL,ARL_ARL,EXIT		;HLRS
						; 4330	=
						; 4331	;NOW THE HALFWORD OPS WHICH CONTROL THE "OTHER" HALF
						; 4332	; ENTER WITH 0,E, (E), OR (AC) IN AR
						; 4333	
						; 4334	=0****00****
U 0212, 0414,0001,0000,0000,0000,4410,0000	; 4335	HRRE:	SKP AR18			;SELECT HRRZ OR HRRO ON SIGN
						; 4336	=
						; 4337	=0****00***0
U 0414, 0060,4001,0000,0000,0005,0633,0020	; 4338	HRRZ:	ARL_0S,ARR_ARR,EXIT
U 0415, 0060,2301,0000,0000,0005,0633,0002	; 4339	HRRO:	ARL_1S,ARR_ARR,EXIT
						; 4340	=
						; 4341	=0****00****
U 0401, 0504,0001,0000,0000,0000,4410,0000	; 4342	HRLE:	SKP AR18
						; 4343	=
						; 4344	=0****00***0
U 0504, 0060,3441,2000,0000,3005,0633,0004	; 4345	HRLZ:	ARL_ARR,ARR_0S,EXIT
U 0505, 0060,2341,2000,0000,3005,0633,0004	; 4346	HRLO:	ARL_ARR,ARR_1S,EXIT
						; 4347	=
						; 4348	=0****00****
U 0405, 0514,0001,0000,0000,0000,4510,0000	; 4349	HLRE:	SKP AR0
						; 4350	=
						; 4351	=0****00***0
U 0514, 0060,0001,4000,0000,3005,0633,0020	; 4352	HLRZ:	ARR_ARL,ARL_0S,EXIT
U 0515, 0060,2341,4000,0000,3005,0633,0002	; 4353	HLRO:	ARR_ARL,ARL_1S,EXIT
						; 4354	=
						; 4355	=0****00****
U 0503, 0702,0001,0000,0000,0000,4510,0000	; 4356	HLLE:	SKP AR0
						; 4357	=
						; 4358	=0****00***0
U 0702, 0060,3401,2000,0000,0005,0633,0000	; 4359	HLLZ:	ARR_0S,ARL_ARL,EXIT
U 0703, 0060,2301,2000,0000,0005,0633,0000	; 4360	HLLO:	ARR_1S,ARL_ARL,EXIT
						; 4361	=
						; 4362	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; BASIC.MIC[10,5351]	19:52 24-Jul-85			DMOVE, DMOVN, DMOVEM, DMOVNM				

						; 4363	.TOC	"DMOVE, DMOVN, DMOVEM, DMOVNM"
						; 4364	;DOUBLE-WORD MOVES
						; 4365	
						; 4366		.DCODE
D 0120, 4001,0516				; 4367	120:	R,	B/0,	J/DMOVE
D 0121, 4100,0516				; 4368		R,	B/1,	J/DMOVN
						; 4369		.UCODE
						; 4370	
						; 4371	; ENTER WITH (E) IN AR
						; 4372	=0****00****
						; 4373	DMOVN:
U 0516, 0120,4001,0000,0000,0013,3633,0000	; 4374	DMOVE:	VMA_VMA+1,LOAD ARX,B DISP	;PICK UP (E+1)
						; 4375	=
						; 4376	=00
U 0120, 1731,3200,0003,0000,0022,0010,0000	; 4377	DLOAD:	ARX_MEM,J/STDAC			;[343] GO STORE DOUBLE AC
U 0121, 0431,3200,0003,0000,0022,0062,0100	; 4378		ARX_MEM,MQ_0.S,CALL.S,J/GTDBR	;LOAD BR WITH DOUBLE OPERAND
						; 4379	=11	AR_-BR LONG,AD FLAGS,		;NEGATE DOUBLE OPERAND
U 0123, 0244,5142,2600,0302,0020,1327,0043	; 4380			SC_#,#/35.		;& STORE RESULT
U 0244, 0241,3401,2000,0000,0217,1010,0000	; 4381	DBLST:	AC0_AR,AR_0S,I FETCH,J/STD1	;STORE HIGH WORD, READY LOW
						; 4382	
						; 4383	
						; 4384	;DOUBLE MOVES TO MEMORY
						; 4385	
						; 4386		.DCODE
D 0124, 3001,0110				; 4387	124:	W,		J/DMOVEM
D 0125, 3000,0111				; 4388		W,		J/DMOVNM
						; 4389		.UCODE
						; 4390	
						; 4391	;ENTER WITH (AC) IN AR
						; 4392	=0****00**00
U 0110, 0341,3200,0201,0302,0036,0010,0044	; 4393	DMOVEM:	ARX_AC1,STORE,SC_#,#/36.,J/DMVM1
U 0111, 0431,3240,0201,0000,0020,0062,0100	; 4394	DMOVNM:	ARX_AC1,MQ_0.S,CALL.S,J/GTDBR	;HIGH WORD IS ALREADY IN AR
						; 4395	=11	AR_-BR LONG,AD FLAGS,		;NEGATE
U 0113, 0341,5142,2600,0302,0036,1327,0043	; 4396			STORE,SC_#,#/35.	; & STORE
						; 4397	=
U 0341, 0344,3401,2003,0000,0002,3610,0000	; 4398	DMVM1:	MEM_AR,VMA_VMA+1,AR_0S
U 0344, 0066,4001,4000,0000,0016,0010,0000	; 4399		AR_SHIFT,STORE,J/STMEM
						; 4400	
U 0431, 0446,3701,0500,0000,0000,0010,0000	; 4401	GTDBR:	ARX_ARX*2			;SHIFT OUT LOW SIGN
U 0446, 0003,0001,0060,0000,0000,0003,0000	; 4402	LDBRL:	BR_AR LONG,RETURN3		;COPY TO BR LONG
						; 4403	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 14
; BASIC.MIC[10,5351]	19:52 24-Jul-85			BOOLEAN GROUP						

						; 4404	.TOC	"BOOLEAN GROUP"
						; 4405		.DCODE
D 0400, 1500,0701				; 4406	400:	I-PF,	AC,	J/SETZ
D 0401, 1500,0701				; 4407		I-PF,	AC,	J/SETZ
D 0402, 0601,0701				; 4408		IW,	M,	J/SETZ
D 0403, 0700,0701				; 4409		IW,	B,	J/SETZ
						; 4410		.UCODE
						; 4411	
						; 4412	=0****00****
U 0701, 0060,3401,2000,0000,0005,0033,0000	; 4413	SETZ:	AR_0S,EXIT
						; 4414	=
						; 4415		.DCODE
D 0404, 5501,1001				; 4416	404:	R-PF,	AC,	J/AND
D 0405, 1500,1001				; 4417		I-PF,	AC,	J/AND
D 0406, 7600,1001				; 4418		RPW,	M,	J/AND
D 0407, 7701,1001				; 4419		RPW,	B,	J/AND
						; 4420		.UCODE
						; 4421	
						; 4422	=0****00****
U 1001, 0060,3600,2000,4000,0025,0033,0000	; 4423	AND:	AR_AR*AC0,AD/AND,EXIT
						; 4424	=
						; 4425		.DCODE
D 0410, 5501,1007				; 4426	410:	R-PF,	AC,	J/ANDCA
D 0411, 1500,1007				; 4427		I-PF,	AC,	J/ANDCA
D 0412, 7600,1007				; 4428		RPW,	M,	J/ANDCA
D 0413, 7701,1007				; 4429		RPW,	B,	J/ANDCA
						; 4430		.UCODE
						; 4431	
						; 4432	=0****00****
U 1007, 0060,3500,2000,4000,0025,0033,0000	; 4433	ANDCA:	AR_AR*AC0,AD/ANDCB,EXIT
						; 4434	=
						; 4435		.DCODE
D 0414, 5500,0100				; 4436	414:	R-PF,	AC,	J/MOVE		;SETM = MOVE
						; 4437	.IF/XADDR
D 0415, 2500,0102				; 4438		EA,	AC,	J/XMOVEI	;XMOVEI <=> SETMI
						;;4439	.IFNOT/XADDR
						;;4440		I-PF,	AC,	J/MOVE
						; 4441	.ENDIF/XADDR
D 0416, 7601,0100				; 4442		RPW,	M,	J/MOVE		;SETMM = NOP THAT WRITES MEMORY
D 0417, 7700,0100				; 4443		RPW,	B,	J/MOVE		;SETMB = MOVE THAT WRITES MEMORY
						; 4444	
D 0420, 5500,1116				; 4445	420:	R-PF,	AC,	J/ANDCM
D 0421, 1501,1116				; 4446		I-PF,	AC,	J/ANDCM
D 0422, 7601,1116				; 4447		RPW,	M,	J/ANDCM
D 0423, 7700,1116				; 4448		RPW,	B,	J/ANDCM
						; 4449		.UCODE
						; 4450	
						; 4451	=0****00****
U 1116, 0060,3000,2000,4000,0025,0033,0000	; 4452	ANDCM:	AR_AR*AC0,AD/ANDCA,EXIT
						; 4453	=
						; 4454		.DCODE
D 0424, 5001,0014				; 4455	424:	R-PF,		J/TDN		;SETA = NOP
D 0425, 1000,0014				; 4456		I-PF,		J/TDN		;SETAI IS, TOO
D 0426, 3600,0100				; 4457		W,	M,	J/MOVE		;SETAM = MOVEM
D 0427, 3600,0100				; 4458		W,	M,	J/MOVE		;SETAB, TOO
						; 4459	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15
; BASIC.MIC[10,5351]	19:52 24-Jul-85			BOOLEAN GROUP						

						; 4460		.DCODE
D 0430, 5500,1201				; 4461	430:	R-PF,	AC,	J/XOR
D 0431, 1501,1201				; 4462		I-PF,	AC,	J/XOR
D 0432, 7601,1201				; 4463		RPW,	M,	J/XOR
D 0433, 7700,1201				; 4464		RPW,	B,	J/XOR
						; 4465		.UCODE
						; 4466	
						; 4467	=0****00****
U 1201, 0060,3100,2000,0000,0025,0033,0000	; 4468	XOR:	AR_AR*AC0,AD/XOR,EXIT
						; 4469	=
						; 4470		.DCODE
D 0434, 5500,1202				; 4471	434:	R-PF,	AC,	J/IOR
D 0435, 1501,1202				; 4472		I-PF,	AC,	J/IOR
D 0436, 7601,1202				; 4473		RPW,	M,	J/IOR
D 0437, 7700,1202				; 4474		RPW,	B,	J/IOR
						; 4475		.UCODE
						; 4476	
						; 4477	=0****00****
U 1202, 0060,3300,2000,4000,0025,0033,0000	; 4478	IOR:	AR_AR*AC0,AD/OR,EXIT
						; 4479	=
						; 4480		.DCODE
D 0440, 5501,1205				; 4481	440:	R-PF,	AC,	J/ANDCB
D 0441, 1500,1205				; 4482		I-PF,	AC,	J/ANDCB
D 0442, 7600,1205				; 4483		RPW,	M,	J/ANDCB
D 0443, 7701,1205				; 4484		RPW,	B,	J/ANDCB
						; 4485		.UCODE
						; 4486	
						; 4487	=0****00****
U 1205, 0060,2400,2000,4000,0025,0033,0000	; 4488	ANDCB:	AR_AR*AC0,AD/ANDC,EXIT
						; 4489	=
						; 4490		.DCODE
D 0444, 5501,1206				; 4491	444:	R-PF,	AC,	J/EQV
D 0445, 1500,1206				; 4492		I-PF,	AC,	J/EQV
D 0446, 7600,1206				; 4493		RPW,	M,	J/EQV
D 0447, 7701,1206				; 4494		RPW,	B,	J/EQV
						; 4495		.UCODE
						; 4496	
						; 4497	=0****00****
U 1206, 0060,2600,2000,0000,0025,0033,0000	; 4498	EQV:	AR_AR*AC0,AD/EQV,EXIT
						; 4499	=
						; 4500		.DCODE
D 0450, 1501,1311				; 4501	450:	I-PF,	AC,	J/SETCA
D 0451, 1501,1311				; 4502		I-PF,	AC,	J/SETCA
D 0452, 0600,1311				; 4503		IW,	M,	J/SETCA
D 0453, 0701,1311				; 4504		IW,	B,	J/SETCA
						; 4505		.UCODE
						; 4506	
						; 4507	=0****00****
U 1311, 0060,2500,2000,0000,0025,0033,0000	; 4508	SETCA:	AR_AR*AC0,AD/SETCB,EXIT
						; 4509	=
						; 4510	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16
; BASIC.MIC[10,5351]	19:52 24-Jul-85			BOOLEAN GROUP						

						; 4511		.DCODE
D 0454, 5501,1510				; 4512	454:	R-PF,	AC,	J/ORCA
D 0455, 1500,1510				; 4513		I-PF,	AC,	J/ORCA
D 0456, 7600,1510				; 4514		RPW,	M,	J/ORCA
D 0457, 7701,1510				; 4515		RPW,	B,	J/ORCA
						; 4516		.UCODE
						; 4517	
						; 4518	=0****00****
U 1510, 0060,2700,2000,4000,0025,0033,0000	; 4519	ORCA:	AR_AR*AC0,AD/ORCB,EXIT
						; 4520	=
						; 4521		.DCODE
D 0460, 5500,1511				; 4522	460:	R-PF,	AC,	J/SETCM
D 0461, 1501,1511				; 4523		I-PF,	AC,	J/SETCM
D 0462, 7601,1511				; 4524		RPW,	M,	J/SETCM
D 0463, 7700,1511				; 4525		RPW,	B,	J/SETCM
						; 4526		.UCODE
						; 4527	
						; 4528	=0****00****
U 1511, 0060,2001,2000,0000,0005,0033,0000	; 4529	SETCM:	ADA/AR,AD/SETCA,AR/AD,EXIT
						; 4530	=
						; 4531		.DCODE
D 0464, 5500,1512				; 4532	464:	R-PF,	AC,	J/ORCM
D 0465, 1501,1512				; 4533		I-PF,	AC,	J/ORCM
D 0466, 7601,1512				; 4534		RPW,	M,	J/ORCM
D 0467, 7700,1512				; 4535		RPW,	B,	J/ORCM
						; 4536		.UCODE
						; 4537	
						; 4538	=0****00****
U 1512, 0060,2200,2000,4000,0025,0033,0000	; 4539	ORCM:	AR_AR*AC0,AD/ORCA,EXIT
						; 4540	=
						; 4541		.DCODE
D 0470, 5501,1513				; 4542	470:	R-PF,	AC,	J/ORCB
D 0471, 1500,1513				; 4543		I-PF,	AC,	J/ORCB
D 0472, 7600,1513				; 4544		RPW,	M,	J/ORCB
D 0473, 7701,1513				; 4545		RPW,	B,	J/ORCB
						; 4546		.UCODE
						; 4547	
						; 4548	=0****00****
U 1513, 0060,2100,2000,4000,0025,0033,0000	; 4549	ORCB:	AR_AR*AC0,AD/ORC,EXIT
						; 4550	=
						; 4551		.DCODE
D 0474, 1501,1514				; 4552	474:	I-PF,	AC,	J/SETO
D 0475, 1501,1514				; 4553		I-PF,	AC,	J/SETO
D 0476, 0600,1514				; 4554		IW,	M,	J/SETO
D 0477, 0701,1514				; 4555		IW,	B,	J/SETO
						; 4556		.UCODE
						; 4557	
						; 4558	=0****00****
U 1514, 0060,2301,2000,0000,0005,0033,0000	; 4559	SETO:	AR_1S,EXIT
						; 4560	=
						; 4561	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			TEST GROUP						

						; 4562	.TOC	"TEST GROUP"
						; 4563		.DCODE
D 0600, 1000,0014				; 4564	600:	I-PF,		J/TDN		;TRN- IS NOP
D 0601, 1000,0014				; 4565		I-PF,		J/TDN		;SO IS TLN-
D 0602, 0000,0013				; 4566		I,	TNE,	J/TDXX
D 0603, 0001,0012				; 4567		I,	TNE,	J/TSXX
D 0604, 0000,0010				; 4568		I,	TNA,	J/TDX
D 0605, 0001,0011				; 4569		I,	TNA,	J/TSX
D 0606, 0401,0013				; 4570		I,	TNN,	J/TDXX
D 0607, 0400,0012				; 4571		I,	TNN,	J/TSXX
						; 4572	
D 0610, 1000,0014				; 4573	610:	I-PF,		J/TDN		;TDN- IS A NOP
D 0611, 1000,0014				; 4574		I-PF,		J/TDN		;TSN- ALSO
D 0612, 4001,0013				; 4575		R,	TNE,	J/TDXX
D 0613, 4000,0012				; 4576		R,	TNE,	J/TSXX
D 0614, 4001,0010				; 4577		R,	TNA,	J/TDX
D 0615, 4000,0011				; 4578		R,	TNA,	J/TSX
D 0616, 4400,0013				; 4579		R,	TNN,	J/TDXX
D 0617, 4401,0012				; 4580		R,	TNN,	J/TSXX
						; 4581	
D 0620, 0500,0010				; 4582	620:	I,	TZ-,	J/TDX
D 0621, 0501,0011				; 4583		I,	TZ-,	J/TSX
D 0622, 0101,0013				; 4584		I,	TZE,	J/TDXX
D 0623, 0100,0012				; 4585		I,	TZE,	J/TSXX
D 0624, 0101,0010				; 4586		I,	TZA,	J/TDX
D 0625, 0100,0011				; 4587		I,	TZA,	J/TSX
D 0626, 0500,0013				; 4588		I,	TZN,	J/TDXX
D 0627, 0501,0012				; 4589		I,	TZN,	J/TSXX
						; 4590	
D 0630, 4501,0010				; 4591	630:	R,	TZ-,	J/TDX
D 0631, 4500,0011				; 4592		R,	TZ-,	J/TSX
D 0632, 4100,0013				; 4593		R,	TZE,	J/TDXX
D 0633, 4101,0012				; 4594		R,	TZE,	J/TSXX
D 0634, 4100,0010				; 4595		R,	TZA,	J/TDX
D 0635, 4101,0011				; 4596		R,	TZA,	J/TSX
D 0636, 4501,0013				; 4597		R,	TZN,	J/TDXX
D 0637, 4500,0012				; 4598		R,	TZN,	J/TSXX
						; 4599	
D 0640, 0600,0010				; 4600	640:	I,	TC-,	J/TDX
D 0641, 0601,0011				; 4601		I,	TC-,	J/TSX
D 0642, 0201,0013				; 4602		I,	TCE,	J/TDXX
D 0643, 0200,0012				; 4603		I,	TCE,	J/TSXX
D 0644, 0201,0010				; 4604		I,	TCA,	J/TDX
D 0645, 0200,0011				; 4605		I,	TCA,	J/TSX
D 0646, 0600,0013				; 4606		I,	TCN,	J/TDXX
D 0647, 0601,0012				; 4607		I,	TCN,	J/TSXX
						; 4608	
D 0650, 4601,0010				; 4609	650:	R,	TC-,	J/TDX
D 0651, 4600,0011				; 4610		R,	TC-,	J/TSX
D 0652, 4200,0013				; 4611		R,	TCE,	J/TDXX
D 0653, 4201,0012				; 4612		R,	TCE,	J/TSXX
D 0654, 4200,0010				; 4613		R,	TCA,	J/TDX
D 0655, 4201,0011				; 4614		R,	TCA,	J/TSX
D 0656, 4601,0013				; 4615		R,	TCN,	J/TDXX
D 0657, 4600,0012				; 4616		R,	TCN,	J/TSXX
						; 4617	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			TEST GROUP						

D 0660, 0701,0010				; 4618	660:	I,	TO-,	J/TDX
D 0661, 0700,0011				; 4619		I,	TO-,	J/TSX
D 0662, 0300,0013				; 4620		I,	TOE,	J/TDXX
D 0663, 0301,0012				; 4621		I,	TOE,	J/TSXX
D 0664, 0300,0010				; 4622		I,	TOA,	J/TDX
D 0665, 0301,0011				; 4623		I,	TOA,	J/TSX
D 0666, 0701,0013				; 4624		I,	TON,	J/TDXX
D 0667, 0700,0012				; 4625		I,	TON,	J/TSXX
						; 4626	
D 0670, 4700,0010				; 4627	670:	R,	TO-,	J/TDX
D 0671, 4701,0011				; 4628		R,	TO-,	J/TSX
D 0672, 4301,0013				; 4629		R,	TOE,	J/TDXX
D 0673, 4300,0012				; 4630		R,	TOE,	J/TSXX
D 0674, 4301,0010				; 4631		R,	TOA,	J/TDX
D 0675, 4300,0011				; 4632		R,	TOA,	J/TSX
D 0676, 4700,0013				; 4633		R,	TON,	J/TDXX
D 0677, 4701,0012				; 4634		R,	TON,	J/TSXX
						; 4635		.UCODE
						; 4636	
						; 4637	;THESE 64 INSTRUCTIONS ARE DECODED BY MASK MODE (IMMEDIATE OR MEMORY)
						; 4638	; IN THE A FIELD, DISPATCH TO HERE ON THE J FIELD, AND RE-DISPATCH
						; 4639	; FOR THE MODIFICATION ON THE B FIELD.
						; 4640	
						; 4641	; ENTER WITH 0,E OR (E) IN AR, B FIELD BITS 1 AND 2 AS FOLLOWS:
						; 4642	; 0 0	NO MODIFICATION
						; 4643	; 0 1	ZEROS
						; 4644	; 1 0	COMPLEMENT
						; 4645	; 1 1	ONES
						; 4646	;   THIS ORDER HAS NO SIGNIFICANCE EXCEPT THAT IT CORRESPONDS TO THE
						; 4647	;   ORDER OF INSTRUCTIONS AT TGROUP.
						; 4648	
						; 4649	;THE HIGH ORDER BIT OF THE B FIELD (B0) IS XOR'D WITH AD CRY0 TO
						; 4650	;   DETERMINE THE SENSE OF THE SKIP:
						; 4651	; 0	SKIP IF CRY0=1 (TXX- AND TXXN)
						; 4652	; 1	SKIP IF CRY0=0 (TXXA AND TXXE)
						; 4653	
						; 4654	=0****00*000
						; 4655	TDX:	TEST FETCH,NO CRY,		;TDXA AND TRXA
U 0010, 0014,2001,0000,0000,0226,0033,0203	; 4656			B DISP,J/TDN
						; 4657	
						; 4658	TSX:	AR_AR SWAP,TEST FETCH,NO CRY,	;TSX, TSXA, TLX, AND TLXA
U 0011, 0014,2041,4000,0000,3226,0033,0203	; 4659			B DISP,J/TDN
						; 4660	
U 0012, 0013,4001,4000,0000,3000,0010,0000	; 4661	TSXX:	AR_AR SWAP			;TSXE, TSXN, TLXE, AND TLXN
						; 4662	
U 0013, 0014,3600,0000,0000,0246,0033,0203	; 4663	TDXX:	TEST AR.AC0,TEST FETCH,B DISP	;TDXE, TDXN, TRXE, AND TRXN
						; 4664	
U 0014, 0221,0001,0000,0000,0000,0010,0000	; 4665	TDN:	J/FINI				;NO MODIFICATION
U 0015, 0065,3000,2000,4000,0000,0010,0000	; 4666	TDZ:	AR_AR*AC0,AD/ANDCA,TIME/2T,J/STAC	;ZEROS
U 0016, 0065,3100,2000,0000,0000,0010,0000	; 4667	TDC:	AR_AR*AC0,AD/XOR,TIME/2T,J/STAC		;COMP
U 0017, 0065,3300,2000,4000,0000,0010,0000	; 4668	TDO:	AR_AR*AC0,AD/OR,TIME/2T,J/STAC		;ONES
						; 4669	=
						; 4670	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			COMPARE -- CAI, CAM					

						; 4671	.TOC	"COMPARE -- CAI, CAM"
						; 4672	
						; 4673		.DCODE
D 0300, 0301,1515				; 4674	300:	I,	SJC-,	J/CAIM	;CAI
D 0301, 0200,1515				; 4675		I,	SJCL,	J/CAIM
D 0302, 0100,1515				; 4676		I,	SJCE,	J/CAIM
D 0303, 0001,1515				; 4677		I,	SJCLE,	J/CAIM
D 0304, 0700,1515				; 4678		I,	SJCA,	J/CAIM
D 0305, 0601,1515				; 4679		I,	SJCGE,	J/CAIM
D 0306, 0501,1515				; 4680		I,	SJCN,	J/CAIM
D 0307, 0400,1515				; 4681		I,	SJCG,	J/CAIM
						; 4682	
D 0310, 4300,1515				; 4683	310:	R,	SJC-,	J/CAIM	;CAM
D 0311, 4201,1515				; 4684		R,	SJCL,	J/CAIM
D 0312, 4101,1515				; 4685		R,	SJCE,	J/CAIM
D 0313, 4000,1515				; 4686		R,	SJCLE,	J/CAIM
D 0314, 4701,1515				; 4687		R,	SJCA,	J/CAIM
D 0315, 4600,1515				; 4688		R,	SJCGE,	J/CAIM
D 0316, 4500,1515				; 4689		R,	SJCN,	J/CAIM
D 0317, 4401,1515				; 4690		R,	SJCG,	J/CAIM
						; 4691		.UCODE
						; 4692	
						; 4693	=0****00****
U 1515, 0073,3100,0000,0000,0246,0010,0201	; 4694	CAIM:	GEN AR*AC0,COMP FETCH,J/NOP
						; 4695	=
						; 4696	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			ARITHMETIC SKIPS -- AOS, SOS, SKIP			

						; 4697	.TOC	"ARITHMETIC SKIPS -- AOS, SOS, SKIP"
						; 4698	;ENTER WITH (E) IN AR
						; 4699	
						; 4700		.DCODE
D 0330, 4300,1516				; 4701	330:	R,	SJC-,	J/SKIP	;NOT A NOP IF AC .NE. 0
D 0331, 4201,1516				; 4702		R,	SJCL,	J/SKIP
D 0332, 4101,1516				; 4703		R,	SJCE,	J/SKIP
D 0333, 4000,1516				; 4704		R,	SJCLE,	J/SKIP
D 0334, 4701,1516				; 4705		R,	SJCA,	J/SKIP
D 0335, 4600,1516				; 4706		R,	SJCGE,	J/SKIP
D 0336, 4500,1516				; 4707		R,	SJCN,	J/SKIP
D 0337, 4401,1516				; 4708		R,	SJCG,	J/SKIP
						; 4709		.UCODE
						; 4710	
						; 4711	=0****00****
						; 4712	SKIP:	FIN STORE,SKIP FETCH,
U 1516, 0072,3703,0003,0000,0246,4610,0202	; 4713			SKP AC#0,J/STSELF	;STORE IN SELF MODE
						; 4714	=
						; 4715	
						; 4716		.DCODE
D 0350, 7301,1517				; 4717	350:	RPW,	SJC-,	J/AOS
D 0351, 7200,1517				; 4718		RPW,	SJCL,	J/AOS
D 0352, 7100,1517				; 4719		RPW,	SJCE,	J/AOS
D 0353, 7001,1517				; 4720		RPW,	SJCLE,	J/AOS
D 0354, 7700,1517				; 4721		RPW,	SJCA,	J/AOS
D 0355, 7601,1517				; 4722		RPW,	SJCGE,	J/AOS
D 0356, 7501,1517				; 4723		RPW,	SJCN,	J/AOS
D 0357, 7400,1517				; 4724		RPW,	SJCG,	J/AOS
						; 4725		.UCODE
						; 4726	
						; 4727	=0****00****
U 1517, 1516,4001,2000,0000,0036,1310,0000	; 4728	AOS:	AR_AR+1,AD FLAGS,STORE,J/SKIP
						; 4729	=
						; 4730	
						; 4731		.DCODE
D 0370, 7301,1600				; 4732	370:	RPW,	SJC-,	J/SOS
D 0371, 7200,1600				; 4733		RPW,	SJCL,	J/SOS
D 0372, 7100,1600				; 4734		RPW,	SJCE,	J/SOS
D 0373, 7001,1600				; 4735		RPW,	SJCLE,	J/SOS
D 0374, 7700,1600				; 4736		RPW,	SJCA,	J/SOS
D 0375, 7601,1600				; 4737		RPW,	SJCGE,	J/SOS
D 0376, 7501,1600				; 4738		RPW,	SJCN,	J/SOS
D 0377, 7400,1600				; 4739		RPW,	SJCG,	J/SOS
						; 4740		.UCODE
						; 4741	
						; 4742	=0****00****
U 1600, 1516,1703,2000,0000,0036,1310,0000	; 4743	SOS:	AR_AR-1,AD FLAGS,STORE,J/SKIP
						; 4744	=
						; 4745	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			CONDITIONAL JUMPS -- JUMP, AOJ, SOJ, AOBJ		

						; 4746	.TOC	"CONDITIONAL JUMPS -- JUMP, AOJ, SOJ, AOBJ"
						; 4747	
						; 4748		.DCODE
D 0320, 0301,1601				; 4749	320:	I,	SJC-,	J/JUMP
D 0321, 0200,1601				; 4750		I,	SJCL,	J/JUMP
D 0322, 0100,1601				; 4751		I,	SJCE,	J/JUMP
D 0323, 0001,1601				; 4752		I,	SJCLE,	J/JUMP
D 0324, 0700,1601				; 4753		I,	SJCA,	J/JUMP
D 0325, 0601,1601				; 4754		I,	SJCGE,	J/JUMP
D 0326, 0501,1601				; 4755		I,	SJCN,	J/JUMP
D 0327, 0400,1601				; 4756		I,	SJCG,	J/JUMP
						; 4757		.UCODE
						; 4758	
						; 4759	=0****00****
U 1601, 0073,3200,2000,0000,0246,0010,0502	; 4760	JUMP:	AR_AC0,JUMP FETCH,J/NOP		; E IS IN VMA (HERE AND BELOW)
						; 4761	=
						; 4762	
						; 4763		.DCODE
D 0340, 0301,1602				; 4764	340:	I,	SJC-,	J/AOJ
D 0341, 0200,1602				; 4765		I,	SJCL,	J/AOJ
D 0342, 0100,1602				; 4766		I,	SJCE,	J/AOJ
D 0343, 0001,1602				; 4767		I,	SJCLE,	J/AOJ
D 0344, 0700,1602				; 4768		I,	SJCA,	J/AOJ
D 0345, 0601,1602				; 4769		I,	SJCGE,	J/AOJ
D 0346, 0501,1602				; 4770		I,	SJCN,	J/AOJ
D 0347, 0400,1602				; 4771		I,	SJCG,	J/AOJ
						; 4772		.UCODE
						; 4773	
						; 4774	=0****00****
U 1602, 0072,4640,2000,0000,0266,1310,0502	; 4775	AOJ:	AR_AC0+1,AD FLAGS,JUMP FETCH,J/STORAC
						; 4776	=
						; 4777	
						; 4778		.DCODE
D 0360, 0300,1603				; 4779	360:	I,	SJC-,	J/SOJ
D 0361, 0201,1603				; 4780		I,	SJCL,	J/SOJ
D 0362, 0101,1603				; 4781		I,	SJCE,	J/SOJ
D 0363, 0000,1603				; 4782		I,	SJCLE,	J/SOJ
D 0364, 0701,1603				; 4783		I,	SJCA,	J/SOJ
D 0365, 0600,1603				; 4784		I,	SJCGE,	J/SOJ
D 0366, 0500,1603				; 4785		I,	SJCN,	J/SOJ
D 0367, 0401,1603				; 4786		I,	SJCG,	J/SOJ
						; 4787		.UCODE
						; 4788	
						; 4789	=0****00****
U 1603, 0476,3240,2000,0000,0020,0010,0000	; 4790	SOJ:	AR_AC0
U 0476, 0072,1703,2000,0000,0266,1310,0502	; 4791	=	AR_AR-1,AD FLAGS,JUMP FETCH,J/STORAC
						; 4792	
						; 4793		.DCODE
D 0252, 0601,1604				; 4794	252:	I,	SJCGE,	J/AOBJ
D 0253, 0200,1604				; 4795		I,	SJCL,	J/AOBJ
						; 4796		.UCODE
						; 4797	
						; 4798	=0****00****
U 1604, 0072,4640,2000,0000,0266,0017,0502	; 4799	AOBJ:	AR_AC0+1,GEN CRY18,JUMP FETCH,J/STORAC
						; 4800	=
						; 4801	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			AC DECODE JUMPS -- JRST, JFCL				

						; 4802	.TOC	"AC DECODE JUMPS -- JRST, JFCL"
						; 4803	
						; 4804		.DCODE
D 0254, 2000,0600				; 4805	254:	EA,	J/JRST		;DISPATCHES TO 1 OF 16 ON AC BITS
D 0255, 0401,0700				; 4806		I,TNN,	J/JFCL
						; 4807		.UCODE
						; 4808	
						; 4809	;A READ DETECTS JRST, AND DISPATCHES TO ONE OF 16 LOC'NS ON AC BITS
						; 4810	
						; 4811	600:				;DRAM REQUIRES JRST AT MULTIPLE OF 200
U 0600, 0221,0001,0000,0000,0000,0010,0000	; 4812	JRST:	J/FINI				;(0) A READ PREFETCHES ON JRST 0,
U 0601, 0630,0001,0040,0000,0000,0024,0412	; 4813	601:	PORTAL,BR/AR,J/BRJMP		;(1) PORTAL
						; 4814	602:
U 0602, 0076,2341,0040,0000,0000,0036,0000	; 4815	JRST2:	EA MOD DISP,BR/AR,J/JRSTF	;(2) JRSTF
U 0603, 1002,4001,0000,0000,0000,0010,0000	; 4816	603:	J/UUO				;(3)
U 0604, 1016,0001,0000,0000,0000,6110,0000	; 4817	604:	SKP KERNEL,J/IHALT		;(4) HALT
						; 4818	.IF/LONG.PC
U 0605, 0624,4001,0000,0000,0012,0010,0000	; 4819	605:	LOAD AR,J/XJRSTF1		;(5) XJRSTF -- VALID ANYWHERE
U 0606, 0604,4001,0000,0000,0000,7310,0000	; 4820	606:	SKP IO LEGAL,J/604		;(6) XJEN -- TRAP IF USER
U 0607, 0664,4031,0200,0000,0020,7325,0000	; 4821	607:	SKP IO LEGAL,ARX_PC+1,J/XPCW1	;(7) XPCW -- TRAP IF USER
						;;4822	.IFNOT/LONG.PC
						;;4823	605:	J/UUO				;(5)
						;;4824	606:	J/UUO				;(6)
						;;4825	607:	J/UUO				;(7)
						; 4826	.ENDIF/LONG.PC
U 0610, 0440,0001,0000,0000,0000,7310,0000	; 4827	610:	SKP IO LEGAL,J/DISM		;(10) JRST 10, DISMISS ONLY
U 0611, 1002,4001,0000,0000,0000,0010,0000	; 4828	611:	J/UUO				;(11)
U 0612, 0440,0001,0000,0000,0000,7310,0000	; 4829	612:	SKP IO LEGAL,J/DISM		;(12) JEN
U 0613, 1002,4001,0000,0000,0000,0010,0000	; 4830	613:	J/UUO				;(13)
						; 4831	.IF/LONG.PC
U 0614, 0551,4033,0200,0000,0020,0625,0030	; 4832	614:	AR_0.C,ARX_PC+1,J/XSFM1		;(14) XSFM [331]
U 0615, 0645,0001,0000,0000,0012,0010,0000	; 4833	615:	LOAD AR,J/PCA			;(15) XJRST [301]
						;;4834	.IFNOT/LONG.PC
						;;4835	614:	J/UUO				;(14)
						;;4836	615:	J/UUO				;(15)
						; 4837	.ENDIF/LONG.PC
U 0616, 1002,4001,0000,0000,0000,0010,0000	; 4838	616:	J/UUO				;(16)
U 0617, 1002,4001,0000,0000,0000,0010,0000	; 4839	617:	J/UUO				;(17)
						; 4840	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			AC DECODE JUMPS -- JRST, JFCL				

						; 4841	;HERE TO FINISH THE MORE COMPLEX FORMS OF JRST
						; 4842	
						;;4843	.IFNOT/XADDR
						;;4844	=1110
						;;4845	JRSTF:	AR_ARX,J/RSTF			;NO XR, RESTORE FROM INDIRECT WORD
						;;4846		AR_XR,J/RSTF			;INDEXED, RESTORE FROM REGISTER
						; 4847	.IF/XADDR
						; 4848	=1110
U 0076, 0334,0001,4000,0000,2000,5010,0000	; 4849	JRSTF:	AR_ARX,SKP PC SEC0,J/RSTF0	;NO XR, RESTORE FROM INDIRECT WORD
U 0077, 0334,3240,2002,0000,0020,5010,0000	; 4850		AR_XR,SKP PC SEC0		;INDEXED, RESTORE FROM REGISTER
						; 4851	=0
U 0334, 1002,4001,0000,0000,0000,0010,0000	; 4852	RSTF0:	J/UUO
						; 4853	.ENDIF/XADDR
U 0335, 0630,0001,0000,0000,0000,0024,0420	; 4854	RSTF:	RSTR FLAGS_AR,J/BRJMP
						; 4855	
						; 4856	=0
U 0440, 1002,4001,0000,0000,0000,0010,0000	; 4857	DISM:	J/UUO				;ATTEMPT TO DISMISS FROM USER
U 0441, 0532,4001,0040,0000,0000,0024,0502	; 4858		DISMISS,BR/AR
U 0532, 0020,0001,0000,0000,0000,0001,0000	; 4859		IR DISP,J/20			;SPLIT OUT AGAIN
						; 4860	630:					;JRST 10,
U 0630, 0073,3242,0000,0000,0317,0010,0000	; 4861	BRJMP:	VMA_BR,FETCH,J/NOP		;RELOAD VMA WITH NEW FLAGS
U 0632, 0076,2301,0000,0000,0000,0036,0000	; 4862	632:	EA MOD DISP,J/JRSTF		;JEN, MUST RESTORE FLAGS
						; 4863	
						; 4864	.IF/LONG.PC
U 0551, 0554,3510,0207,0000,0020,6210,0175	; 4865	XSFM1:	ARX_ARX ANDC ADMSK,SKP USER    	;[331] XSFM now legal everywhere
U 0554, 0676,4001,0000,0000,2000,0250,0000	; 4866	=0	AR12-17_PREV SEC,CALL [ARSWAP]	;[334] SAVE PCS IN AR
						; 4867		ARL_ARXL,ARR_ARR,        	;STORE FLAG WORD
U 0555, 0634,0001,0000,0000,2016,0022,0004	; 4868			STORE,J/XSFM2
						; 4869	;					;[334]
						; 4870	
						;;4871	.IFNOT/XADDR
						;;4872	=0
						;;4873	XPCW1:	J/UUO				;BAD USE OF XPCW 
						;;4874		AR_ARX ANDC ADMSK,STORE,	;STORE FLAGS WORD
						;;4875			BRX/ARX,ARX/AD,
						;;4876			IR DISP,J/20		;IS THIS XSFM OR XPCW
						; 4877	.IF/XADDR
						; 4878	=0
U 0664, 1002,4001,0000,0000,0000,0010,0000	; 4879	XPCW1:	J/UUO				;BAD USE OF XPCW
						; 4880		AR_ARX ANDC ADMSK,BRX/ARX,	;SAVE FLAGS,PC IN BRX AND
U 0665, 0621,3510,2227,0000,0020,0010,0175	; 4881			ARX/AD			;FLAGS IN ARX
U 0621, 0670,4001,0040,0000,0000,6222,0030	; 4882		AR_0.S,BR/AR,SKP USER
U 0670, 0676,4001,0000,0000,2000,0250,0000	; 4883	=0	AR12-17_PREV SEC,CALL [ARSWAP]	;[334] SAVE PCS IN AR
						; 4884		ARL_BRL,ARR_ARR,STORE,		;STORE FLAGS WORD
U 0671, 0020,3242,0000,0000,0016,0601,0002	; 4885			IR DISP,J/20		;IS THIS XSFM, OR XPCW?
						; 4886	.ENDIF/XADDR
						; 4887	627:	FIN STORE,AR_ARX*BRX,AD/ANDCA,	;XPCW.  GET ADDRESS TO AR
U 0627, 0636,3002,6003,0000,0016,3610,0000	; 4888			VMA_VMA+1,STORE,J/XPCW2
						; 4889	634:
U 0634, 0073,4001,0003,0000,0217,0010,0000	; 4890	XSFM2:	FIN STORE,I FETCH,J/NOP		;XSFM.  DONE.
						; 4891	
						; 4892	XJRSTF1:FIN XFER,VMA_VMA+1,LOAD AR,	;GETS TO 625 OR 626 FOR XJRSTF
U 0624, 0020,3240,0003,0000,0032,3601,0000	; 4893			IR DISP,J/20		; OR XJEN
						; 4894	
						;;4895	.IFNOT/XADDR
						;;4896	XPCW2:	FIN STORE,VMA_VMA+1,LOAD AR	;STORE PC ADDR, GET NEW FLAGS; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7-1
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			AC DECODE JUMPS -- JRST, JFCL				

						;;4897		FIN XFER,VMA_VMA+1,LOAD AR	;GOT FLAGS GET PC ADDRESS
						;;4898	625:	RSTR FLAGS_AR,AR_MEM,J/ARJMP	;XJRSTF WINDS UP HERE
						;;4899	626:	RSTR FLAGS_AR,AR_MEM		;XJEN WINDS UP HERE
						;;4900		DISMISS,J/ARJMP
						; 4901	.IF/XADDR
U 0636, 0641,4001,0003,0000,0012,3610,0000	; 4902	XPCW2:	FIN STORE,VMA_VMA+1,LOAD AR	;STORE PC ADDR, GET NEW FLAGS
U 0641, 0642,3200,0003,0000,0032,3610,0000	; 4903		FIN XFER,VMA_VMA+1,LOAD AR	;GOT FLAGS, GET PC ADDRESS
U 0642, 0751,3200,0003,0000,0022,0024,0420	; 4904		RSTR FLAGS_AR,AR_MEM,J/ARJMP
						; 4905	625:	RSTR FLAGS_AR,AR_MEM,       	;XJRSTF WINDS UP HERE
						; 4906			ARX_AR SWAP,		;SAVE PCS,FLAGS IN ARX
U 0625, 0734,3200,0403,0000,3022,6224,0420	; 4907			SKP USER,J/XJRSTF2
						; 4908	626:	RSTR FLAGS_AR,AR_MEM,SKP USER,	;XJEN WINDS UP HERE
U 0626, 0674,3200,0403,0000,3022,6224,0420	; 4909			ARX_AR SWAP		;SAVE PCS,FLAGS IN ARX
U 0674, 0734,4001,0000,0000,0000,0024,0502	; 4910	=0	DISMISS,J/XJRSTF2
U 0675, 0751,4001,0000,0000,0000,0024,0502	; 4911		DISMISS,J/ARJMP
						; 4912	=0
U 0734, 0750,4001,0000,0000,0000,6210,0000	; 4913	XJRSTF2:SKP USER,J/XJRSTF3
U 0735, 0073,3703,0000,0000,0317,0010,0000	; 4914		VMA_AR,FETCH,J/NOP
						; 4915	=0
U 0750, 0654,4001,0000,0000,0000,0050,0000	; 4916	XJRSTF3:CALL,J/LDPCS			;RESTORE PCS FROM FLAGS WORD
						; 4917	.ENDIF/XADDR
U 0751, 0073,3703,0000,0000,0317,0010,0000	; 4918	ARJMP:	VMA_AR,FETCH,J/NOP		;DONE
						; 4919	.IF/XADDR
U 0645, 0751,3200,0003,0000,0022,1610,0000	; 4920	PCA:	AR_MEM,SR_0,J/ARJMP		;[301] End of LUUO and XJRST
						; 4921	;
U 0654, 0661,4001,0000,0000,0000,2210,0400	; 4922	LDPCS:	GET ECL EBUS
U 0661, 0666,3711,0000,0000,0060,2010,0426	; 4923		LD PCS
						; 4924		COND/EBUS CTL,EBUS CTL/2,	;RELEASE ECL BUS
U 0666, 0001,0001,0000,0000,0000,2203,0002	; 4925			RETURN1
						; 4926	.ENDIF/XADDR
						; 4927	.ENDIF/LONG.PC
						; 4928	;
						; 4929	;	[334] Subroutine to swap AR halves.  Used in a couple of places.
						; 4930	;
U 0676, 0001,0001,4000,0000,3000,0003,0000	; 4931	ARSWAP:	AR_AR SWAP,RETURN1		;[334] Rearrange things
						; 4932	;
						; 4933	700:					;JFCL MUST BE AT JRST+100
U 0700, 0034,3242,0600,0302,0000,0010,0015	; 4934	JFCL:	ARX_BRX,SC_#,#/13.		;GET BACK AC FIELD
						; 4935	=0*	AR_SHIFT,ARX_0S,		;MOVE AC TO AR32-35
U 0034, 2662,3441,4200,0302,0000,0050,0040	; 4936			SC_#,#/32.,CALL,J/SHIFT	;SHIFTER WILL MOVE TO 0-3
U 0036, 0731,3731,2040,0000,0000,0024,0602	; 4937		BR/AR,AR_PC,JFCL T		;GET PC FLAGS INTO AR
U 0731, 0741,3602,0000,0000,0246,0010,0503	; 4938		TEST AR.BR,JFCL FETCH		;JUMP IF TEST SATISFIED
U 0741, 0745,3502,2000,0000,0000,0010,0000	; 4939		AR_AR*BR,AD/ANDCB		;CLEAR TESTED FLAGS IN AR
U 0745, 0221,4001,0000,0000,0000,0024,0622	; 4940		JFCL S,J/FINI			;SET PC FROM THEM
						; 4941	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			HALT LOOP						

						; 4942	.TOC	"HALT LOOP"
						; 4943	;HERE WHILE PROCESSOR IS "HALTED"
						; 4944	
						; 4945	1016:
						; 4946	UUO107:					;OP 107 COMES HERE
U 1016, 1002,4001,0000,0000,0000,0010,0000	; 4947	IHALT:	J/UUO				;HERE IF HALT NOT IN KERNEL
U 1017, 0752,4001,0000,0000,0000,0024,0442	; 4948	1017:	HALT
						; 4949	
						; 4950	CHALT:	AR_0S,CLR SC,CLR FE,SET HALTED,	;KERNEL OR CONSOLE HALT
U 0752, 0754,3441,2000,0403,0100,1515,0302	; 4951			VMA/PC,PC_VMA		; IF JRST 4, COPY EA TO PC
						;;4952	.IF/PAGCNT				;[327] PFH, DATAO PAG bit 2 counts
						;;4953		TRX2_AR				;[327] Zero count registers
						;;4954		TRX3_AR
						; 4955	.ENDIF/PAGCNT				;[327]
						; 4956	=0
						; 4957	HALT1:	SKP -START,TIME/3T,		;CHECK FOR CONTINUE BUTTON
U 0754, 0764,0001,0400,2421,1020,7110,0000	; 4958			FE_AR0-8,ARX_AR,J/HALT2	;PICK UP OPCODE IN CASE XCT
U 0755, 0144,0001,0000,0000,0000,7710,0000	; 4959		TAKE INTRPT			;HERE IF EXAMINE/DEPOSIT UP
						; 4960	=0
U 0764, 0226,4001,0000,3000,0020,1534,0000	; 4961	HALT2:	GEN FE-1,BYTE DISP,CONTINUE,J/UNHALT	;INSTR FROM SWITCHES?
U 0765, 0754,0001,0000,0000,0000,7010,0000	; 4962		SKP INTRPT,TIME/2T,J/HALT1	;HALT LOOP MUST BE AN ODD
						; 4963						; NUMBER OF TICKS TO ALLOW
						; 4964						; DIAGNOSTICS TO SYNCRONIZE
						; 4965						; EBOX WITH E & SBUS CLK PHASES.
						; 4966	=110
U 0226, 1645,4001,0000,0401,0000,1510,0310	; 4967	UNHALT:	SET CONS XCT,CLR FE,J/UXCT	;XCT ONE FROM "SWITCHES"
U 0227, 0000,5063,0000,0000,0040,5410,0000	; 4968		SKP AR EQ,J/START		;NOT AN INSTR.  START, OR CONT?
						; 4969	
						; 4970	
						; 4971	.TOC	"MAP, XCT"
						; 4972	
						; 4973		.DCODE
D 0256, 4000,1114				; 4974	256:	R,		J/XCT	;OPERAND FETCHED AS DATA
D 0257, 0500,1115				; 4975		I,	AC,	J/MAP
						; 4976		.UCODE
						; 4977	
						; 4978	=0****00***0
U 1114, 1640,0001,0000,0000,0000,7010,0000	; 4979	XCT:	SKP INTRPT,J/XCT1		;CHECK FOR XCT . LOOP
						; 4980	
U 1115, 0756,0001,0040,0000,0007,0010,0140	; 4981	MAP:	MAP,BR/AR			;MAP E, GO READ BACK EBRG
						; 4982	=
						; 4983	.IF/KLPAGE				;IN KL PAGING MODE,
U 0756, 0774,0001,0000,0000,0000,1610,0015	; 4984		SR_MAP				;MAP CAN PAGE FAIL
						; 4985	.ENDIF/KLPAGE
						; 4986	=0
						; 4987	RDEBRG:	AR_0S,SKP IO LEGAL,MB WAIT,	;FINISH READ REG FUNC
U 0774, 3046,3441,2000,0000,0002,7350,0000	; 4988			CALL,J/GETEEB		;AND GET EBUS
U 0775, 0766,0001,3000,0000,0060,2010,0567	; 4989		AR_EBUS REG			;READ DATA
U 0766, 0066,0001,0000,0000,0005,2233,0000	; 4990		REL ECL EBUS,B WRITE,J/ST6	;GIVE IT TO USER
						; 4991	.IF/KLPAGE
						; 4992	=0
U 1020, 1002,0001,0000,0000,0000,1610,0000	; 4993	MAP2:	SR_0,J/UUO			;NO MAPS IN USER MODE
U 1021, 1034,0001,0000,0000,0000,7010,0000	; 4994		SKP INTRPT			;DO NOT BUM THIS CODE OUT
						; 4995	=0					;IT IS NECESSARY TO DO NOTHING
U 1034, 0072,0001,0000,0000,0217,0010,0000	; 4996		I FETCH,J/STORAC		;INTERESTING AFTER A MAP IF
U 1035, 0071,0001,0000,0000,0000,2110,0105	; 4997		SET ACCOUNT EN,J/TAKINT		;AN INTERRUPT IS PENDING; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8-1
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			MAP, XCT						

						; 4998	.ENDIF/KLPAGE
						; 4999	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ		

						; 5000	.TOC	"STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ"
						; 5001	
						; 5002		.DCODE
D 0260, 2000,1315				; 5003	260:	EA,		J/PUSHJ
D 0261, 4001,1314				; 5004		R,	B/0,	J/PUSH
D 0262, 2000,1316				; 5005		EA,	B/0,	J/POP
D 0263, 0000,1317				; 5006		I,		J/POPJ
						; 5007		.UCODE
						; 5008	
						; 5009	;PUSHJ
						; 5010	; ENTER WITH E IN AR
						; 5011	;PUSH
						; 5012	; ENTER WITH (E) IN AR
						; 5013	
						; 5014	=0****00***0
						;;5015	.IFNOT/MODEL.B
						;;5016	PUSH:	ARX_AC0+1,GEN CRY18,SKP CRY0,	;BUMP BOTH HALVES OF AC,
						;;5017			VMA/AD,STORE,J/STMAC	;PUT AR ONTO LIST
						;;5018	
						;;5019	PUSHJ:	BR/AR,AR_PC+1			;SAVE JUMP ADDR, GET PC
						;;5020	=	ARX_AC0+1,GEN CRY18,SKP CRY0,	;COMPUTE STACK ADDRESS
						;;5021			VMA/AD,STORE,J/JSTAC	;AND PREPARE TO STORE PC
						; 5022	.IF/MODEL.B		;COULD SIMPLIFY IFNOT XADDR
U 1314, 1160,4640,0200,0000,0151,5420,0041	; 5023	PUSH:	ARX_AC0+1,PUSH,SKP CRY0,J/STMAC	;BUMP AC ACCORDING TO FORMAT
						; 5024						; AND SECTION NUMBER
U 1315, 1064,4033,2040,0000,0020,5025,0000	; 5025	PUSHJ:	BR/AR,AR_PC+1,SKP PC SEC0	;GET PC WITH FLAGS
						; 5026	=
U 1064, 1065,3600,2007,0000,0020,0010,0175	; 5027	=0	AR_AR AND ADMSK			;STRIP OFF FLAGS IF NOT SEC0
U 1065, 0132,4640,0200,0000,0151,5420,0041	; 5028		ARX_AC0+1,PUSH,SKP CRY0,J/JSTAC	;UPDATE STACK POINTER, STORE
						; 5029	.ENDIF/MODEL.B
						; 5030	=00
U 0130, 0770,3701,0000,0000,0313,0050,0000	; 5031	JRA1:	VMA_AR,LOAD ARX,CALL,J/XFERW	;GET SAVED AC
						; 5032	=10
						; 5033	JSTAC:	FIN STORE,VMA_BR,FETCH,		;STORE PC, JUMP ADDR TO VMA
U 0132, 0072,3202,4003,0000,2317,0010,0000	; 5034			AR_ARX,J/STORAC		;PREPARE TO STORE AC VALUE
U 0133, 0132,4001,0003,0000,0002,1110,0040	; 5035		TRAP2,MEM_AR,J/JSTAC		;CAUSE PDL OVRFLO
						; 5036	
						; 5037	=0
						; 5038	STMAC:	FIN STORE,I FETCH,		;STORE RESULT, GET NEXT INSTR
U 1160, 0072,4001,4003,0000,2217,0033,0000	; 5039			AR_ARX,B DISP,J/STSELF	;STORE AC IF B=0
						; 5040		MEM_AR,TRAP2,			;PDL OVFLO, CAUSE TRAP
U 1161, 0067,0001,4003,0000,2002,1110,0040	; 5041			AR_ARX,J/IFSTAC		;UPDATE AC BEFORE TRAPPING
						; 5042	;
						; 5043	;	A one line subroutine to wait for a memory fetch (either AR
						; 5044	;	or ARX) and return.  Used by all sorts of things.
						; 5045	;
U 0770, 0002,3240,0003,0000,0022,0003,0000	; 5046	XFERW:	AR_MEM,ARX_MEM,TIME/3T,RETURN2	; Cross reference both macros [313]
						; 5047	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			STACK INSTRUCTIONS -- PUSHJ, PUSH, POP, POPJ		

						; 5048	;POP, POPJ
						; 5049	;ENTER WITH E IN AR
						; 5050	
						;;5051	.IFNOT/MODEL.B
						;;5052	=0****00***0
						;;5053	POP:	BR/AR,AR_AC0,VMA/AD,		;GET PUSHDOWN POINTER
						;;5054			LOAD AR,J/POP1		;BEGIN DATA FETCH FROM STACK
						;;5055	
						;;5056	POPJ:	AR_AC0,VMA/AD,LOAD ARX		;START FETCH FROM STACK
						;;5057	=	AR_AR-1,INH CRY18,SKP CRY0	;DECR STACK POINTER, CHECK UNDERFLOW
						;;5058	=0	ARX_MEM,TRAP2,J/POPJ1		;UNDERFLOW OCCURRED
						;;5059		ARX_MEM				;GET STACK WORD
						;;5060	POPJ1:	AC0_AR,VMA_ARX,FETCH,J/NOP	;SET NEW AC VALUE, JUMP
						;;5061	
						;;5062	
						;;5063	POP1:	ARX_AR-1,INH CRY18,SKP CRY0	;ADJUST POINTER, CHECK TRAP
						;;5064	=0	AR_MEM,TRAP2			;PDL OVFLO, CAUSE TRAP
						;;5065		AR_MEM,SR_#,#/100		;SET DEST CONTEXT FLAG
						;;5066		VMA_BR,STORE,SR_0,J/STMAC	;PUT RESULT AWAY, THEN AC
						; 5067	.IF/MODEL.B		;COULD SIMPLIFY IFNOT XADDR
						; 5068	=0****00***0
U 1316, 1015,3200,2040,0000,0131,0010,0421	; 5069	POP:	BR/AR,AR_AC0,POP AR,J/POP2	;GET FROM STACK
						; 5070	
U 1317, 0776,3200,2000,0000,0131,0010,0621	; 5071	POPJ:	AR_AC0,POP AR-ARX		;GET STACK TO AR AND ARX
						; 5072	=	AR_AR-1,TIME/3T,		;BACK OFF POINTER
U 0776, 1174,1701,2000,0000,0020,5420,0000	; 5073			AC0,STACK UPDATE,SKP CRY0	; UNDERFLOW?
						; 5074	=0	BR/AR,AR_MEM,ARL_0.S,		;AC TO BR, HALFWORD PC TO AR
						; 5075			ARX_MEM,TIME/3T,	;FULL PC TO ARX.
U 1174, 1260,3200,0043,0000,0022,5022,0020	; 5076			SKP PC SEC0,J/POPJT	; GO SET TRAP
						; 5077		BR/AR,AR_MEM,ARL_0.S,		;AC TO BR, HALFWORD PC TO AR
						; 5078			ARX_MEM,TIME/3T,	;FULL PC TO ARX.
U 1175, 1254,3240,0043,0000,0022,5022,0020	; 5079			SKP PC SEC0		;EXTENDED FORM?
						; 5080	=0
U 1254, 1003,3711,0000,0000,0317,0010,0000	; 5081	POPJ2:	VMA_ARX,FETCH,J/POPJ4		;YES.  LOAD ENTIRE ADDR
U 1255, 1003,3703,0000,0000,0317,0010,0000	; 5082	POPJ3:	VMA_AR,FETCH			;NO, LOAD HALFWORD ADDRESS
U 1003, 0065,3242,2000,0000,0000,0010,0000	; 5083	POPJ4:	AR_BR,J/STAC			;GET AC BACK, GO STUFF IT
						; 5084	
						; 5085	;HERE IF POPJ GETS STACK UNDERFLOW
						; 5086	; SKIP IF PC SECTION 0
						; 5087	=0
U 1260, 1254,4001,0000,0000,0000,1110,0040	; 5088	POPJT:	TRAP2,J/POPJ2
U 1261, 1255,0001,0000,0000,0000,1110,0040	; 5089		TRAP2,J/POPJ3
						; 5090	
						; 5091	POP2:	ARX_AR-1,AC0,STACK UPDATE,	;BACK UP POINTER
U 1015, 1264,1703,0200,0000,0040,5420,0000	; 5092			SKP CRY0
U 1264, 1023,3200,0003,0000,0022,1110,0040	; 5093	=0	AR_MEM,TRAP2,J/POP3
U 1265, 1023,3240,0003,0000,0022,0010,0000	; 5094		AR_MEM
U 1023, 1160,3242,0000,0000,0111,0010,0042	; 5095	POP3:	GEN BR,WRITE (E),J/STMAC	;STORE RESULT & AC
						; 5096	.ENDIF/MODEL.B
						; 5097	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			SUBROUTINE CALL/RETURN -- JSR, JSP, JSA, JRA		

						; 5098	.TOC	"SUBROUTINE CALL/RETURN -- JSR, JSP, JSA, JRA"
						; 5099	
						; 5100		.DCODE
D 0264, 2001,1401				; 5101	264:	EA,		J/JSR		; [301] Make sure these can cross
D 0265, 2000,1400				; 5102		EA,		J/JSP		; section boundaries
D 0266, 0000,1402				; 5103		I,		J/JSA
D 0267, 0001,1403				; 5104		I,		J/JRA
						; 5105		.UCODE
						; 5106	
						; 5107	=0****00***0
						;;5108	.IFNOT/XADDR
						;;5109	JSP:	AR_PC+1,FETCH,J/STORAC
						;;5110	
						;;5111	JSR:	AR_PC+1,STORE
						;;5112	=	FIN STORE,VMA_VMA+1,FETCH,J/NOP
						; 5113	.IF/XADDR
U 1400, 1274,4031,2000,0000,0037,5025,0000	; 5114	JSP:	AR_PC+1,FETCH,SKP PC SEC0,J/JSP1
						; 5115	
U 1401, 1270,4033,2000,0000,0020,5025,0000	; 5116	JSR:	AR_PC+1,SKP PC SEC0
						; 5117	=
U 1270, 1025,3600,2007,0000,0036,0010,0175	; 5118	=0	AR_AR AND ADMSK,STORE,J/JSR1
U 1271, 1025,0001,0000,0000,0016,0010,0000	; 5119		STORE				;IN SECT 0, SAVE FLAGS, TOO
U 1025, 0073,0001,0003,0000,0017,3610,0000	; 5120	JSR1:	FIN STORE,VMA_VMA+1,FETCH,J/NOP
						; 5121	
						; 5122	=0
U 1274, 0065,3600,2007,4000,0020,0010,0175	; 5123	JSP1:	AR_AR AND ADMSK,J/STAC		;NON-ZERO SEC, NO FLAGS
U 1275, 0221,4001,0000,0000,0000,1010,0000	; 5124		AC0_AR,J/FINI
						; 5125	.ENDIF/XADDR
						; 5126	
						; 5127	
						; 5128	=0****00***0
U 1402, 1044,3200,2400,0000,3036,0010,0000	; 5129	JSA:	ARX_AR SWAP,AR_AC0,STORE,J/JSA1	;SAVE E IN ARX LEFT, GET AC
						; 5130	
						; 5131	JRA:
						;;5132	.IFNOT/XADDR				;[235]
						;;5133		BR/AR, AR_AC0			;get AC, save jump address.
						;;5134	=
						;;5135		ARR_ARL, ARL_0.M, J/JRA1	;E to ARR.
						; 5136	.IF/XADDR				;[235]
U 1403, 1032,4001,0000,0000,1000,0210,0000	; 5137		AR12-17_PC SEC			;[235] put section in jump address.
						; 5138	=
U 1032, 1334,3200,2040,0000,0020,5010,0000	; 5139		BR/AR, AR_AC0, SKP PC SEC0	;[235] save jump address, get AC.
U 1334, 1042,4001,4000,0000,3001,0010,0020	; 5140	=0	ARR_ARL, ARL_0.M, J/JRAB	;[235] in section part to ARR.
U 1335, 0130,4001,4000,0000,3001,0010,0020	; 5141		ARR_ARL, ARL_0.M, J/JRA1	;[235] E to ARR.
U 1042, 0130,0001,0000,0000,1000,0210,0000	; 5142	JRAB:	AR12-17_PC SEC, J/JRA1		;[235] section to ARL.
						; 5143	.ENDIF/XADDR				;[235]
						; 5144	
U 1044, 1050,0001,0003,0000,0017,3610,0000	; 5145	JSA1:	FIN STORE,VMA_VMA+1,FETCH	;JUMP TO E+1
U 1050, 0065,4033,2000,0000,2020,0022,0004	; 5146		ARR_PC+1,ARL_ARXL,J/STAC	;PC+1,,E GOES TO AC
						; 5147	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			UUO'S							

						; 5148	.TOC	"UUO'S"
						; 5149	;LUUO'S TRAP TO CURRENT CONTEXT
						; 5150	; EXTENDED INSTRUCTION SET IS "HIDDEN" BENEATH LUUO OPCODES
						; 5151	;
						; 5152	;	WARNING:  use extreme caution if E1 for MOVSRJ or CMPSE should
						; 5153	;	ever be used for anything, as they are sign smeared if they are
						; 5154	;	> 377777 (they fall in with MOVSO and friends at EXT2). [301]
						; 5155	;	Use similar caution if new EXTEND codes are created which
						; 5156	;	must have the DCODE B field be 1 or 3.
						; 5157	;
						; 5158		.DCODE
D 0000, 2000,1002				; 5159	000:	EA,		J/UUO
D 0001, 2200,1005				; 5160		EA,	SJCL,	J/L-CMS		;CMSX HIDDEN BENEATH LUUO
D 0002, 2100,1005				; 5161		EA,	SJCE,	J/L-CMS
D 0003, 2001,1005				; 5162		EA,	SJCLE,	J/L-CMS
						; 5163	
D 0004, 2200,1006				; 5164	004:	EA,	B/2,	J/L-EDIT	;EDIT
D 0005, 2601,1005				; 5165		EA,	SJCGE,	J/L-CMS
D 0006, 2501,1005				; 5166		EA,	SJCN,	J/L-CMS
D 0007, 2400,1005				; 5167		EA,	SJCG,	J/L-CMS
						; 5168	
D 0010, 2101,1010				; 5169	010:	EA,	B/1,	J/L-DBIN	;CVTDBO
D 0011, 2401,1010				; 5170		EA,	B/4,	J/L-DBIN	;CVTDBT
D 0012, 2100,1011				; 5171		EA,	B/1,	J/L-BDEC	;CVTBDO
D 0013, 2001,1011				; 5172		EA,	B/0,	J/L-BDEC	;CVTBDT
						; 5173	
D 0014, 2100,1012				; 5174	014:	EA,	B/1,	J/L-MVS		;MOVSO
D 0015, 2001,1012				; 5175		EA,	B/0,	J/L-MVS		;MOVST
D 0016, 2200,1012				; 5176		EA,	B/2,	J/L-MVS		;MOVSLJ
D 0017, 2301,1012				; 5177		EA,	B/3,	J/L-MVS		;MOVSRJ
						; 5178	
						; 5179	020:	
D 0020, 2000,1013				; 5180		EA,		J/L-XBLT	;XBLT
D 0021, 2501,1014				; 5181		EA,	AC,	J/L-GTPI	;GSNGL
						; 5182	.IF/EXTEXP				;[337]
D 0022, 2001,1104				; 5183		EA,		J/L-SFTE	;GDBLE
D 0023, 2000,1105				; 5184		EA,	B/0,	J/L-GTDI	;DGFIX
D 0024, 2201,1106				; 5185	024:	EA,	B/2,	J/L-GTSI	;GFIX
D 0025, 2400,1107				; 5186		EA,	B/4,	J/L-GTDR	;DGFIXR
D 0026, 2601,1110				; 5187		EA,	B/6,	J/L-GTSR	;GFIXR
D 0027, 2000,1111				; 5188		EA,		J/L-DITE	;DGFLTR
D 0030, 2000,1112				; 5189	030:	EA,		J/L-SITE	;GFLTR
D 0031, 2001,1113				; 5190		EA,		J/L-EFSC	;GFSC
						;;5191	.IFNOT/EXTEXP
						;;5192		EA,	J/LUUO
						;;5193		EA,	J/LUUO
						;;5194	024:	EA,	J/LUUO
						;;5195		EA,	J/LUUO
						;;5196		EA,	J/LUUO
						;;5197		EA,	J/LUUO
						;;5198	030:	EA,	J/LUUO
						;;5199		EA,	J/LUUO
						; 5200	.ENDIF/EXTEXP
D 0032, 2001,1006				; 5201		EA,	J/LUUO			;These are reserved to Cobol.
D 0033, 2001,1006				; 5202		EA,	J/LUUO
D 0034, 2001,1006				; 5203		EA,	J/LUUO; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12-1
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			UUO'S							

D 0035, 2001,1006				; 5204		EA,	J/LUUO
D 0036, 2001,1006				; 5205		EA,	J/LUUO
D 0037, 2001,1006				; 5206		EA,	J/LUUO
						; 5207	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			UUO'S							

						; 5208	;MONITOR UUO'S -- TRAP TO EXEC
						; 5209	
D 0040, 2000,1002				; 5210	040:	EA,	J/MUUO		;CALL
D 0041, 2000,1002				; 5211		EA,	J/MUUO		;INIT
D 0042, 2000,1002				; 5212		EA,	J/MUUO
D 0043, 2000,1002				; 5213		EA,	J/MUUO
D 0044, 2000,1002				; 5214		EA,	J/MUUO
D 0045, 2000,1002				; 5215		EA,	J/MUUO
D 0046, 2000,1002				; 5216		EA,	J/MUUO
D 0047, 2000,1002				; 5217		EA,	J/MUUO		;CALLI
D 0050, 2000,1002				; 5218		EA,	J/MUUO		;OPEN
D 0051, 2000,1002				; 5219		EA,	J/MUUO		;TTCALL
D 0052, 2000,1002				; 5220		EA,	J/MUUO
D 0053, 2000,1002				; 5221		EA,	J/MUUO
D 0054, 2000,1002				; 5222		EA,	J/MUUO
D 0055, 2000,1002				; 5223		EA,	J/MUUO		;RENAME
D 0056, 2000,1002				; 5224		EA,	J/MUUO		;IN
D 0057, 2000,1002				; 5225		EA,	J/MUUO		;OUT
D 0060, 2000,1002				; 5226		EA,	J/MUUO		;SETSTS
D 0061, 2000,1002				; 5227		EA,	J/MUUO		;STATO
D 0062, 2000,1002				; 5228		EA,	J/MUUO		;GETSTS
D 0063, 2000,1002				; 5229		EA,	J/MUUO		;STATZ
D 0064, 2000,1002				; 5230		EA,	J/MUUO		;INBUF
D 0065, 2000,1002				; 5231		EA,	J/MUUO		;OUTBUF
D 0066, 2000,1002				; 5232		EA,	J/MUUO		;INPUT
D 0067, 2000,1002				; 5233		EA,	J/MUUO		;OUTPUT
D 0070, 2000,1002				; 5234		EA,	J/MUUO		;CLOSE
D 0071, 2000,1002				; 5235		EA,	J/MUUO		;RELEAS
D 0072, 2000,1002				; 5236		EA,	J/MUUO		;MTAPE
D 0073, 2000,1002				; 5237		EA,	J/MUUO		;UGETF
D 0074, 2000,1002				; 5238		EA,	J/MUUO		;USETI
D 0075, 2000,1002				; 5239		EA,	J/MUUO		;USETO
D 0076, 2000,1002				; 5240		EA,	J/MUUO		;LOOKUP
D 0077, 2000,1002				; 5241		EA,	J/MUUO		;ENTER
						; 5242	
						; 5243	;EXPANSION OPCODES
						; 5244	
D 0100, 2000,1002				; 5245	100:	EA,	J/UUO		;UJEN
D 0101, 2000,1002				; 5246		EA,	J/UUO
						;;5247	.IFNOT/EXTEXP
						;;5248	102:	EA,	J/UUO
						;;5249		EA,	J/UUO
						; 5250	.ENDIF/EXTEXP
						; 5251		.UCODE
						; 5252	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 14
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			UUO'S							

						; 5253	;HERE FOR UNDEFINED OPS (UUO'S) AND ILLEGAL INSTRUCTIONS
						; 5254	;E IS IN AR, OPCODE AND AC IN BRX
						; 5255	
						; 5256	;HERE ON LUUO'S
						; 5257	; E IN AR, INSTR IN BRX
						; 5258	1005:
U 1005, 1006,0001,0000,0000,0000,0010,0000	; 5259	L-CMS:	J/LUUO				;LOC FOR HIDING STRING COMPARE
						; 5260	1006:
						; 5261	L-EDIT:					;HIDE EDIT HERE
						;;5262	.IFNOT/XADDR
						;;5263	LUUO:	ARX_BRX,SC_#,#/13.,
						;;5264			SKP INTRPT,CALL,J/ROTS	;COMBINE E WITH UUO
						;;5265	1007:	AR_SHIFT,VMA_40,STORE		;STORE OPCODE ETC AT 40
						;;5266		FIN STORE,VMA_41,
						;;5267			LOAD ARX,J/XCTW		;GO PERFORM 41
						; 5268	.IF/XADDR
U 1006, 0444,3242,0600,0000,0000,5010,0000	; 5269	LUUO:	ARX_BRX,SKP PC SEC0,J/LUUO1	;WHICH KIND OF UUO?
						; 5270	=0***0
U 0444, 1474,4001,0000,0000,0000,5750,0000	; 5271	LUUO1:	SKP -LOCAL AC ADDR,CALL,J/UUOCOM	;EXTENDED.  DO IT
						; 5272		BR/AR,AR_ARX ANDC ADMSK,	;COMPATABLE.  ADDR TO BR
U 0445, 1434,3510,2047,4000,0020,7010,0175	; 5273			SKP INTRPT,J/LUUO2	; DO IT THE OLD WAY
U 0464, 0220,0001,0000,0000,0100,3010,0420	; 5274		VMA_#,#/420			;PT LOC FOR LUUO BLOCK POINTER
						; 5275	=
U 0220, 0770,4001,0000,0000,0013,0066,0033	; 5276	=00	LOAD ARX,PT REF,CALL,J/XFERW	;GET LUUO BLOCK ADDRESS
U 0222, 1276,3711,0000,0000,0316,0050,0000	; 5277	=10	VMA_ARX,STORE,CALL,J/UUOC2	;STORE UUO OPCODE AND FLAGS
						; 5278		FIN STORE,VMA_VMA+1,LOAD AR,	;NOW GET A NEW PC
U 0223, 0645,0001,0003,0000,0012,3610,0000	; 5279			J/PCA			; [301]
						; 5280	
						; 5281	;HERE FOR COMPATIBLE UUO
						; 5282	=0
						; 5283	LUUO2:	AR_AR*BR,AD/OR,VMA_#,#/40,	;SAVE OPCODE AND EA
U 1434, 1061,3302,2004,0000,0116,3010,0040	; 5284			STORE,J/LUUO3		;THEN GET INSTR FROM 41
U 1435, 0144,0001,0000,0000,0000,7710,0000	; 5285		TAKE INTRPT			;ONE MOMENT, PLEASE
U 1061, 0075,4001,0003,0000,0013,3610,0000	; 5286	LUUO3:	FIN STORE,VMA_VMA+1,LOAD ARX,J/XCTW
						; 5287	.ENDIF/XADDR
						; 5288	
						; 5289	1010:
U 1010, 1006,0001,0000,0000,0000,0010,0000	; 5290	L-DBIN:	J/LUUO				;DBIN AT 2010
						; 5291	1011:
U 1011, 1006,0001,0000,0000,0000,0010,0000	; 5292	L-BDEC:	J/LUUO				;BDEC AT 2011
						; 5293	1012:
U 1012, 1006,0001,0000,0000,0000,0010,0000	; 5294	L-MVS:	J/LUUO				;MOVE STRING AT 2012
						; 5295	
						; 5296	;ROTATE SUBROUTINE
						; 5297	
						; 5298	=0
U 1454, 0003,4001,4400,5302,0000,0003,0044	; 5299	ROTS:	AR_SHIFT,ARX_SHIFT,SC_#-SC,#/36.,RETURN3
U 1455, 0144,0001,0000,0000,0000,7710,0000	; 5300		TAKE INTRPT
						; 5301	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			UUO'S							

						; 5302	;HERE ON MUUO'S
						; 5303	; E IN AR, OP AND AC IN BRX
						;;5304	.IFNOT/XADDR
						;;5305	1002:					;FIXED ADDRESS TO COOPERATE
						;;5306						;WITH EXTEND AND OTHER OPS
						;;5307	UUO:					;UNDEFINED OP'S .GE. 100
						;;5308	MUUO:	ARX_BRX,SC_#,#/13.,
						;;5309			SKP INTRPT,CALL,J/ROTS
						;;5310	.IFNOT/SHIFT.MUUO
						;;5311	1003:	AR_SHIFT,VMA_#,#/424
						;;5312	.IF/SHIFT.MUUO
						;;5313	1003:	AR_SHIFT,VMA_#,#/425
						;;5314	.ENDIF/SHIFT.MUUO
						;;5315		STORE,UPT REF			;FIRST, STORE INSTRUCTION
						;;5316		FIN STORE,AR_PC+1,VMA_VMA+1,STORE	;NEXT, PC
						;;5317	=100	MEM_AR,VMA_VMA+1,SC_#,#/60,
						;;5318			CALL,J/GTEEB1
						;;5319		DATAI PAG(L),CALL,J/PCTXT	;GET PROCESS CONTEXT VARIABLES
						;;5320	=11	LD PREV CTXT			;PCS FROM PC, CWSX FROM SXCT
						;;5321		AR_SHIFT,ARL_BRL.S,		;COMBINE UBR WITH AC BLKS, CWSX
						;;5322			STORE,			; STORE THAT AT 426 (XADDR =427)
						;;5323			COND/EBUS CTL,EBUS CTL/2; & RELEASE ECL EBUS
						;;5324		MEM_AR,VMA_430+MODE		;NOW READY TO GET NEW PC
						;;5325		LOAD AR,UPT REF			;FETCH NEW PC
						;;5326	NEWPC:	AR_MEM,SR_0,J/START		;USE IT
						;;5327	.IF/MODEL.B
						;;5328	SETPC:	VMA_AR AND ADMSK,FETCH,J/NOP	;USE LOW AR AS ADDRESS
						;;5329	.IFNOT/MODEL.B
						;;5330	SETPC:	PC_VMA,J/BRJMP			;LOAD PC, INCLUDING SECTION
						;;5331	.ENDIF/MODEL.B
						; 5332	.IF/XADDR
						; 5333	1002:
						; 5334	UUO:					;A PEDANTIC DISTINCTION...
						; 5335	MUUO:	ARX_BRX,SKP -LOCAL AC ADDR,	;PULL TOGETHER PIECES OF UUO
U 1002, 1474,3202,0600,0000,0000,5750,0000	; 5336			CALL,J/UUOCOM
U 1022, 1070,0001,0000,0000,0100,3210,0430	; 5337	1022:	VMA_430+MODE			;GET NEW PC
U 1070, 1117,0001,0000,0000,0013,0026,0223	; 5338		LOAD ARX,UPT REF
U 1117, 1470,3200,0003,0000,0122,3010,0424	; 5339		ARX_MEM,VMA_#,#/424		;LOC'N OF MUUO DATA BLOCK
						; 5340	=0	BRX/ARX,STORE,UPT REF,		;STORE OPCODE, FLAGS
U 1470, 1276,4001,0020,0000,0016,0066,0223	; 5341			CALL,J/UUOC2		;NOW RETURN TO COMMON CODE
U 1471, 0230,3733,2003,0302,0002,0010,0004	; 5342		MEM_AR,AR_PC,SC_#,#/4		;READY TO SETUP NEW FLAGS
						; 5343	=00	VMA_VMA+1,SC_#,#/60,		;SET UP FOR CONTEXT WORD
						; 5344			SH DISP,AR_0S,		;TEST USER AND PUBLIC FLAGS
U 0230, 0432,3401,2000,0302,0020,3647,0060	; 5345			CALL,J/MUUOF		;SET NEW PREV FLAGS, GET EBUS
						; 5346		DATAI PAG(L),ARX_1B17-1,	;GO COLLECT DATAI PAG INFO
U 0231, 3570,1761,3200,0000,0060,2057,0511	; 5347			CALL,J/PCTXT
U 0233, 1164,3731,0000,0000,0060,2010,0426	; 5348	=11	LD PREV CTXT			;PCS FROM PC, CWSX FROM SXCT
						; 5349		AR_SHIFT,ARL_BRL.S,		;COMBINE UBR WITH AC BLKS, CWSX
						; 5350			STORE,			; STORE THAT AT 426 (XADDR =427)
U 1164, 1222,3202,4000,0000,0016,2222,0002	; 5351			COND/EBUS CTL,EBUS CTL/2; & RELEASE ECL EBUS
U 1222, 1232,3242,6003,0000,0002,1610,0000	; 5352		MEM_AR,AR_BRX,SR_0		;NOW GET NEW PC
U 1232, 0073,3600,0007,4000,0337,0010,0175	; 5353	SETPC:	VMA_AR AND ADMSK,FETCH,J/NOP
						; 5354	
						; 5355	=0
U 1474, 1252,4041,0000,0000,0021,0017,0002	; 5356	UUOCOM:	ARL_1.M,J/UUOC1			;FORCE AC ADDRESS
U 1475, 1252,4001,0000,0400,3001,0010,0200	; 5357		CLR P				;NOT AC, GET CLEAN SECT #; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15-1
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			UUO'S							

U 1252, 1262,3510,2017,4000,1020,0010,0175	; 5358	UUOC1:	MQ_AR,AR_ARX ANDC ADMSK		;SAVE ADDR IN MQ.  GET OPCODE
U 1262, 1544,4001,0040,0000,0000,6222,0030	; 5359		BR/AR,AR_0.S,SKP USER		;SAVE OPCODE IN BR
U 1544, 1545,4001,0000,0000,2000,0210,0000	; 5360	=0	AR12-17_PREV SEC		;GET PCS
U 1545, 1266,3302,2000,0000,0000,0010,0000	; 5361		AR_AR*BR,AD/OR			;OPCODE/PCS COMBINED
U 1266, 1272,3530,2407,0000,3020,0010,0175	; 5362		ARX_AR SWAP,AR_PC FLAGS		;GET FLAGS FROM PC
U 1272, 0020,4001,4000,0000,2000,0603,0000	; 5363		ARL_ARL,AR_ARX,RETURN20		;FLAGS AND OPCODE COMBINED
						; 5364						;GO BACK TO CALLER TO STORE
U 1276, 1332,4033,0203,0000,0022,0025,0000	; 5365	UUOC2:	MEM_AR,ARX_PC+1			;FINISH STORE
						; 5366		AR_ARX AND ADMSK,		;PC+1 ADDRESS TO AR
U 1332, 1421,3610,2307,4000,0036,3610,0175	; 5367			VMA_VMA+1,STORE,ARX/MQ	;PUT PC AWAY, GET EFFECTIVE ADDR
						; 5368		FIN STORE,AR_ARX,
U 1421, 0001,0001,4003,0000,2016,3603,0000	; 5369			VMA_VMA+1,STORE,RETURN1	;PUT EA AWAY.
						; 5370	
						; 5371	=1010					;HERE TO SETUP NEW FLAGS
U 0432, 3047,4001,0000,0000,0000,0024,0020	; 5372	MUUOF:	SET FLAGS_AR,J/GTEEB1		;GO GET ECL EBUS
U 0433, 0432,4001,0000,0000,0000,0110,0400	; 5373		AR0-8_#,#/400,J/MUUOF		;PREV CTXT SUPERVISOR
U 0436, 0432,4001,0000,0000,0000,0110,0004	; 5374		AR0-8_#,#/004,J/MUUOF		;  USER/CONCEALED
U 0437, 0432,0001,0000,0000,0000,0110,0404	; 5375		AR0-8_#,#/404,J/MUUOF		;  USER/PUBLIC
						; 5376	.ENDIF/XADDR
						; 5377	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			JSYS, ADJSP						

						; 5378	.TOC	"JSYS, ADJSP"
						;;5379	.IF/DIAG.INST
						;;5380		.DCODE
						;;5381	104:	EA,		J/DIADISP		;DIAG
						;;5382		I,	B/0,	J/ADJSP
						;;5383		.UCODE
						;;5384	
						;;5385	;HERE FOR DIAG INSTRUCTION
						;;5386	
						;;5387	1001:
						;;5388	DIADISP:	J/DIAGINSTR
						;;5389	
						; 5390	.IFNOT/DIAG.INST
						; 5391		.DCODE
D 0104, 2000,1002				; 5392	104:	EA,		J/UUO		;JSYS
D 0105, 0000,1000				; 5393		I,	B/0,	J/ADJSP
						; 5394		.UCODE
						; 5395	.ENDIF/DIAG.INST
						; 5396	
						; 5397	;HERE FOR ADJSP INSTRUCTION
						; 5398	; ENTER WITH E IN AR, PREFETCH IN PROGRESS
						; 5399	
						; 5400	1000:					;PUT ADJSP NEXT TO UUO
						;;5401	.IFNOT/XADDR
						;;5402	ADJSP:	ARL_ARR,ARR_ARR			;PUT E IN BOTH HALVES
						;;5403		AR_AR*AC0,AD/A+B,INH CRY18,	;ADJUST POINTER,
						;;5404			ARX/AD,SKP AR0		;SKIP IF NEGATIVE
						; 5405	.IF/XADDR
U 1000, 1425,4061,0200,0000,3020,0610,0004	; 5406	ADJSP:	ARX_1,ARL_ARR,ARR_ARR		;MUST TEST FOR SHORT STACK
						; 5407		AC0,STACK UPDATE,		;MCL SHORT STACK ENABLES CRY18
U 1425, 1554,1711,0000,0000,0040,5420,0000	; 5408			GEN ARX-1,SKP CRY0	; THUS CRY IFF LONG POINTER
						; 5409	=0	AR_AR*AC0,AD/A+B,INH CRY18,	;ADJUST POINTER,
U 1554, 1614,0600,2200,0000,0020,4511,0000	; 5410			ARX/AD,SKP AR0,J/ADJSP1	;SKIP IF NEGATIVE
U 1555, 1432,5401,0000,0000,0237,0616,0002	; 5411		ARL_SIGN,ARR_ARR,I FETCH	;LONG:  SIGN EXTEND E
U 1432, 0065,0600,2000,0000,0020,0010,0000	; 5412		AR_AR*AC0,AD/A+B,J/STAC		;DONE
						; 5413	.ENDIF/XADDR
						; 5414	=0
						; 5415	ADJSP1:	GEN AR*AC0,AD/ANDCA,		;TEST FOR - TO + CHANGE
U 1614, 1160,3000,0000,4000,0020,5510,0000	; 5416			SKP AD0,J/STMAC
						; 5417		GEN AR*AC0,AD/ANDCB,		;TEST FOR + TO - CHANGE
U 1615, 1160,3500,0000,4000,0020,5510,0000	; 5418			SKP AD0,J/STMAC
						; 5419	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 17
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			XCT, PXCT, SXCT						

						; 5420	.TOC	"XCT, PXCT, SXCT"
						; 5421	;HERE FOR EXTENDED ADDRESSING INSTRUCTIONS
						; 5422	
						; 5423	=0
U 1640, 1644,4001,0000,0000,0000,6210,0000	; 5424	XCT1:	SKP USER,J/PXCT			;HERE ON XCT, NO INTERRUPT
U 1641, 0144,0001,0000,0000,0000,7710,0000	; 5425		TAKE INTRPT			;GET OUT OF LONG XCT CHAIN
						; 5426	=0
U 1644, 1645,4001,0000,0000,0000,1510,0020	; 5427	PXCT:	SET PXCT			;SETUP PXCT CONTROLS FROM 9-12
U 1645, 0152,3703,0200,0000,0000,1410,0000	; 5428	UXCT:	ARX_AR (AD),LOAD IR,#/0,J/XCTGO	;COPY INSTR TO ARX, IR
						; 5429	
						; 5430		.DCODE
						; 5431	.IFNOT/SXCT
						;;5432	  .IF/DIAG.INST
						;;5433	106:	R,	J/DIAGXCT			;DIAG MODEL B SXCT = XCT
						;;5434		EA,	J/XCTUUO
						;;5435	.UCODE
						;;5436				;UNSUPPORTED FEATURE
						;;5437				;SEPERATE FROM XCT BECAUSE OF DRAM CONSTRAINTS
						;;5438	=0****00***0
						;;5439	DIAGXCT:	SKP INTRPT,J/XCT1		;CHECK FOR XCT . LOOP
						;;5440	XCTUUO:	J/UUO				;INST PAIR FOR XCT
						;;5441	=
						; 5442	  .IFNOT/DIAG.INST
						;;5443	   .IFNOT/EXTEXP
						;;5444	    .IFNOT/PUSHM
						;;5445	106:	EA,	J/UUO
						;;5446		EA,	J/UUO
						;;5447		.UCODE
						;;5448	    .IF/PUSHM
						;;5449	106:	R,	J/PUSHM			;PUSH MULTIPLE
						;;5450	107:	EA,	J/POPM			;POP MULTIPLE
						;;5451	.UCODE
						;;5452	    .ENDIF/PUSHM
						; 5453	   .ENDIF/EXTEXP
						; 5454	  .ENDIF/DIAG.INST
						; 5455	
						;;5456	.IF/SXCT	;NOTE: THE SXCT INSTRUCTION IS A TEMPORARY MECHANISM
						;;5457	106:	R,	J/SXCT		;INTENDED FOR DIAGNOSTICS ONLY
						;;5458		EA,	J/UUO107
						;;5459		.UCODE
						;;5460	
						;;5461	1014:					;PUT NEXT TO UUO107
						;;5462	SXCT:	SKP KERNEL,CALL,J/IOCHK		;LEGAL IN KERNEL MODE ONLY
						;;5463	1015:	BR/AR,ARX_AR,AR_AC0,		;SHUFFLE INSTR TO GET BASE REG
						;;5464			SET SXCT		;SETUP HARDWARE FLAGS
						;;5465		SKP AC#0			;CHOOSE LOOP FOR EA CALC
						;;5466	=0	BR/AR,AR_BR,LOAD IR,		;AC0 IS BASE INDEX
						;;5467			BRX/ARX,ARL_0.M,
						;;5468			EA MOD DISP,J/SXCTB
						;;5469		AR_BR,LOAD IR,ARL_0.M,		;GET EXT ADDR FROM XR OR INDRCT
						;;5470			BRX/ARX,J/XIND2
						;;5471	=00
						;;5472	PXLOOP:	GEN AR,A READ			;GO DO INSTR
						;;5473		AR_AR+XR,A READ
						;;5474		GEN AR,A INDRCT,SKP INTRPT,J/XIND1
						;;5475		GEN AR+XR,A INDRCT,SKP INTRPT; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 17-1
; SKPJMP.MIC[10,5351]	19:52 24-Jul-85			XCT, PXCT, SXCT						

						;;5476	
						;;5477	=0
						;;5478	XIND1:	AR_MEM,ARX_MEM,EA TYPE DISP,J/XIND2
						;;5479		MB WAIT,TAKE INTRPT
						;;5480	=00
						;;5481	XIND2:	EA MOD DISP,J/PXLOOP		;CURRENT OR PREV WITHOUT CWSX
						;;5482		AR_ARX (AD),A READ		;PREV AND CWSX
						;;5483		AR_ARX (AD),A READ		;SXCT 0,
						;;5484		EA MOD DISP,J/SXCTB		;SXCT B,
						;;5485	
						;;5486	=00
						;;5487	SXCTB:	AR_AR+BR,A READ			;GO
						;;5488		AR_AR+XR,ARL_0.C,J/SXCTB	;NO MORE INDIRECTS
						;;5489		GEN AR,A INDRCT,		;FOLLOW INDRCT POINTER
						;;5490			SKP INTRPT,J/XIND1
						;;5491		GEN AR+XR,A INDRCT,
						;;5492			SKP INTRPT,J/XIND1
						; 5493	.ENDIF/SXCT
						; 5494	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; SHIFT.MIC[10,5351]	19:52 24-Jul-85			ROTATES AND LOGICAL SHIFTS -- ROT, LSH, JFFO		

						; 5495	.TOC	"ROTATES AND LOGICAL SHIFTS -- ROT, LSH, JFFO"
						; 5496	
						; 5497		.DCODE
D 0240, 0001,1406				; 5498	240:	I,	B/0,	J/ASH
D 0241, 0000,1407				; 5499		I,	B/0,	J/ROT
D 0242, 0201,1404				; 5500		I,	B/2,	J/LSH
D 0243, 0001,1405				; 5501		I,		J/JFFO
D 0244, 0101,1410				; 5502		I,	B/1,	J/ASHC
D 0245, 0001,1411				; 5503		I,		J/ROTC
D 0246, 0001,1004				; 5504		I,		J/LSHC
D 0247, 2000,1002				; 5505		EA,		J/UUO
						; 5506		.UCODE
						; 5507	
						; 5508	;ENTER WITH 0,E IN AR
						; 5509	; NOTE THAT VALUES OF SC GREATER THAN 36
						; 5510	; CAUSE THE SHIFTER TO SELECT ARX.
						; 5511	
						; 5512	=0****00***0
						; 5513	LSH:	AR_AC0,ARL/AD,ARX_0.M,SC_EA,
U 1404, 0330,3240,2000,0002,0021,4413,0042	; 5514			SKP AR18,J/SHR1
						; 5515	
U 1405, 1710,3240,2000,0302,0020,5610,0006	; 5516	JFFO:	AR_AC0,SKP AD NE,SC_#,#/6
						; 5517	=
U 1710, 0073,4001,0001,0000,0217,1010,0000	; 5518	=0	AC1_AR,I FETCH,J/NOP		;AC WAS ZERO, NO JUMP
						; 5519		ARX+MQ_0.M,FE_P,SKP SCAD NE,	;TEST FIRST 6 BITS
U 1711, 1714,0001,4000,0101,0021,5210,0144	; 5520			AR_SHIFT,ARL/SH		;DISCARD THEM
						; 5521	=0
						; 5522	JFFO1:	AR_SHIFT,FE_P,SKP SCAD NE,	;TEST NEXT 6 BITS
U 1714, 1714,1701,4600,0101,0020,5210,0000	; 5523			ARX_ARX-1,J/JFFO1	;LOOP, COUNTING, TILL NE
						; 5524		P_FE,ARR_0.S,			;RESTORE 6 NON-ZERO BITS
U 1715, 1720,5123,0500,0000,3021,0022,0210	; 5525			ARX_ARX*-6		;GET POS GROUP COUNT*6
						; 5526	=0
						; 5527	JFFO2:	SKP AR0,AR_2(AR+1),		;LOOP TO FIND A 1
U 1720, 1720,4003,5600,0000,0040,4510,0000	; 5528			ARX_ARX+1,J/JFFO2	;COUNTING AS WE GO
U 1721, 2124,1701,6000,0000,0037,0010,0000	; 5529		AR_ARX-1,FETCH,J/STRAC1
						; 5530	
						; 5531	=0****00***0
						; 5532	ASH:	SC_EA,SKP AR18,			;GET SHIFT AMOUNT
U 1406, 1754,3441,2000,0002,0000,4413,0000	; 5533			AR_0S,J/ASHL		;SET LOW PART = 0
U 1407, 0330,3240,2200,0002,0040,4413,0000	; 5534	ROT:	AR_AC0,ARX_AC0,SC_EA,SKP AR18
						; 5535	=
						; 5536	;SINGLE-WORD LSH/ROT
						; 5537	; FOR ROT, B=0, AR AND ARX BOTH CONTAIN AC
						; 5538	; FOR LSH, B=2, AR HAS AC, ARX IS ZERO
						; 5539	
						; 5540	=00
						; 5541	SHR1:	AR_SHIFT,SC_#+SC,#/-36.,	;DO POS (LEFT) SHIFT, CHK RANGE
U 0330, 0332,4001,4000,2302,0020,5110,0734	; 5542			SKP SCAD0,J/SHR2
						; 5543		ARX_AR (AD),AR_ARX (ADX),
						; 5544			SC_#+SC,#/36.,
U 0331, 0330,3701,6200,2302,0020,5133,0044	; 5545			B DISP,SKP SCAD0,J/SHR1	;MAKE NEG SHIFT TO EQUIV POS
						; 5546	SHR2:	AR_SHIFT,SC_#+SC,#/-36.,
U 0332, 0332,4001,4000,2302,0020,5110,0734	; 5547			SKP SCAD0,J/SHR2	;BRING SC INTO RANGE
U 0333, 0073,0001,0000,0000,0217,1010,0000	; 5548		AC0_AR,I FETCH,J/NOP		;DONE
						; 5549	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; SHIFT.MIC[10,5351]	19:52 24-Jul-85			ROTATE AND LOGICAL SHIFT COMBINED -- ROTC, LSHC		

						; 5550	.TOC	"ROTATE AND LOGICAL SHIFT COMBINED -- ROTC, LSHC"
						; 5551	
						; 5552	=0****00***0
						; 5553	ASHC:	SC_EA,SKP AR18,			;SETUP SHIFT COUNT
U 1410, 1754,3240,5001,0002,0020,4413,0000	; 5554			AR_AC1*2,J/ASHL		;GET LOW WORD
U 1411, 1441,3200,0201,0000,0020,0010,0000	; 5555	ROTC:	ARX_AC1
U 1441, 1724,3200,2000,0002,0020,4413,0000	; 5556	=	AR_AC0,SC_EA,SKP AR18		;SETUP BOTH AC'S
						; 5557	=0
						; 5558	ROT3:	MQ_SHIFT,ARX_AR (AD),
U 1724, 1442,3701,6210,0000,0000,0010,0000	; 5559			AR_ARX (ADX),J/ROT4
						; 5560		ARX_AR (AD),AR_ARX (ADX),
U 1725, 1724,3703,6200,2302,0020,5110,0044	; 5561			SC_#+SC,#/36.,SKP SCAD0,J/ROT3
						; 5562	
						; 5563	ROT4:	AR_MQ,ARX_SHIFT,
U 1442, 1730,3723,2400,2302,0020,5110,0734	; 5564			SC_#+SC,#/-36.,SKP SCAD0
						; 5565	=0	MQ_SHIFT,ARX_AR (AD),
U 1730, 1442,3701,6210,0000,0000,0010,0000	; 5566			AR_ARX (ADX),J/ROT4
U 1731, 2124,4001,4000,0000,2217,1010,0000	; 5567	STDAC:	AC0_AR,AR_ARX,I FETCH,J/STRAC1
						; 5568	
						; 5569	
						; 5570	1004:					;NEXT TO UUO
U 1004, 1451,3240,0201,0000,0021,0010,0100	; 5571	LSHC:	ARX_AC1,MQ_0.M
U 1451, 1734,3200,2000,0303,0020,4413,0044	; 5572	LSH1:	AR_AC0,SC_EA,FE_#,#/36.,SKP AR18
						; 5573	=0
						; 5574	LSH2:	MQ_SHIFT,AR_ARX (ADX),
U 1734, 1462,3721,6310,0301,0000,0010,0734	; 5575			ARX/MQ,FE_#,#/-36.,J/LSH3
						; 5576		ARX_AR (AD),AR_0.M,MQ_ARX,
U 1735, 1734,3703,0210,2002,2021,5110,0030	; 5577			SC_FE+SC,SKP SCAD0,J/LSH2
						; 5578	
						; 5579	LSH3:	AR_MQ,ARL/AD,ARX_SHIFT,MQ_0.M,
U 1462, 1750,3721,2400,2002,0021,5110,0102	; 5580			SC_FE+SC,SKP SCAD0
U 1750, 1462,3701,6310,0000,0000,0010,0000	; 5581	=0	MQ_SHIFT,AR_ARX (ADX),ARX/MQ,J/LSH3
U 1751, 2124,4001,4000,0000,2217,1010,0000	; 5582		AC0_AR,AR_ARX,I FETCH,J/STRAC1
						; 5583	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; SHIFT.MIC[10,5351]	19:52 24-Jul-85			ARITHMETIC SHIFTS -- ASH, ASHC				

						; 5584	.TOC	"ARITHMETIC SHIFTS -- ASH, ASHC"
						; 5585	
						; 5586	;COMMON CODE FOR ARITHMETIC SHIFTS
						; 5587	
						; 5588	=0
						; 5589	ASHL:	ARX_AR,AR_AC0,			;INPUT NOW IN AR LONG
U 1754, 1764,3240,2400,2400,1040,5210,0000	; 5590			SKP SC NE,J/ASHL1	;CHECK FOR NULL SHIFT
						; 5591		ARX_AR,AR_AC0,			;HERE IF RIGHT SHIFT
U 1755, 1760,3200,2400,2302,1040,5110,0044	; 5592			SC_#+SC,#/36.,SKP SCAD0	;CHECK FOR LONG ONE
						; 5593	=0
U 1760, 1472,5401,2440,0000,0020,0016,0000	; 5594	ASHR1:	BR/AR,ARX_SHIFT,AR_SIGN,J/ASHR2	;LOW OUTPUT TO ARX
						; 5595		ARX_AR,AR_SIGN,			;HERE IF SHIFT COUNT .GT. 36
						; 5596			SC_#+SC,#/36.,		;BRING COUNT UP BY 36
U 1761, 1760,5441,2400,2302,1020,5116,0044	; 5597			SKP SCAD0,J/ASHR1	;LOOP TILL COUNT REASONABLE
						; 5598	
						; 5599	ASHR2:	BRX/ARX,ARX_BR,			;HIGH INPUT TO ARX
U 1472, 2050,3202,0220,0000,0000,0033,0000	; 5600			B DISP,J/ASHX
						; 5601	
						; 5602	;HERE FOR LEFT ARITHMETIC SHIFT
						; 5603	
						; 5604	=0
U 1764, 0073,4001,0000,0000,0217,0010,0000	; 5605	ASHL1:	I FETCH,J/NOP			;SHIFT 0 IS A NOP
U 1765, 1476,5441,2060,0000,0020,0016,0000	; 5606		BR_AR LONG,AR_SIGN		;SAVE INPUT, GEN SIGN WORD
U 1476, 0560,3201,2640,0000,0000,0010,0000	; 5607		BR/AR,AR_BR*2 LONG		;SAVE SIGN, GET MAGNITUDE BITS
						; 5608	=0*
						; 5609	ASHL2:	BRX/ARX,ARX_AR,AR_BR,		;HI IN TO ARX, LOW TO BRX
U 0560, 2662,3242,2420,0000,1000,0050,0000	; 5610			CALL,J/SHIFT		;CALL SHIFTER TO GET BITS LOST
U 0562, 1774,3102,0004,0000,0020,5610,0000	; 5611		SKP AR NE BR			;ANY BITS DIFFERENT FROM SIGN?
						; 5612	=0
						; 5613	ASHL3:	AR_ARX,ARX_BRX,			;RESTORE HI TO AR, LOW TO ARX
U 1774, 2040,3242,4600,2300,2020,5110,0734	; 5614			GEN #+SC,#/-36.,SKP SCAD0,J/ASHL4
U 1775, 1774,4001,0000,0000,0000,1110,0420	; 5615		SET AROV,J/ASHL3		;BITS SHIFTED OUT NE SIGN
						; 5616	=0
						; 5617	ASHL4:	AR_ARX,ARX_0S,			;HERE IF E .GT. 36
U 2040, 0560,3441,4200,2302,2000,0010,0734	; 5618			SC_#+SC,#/-36.,J/ASHL2	;SHIFT 36 PLACES, TRY AGAIN
						; 5619		MQ_SHIFT,AR_BRX,CLR ARX,	;HIGH OUTPUT TO MQ,
U 2041, 2044,3202,6010,2302,0000,0533,0777	; 5620			SC_#+SC,#/-1,B DISP	;COMPENSATE FOR EXTRA SHIFT
						; 5621	=0
						; 5622	ASHL5:	AR_BR,BRX/ARX,ARX/MQ,		;SIGN TO AR, HIGH OUT TO ARX
						; 5623			SC_#,#/35.,		;READY TO COMBINE THEM
U 2044, 2050,3202,2320,0302,0000,0033,0043	; 5624			B DISP,J/ASHX		;STORE AS APPROPRIATE
U 2045, 2044,4001,0400,0000,0000,0010,0000	; 5625		ARX_SHIFT,J/ASHL5		;LOW OUTPUT TO ARX
						; 5626	
						; 5627	;HERE TO GET FINAL RESULTS.
						; 5628	
						; 5629	=0
U 2050, 0072,4001,4000,0000,0217,0010,0000	; 5630	ASHX:	AR_SHIFT,I FETCH,J/STORAC	;HERE AFTER ASH
						; 5631		AR_SHIFT,ARX_BRX,		;HERE AFTER ASHC
U 2051, 0061,3202,4600,0302,0000,0010,0043	; 5632			SC_#,#/35.,J/ST2AC
						; 5633	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; ARITH.MIC[10,5351]	19:52 24-Jul-85			ADD, SUB						

						; 5634	.TOC	"ADD, SUB"
						; 5635	
						; 5636		.DCODE
D 0270, 5500,1605				; 5637	270:	R-PF,	AC,	J/ADD
D 0271, 1501,1605				; 5638		I-PF,	AC,	J/ADD
D 0272, 7601,1605				; 5639		RPW,	M,	J/ADD
D 0273, 7700,1605				; 5640		RPW,	B,	J/ADD
						; 5641		.UCODE
						; 5642	
						; 5643	=0****00****
U 1605, 0060,0600,2000,0000,0025,1333,0000	; 5644	ADD:	AR_AR*AC0,AD/A+B,AD FLAGS,EXIT
						; 5645	=
						; 5646	
						; 5647	
						; 5648		.DCODE
D 0274, 5500,1606				; 5649	274:	R-PF,	AC,	J/SUB
D 0275, 1501,1606				; 5650		I-PF,	AC,	J/SUB
D 0276, 7601,1606				; 5651		RPW,	M,	J/SUB
D 0277, 7700,1606				; 5652		RPW,	B,	J/SUB
						; 5653		.UCODE
						; 5654	
						; 5655	=0****00****
U 1606, 1533,3240,2040,0000,0020,0010,0000	; 5656	SUB:	AR_AC0,BR/AR
U 1533, 0060,5102,2000,0000,0025,1333,0000	; 5657	=	AR_AR-BR,AD FLAGS,EXIT
						; 5658	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; ARITH.MIC[10,5351]	19:52 24-Jul-85			MUL, IMUL						

						; 5659	.TOC	"MUL, IMUL"
						; 5660	
						; 5661		.DCODE
D 0220, 4501,0302				; 5662	220:	R,	AC,	J/IMUL
D 0221, 0500,0302				; 5663		I,	AC,	J/IMULI
D 0222, 6600,0302				; 5664		RW,	M,	J/IMUL
D 0223, 6701,0302				; 5665		RW,	B,	J/IMUL
						; 5666		.UCODE
						; 5667	
						; 5668	.IFNOT/IMULI.OPT
						; 5669	=0****00*01*
						; 5670	IMULI:
						;;5671	.IF/IMULI.OPT
						;;5672	=0****00*000
						;;5673	IMULI:	SKP AR18,GEN AC0,SIGNS DISP,	;OPTIMIZE SPECIAL CASE
						;;5674			TIME/3T,SC_#,#/17.
						;;5675	=010	MQ_AR,AR_AC0,			;HERE FOR IMULI OF + BY +
						;;5676			CLR ARX,FE_#,#/-9.,	; 9 STEPS WILL DO
						;;5677			CALL,J/MULSUB
						; 5678	.ENDIF/IMULI.OPT
						; 5679	IMUL:	MQ_AR,AR_AC0,			;M'IER TO MQ, M'CAND TO AR
						; 5680			CLR ARX,FE_#,#/-18.,
U 0302, 1534,3200,2010,0301,1020,0550,0756	; 5681			CALL,J/MULSUB		;CALL MULTIPLY SUBROUTINE
						;;5682	.IF/IMULI.OPT
						;;5683	=110	AR_SHIFT,SKP AR NE,INH CRY18,	;HERE FROM IMULI
						;;5684			I FETCH,J/MUL1		; AFTER SHORT MULTIPLY
						; 5685	.ENDIF/IMULI.OPT
U 0306, 2054,0001,0000,0302,0040,5616,0043	; 5686		SC_#,#/35.,SKP AR SIG		;CHECK OVERFLOW AND STORE
						; 5687	=
						; 5688	=0
U 2054, 0066,4001,4000,0000,0005,0033,0000	; 5689	IMUL2:	AR_SHIFT,B WRITE,J/ST6		;STORE LOW WORD OF PRODUCT
U 2055, 2054,5401,2000,0000,0020,1116,0420	; 5690		SET AROV,AR_SIGN,J/IMUL2	;NOTE OVERFLOW...
						; 5691	
						; 5692	
						; 5693		.DCODE
D 0224, 4100,0400				; 5694	224:	R,	DBL AC,	J/MUL
D 0225, 0101,0400				; 5695		I,	DBL AC,	J/MUL
D 0226, 6600,0400				; 5696		RW,	M,	J/MUL
D 0227, 6201,0400				; 5697		RW,	DBL B,	J/MUL
						; 5698		.UCODE
						; 5699	
						; 5700	=0****00*000
						; 5701	MUL:	MQ_AR,CLR ARX,			;MULTIPLIER TO MQ
						; 5702			AR_AC0,FE_#,#/-18.,	;SETUP MULTIPLICAND AND STEP CNT
U 0400, 1534,3200,2010,0301,1020,0550,0756	; 5703			CALL,J/MULSUB		;AND GO TO SUBROUTINE
U 0404, 0406,3602,0004,0000,0020,5510,0000	; 5704	=100	GEN AR*BR,AD/AND,SKP AD0	;M'IER NEG, CHECK M'CAND & PROD TOO
						; 5705	=110
U 0406, 0060,0001,0000,0302,0005,0033,0043	; 5706	MUL1:	SC_#,#/35.,EXIT			;STORE DOUBLE RESULT
U 0407, 0406,0001,0000,0000,0000,1110,0420	; 5707		SET AROV,J/MUL1			;MUST HAVE SQUARED 400000,,0
						; 5708	=
						; 5709	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; ARITH.MIC[10,5351]	19:52 24-Jul-85			MULTIPLY SUBROUTINE					

						; 5710	.TOC	"MULTIPLY SUBROUTINE"
						; 5711	; ENTER WITH MULTIPLIER IN MQ,
						; 5712	; MULTIPLICAND IN AR!ARX, MINUS STEP COUNT IN FE
						; 5713	; RETURNS PRODUCT IN AR!ARX!MQ.
						; 5714	; RETURN 4, 6 TELLS SIGN OF MULTIPLIER
						; 5715	; 4 AND 6 ARE USED SO CALLER CAN IGNORE
						; 5716	; DIFFERENCE BY ALIGNMENT OF CALL LOC'N
						; 5717	;[TIME=4+2(-FE)+(# OF ARITH STEPS)] ... IF FE=-18, 40-58.
						; 5718	;
						; 5719	;Recall:
						; 5720	; MUL		"FE_FE+1,DISP/MUL,MQ/MQ*.25"
						; 5721	;
						; 5722	
						; 5723	MULSUB:	BR_AR LONG,AR_0S,ARX_0S,	;M'CAND TO BR LONG, CLEAR PROD
U 1534, 0320,3441,2270,4001,0000,0030,0000	; 5724			MUL,J/MULP		;START THE MULTIPLICATION
						; 5725	=000					;GRAB AN 8-WORD BLOCK
						; 5726	MULP:
						; 5727	.IF/MODEL.B
U 0320, 0006,3701,5500,2401,0000,0703,0001	; 5728		(AR+ARX+MQ)*2,FE_SC,RETURN6	;XADDR MACHINE HAS
U 0321, 0006,3701,5500,2401,0000,0703,0001	; 5729		(AR+ARX+MQ)*2,FE_SC,RETURN6	; NO "CRA MUL DONE"
U 0322, 0006,3701,5500,2401,0000,0703,0001	; 5730		(AR+ARX+MQ)*2,FE_SC,RETURN6
						; 5731	.ENDIF/MODEL.B
U 0323, 0006,3701,5500,2401,0000,0703,0001	; 5732	=011	(AR+ARX+MQ)*2,FE_SC,RETURN6	;DISCARD REDUNDANT SIGN BIT
						; 5733	
U 0324, 0320,3703,7710,4001,0000,0030,0000	; 5734	=100	AR_AR*.25 LONG,MUL,J/MULP	;M'IER BITS 00 AFTER POS STEP
						; 5735		AR_(AR+BR)*.25,ARX/ADX*.25,	;01 AFTER +
U 0325, 0320,0602,7714,4001,0020,0030,0000	; 5736			MUL,J/MULP
						; 5737		AR_(AR-2BR)*.25,ARX/ADX*.25,	;10 AFTER +
U 0326, 0420,5101,7714,4001,0020,0030,0000	; 5738			MUL,J/MULM
						; 5739		AR_(AR-BR)*.25,ARX/ADX*.25,
U 0327, 0420,5102,7714,4001,0020,0030,0000	; 5740			MUL,J/MULM		;11 AFTER +
						; 5741	
						; 5742	=000					;ANOTHER 8-WORD BLOCK FOR
						; 5743	MULM:					; AFTER SUBTRACTION STEPS
						; 5744	.IF/MODEL.B
U 0420, 0004,3703,5500,2401,0000,0703,0001	; 5745		(AR+ARX+MQ)*2,FE_SC,RETURN4
U 0421, 0004,3703,5500,2401,0000,0703,0001	; 5746		(AR+ARX+MQ)*2,FE_SC,RETURN4
U 0422, 0004,3703,5500,2401,0000,0703,0001	; 5747		(AR+ARX+MQ)*2,FE_SC,RETURN4
						; 5748	.ENDIF/MODEL.B
U 0423, 0004,3703,5500,2401,0000,0703,0001	; 5749	=011	(AR+ARX+MQ)*2,FE_SC,RETURN4	;M'IER WAS NEGATIVE
						; 5750	
						; 5751	=100	AR_(AR+BR)*.25,ARX/ADX*.25,	;M'IER BITS 00 AFTER NEG STEP
U 0424, 0320,0602,7714,4001,0020,0030,0000	; 5752			MUL,J/MULP
						; 5753		AR_(AR+2BR)*.25,ARX/ADX*.25,	;01 AFTER -
U 0425, 0320,0601,7714,4001,0020,0030,0000	; 5754			MUL,J/MULP
						; 5755		AR_(AR-BR)*.25,ARX/ADX*.25,	;10 AFTER -
U 0426, 0420,5102,7714,4001,0020,0030,0000	; 5756			MUL,J/MULM
U 0427, 0420,3701,7710,4001,0000,0030,0000	; 5757		AR_AR*.25 LONG,MUL,J/MULM	;11 AFTER -
						; 5758	
						; 5759	;HERE TO CONTINUE A LONG MULTIPLICATION
						; 5760	; WITH PARTIAL PRODUCT IN AR LONG
						; 5761	
U 1542, 0320,3441,0010,4001,0000,0030,0000	; 5762	MULREE:	AD/0S,MUL,J/MULP		;DIVE IN WITHOUT CLOBBERING AR
						; 5763	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; ARITH.MIC[10,5351]	19:52 24-Jul-85			DIV, IDIV						

						; 5764	.TOC	"DIV, IDIV"
						; 5765	
						; 5766		.DCODE
D 0230, 4100,0411				; 5767	230:	R,	DBL AC,	J/IDIV
D 0231, 0101,0411				; 5768		I,	DBL AC,	J/IDIV
D 0232, 6600,0411				; 5769		RW,	M,	J/IDIV
D 0233, 6201,0411				; 5770		RW,	DBL B,	J/IDIV
						; 5771	
D 0234, 4101,0410				; 5772	234:	R,	DBL AC,	J/DIV
D 0235, 0100,0410				; 5773		I,	DBL AC,	J/DIV
D 0236, 6601,0410				; 5774		RW,	M,	J/DIV
D 0237, 6200,0410				; 5775		RW,	DBL B,	J/DIV
						; 5776		.UCODE
						; 5777	
						; 5778	=0****00*000
						; 5779	DIV:	BR/AR,ARX+MQ_0.M,		;DIVISOR TO BR
						; 5780			AR_AC1*2,ARL/AD*2,	;LOW DIVIDEND TO AR
U 0410, 1561,3200,5041,0000,0021,0050,0145	; 5781			CALL.M,J/DIV1		;GET HIGH DIVIDEND
						; 5782	.IF/MODEL.B
U 0411, 1551,4041,2040,0000,0020,0010,0000	; 5783	IDIV:	BR/AR, AR_1, J/IDIV2		;[236]
						; 5784	IDIV1:	BR/AR,ARX+MQ_0.M,SC_1,		;DIVISOR TO BR
						; 5785			AR_AC0,ARL/AD,CALL.M,	;DIVIDEND TO AR
U 0412, 2130,3240,2040,4402,0021,4550,0142	; 5786			SKP AR0,J/DIV2		;TEST DIVISOR SIGN
						;;5787	.IFNOT/MODEL.B
						;;5788	=10
						;;5789	IDIV:	BR/AR,ARX+MQ_0.M,SC_1,		;DIVISOR TO BR
						;;5790			AR_AC0,ARL/AD,CALL.M,	;DIVIDEND TO AR
						;;5791			SKP AR0,J/DIV2		;TEST DIVISOR SIGN
						; 5792	.ENDIF/MODEL.B
						; 5793	=011
U 0413, 0073,4001,0000,0000,0000,1110,0424	; 5794	NODIVD:	SET NO DIVIDE,J/NOP		;HERE IF DIVIDE IMPOSSIBLE
						; 5795	
						; 5796	=110	ARX_AR,AR_-BRX,			;REMAIN TO ARX, GET CORRECT QUOTIENT
U 0416, 0060,5142,6400,0302,1025,0033,0044	; 5797			SC_#,#/36.,EXIT
						; 5798		ARX_AR,AR_BRX,			;HERE FOR POS QUOTIENT
U 0417, 0060,3202,6400,0302,1005,0033,0044	; 5799			SC_#,#/36.,EXIT
						; 5800	=
						; 5801	.IF/MODEL.B
U 1551, 2114,5102,0004,0000,0040,5610,0000	; 5802	IDIV2:	GEN AR-BR, SKP AD NE		;[236] IS DIVISOR = 1?
						; 5803	=0	AR_AC0, CLR ARX, SC_#, #/36.,	;[236] YES, ANSWER IS AC0.
U 2114, 0060,3240,2000,0302,0025,0533,0044	; 5804			EXIT			;[236]
U 2115, 0412,3202,2000,0000,0000,0010,0000	; 5805		AR_BR, J/IDIV1			;
						; 5806	.ENDIF/MODEL.B
						; 5807	;HERE ON DIVIDE TO SET UP DIVIDEND
						; 5808	
						; 5809	DIV1:	BRX/ARX,ARX_AR,AR_AC0,		;CLR BRX, DIVIDEND IN AR LONG
						; 5810			FE_#,#/33.,TIME/3T,	;SETUP ITERATION COUNT
U 1561, 0454,3240,2420,0301,1020,0032,0041	; 5811			SIGNS DISP,J/DIVS1	;ENTER SUBR
						; 5812	
						; 5813	;HERE ON IDIV TO SET UP DIVIDEND.  SKIP IF DIVISOR NEG
						; 5814	
						; 5815	=0
						; 5816	DIV2:	BRX/ARX,ARX_SHIFT,AR_SIGN,	;CLR BRX, DIVIDEND TO AR LONG
						; 5817			FE_#,#/33.,		;SETUP LOOP COUNT
U 2130, 0454,5401,2420,0301,0020,4516,0041	; 5818			SKP AR0,J/DIVS1		;ENTER SUBR ACCORDING TO SIGNS
						; 5819		BRX/ARX,ARX_SHIFT,AR_SIGN,	;CLR BRX, DIVIDEND TO AR LONG; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4-1
; ARITH.MIC[10,5351]	19:52 24-Jul-85			DIV, IDIV						

						; 5820			FE_#,#/33.,		;SETUP LOOP COUNT
U 2131, 0456,5441,2420,0301,0020,4516,0041	; 5821			SKP AR0,J/DIVS2		;ENTER SUBR ACCORDING TO SIGNS
						; 5822	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; ARITH.MIC[10,5351]	19:52 24-Jul-85			INTEGER DIVIDE SUBROUTINE				

						; 5823	.TOC	"INTEGER DIVIDE SUBROUTINE"
						; 5824	; ENTER WITH SIGNS DISPATCH OF DIVISOR AND DIVIDEND,
						; 5825	; DIVISOR IN BR, BRX CLR; DIVIDEND IN AR!ARX
						; 5826	; STEP COUNT IN FE (# OF QUOTIENT BITS -2)
						; 5827	; IF NO DIVIDE, RETURN 3 WITH IFETCH STARTED
						; 5828	; OTHERWISE, RETURN WITH SIGNED REMAINDER IN AR,
						; 5829	; POSITIVE QUOTIENT IN BRX AND MQ.
						; 5830	; RETURN 6 IF QUOTIENT SHOULD BE NEGATIVE,
						; 5831	; RETURN 7 IF QUOTIENT SHOULD BE POSITIVE.
						; 5832	;[TIME=14+3(FE)+3(D'END NEG)+3(RESTORE REQ'D)+1(REMAINDER NEG)]
						; 5833	; ... IF FE=33, 113-120
						; 5834	;
						; 5835	;Recall:
						; 5836	; DIVIDE	"FE_FE-1,DISP/DIV,MQ/MQ*2"
						; 5837	;
						; 5838	=1100
						; 5839	DIVS1:	DIVIDE,AR_2(AR-BR),
U 0454, 0462,5102,5500,3001,0020,0031,0000	; 5840			ARX/ADX*2,J/DIVS3	;BOTH D'END AND D'SOR POS
U 0455, 0454,5143,7700,0000,0020,0027,0000	; 5841		AR_-AR LONG,J/DIVS1		;MAKE POS DIVIDEND, THEN CHK
						; 5842	DIVS2:	DIVIDE,AR_2(AR+BR),
U 0456, 0522,0602,5504,3001,0020,0031,0000	; 5843			ARX/ADX*2,J/DIVS4	;D'END POS, D'SOR NEG
U 0457, 0456,5163,7700,0000,0020,0027,0000	; 5844		AR_-AR LONG,J/DIVS2
						; 5845	
						; 5846	=010
						; 5847	DIVS3:	DIVIDE,AR_2(AR+BR),ARX/ADX*2,
U 0462, 0540,0602,5504,3001,0020,0071,0005	; 5848			ARL/AD*2,CALL.M,J/DIVLP	;START DIVIDING
U 0463, 0003,4001,0000,0000,0217,0003,0000	; 5849		I FETCH,RETURN3			;RETURN TO CALLER WITH NO DIVIDE
						; 5850	
U 0466, 0006,5162,2020,0000,0020,0003,0000	; 5851		AR_-BR,BRX/ARX,RETURN6		;D'END NEG, SO NEGATE QUO & REM
U 0467, 0007,0001,0020,0000,0000,0003,0000	; 5852		BRX/ARX,RETURN7			;EVERYTHING POSITIVE
						; 5853	
						; 5854	
						; 5855	=010
						; 5856	DIVS4:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,
U 0522, 0540,5102,5500,3001,0020,0071,0005	; 5857			ARL/AD*2,CALL.M,J/DIVLP	;BEGIN DIVISION FOR REAL BITS
U 0523, 0003,4001,0000,0000,0217,0003,0000	; 5858		I FETCH,RETURN3			;ABORT FOR IMPOSSIBLE DIVISION
						; 5859	
U 0526, 0006,4001,0020,0000,0000,0003,0000	; 5860		BRX/ARX,RETURN6			;NEGATE QUO
U 0527, 0007,5142,2020,0000,0020,0003,0000	; 5861		AR_-BR,BRX/ARX,RETURN7		;NEGATE REM
						; 5862	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; ARITH.MIC[10,5351]	19:52 24-Jul-85			BASIC DIVIDE LOOP					

						; 5863	.TOC	"BASIC DIVIDE LOOP"
						; 5864	; THE LOOP ITSELF IS AN INNER SUBROUTINE, TO MAKE IT SUITABLE
						; 5865	; FOR USE IN DOUBLE-LENGTH DIVISION.
						; 5866	; THE DOUBLE LENGTH REMAINDER IS RETURNED IN BR!BRX (RESTORED)
						; 5867	; THE SINGLE LENGTH QUOTIENT (LOW PART IF DBL-LEN DIVISION) IN ARX
						; 5868	; RETURN 6 IF QUOTIENT (REALLY AC0.XOR.BR) NEGATIVE, OR 7 IF POSITIVE
						; 5869	;[TIME=12+3(FE)+3(RESTORE REQ'D)] ... IF FE=33, 111-114.
						; 5870	
						; 5871	=000
U 0540, 0540,0602,5500,3001,0020,0031,0000	; 5872	DIVLP:	DIVIDE,AR_2(AR+BR),ARX/ADX*2,J/DIVLP
U 0541, 0540,5102,5504,3001,0020,0031,0000	; 5873		DIVIDE,AR_2(AR-BR),ARX/ADX*2,J/DIVLP
U 0542, 0540,5102,5504,3001,0020,0031,0000	; 5874	DIV-:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,J/DIVLP
U 0543, 0540,0602,5500,3001,0020,0031,0000	; 5875	DIV+:	DIVIDE,AR_2(AR+BR),ARX/ADX*2,J/DIVLP
U 0544, 0564,0602,2604,3001,0020,0031,0000	; 5876		DIVIDE,AR_AR+BR,ARX/ADX,J/DIVX
U 0545, 0564,5102,2600,3001,0020,0031,0000	; 5877		DIVIDE,AR_AR-BR,ARX/ADX,J/DIVX
U 0546, 0564,5102,2600,3001,0020,0031,0000	; 5878		DIVIDE,AR_AR-BR,ARX/ADX,J/DIVX		;NO SHIFT ON FINAL STEP
U 0547, 0564,0602,2604,3001,0020,0031,0000	; 5879		DIVIDE,AR_AR+BR,ARX/ADX,J/DIVX
						; 5880	
						; 5881	;HERE AFTER FINAL DIVIDE STEP
						; 5882	; MQ HAS POSITIVE FORM QUOTIENT
						; 5883	; AR!ARX HAS REMAINDER, EXCEPT THAT IT MUST BE RESTORED IF IT IS
						; 5884	; NEGATIVE (IT'S NEGATIVE IF THERE WAS NO CARRY ON FINAL STEP)
						; 5885	; THE ORIGINAL DIVIDEND IS STILL IN AC0, SO WE CHECK ITS SIGN
						; 5886	; TO DETERMINE WHETHER TO NEGATE THE (RESTORED) REMAINDER.
						; 5887	
						; 5888	=100
U 0564, 0565,0602,2600,0000,0020,0027,0000	; 5889	DIVX:	AR_AR+BR LONG			;RESTORE REMAIN WITH POS D'SOR
						; 5890		BR_AR LONG,ARX/MQ,FE_SC,	;LONG REMAIN TO BR, QUO TO ARX
U 0565, 0006,2500,0360,2401,0020,5503,0000	; 5891			SKP AC0+,RETURN6	;RETURN TESTING D'END SIGN
U 0566, 0567,5102,2600,0000,0020,0027,0000	; 5892		AR_AR-BR LONG			;RESTORE REMAIN WITH NEG D'SOR
						; 5893		BR_AR LONG,ARX/MQ,FE_SC,
U 0567, 0006,3200,0360,2401,0020,5503,0000	; 5894			SKP AC0-,RETURN6
						; 5895	
						; 5896	
						; 5897	;SUBROUTINE FOR FIRST PART OF LONG DIVISIONS
						; 5898	; ENTER AT DDVSUB WITH SKP BR0
						; 5899	; RETURN3 IF SHOULD RESUME WITH ADD STEP
						; 5900	; RETURN5 IF SHOULD RESUME WITH SUBTRACT
						; 5901	
						; 5902	=000
U 0720, 0720,0602,5504,3001,0020,0031,0000	; 5903	DDVLP:	AR_2(AR+BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0721, 0720,5102,5500,3001,0020,0031,0000	; 5904		AR_2(AR-BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0722, 0720,5102,5500,3001,0020,0031,0000	; 5905	DDVSUB:	AR_2(AR-BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0723, 0720,0602,5504,3001,0020,0031,0000	; 5906		AR_2(AR+BR),ARX/ADX*2,DIVIDE,J/DDVLP
U 0724, 0003,3723,2010,0301,1000,0003,0040	; 5907		AR_MQ,MQ_AR,FE_#,#/32.,RETURN3
U 0725, 0005,3723,2010,0301,1000,0003,0040	; 5908		AR_MQ,MQ_AR,FE_#,#/32.,RETURN5
U 0726, 0005,3723,2010,0301,1000,0003,0040	; 5909		AR_MQ,MQ_AR,FE_#,#/32.,RETURN5
U 0727, 0003,3723,2010,0301,1000,0003,0040	; 5910		AR_MQ,MQ_AR,FE_#,#/32.,RETURN3
						; 5911	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; ARITH.MIC[10,5351]	19:52 24-Jul-85			DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV

						; 5912	.TOC	"DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV"
						; 5913	
						; 5914		.DCODE
						;;5915	.IFNOT/DBL.INT
						;;5916	114:	EA,	J/UUO
						;;5917		EA,	J/UUO
						;;5918		EA,	J/UUO
						;;5919		EA,	J/UUO
						; 5920	.IF/DBL.INT
D 0114, 4001,0211				; 5921	114:	R,	B/0,	J/DASMD		;DADD
D 0115, 4200,0211				; 5922		R,	B/2,	J/DASMD		;DSUB
D 0116, 4400,0211				; 5923		R,	B/4,	J/DASMD		;DMUL
D 0117, 4000,0210				; 5924		R,		J/DDIV
						; 5925		.UCODE
						; 5926	
						; 5927	;HERE FOR DOUBLE WORD ADD, SUBTRACT, MULTIPLY, OR DIVIDE
						; 5928	;ENTER WITH (E) IN AR, E IN VMA
						; 5929	
						; 5930	=0****00**00
U 0210, 1732,3240,0205,0000,0020,0710,0003	; 5931	DDIV:	ARX_AC3,CLR MQ,J/DDIV0		;GET LOWEST PART OF D'END
						; 5932	
						; 5933	DASMD:	BR/AR,AR_AC1*2,ARL/AD*2,	;HIGH MEM WORD TO BR
						; 5934			VMA_VMA+1,LOAD ARX,	;ASK FOR LOW WORD
U 0211, 0770,3200,5041,0000,0033,3662,0105	; 5935			MQ_0.S,CALL.S,J/XFERW	;AND WAIT FOR IT
U 0213, 1572,3721,0500,0000,0000,0010,0000	; 5936	=11	ARX_ARX*2			;SHIFT LOW MEM WORD LEFT
						; 5937	=	BRX/ARX,ARX_AR,AR_AC0,		;ALL DATA IN PLACE
U 1572, 0470,3240,2420,0302,1020,0033,0043	; 5938			SC_#,#/35.,B DISP	;DO THE OPERATION
						; 5939	
						; 5940	;HERE WITH (E) IN BR, (E+1)*2 IN BRX
						; 5941	; (AC) IN AR, (AC+1)*2 IN ARX
						; 5942	
U 0470, 0061,0602,2604,0000,0022,1327,0000	; 5943	=00*	AR_AR+BR LONG,AD FLAGS,EXIT DBL	;DADD
						; 5944	
U 0472, 0061,5102,2600,0000,0022,1327,0000	; 5945		AR_AR-BR LONG,AD FLAGS,EXIT DBL	;DSUB
						; 5946	
						; 5947		MQ_SHIFT,AR_0S,ARX_0S,		;DMUL, USE AC1 AS INITIAL M'IER
U 0474, 0530,3441,2210,0301,0000,0010,0756	; 5948			FE_#,#/-18.,J/DMULT	;SETUP STEP COUNT
						; 5949	=
						; 5950	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; ARITH.MIC[10,5351]	19:52 24-Jul-85			DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV

						; 5951	;HERE FOR DOUBLE WORD MULTIPLY
						; 5952	
						; 5953	=00*
U 0530, 0320,3401,0010,4001,0000,0070,0000	; 5954	DMULT:	AD/0S,MUL,CALL.M,J/MULP		;BEGIN MULTIPLY
U 0534, 0536,0602,2600,0000,0020,0027,0000	; 5955	=10*	AR_AR+BR LONG			;CANCEL EFFECTS OF LOW BIT 0
U 0536, 1607,3721,2010,0000,1000,0010,0000	; 5956		MQ_AR,AR_MQ			;EXCH HI AND LOW PRODUCT WORDS
						; 5957	
						; 5958	;HERE AFTER 1ST CALL ON MPY SUBR.  SAVE LOW WORD OF PROD, GET HIGH M'IER
						; 5959	
U 1607, 1612,4001,0005,0000,0000,1010,0000	; 5960		AC3_AR				;LOW WORD OF PRODUCT
U 1612, 0740,3240,2000,0000,0020,0010,0000	; 5961		AR_AC0				;GET HIGH M'IER WORD
						; 5962	=000	MQ_AR,AR_MQ,CALL,		;DIVE IN AGAIN
U 0740, 1542,3721,2010,0301,1000,0050,0756	; 5963			FE_#,#/-18.,J/MULREE	;CONTINUE THE MULTIPLY
U 0744, 0746,3602,0000,0000,0020,5510,0000	; 5964	=100	GEN AR*BR,AD/AND,SKP AD0	;SKP IF M'IER, M'CAND, & PROD NEG
						; 5965	=110
						; 5966	DMUL1:	AC0_AR,AR_SIGN,
U 0746, 1622,5401,2000,0302,0020,1016,0043	; 5967			SC_#,#/35.,J/DMUL2	;STORE HIGH WORD OF PRODUCT
U 0747, 0746,4001,0000,0000,0000,1110,0420	; 5968		SET AROV,J/DMUL1
						; 5969	
						; 5970	;MULTIPLY NOW COMPLETE, STORE RESULTS WITH PROPER SIGN IN BIT 0
						; 5971	
U 1622, 1626,4001,4040,0000,0000,0010,0000	; 5972	DMUL2:	BR/AR,AR_SHIFT			;GET 2ND WITH SIGN, SAVE SIGN
U 1626, 1632,4001,4301,0000,2000,1010,0000	; 5973		AC1_AR,AR_ARX,ARX/MQ		;READY TO BUILD 3RD WORD
U 1632, 1712,3242,2400,0000,0000,0012,0000	; 5974		ARX_SHIFT,AR_BR,MQ_MQ*2		;SIGNIFICANT BITS TO ARX, SIGN TO AR
						; 5975		AR_SHIFT,ARX_AC3,		;3RD WORD IN AR, GET LOW
U 1712, 1716,3240,4215,0000,0020,0012,0000	; 5976			MQ_MQ*.25		;EXTRA PROD BIT TO MQ 35
U 1716, 0631,3723,2004,0000,0000,1010,0000	; 5977		AC2_AR,AR_MQ			;,I FETCH WHEN TIMING FIXED
						; 5978	=0*	ARX_SHIFT,AR_BR,I FETCH,	;LOW WORD AND SIGN READY
U 0631, 2662,3202,2400,0000,0217,0050,0000	; 5979			CALL,J/SHIFT		; GET LOW WORD TO AR
U 0633, 0221,4001,0005,0000,0000,1010,0000	; 5980	STRAC3:	AC3_AR,FINISH			;GANZ GETAN
						; 5981	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; ARITH.MIC[10,5351]	19:52 24-Jul-85			DOUBLE INTEGER ARITHMETIC -- DADD, DSUB, DMUL, DDIV

						; 5982	;HERE FOR DOUBLE INTEGER DIVISION
						; 5983	;AR HAS (E), ARX HAS (AC3), AND MQ IS CLEAR
						; 5984	
U 1732, 1736,3243,4507,4402,2000,1010,0166	; 5985	DDIV0:	T0_AR,AR_ARX,ARX_ARX*8,SC_1	;SAVE (E) IN T0
						; 5986		BRX/ARX,ARX_SHIFT,		;AC3 3-35 TO BRX, 1-2 TO ARX
U 1736, 1742,3200,2424,0302,0020,0010,0002	; 5987			AR_AC2,SC_#,#/2		;GET AC2 READY
						; 5988		AR_SHIFT,BR/AR,			;AC2 BITS 2-35 WITH AC3 1-2
U 1742, 1747,3200,4241,0000,0020,3610,0000	; 5989			ARX_AC1,VMA_VMA+1	;READY TO GET (E+1)
						; 5990		BR/AR,AR_ARX,ARX_BR*2,		;LOW DOUBLE WORD NOW IN BR LONG
U 1747, 1752,3241,4240,4403,2000,0010,0000	; 5991			SC_1,FE_1
U 1752, 2132,3240,2400,0000,0020,5510,0000	; 5992		ARX_SHIFT,AR_AC0,SKP AD0	;HIGH DOUBLEWORD IN AR LONG
						; 5993	=0
						; 5994	DDIV1:	BR_AR LONG,AR_BRX,ARX_BR,	;HI POS D'END TO BR
U 2132, 1224,3242,6260,0000,0012,0010,0000	; 5995			LOAD AR,J/DDIV2		;GET LOW D'SOR READY
						; 5996		BR_AR LONG,AR_-BR LONG,		;NEGATE LOW D'END
U 2133, 2134,5142,2660,3401,0040,5427,0000	; 5997			FE_-1,SKP CRY0		;TEST FOR CARRY PROPAGATION
U 2134, 2132,2502,2660,0000,0000,0010,0000	; 5998	=0	BR_AR LONG,AR_BR COMP LONG,J/DDIV1
U 2135, 2132,5162,2660,0000,0020,0027,0000	; 5999		BR_AR LONG,AR_-BR LONG,J/DDIV1	;FINISH NEGATION OF D'END
						; 6000	=0*
						; 6001	DDIV2:	T1_AR,MQ_ARX,ARX_0S,		;LOWEST D'END TO T1, NEXT TO MQ
U 1224, 0770,3441,0217,0000,2000,1050,0171	; 6002			CALL,J/XFERW		; WAIT FOR (E+1)
U 1226, 2140,3200,2407,0000,0040,5110,0166	; 6003		ARX_SHIFT,AR_T0,SKP FE0		;DIVISOR NOW IN AR LONG
						; 6004	=0	AR_BR LONG,BR_AR LONG,		;PUT OPERANDS IN PLACE FOR DIV
U 2140, 0473,3202,2660,0000,0020,0032,0000	; 6005			SIGNS DISP,J/DDIV3	;TEST D'SOR SIGN
						; 6006		AR_BR LONG,BR_AR LONG,SET SR2,	;NOTE D'END NEGATIVE
U 2141, 0473,3202,2660,0000,0020,1632,0062	; 6007			SIGNS DISP,J/DDIV3
						; 6008	
						; 6009	;HERE WITH THE DIVISOR IN BR LONG,
						; 6010	; THE HIGH PART OF THE MAGNITUDE OF THE DIVIDEND IN AR LONG,
						; 6011	; AND THE LOW PART OF THE MAGNITUDE OF THE DIVIDEND IN MQ AND T1
						; 6012	; SKIP IF DIVISOR NEGATIVE, & CHECK FOR NO-DIVIDE.
						; 6013	=1011
						; 6014	DDIV3:	AR_2(AR-BR),ARX/ADX*2,MQ_MQ*2,	;SEE IF FIRST DIVIDE STEP
U 0473, 1040,5102,5504,0000,0040,5412,0000	; 6015			SKP CRY0,J/DDIV4	; GENERATES A 1
U 0477, 1040,0602,5500,0000,0040,5412,0000	; 6016		AR_2(AR+BR),ARX/ADX*2,MQ_MQ*2,SKP CRY0
						; 6017	=000
U 1040, 0720,4001,0000,0301,0000,4250,0041	; 6018	DDIV4:	FE_#,#/33.,SKP BR0,CALL,J/DDVLP	;GO DO FIRST HALF OF DIVIDE
U 1041, 0413,0001,0000,0000,0217,0010,0000	; 6019		I FETCH,J/NODIVD		;TOO MANY QUOTIENT BITS
U 1043, 1756,4001,0001,0402,0000,1010,0000	; 6020	=011	AC1_AR,CLR SC,J/DDIV6		;SAVE HI QUOTIENT IN AC1
U 1045, 1756,4001,0001,3402,0000,1010,0000	; 6021	=101	AC1_AR,SC_1S			;SET FLAG FOR RESUMPTION
						; 6022	=
U 1756, 1024,3200,2007,0000,0020,0010,0171	; 6023	DDIV6:	AR_T1				;GET LOWEST DIVIDEND BITS
						; 6024	=100	MQ_AR,AR_MQ,CALL,		;FINISH DIVISION, GENERATING
U 1024, 0540,3721,2010,0000,1000,4750,0000	; 6025			SKP SC0,J/DIVLP		; 35 MORE QUOTIENT BITS
U 1026, 0535,3240,2001,0000,0020,1605,0061	; 6026	=110	AR_AC1,SR DISP,SET SR3,J/DDVX1	;QUOTIENT NEGATIVE.  NOTE
U 1027, 0535,3240,2001,0000,0020,0005,0000	; 6027		AR_AC1,SR DISP			;HERE'S HIGH PART OF QUOTIENT
						; 6028	=1101
U 0535, 1771,3242,2660,0000,0000,0010,0000	; 6029	DDVX1:	BR_AR LONG,AR_BR LONG,J/DDVX2	;POS REMAINDER.  GO STORE
U 0537, 1771,5142,2660,0000,0020,0027,0000	; 6030		BR_AR LONG,AR_-BR LONG,J/DDVX2	;NEGATE REMAINDER
U 1771, 1776,5441,2004,0302,0020,1016,0043	; 6031	DDVX2:	AC2_AR,AR_SIGN,SC_#,#/35.
U 1776, 0116,4001,4000,0000,0000,0005,0000	; 6032		AR_SHIFT,SR DISP		;GET LOW WORD OF REM.  TEST QUO SIGN
U 0116, 0061,3202,2505,0000,0002,1010,0000	; 6033	=1110	AC3_AR,AR_BR,ARX/ADX*2,EXIT DBL	;GET QUOTIENT, SQUEEZE OUT HOLE
						; 6034		AC3_AR,AR_-BR,ARX/ADX*2,AD LONG,;GET NEGATIVE QUOTIENT
U 0117, 0061,5162,2505,0000,0022,1027,0000	; 6035			EXIT DBL
						; 6036	.ENDIF/DBL.INT
						; 6037	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR	

						; 6038	.TOC	"SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR"
						; 6039	
						; 6040		.DCODE
						; 6041	
D 0140, 4100,0310				; 6042	140:	R,	FL-AC,	B0/0,	J/FAD
D 0141, 4001,0313				; 6043		R,		B0/0,	J/FPNO
D 0142, 6201,0310				; 6044		RW,	FL-MEM,	B0/0,	J/FAD
D 0143, 6300,0310				; 6045		RW,	FL-BOTH,B0/0,	J/FAD
						; 6046	
D 0144, 4100,0203				; 6047		R,	FL-AC,		J/FADR
D 0145, 0100,0202				; 6048		I,	FL-AC,	B0/0,	J/FADRI
D 0146, 6201,0203				; 6049		RW,	FL-MEM,		J/FADR
D 0147, 6300,0203				; 6050		RW,	FL-BOTH,	J/FADR
						; 6051	
D 0150, 4501,0310				; 6052	150:	R,	FL-AC,	B0/1,	J/FSB
D 0151, 4400,0313				; 6053		R,		B0/1,	J/FPNO
D 0152, 6600,0310				; 6054		RW,	FL-MEM,	B0/1,	J/FSB
D 0153, 6701,0310				; 6055		RW,	FL-BOTH,B0/1,	J/FSB
						; 6056	
D 0154, 4101,0207				; 6057		R,	FL-AC,		J/FSBR
D 0155, 0501,0202				; 6058		I,	FL-AC,	B0/1,	J/FSBRI
D 0156, 6200,0207				; 6059		RW,	FL-MEM,		J/FSBR
D 0157, 6301,0207				; 6060		RW,	FL-BOTH,	J/FSBR
						; 6061		.UCODE
						; 6062	
						; 6063	.IFNOT/FPLONG
						; 6064	=0****00**00
						; 6065	FAD:
U 0310, 0203,4001,0000,0000,0000,1633,0001	; 6066	FSB:	SR_#,#/1,B DISP,J/FADR		;FLAG NO ROUND, GO FAD/FSB
U 0311, 1413,0001,0000,0000,0000,1610,0001	; 6067	FMP:	SR_#,#/1,J/FMPR
U 0312, 1415,0001,0000,0000,0000,1610,0001	; 6068	FDV:	SR_#,#/1,J/FDVR
U 0313, 1002,3242,2000,0000,0000,0010,0000	; 6069	FPNO:	AR_BR,J/UUO
						; 6070	=
						;;6071	.IF/FPLONG
						;;6072	=0****00***0
						;;6073	FAD:
						;;6074	FSB:	SR_#,#/1,B DISP,J/FADR		;FLAG TRUNCATE MODE, GO FAD
						;;6075	FADL:
						;;6076	FSBL:	SR_#,#/2,B DISP,J/FADR		;FLAG LONG MODE
						; 6077	.ENDIF/FPLONG
						; 6078	=
						; 6079	=0****00*010
						; 6080	FADRI:
U 0202, 0203,0001,4000,0000,3000,0033,0000	; 6081	FSBRI:	AR_AR SWAP,B DISP
						; 6082	FADR:	FE_EXP,EXP_SIGN,SC/SCAD,
U 0203, 2046,3441,0200,0203,1000,0022,0200	; 6083			ARX_0S,J/FAS
						; 6084	=111
U 0207, 2042,3401,0200,0203,1000,0022,0200	; 6085	FSBR:	FE_EXP,SC/SCAD,EXP_SIGN,ARX_0S
U 2042, 2046,5143,7000,0000,0020,0010,0000	; 6086	=	AR_-AR,J/FAS			;NEGATE SUBTRAHEND
						; 6087	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE FLOATING ADD & SUB -- FAD, FADR, FSB, FSBR	

						; 6088	;FIND OPERAND WITH LARGER EXP, LEAVING IT IN BR,
						; 6089	; AND ITS EXP-1 IN FE.  THE SMALLER OPERAND IS LEFT IN AR,
						; 6090	; SHIFTED RIGHT BY THE DIFFERENCE BETWEEN THE EXPONENTS -1
						; 6091	
U 2046, 2052,3240,2060,0000,0020,0010,0000	; 6092	FAS:	BR/AR,BRX/ARX,AR_AC0		;SAVE MEM OP IN BR, GET AC
U 2052, 2142,0001,0000,5202,1020,5122,0200	; 6093		SC_EXP-SC,EXP_SIGN,SKP SCAD0	;FIND LARGER OPERAND
U 2142, 2056,3241,2040,2001,0000,0010,0000	; 6094	=0	FE_FE+SC,BR/AR,AR_BR*2,J/FAS1	;AC EXP .GE. MEM
						; 6095		MQ_AR,SC_#+SC,#/37.,		;MEM OP LARGER, SHIFT AC OP
U 2143, 2150,0001,0010,2302,1020,5110,0045	; 6096			SKP SCAD0,J/FAS2	;COMPUTE SHIFT AMOUNT
						; 6097	
U 2056, 2150,0001,0010,5302,1020,5110,0044	; 6098	FAS1:	MQ_AR,SC_#-SC,#/36.,SKP SCAD0	;CHECK SHIFT AMOUNT
						; 6099	=0
U 2150, 2061,5441,2310,0000,0020,0016,0000	; 6100	FAS2:	MQ_SHIFT,ARX/MQ,AR_SIGN,J/FAS3	;LOW TO MQ, READY TO GET HI
						; 6101		AR_SIGN,ARX_AR,			;HERE IF EXP DIFF .GT. 36
U 2151, 2152,5401,2400,2302,1020,5116,0044	; 6102			SC_#+SC,#/36.,SKP SCAD0	; .GT. 72?
U 2152, 2064,0001,0400,4001,0001,0010,0100	; 6103	=0	ARX_SHIFT,MQ_0.M,FE_FE+1,J/FAS5
U 2153, 2064,4001,0400,4001,1001,0010,0100	; 6104		ARX_AR,MQ_0.M,FE_FE+1,J/FAS5	;SHIFTED CLEAR OUT
						; 6105	
						; 6106	FAS3:	AR_SHIFT,ARL/SH,ARX/MQ,
U 2061, 2064,4001,4300,4001,0001,0010,0104	; 6107			MQ_0.M,FE_FE+1		;READY TO ADD
						; 6108	
						; 6109	FAS5:	AR_(AR+2BR)*.25,ARX/ADX*.25,	;HERE FOR ADD OR SUB
U 2064, 1120,0601,7704,0000,0060,0035,0000	; 6110			NORM,J/SNORM
						; 6111	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE FLOATING MULTIPLY -- FMP, FMPR			

						; 6112	.TOC	"SINGLE FLOATING MULTIPLY -- FMP, FMPR"
						; 6113	
						; 6114		.DCODE
D 0160, 4101,0311				; 6115	160:	R,	FL-AC,	J/FMP
D 0161, 4001,0313				; 6116		R,		J/FPNO
D 0162, 6200,0311				; 6117		RW,	FL-MEM,	J/FMP
D 0163, 6301,0311				; 6118		RW,	FL-BOTH,J/FMP
						; 6119	
D 0164, 4100,1413				; 6120		R,	FL-AC,	J/FMPR
D 0165, 0100,1412				; 6121		I,	FL-AC,	J/FMPRI
D 0166, 6201,1413				; 6122		RW,	FL-MEM,	J/FMPR
D 0167, 6300,1413				; 6123		RW,	FL-BOTH,J/FMPR
						; 6124		.UCODE
						;;6125	.IF/FPLONG
						;;6126	=0****00***0
						;;6127	FMP:	SR_#,#/1,J/FMPR			;FLAG TRUNCATE MODE
						;;6128	FMPL:	SR_#,#/2,J/FMPR			;LONG MODE
						;;6129	=
						; 6130	.ENDIF/FPLONG
						; 6131	=0****00***0
U 1412, 1413,4001,4000,0000,3000,0010,0000	; 6132	FMPRI:	AR_AR SWAP
U 1413, 2066,3441,0200,0202,1000,0022,0200	; 6133	FMPR:	SC_EXP,EXP_SIGN,ARX_0S		;PREPARE M'IER FRACTION
						; 6134	
U 2066, 0122,3240,2010,0301,1020,0010,0762	; 6135	=	MQ_AR,AR_AC0,FE_#,#/-14.	;M'IER TO MQ, GET M'CAND
						; 6136	=01*	SC_EXP+SC,EXP_SIGN,		;SEPARATE M'CAND FRACTION FROM EXP
U 0122, 1534,0001,0000,2202,1000,0062,0200	; 6137			CALL.S,J/MULSUB		;AND BEGIN MULTIPLY
U 0126, 1120,3203,0000,2301,0040,0035,0600	; 6138	=11*	FE_#+SC,#/-200,NORM AR,J/SNORM
						; 6139	=
						; 6140	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE FLOATING DIVIDE -- FDV, FDVR			

						; 6141	.TOC	"SINGLE FLOATING DIVIDE -- FDV, FDVR"
						; 6142	
						; 6143		.DCODE
D 0170, 4101,0312				; 6144	170:	R,	FL-AC,	J/FDV
D 0171, 4100,0313				; 6145		R,	FL-AC,	J/FPNO
D 0172, 6200,0312				; 6146		RW,	FL-MEM,	J/FDV
D 0173, 6301,0312				; 6147		RW,	FL-BOTH,J/FDV
						; 6148	
D 0174, 4100,1415				; 6149		R,	FL-AC,	J/FDVR
D 0175, 0100,1414				; 6150		I,	FL-AC,	J/FDVRI
D 0176, 6201,1415				; 6151		RW,	FL-MEM,	J/FDVR
D 0177, 6300,1415				; 6152		RW,	FL-BOTH,J/FDVR
						; 6153		.UCODE
						;;6154	.IF/FPLONG
						;;6155	=0****00***0
						;;6156	FDVL:	FE_EXP-1,EXP_SIGN,ARX+MQ_0.S,J/FDVL1
						;;6157	FDV:	SR_#,#/1,J/FDVR			;FLAG TRUNCATE MODE
						;;6158	=
						; 6159	.ENDIF/FPLONG
						; 6160	=0****00***0
U 1414, 1415,4001,4000,0000,3000,0010,0000	; 6161	FDVRI:	AR_AR SWAP
U 1415, 1060,0001,0000,4202,1000,0022,0340	; 6162	FDVR:	SC_EXP+1,EXP_SIGN,ARX+MQ_0.S	;SETUP DIVISOR
						; 6163	=
						; 6164	=000	BR/AR,BRX/ARX,			;DIVISOR TO BR, CLR BRX
						; 6165			AR_AC0,FE_#,#/27.,	;GET DIVIDEND, STEP COUNT
U 1060, 2162,3200,2060,0301,0020,5550,0033	; 6166			SKP AD0,CALL,J/FDVCHK
						; 6167	
U 1062, 0542,0001,0000,0000,0000,4250,0000	; 6168	=10	SKP BR0,CALL,J/DIV-		;OK, BEGIN DIVISION
U 1063, 0066,4001,0000,0000,0000,1110,0624	; 6169		SET FL NO DIV,J/IFNOP		;NO DIVIDE, SORRY
						; 6170	
						; 6171	;RETURN HERE WITH QUOTIENT IN ARX.  WE TOOK 29 DIVIDE STEPS, TO
						; 6172	; GUARANTEE HAVING A ROUNDING BIT EVEN IF THE FIRST STEP GENERATES
						; 6173	; A QUOTIENT BIT OF ZERO.  THEREFORE, THE MSB OF QUOTIENT IS EITHER
						; 6174	; IN BIT 7 OR 8, AND NORM WILL FIND IT IN ONE STEP.
						; 6175	
						; 6176	=110	AR_ARX,FE_FE+#,#/2,		;NEGATIVE QUOTIENT
U 1066, 2160,7162,4000,2031,2040,5410,0002	; 6177			SKP BR EQ,J/FDVNEG	;CHECK FOR MORE QUO TO COME
						; 6178		AR_ARX*.25,ARX_ARX*.25,NORM,	;JUNK IS 36 BITS AWAY FROM MSB
U 1067, 1120,3713,7700,2031,0040,0035,0002	; 6179			FE_FE+#,#/2,J/SNORM	;POS QUOTIENT, NORMALIZE
						; 6180	=
						; 6181	;HERE IF QUOTIENT SHOULD BE NEGATIVE, WITH POSITIVE FORM IN
						; 6182	; AR AND ARX.  SKIP IF REMAINDER (IN BR) IS ZERO.  IN THIS CASE,
						; 6183	; WE CLEAR ARX, BECAUSE AR CONTAINS THE ENTIRE QUOTIENT.
						; 6184	; IF, HOWEVER, THE REMAINDER IS NOT ZERO, WE INFER
						; 6185	; THAT AN INFINITE PRECISION DIVISION WOULD GENERATE MORE ONES
						; 6186	; IN THE QUOTIENT.  IF THAT IS THE CASE, WE LEAVE ARX WITH THE
						; 6187	; QUOTIENT, SO THE NEGATION PROCESS WILL WORK CORRECTLY TO RETURN
						; 6188	; THE HIGH ORDER PART OF THE INFINITE-PRECISION NEGATIVE QUOTIENT.
						; 6189	=0
U 2160, 1120,3703,7700,0000,0040,1635,0064	; 6190	FDVNEG:	SET SR1,AR_AR*.25 LONG,NORM,J/SNORM
U 2161, 2160,3441,0200,0000,0000,0010,0000	; 6191		ARX_0S,J/FDVNEG			;REMAINDER WENT TO ZERO
						; 6192	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE FLOATING DIVIDE -- FDV, FDVR			

						; 6193	;HERE FOR FDVL
						; 6194	
						;;6195	.IF/FPLONG
						;;6196	
						;;6197	;FDVL:	FE_EXP-1,EXP_SIGN,CLR ARX+MQ
						;;6198	=000
						;;6199	FDVL1:	AR_AC1,BR_AR LONG,		;SAVE DIVISOR IN BR LONG
						;;6200			SC_#,#/9.,CALL		;READY TO SHIFT LOW DIVIDEND
						;;6201		ARX_SHIFT,AR_AC0,		;DIVIDEND IN PLACE
						;;6202			SC_FE,FE_#,#/24.,	;EXP TO SC, STEP COUNT TO FE
						;;6203			SKP AD0,J/FDVCHK	;GO CHECK FOR NO DIVIDE
						;;6204	=010	CALL,SKP BR0,J/FDVL2		;GO BEGIN DIVIDE
						;;6205		SET FL NO DIV,J/IFNOP		;CAN'T DIVIDE, ABORT
						;;6206	
						;;6207	=110	AR_AC0,SR_#,#/5,		;NEG QUO, FLAG TRUNCATE MODE
						;;6208			SR DISP,J/FDVL4		; WAS IT 26 OR 27 STEPS?
						;;6209		AR_AC0,SR_#,#/1,		;POS QUO
						;;6210			SR DISP,J/FDVL4
						;;6211	=
						;;6212	
						;;6213	
						;;6214	;COME HERE TO START THE DIVISION.  ON THE FIRST STEP, WE CHECK
						;;6215	; TO SEE WHETHER A 1 HAS BEEN GENERATED IN THE QUOTIENT.  IF SO,
						;;6216	; 26 ADDITIONAL STEPS WILL GENERATE THE FULL 27 SIGNIFICANT BITS
						;;6217	; OF THE QUOTIENT.  IF NOT, 27 STEPS ARE REQUIRED.
						;;6218	
						;;6219	=0
						;;6220	FDVL2:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,J/FDVL3	;FIRST DIVIDE STEP
						;;6221		DIVIDE,AR_2(AR+BR),ARX/ADX*2		; DOES IT GENERATE A 1?
						;;6222	=00
						;;6223	FDVL3:	DISP/DIV,MQ/MQ*2,		;NO, TAKE AN EXTRA DIVIDE STEP
						;;6224			AR_2(AR+BR),ARX/ADX*2,J/DIVLP	; WITHOUT COUNTING FE
						;;6225		SR_1,SC_#+SC,#/1,J/DIV-		;YES, 27 STEPS WILL NORMALIZE QUO
						;;6226		DISP/DIV,MQ/MQ*2,AR_2(AR-BR),ARX/ADX*2,J/DIVLP
						;;6227		SR_1,SC_#+SC,#/1,J/DIV+
						;;6228	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE FLOATING DIVIDE -- FDV, FDVR			

						;;6229	;WE COME HERE AFTER DOING THE DIVISION, EITHER 26 OR 27 STEPS
						;;6230	; AS REQUIRED TO GENERATE A NORMALIZED QUOTIENT FROM NORMALIZED
						;;6231	; OPERANDS.  NOW FIGURE OUT WHAT EXPONENT THE REMAINDER SHOULD HAVE.
						;;6232	
						;;6233	=0
						;;6234	FDVL4:	SC_EXP-#,#/27.,			;DIVIDEND EXP-27
						;;6235			AR_BR,SKP AR0,J/FDVL6	;GET REMAINDER, TEST D'END SIGN
						;;6236		SC_EXP-#,#/26.,			;D'END EXP-26
						;;6237			AR_BR,SKP AR0
						;;6238	
						;;6239	;HERE WITH REMAINDER IN AR, ITS EXP IN SC
						;;6240	; SKIP IF D'END (AND THEREFORE REM) NEGATIVE.
						;;6241	
						;;6242	=0
						;;6243	FDVL6:	EXP_SC,BYTE DISP,		;TEST FOR UNDERFLOW
						;;6244			SKP AR EQ,J/FDVL7	; OR REM =0
						;;6245		AR_-BR,SKP CRY0,		;NEGATE REM, CHECK =0
						;;6246			GEN SC,BYTE DISP	; AND LOOK FOR EXP UFLO
						;;6247	=110	EXP_-SC-1,J/FDVL7		;ONE'S COMPLEMENT EXP
						;;6248		AR_0S				;REM =0 OR EXP UFLO
						;;6249	=110
						;;6250	FDVL7:	AC1_AR,ARX+MQ_0.M,		;SAVE REMAINDER
						;;6251			AR_MQ,ARL/AD,J/SNR2	;GO NORMALIZE QUOTIENT
						;;6252		AR_0S,J/FDVL7
						; 6253	.ENDIF/FPLONG
						; 6254	
						; 6255	
						; 6256	;SUBR TO CHECK FOR FLOATING NO DIVIDE
						; 6257	; ENTER WITH SKP ON DIVIDEND SIGN, IN AR LONG, WITH
						; 6258	; DIVISOR EXP IN SC, DIVISOR IN BR
						; 6259	
						; 6260	=0
U 2162, 2170,4001,0000,5202,1000,4222,0200	; 6261	FDVCHK:	SC_EXP-SC,EXP_SIGN,SKP BR0,J/FDVCK1
U 2163, 2162,5163,7700,0000,0020,0027,0000	; 6262		AR_-AR LONG,J/FDVCHK		;GET POSITIVE DIVIDEND
						; 6263	=0
						; 6264	FDVCK1:	GEN AR-2BR,SKP CRY0,		;TEST FOR NO DIVIDE
U 2170, 0002,5101,0004,2302,0040,5403,0177	; 6265			SC_#+SC,#/177,RETURN2	;AND CORRECT EXP
						; 6266		GEN AR+2BR,SKP CRY0,		;SAME TEST, NEG DIVISOR
U 2171, 0002,0601,0000,2302,0040,5403,0177	; 6267			SC_#+SC,#/177,RETURN2	;AND SAME EXP CORRECTION
						; 6268	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; FP.MIC[10,5351]	19:52 24-Jul-85			UFA, DFN, FSC						

						; 6269	.TOC	"UFA, DFN, FSC"
						; 6270	
						; 6271		.DCODE
						;;6272	.IF/FPLONG
						;;6273	130:	R,		J/UFA
						;;6274		RPW,		J/DFN
						; 6275	.IFNOT/FPLONG
D 0130, 2000,1002				; 6276	130:	EA,	J/UUO			;UFA
D 0131, 2000,1002				; 6277		EA,	J/UUO			;DFN
						; 6278	.ENDIF/FPLONG
D 0132, 0100,1502				; 6279	132:	I,	FL-AC,	J/FSC		;Must adjoin 133 (IBP/ADJBP)
						; 6280		.UCODE
						; 6281	
						;;6282	.IF/FPLONG
						;;6283	=0****00***0
						;;6284	DFN:	FE_AR0-8,AR0-8_#,#/0,		;SAVE LOW EXP, CLR SO CAN 
						;;6285			ARX_0S,J/DFN1		; DETECT FRACTION = 0
						;;6286	UFA:	FE_EXP,SC/SCAD,EXP_SIGN,ARX_0S
						;;6287	=
						;;6288	=000	BR_AR LONG,AR_AC0,CALL,J/EXPD
						;;6289	=100	ARX_AR,AR_SIGN,ARL/AD,		;READY TO UNNORMALIZE SMALLER OP
						;;6290			CALL.M,J/SHIFT
						;;6291		AR_SIGN,ARX/AD			;LOST SMALLER OP, USE ITS SIGN
						;;6292		AR_AR+BR,SKP AD NE,		;IS RESULT SIGNIFICANT?
						;;6293			SC_FE,I FETCH
						;;6294	=
						;;6295	=0	AC1_AR,J/FINI			;NO, CLEAR RESULT AC
						;;6296		SKP EXP NE,BR/AR		;IS RIGHT SHIFT REQ'D?
						;;6297	=0	SKP AR0,FETCH WAIT,J/UFA4	;NO, IS RESULT NEG?
						;;6298		AR_BR*.5,GEN FE-#,#/377,SKP SCAD NE,FETCH WAIT
						;;6299	=0	FE_-1,SET FLOV
						;;6300		FE_FE+1,SC/SCAD,SKP AR0
						;;6301	=0
						;;6302	UFA4:	AR0-8_SC,J/STAC1		;POS, PUT IN EXP STRAIGHT
						;;6303		AR0-8_-SC-1,J/STAC1		;NEG, USE COMPLEMENT OF EXP
						;;6304	
						;;6305	
						;;6306	DFN1:	AR_-AR,SKP CRY0			; LOW FRACTION =0?
						;;6307	=0	AR0-8_FE,STORE,			;STORE LOW WORD BACK TO MEM
						;;6308			ARX_AC0 COMP,J/STMAC	; GET COMPLEMENTED HIGH WORD
						;;6309		AR0-8_FE,STORE,			;LOW WORD WAS ZERO, INSTALL EXP
						;;6310			ARX_-AC0,J/STMAC	; GET NEGATED HIGH WORD
						; 6311	.ENDIF/FPLONG
						; 6312	;
						; 6313	;FSC
						; 6314	;ENTER WITH E IN AR
						; 6315	1502:					;[345] Next to IBP because of DRAM
						; 6316	FSC:	SC_EA,ARX+MQ_0.M,
U 1502, 2071,3240,2000,0002,0021,0013,0142	; 6317			AR_AC0,ARL/AD
U 2071, 1122,4001,0000,2201,1000,0022,0200	; 6318	=	FE_EXP+SC,EXP_SIGN,J/SNR2	;NORMALIZE SCALED RESULT
						; 6319	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; FP.MIC[10,5351]	19:52 24-Jul-85			FIX, FIXR, FLTR, EXTEND					

						; 6320	.TOC	"FIX, FIXR, FLTR, EXTEND"
						; 6321	
						; 6322		.DCODE
D 0122, 4001,1500				; 6323	122:	R,		J/FIX		;UNROUNDED
D 0123, 4000,1501				; 6324		R,		J/EXTEND	;EXTENDED INSTRUCTION SET
						; 6325	
D 0126, 4000,1417				; 6326	126:	R,		J/FIXR		;ROUNDED
D 0127, 4100,1416				; 6327		R,	FL-AC,	J/FLTR
						; 6328		.UCODE
						; 6329	;FLTR
						; 6330	;ENTER WITH (E) IN AR
						; 6331	=0****00***0
						; 6332	FLTR:	FE_#,#/277,ARX_AR,SKP AR0,	;BINARY POINT TO RIGHT OF ARX
U 1416, 1120,5401,2400,0301,1020,4516,0277	; 6333			AR_SIGN,J/SNORM		; SIGN EXTENDED.  GO NORMALIZE
						; 6334	
						; 6335	;FIX AND FIXR
						; 6336	;ENTER WITH (E) IN AR
						; 6337	;	FIX AND FIXR DIFFER ONLY IN THE ROUNDING CRITERION:
						; 6338	;FIXR ADDS 1 TO THE INTEGER PART IF THE FRACTION PART IS ONE-HALF
						; 6339	;OR GREATER.  FIX DROPS THE FRACTION PART OF POSITIVE NUMBERS, BUT ADDS
						; 6340	;1 TO THE INTEGER PART OF NEGATIVE NUMBERS IF THE FRACTION PART IS NOT
						; 6341	;ALL ZERO.
						; 6342	;	THIS IS IMPLEMENTED BY CHOOSING A FRACTION (THE ROUNDING
						; 6343	;CONSTANT) TO ADD TO THE INPUT, SUCH THAT A CARRY WILL OCCUR INTO THE
						; 6344	;INTEGER PART UNDER THE APPROPRIATE CONDITIONS.  FOR FIXR, THE ROUNDING
						; 6345	;CONSTANT IS EXACTLY ONE-HALF.  FOR FIX, IT IS ZERO ON POSITIVE INPUT,
						; 6346	;OR THE LARGEST POSSIBLE FRACTION (ALL 1S) ON NEGATIVE INPUT.
						; 6347	
						; 6348	=0****00****
						; 6349	FIXR:	FE_EXP-#,#/244,SKP SCAD0,	;GET BINARY POINT POSITION
U 1417, 2172,4061,0700,5231,0020,5110,0244	; 6350			ARX_1B1,J/FIX1		;GET ROUNDING CONSTANT
						; 6351	=
						; 6352	=0****00***0
						; 6353	FIX:	FE_EXP-#,#/244,SKP SCAD0,	;GET BINARY POINT POSITION
U 1500, 2172,5401,0200,5231,0020,5116,0244	; 6354			ARX_AR SIGN,J/FIX1	;SET ROUNDING CONSTANT, GO FIX
						; 6355	
						; 6356	.IF/EXTEND
						;;6357	.IFNOT/XADDR
						;;6358	.IF/MODEL.B				;[246]
						;;6359	EXTEND:	FE_#+AR0-8,#/-32,SKP SCAD0,	;VALID EXTENDED OPERATION?
						;;6360			ARX_AR,AR_BRX,J/EXT1	; OPR TO ARX, AC TO AR
						;;6361	.IFNOT/MODEL.B
						;;6362	EXTEND:	FE_#+AR0-8,#/-20,SKP SCAD0,	;[246] VALID EXTENDED OPERATION?
						;;6363			ARX_AR,AR_BRX,J/EXT1	;[246] OPR TO ARX, AC TO AR
						;;6364	.ENDIF/MODEL.B				;[246]
						; 6365	.IF/XADDR
						; 6366	EXTEND:	SC_#+AR0-8,#/-32,SKP SCAD0,	;VALID EXTENDED OPERATION?
U 1501, 2700,3202,2400,2322,1020,5110,0746	; 6367			ARX_AR,AR_BR,J/EXTF1	; OPR TO ARX, AC TO AR
						; 6368	.ENDIF/XADDR
						;;6369	.IFNOT/EXTEND
						;;6370	EXTEND:	AR_BR,J/UUO
						; 6371	.ENDIF/EXTEND
						; 6372	=
						; 6373	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; FP.MIC[10,5351]	19:52 24-Jul-85			FIX, FIXR, FLTR, EXTEND					

						; 6374	;HERE FOR FIX.  CONVERT FLOATING TO INTEGER
						; 6375	
						; 6376	=0
U 2172, 0066,4001,0000,0000,0000,1110,0420	; 6377	FIX1:	SET AROV,J/IFNOP		;CAN'T DO IT, GIVE UP
U 2173, 2074,3721,0540,0000,0000,0410,0000	; 6378		BR/AR,CLR AR,ARX_ARX*2		;ROUNDING CONSTANT READY IN ARX
						; 6379		BR_AR LONG,AR_BR,CLR ARX,	;MANTISSA TO AR LONG
U 2074, 2076,3202,2060,0302,0000,0510,0011	; 6380			SC_#,#/9.		;READY TO SHIFT OFF EXPONENT
						; 6381		ARX_SHIFT,AR_SIGN,		;MANTISSA LEFT ALIGNED IN ARX
U 2076, 2200,5401,2400,2032,0020,5116,0044	; 6382			SC_FE+#,#/36.,SKP SCAD0	;ANY INTEGER BITS?
						; 6383	=0	MQ_SHIFT,			;YES, PUT THEM IN MQ
						; 6384			AR_ARX (ADX),CLR ARX,	;SHIFT MANTISSA LEFT 36 PLACES
U 2200, 2106,3701,6010,0000,0217,0510,0000	; 6385			I FETCH,J/FIX2		;AND PREFETCH NEXT
U 2201, 0072,3401,2000,0000,0217,0010,0000	; 6386		AR_0S,I FETCH,J/STORAC		;ALL SIGNIFICANCE LOST
U 2106, 2112,3723,2400,0000,0000,0010,0000	; 6387	FIX2:	ARX_SHIFT,AR_MQ			;INTEGER IN AR, FRACTION IN ARX
U 2112, 0065,0602,2000,0000,0020,0027,0000	; 6388		AR_AR+BR,AD LONG,J/STAC		;ROUND AND STORE
						; 6389	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE PRECISION FLOATING NORMALIZATION			

						; 6390	.TOC	"SINGLE PRECISION FLOATING NORMALIZATION"
						; 6391	
						; 6392	;HERE TO NORMALIZE SINGLE PRECISION RESULTS
						; 6393	;SR2-3 TELL HOW TO STORE RESULTS:
						; 6394	;XX00 ... ROUND, SINGLE PRECISION
						; 6395	;XX01 ... TRUNCATE, SINGLE PRECISION
						; 6396	;XX10 ... LONG MODE (IMPLIES TRUNCATION)
						; 6397	;IN ADDITION, THIS CODE SETS SR 1 IF ANSWER IS NEGATIVE, SO X1YZ
						; 6398	; CORRESPONDS TO X0YZ EXCEPT THAT THE RESULT MUST BE NEGATED.
						; 6399	
						; 6400	;DISPATCH TO SNORM WITH "DISP/NORM,AR/AD*.25"
						; 6401	; THUS THE 8 POSSIBILITIES ARE:
						; 6402	;SNORM		AD=0	AR=0	EITHER ANSWER IS ZERO, OR MSB IS IN ARX
						; 6403	;SNORM+1	AD0	AR NEG	RESULT IS NEG.  MAKE POS, TRY AGAIN
						; 6404	;SNORM+2	AD1-6	AR3-8	MSB TOO FAR LEFT, SHIFT RIGHT & RETRY
						; 6405	;SNORM+3	AD7	AR9	RESULT IS CORRECTLY NORMALIZED
						; 6406	;SNORM+4	AD8	AR10	SHIFT LEFT ONCE FOR NORMALIZATION
						; 6407	;SNORM+5	AD9	AR11	SHIFT LEFT 2 PLACES
						; 6408	;SNORM+6	AD10	AR12	SHIFT LEFT THRICE
						; 6409	;SNORM+7	AD11-35	AR13-35	SHIFT LEFT A LOT, TRY AGAIN
						; 6410	
						; 6411	=000
						; 6412	SNORM:	AR_ARX,ARL/SH,SKP ARX NE,	;AR IS ZERO, GET ARX
U 1120, 2204,3713,4000,0000,2041,5410,0044	; 6413			ARX_0.M,J/SNZERO
U 1121, 1120,5143,7700,0000,0060,1635,0064	; 6414		NORM -AR,SET SR1,J/SNORM	;REMEMBER NEGATIVE, GO POSITIVE
						; 6415	SNR2:	AR_AR*.25 LONG,FE_FE+#,#/2,	;SHIFT RIGHT,
U 1122, 1120,3701,7700,2031,0040,0035,0002	; 6416			NORM,J/SNORM		;TRY AGAIN
U 1123, 0024,0001,0000,0000,0000,0005,0000	; 6417		SR DISP,J/SROUND		;AD7 -> AR9, IS ROUND REQ'D?
						; 6418		AR_AR*2 LONG,FE_FE-1,		;AD8 -> AR10, ONCE LEFT AND DONE
U 1124, 0024,3701,5500,3001,0000,0005,0000	; 6419			SR DISP,J/SROUND
						; 6420		AR_AR*4 LONG,FE_FE-#,#/2,	;AD9 -> AR11
U 1125, 0024,3243,2600,5031,0000,0005,0002	; 6421			SR DISP,J/SROUND
						; 6422		AR_AR*8 LONG,FE_FE-#,#/3,	;AD10 -> AR12
U 1126, 0024,3243,5500,5031,0000,0005,0003	; 6423			SR DISP,J/SROUND
						;;6424	.IFNOT/SNORM.OPT
						;;6425		SKP AR NE,INH CRY18,SC_#,#/7	;LOOK FOR AR13-17
						;;6426	=0	SC_#,#/13.			;LH IS 0.  SHIFT FARTHER
						;;6427		MQ_SHIFT,AR_ARX (ADX),CLR ARX,	;HIGH TO MQ, GET READY FOR LOW
						;;6428			FE_FE-SC		; ADJUST EXPONENT
						;;6429		ARX_SHIFT,AR_MQ,J/SNR2		;FRACTION REPOSITIONED. GO AGAIN
						;;6430	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE PRECISION FLOATING NORMALIZATION			

						;;6431	;HERE IS THE FASTER VERSION OF LONG NORMALIZATION SHIFTS,
						;;6432	; WHICH TAKES FOUR WORDS MORE BUT IS A BIT QUICKER IN THE
						;;6433	; INTERMEDIATE NORMALIZATION CASES.
						;;6434	
						; 6435	.IF/SNORM.OPT
						; 6436		ADA EN/0S,ADB/AR*4,AD/ANDCA,	;GENERATE AR*4
						; 6437			AR/AD*2,ARX/ADX*2,	; AR_AR*8 LONG
						; 6438			SC_#,#/12.,		;READY TO SHIFT FARTHER
U 1127, 2202,3043,5500,0302,0020,5417,0014	; 6439			GEN CRY18,SKP CRY0	; TEST AR0-19 FOR ZERO
						; 6440	
						; 6441	=0	AR_AR*8 LONG,BR_AR LONG,	;IT WAS IN AR13-19
U 2202, 1144,3203,5560,5031,0040,0035,0006	; 6442			FE_FE-#,#/6,NORM,J/SN1	; NOW IN AR10-16, AD8-14
						; 6443		MQ_SHIFT,AR_ARX (ADX),		;13-19=0, SHIFT TO TRY 20-35
U 2203, 2116,3721,6010,0302,0000,0510,0012	; 6444			CLR ARX,SC_#,#/10.
						; 6445		ARX_SHIFT,AR_MQ*.25,		;REPOSITION FRACTION IN AR LONG
						; 6446			FE_FE-#,#/13.,		;COMPENSATE EXPONENT
U 2116, 1120,3721,7400,5031,0040,0035,0015	; 6447			NORM,J/SNORM
						; 6448	=100
						; 6449	SN1:	AR_BR*2 LONG,FE_FE+#,#/2,	;MSB IN AD8, SO IN BR10
U 1144, 0024,3241,2600,2031,0000,0005,0002	; 6450			SR DISP,J/SROUND
						; 6451		AR_BR*4 LONG,FE_FE+1,		;MSB IN AD9, THUS IN BR11
U 1145, 0024,3241,5500,4001,0000,0005,0000	; 6452			SR DISP,J/SROUND
U 1146, 0024,0001,0000,0000,0000,0005,0000	; 6453		SR DISP,J/SROUND		;AD10 -> AR9, A LUCKY GUESS
						; 6454		AR_AR*8 LONG,BR_AR LONG,	;TRY SHIFTING 3 MORE
U 1147, 1144,3203,5560,5031,0040,0035,0003	; 6455			FE_FE-#,#/3,NORM,J/SN1
						; 6456	.ENDIF/SNORM.OPT
						; 6457	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE PRECISION FLOATING NORMALIZATION			

						; 6458	;HERE WHEN AD ENTIRELY ZERO ON NORMALIZE ATTEMPT.  SKIP IF ARX
						; 6459	; IS NOT ZERO, HAVING COPIED IT TO AR (IE, LEFT SHIFT 36 PLACES).
						; 6460	; OTHERWISE, THE ENTIRE RESULT IS ZERO, SO WE STORE THAT.
						; 6461	=0
						; 6462	SNZERO:	CLR FE,AR+ARX+MQ_0.M,		;RESULT = 0
U 2204, 0151,4001,0000,0401,0001,0005,0170	; 6463			SR DISP,J/SRND5
						; 6464		AR_AR*.25 LONG,FE_FE-#,#/34.,	;HAVE MOVED LEFT 36, GO RIGHT 2
U 2205, 1120,3701,7700,5031,0040,0035,0042	; 6465			NORM,J/SNORM		;AND TRY THAT
						; 6466	
						; 6467	
						; 6468	;WE GET HERE WITH A NORMALIZED POSITIVE FRACTION IN AR'ARX,
						; 6469	; THE CORRECTED EXPONENT IN FE, AND SR INDICATES THE PROPER SIGN
						; 6470	; FOR THE RESULT AND WHETHER THE ANSWER SHOULD BE ROUNDED,
						; 6471	; TRUNCATED, OR LONG.
						; 6472	
						;;6473	.IF/FPLONG
						;;6474	=100
						; 6475	.IFNOT/FPLONG
						; 6476	=1*0
						; 6477	.ENDIF/FPLONG
U 0024, 0035,3441,2060,0000,0000,0010,0000	; 6478	SROUND:	BR_AR LONG,AR_0S,J/SRND2	;PREPARE TO ROUND BY ADDING THE
						; 6479						; PART OF THE FRACTION WE WILL
						; 6480						; DISCARD (CARRY IF ARX0)
						; 6481		BR_AR LONG,CLR AR,ARX_1S,	;TRUNCATE MODE
U 0025, 0031,2301,0260,0000,0000,0405,0000	; 6482			SR DISP,J/STRNC		; HANDLING DEPENDS ON SIGN
						;;6483	.IF/FPLONG
						;;6484		BR_AR LONG,CLR AR,ARX_1S,	;LONG MODE
						;;6485			SC_#,#/9.
						;;6486	=	ARX_SHIFT,SR DISP		;MASK = 0,,000777 TO ARX
						;;6487	=01*
						;;6488		BR_AR LONG,AR_BR LONG,J/SRND4	;POS, TRUNCATE BY ANDING
						;;6489		AR_AR+BR,ARX/ADX,BR_AR LONG,	;NEG, MUST DIDDLE
						;;6490			NORM,J/SRND3		; NORM FORCES LONG ARITH
						; 6491	.ENDIF/FPLONG
						; 6492	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; FP.MIC[10,5351]	19:52 24-Jul-85			SINGLE PRECISION FLOATING NORMALIZATION			

						; 6493	;HERE TO PERFORM ROUNDING OR TRUNCATION OF SINGLE-PRECISION RESULTS,
						; 6494	; AND CHECK FOR CARRY INTO EXPONENT FIELD REQUIRING RENORMALIZATION
						; 6495	
						; 6496	=0*1
U 0031, 0135,3202,2000,0000,0000,0510,0000	; 6497	STRNC:	AR_BR,CLR ARX,J/SRND4		;POS TRUNCATE, GO STUFF IN EXP
U 0035, 0134,0602,2004,0000,0060,0535,0000	; 6498	SRND2:	AR_AR+BR,NORM,CLR ARX		;NORM FORCES LONG ARITH
						; 6499						; SO THIS ADDS ARX TO BR'BRX
						; 6500	=1*0
U 0134, 0135,0301,7000,4001,0020,0010,0000	; 6501	SRND3:	AR_AR*.5,FE_FE+1		;RENORMALIZE
						; 6502	SRND4:	EXP_FE TST,SR DISP,		;STUFF EXP, CHECK NEG OR LONG
U 0135, 0151,3502,0600,0000,2000,0705,0410	; 6503			ARX_ARX*BRX,AD/ANDCB	;CLEAR TRUNCATED FRACTION
						; 6504	
						; 6505	;HERE TO STORE RESULT AS A FUNCTION OF SINGLE OR LONG PRECISION
						; 6506	; AND POSITIVE OR NEGATIVE...
						;;6507	.IF/FPLONG
						;;6508	=001
						; 6509	.IFNOT/FPLONG
						; 6510	=0*1
						; 6511	.ENDIF/FPLONG
U 0151, 0066,4001,0000,0000,0005,1633,0000	; 6512	SRND5:	SR_0,B WRITE,J/ST6		;POS & NOT LONG
						;;6513	.IF/FPLONG
						;;6514	SLNG3:	AC0_AR,AR_0S,SC_#,#/27.,J/SLNG4	;STORE HIGH PART OF LONG ANS
						; 6515	.ENDIF/FPLONG
U 0155, 0066,5143,7000,0000,0025,1633,0000	; 6516		AR_-AR,SR_0,B WRITE,J/ST6	;NEG & NOT LONG
						;;6517	.IF/FPLONG
						;;6518		AR_-AR LONG,J/SLNG3		;LONG NEG, MAKE IT SO
						;;6519	
						;;6520	SLNG4:	AR_SHIFT,I FETCH
						;;6521		AR0-8_FE-SC,BYTE DISP,		;TEST FOR EXP UNDERFLOW
						;;6522			SKP AR EQ		; OR LOW WORD ZERO
						;;6523	
						;;6524	=110
						; 6525	.ENDIF/FPLONG
U 2124, 0136,4001,0000,0000,0000,1610,0000	; 6526	STRAC1:	SR_0,J/STAC1			;PUT AWAY LOW WORD OF LONG RESULT
						;;6527	.IF/FPLONG
						;;6528		AR_0S,SR_0,J/STAC1		;CLEAR LOW WORD IN AC1
						; 6529	.ENDIF/FPLONG
						; 6530	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 14
; FP.MIC[10,5351]	19:52 24-Jul-85			DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV

						; 6531	.TOC	"DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV"
						; 6532	
						; 6533		.DCODE
D 0110, 4001,0301				; 6534	110:	R,	B/0,	J/DFLOAT	;DFAD
D 0111, 4200,0301				; 6535		R,	B/2,	J/DFLOAT	;DFSB
D 0112, 4400,0301				; 6536		R,	B/4,	J/DFLOAT	;DFMP
D 0113, 4601,0301				; 6537		R,	B/6,	J/DFLOAT	;DFDV
						; 6538		.UCODE
						; 6539	
						; 6540	=0****00**0*
						; 6541	DFLOAT:	FE_EXP,EXP_SIGN,SC/SCAD,MQ_0.S,
						; 6542			VMA_VMA+1,LOAD ARX,
U 0301, 0770,0001,0000,0203,1013,3662,0300	; 6543			CALL.S,J/XFERW		;GET LOW WORD
U 0303, 0570,3701,0500,0000,0000,0033,0000	; 6544		ARX_ARX*2,B DISP		;LOW BIT 0 IGNORED
						; 6545	=
						; 6546	=00*
U 0570, 0430,3200,5061,0000,0020,0010,0000	; 6547	DFAS:	BR_AR LONG,AR_AC1*2,J/DFAS1	;MEM OP READY, GET AC OP
						; 6548	
U 0572, 0570,5163,7700,0000,0020,0027,0000	; 6549		AR_-AR LONG,J/DFAS		;DFSB, NEGATE AND ADD
						; 6550	
						; 6551	.IF/MODEL.B
						; 6552		BR_AR LONG,GEN ARX,SKP AD NE,	;[241]HERE FOR DOUBLE FLT MUL
U 0574, 2230,3711,0060,0301,0020,5610,0756	; 6553			FE_#,#/-18.,J/DFMP	;[241]BEGIN TEST FOR STICKY BIT
						;;6554	.IFNOT/MODEL.B
						;;6555		AR_AC1,BR_AR LONG,		;HERE FOR DBL FLOATING MUL
						;;6556			FE_#,#/-18.,J/DFMP
						; 6557	.ENDIF/MODEL.B
						; 6558	
						; 6559		GEN AR*AC0,AD/XOR,SKP AD0,	;DFDV.  WILL QUO BE NEG?
						; 6560			BR_AR LONG,		;SAVE D'SOR IN BR, BRX
U 0576, 1220,3100,0060,3002,0020,5510,0000	; 6561			SC_FE-1,J/DFDV
						; 6562	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15
; FP.MIC[10,5351]	19:52 24-Jul-85			DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV

						; 6563	;HERE FOR DFAD AND DFSB
						; 6564	; MEM OPERAND IS IN BR (NEGATED IF DFSB)
						; 6565	; FE AND SC HAVE ITS EXPONENT
						; 6566	
						; 6567	=0*0
U 0430, 2216,3240,2400,0000,1020,0050,0000	; 6568	DFAS1:	ARX_AR,AR_AC0,CALL,J/EXPD	;AC OPERAND IN PLACE
						; 6569	=1*0
						; 6570	DFAS2:	ARX_AR,AR_SIGN,			;GET SHIFTED HIGH WORD
						; 6571			GEN #+SC,#/-36.,	;IS ANY SHIFT REQUIRED?
U 0434, 2212,5401,2400,2300,1020,5116,0734	; 6572			SKP SCAD0,J/DFAS3
						; 6573		ARX_AR,AR_SIGN,			;DIFF IS > 36
U 0435, 2210,5401,2400,2302,1020,5116,0044	; 6574			SC_#+SC,#/36.,SKP SCAD0	;CHECK FOR >72
						; 6575	.IF/MODEL.B
						; 6576	=0	AC0_AR,MQ_SHIFT,AR_ARX (ADX),
U 2210, 2154,3721,6310,0000,0000,1010,0000	; 6577			ARX/MQ,J/DFAS6		;[241]36 < DIFF < 72
						;;6578	.IFNOT/MODEL.B
						;;6579	=0	AC0_AR,MQ_SHIFT,AR_ARX (ADX),
						;;6580			ARX/MQ,J/DFAS4		;36 < DIFF < 72
						; 6581	.ENDIF/MODEL.B
						; 6582		AR_BR,ARL/AD,ARX_BRX,		;DIFF >72
U 2211, 1166,3202,2600,0000,0001,0010,0102	; 6583			MQ_0.M,J/DNTRY		;NORMALIZE LARGER OP
						; 6584	=0
						; 6585	DFAS3:	AR_ARX,ARL/SH,ARX/MQ,		;NO SHIFT REQUIRED
U 2212, 2146,4001,4300,0000,2001,0010,0104	; 6586			MQ_0.M,J/DFAS5
U 2213, 2126,4001,4000,0000,0000,0010,0000	; 6587		AR_SHIFT			;BEGIN SHIFTING SMALLER OP
U 2126, 2136,0001,4300,0000,2000,1010,0000	; 6588		AC0_AR,AR_ARX,ARX/MQ		;HI PART TO AC
						; 6589		MQ_SHIFT,AR_ARX (ADX),		;MID PART TO MQ
U 2136, 2144,3721,6010,0000,0000,0510,0000	; 6590			CLR ARX			;SHIFT ZEROS IN FROM RIGHT
U 2144, 2146,3240,2310,0000,0020,0010,0000	; 6591	DFAS4:	MQ_SHIFT,ARX/MQ,AR_AC0		;ALL PIECES NOW IN PLACE
						; 6592	DFAS5:	AR_AR+BR,ARX/ADX,SC_#,#/4,	;HERE WHEN OPERANDS ALIGNED
U 2146, 1240,0602,2600,0302,0060,0035,0004	; 6593			NORM,J/DNORM		;ADD, AND NORMALIZE RESULT
						; 6594	.IF/MODEL.B
U 2154, 2156,3723,2010,0000,0000,0010,0000	; 6595	DFAS6:	MQ_SHIFT,AR_MQ			;[241]GET H,L, PUT S,H IN AR
U 2156, 2174,3441,4201,0000,2000,1010,0000	; 6596		AC1_AR,AR_ARX,ARX_0S		;[241]STORE S,H
U 2174, 2206,3240,4201,0000,0020,0010,0000	; 6597		ARX_AC1,AR_SHIFT		;[241]GET L,0, GET S,H BACK
U 2206, 2214,3703,0000,0000,0020,5610,0000	; 6598		GEN AR,SKP AD NE		;[241]TEST FOR 0'S,
U 2214, 2146,3200,2000,0000,0020,1610,0040	; 6599	=0	CLR SR3,AR_AC0,J/DFAS5		;[241]DO 2'S COMP, ALL IN PLACE
U 2215, 2146,3200,2000,0000,0020,1610,0061	; 6600		SET SR3,AR_AC0,J/DFAS5		;[241]DO 1'S COMP, ALL IN PLACE
						; 6601	.ENDIF/MODEL.B
						; 6602	
						; 6603	;SUBROUTINE TO CHOOSE OPERAND WITH SMALLER EXPONENT, AND
						; 6604	; PREPARE FOR SHIFTING IT.
						; 6605	; ENTER WITH ONE OPERAND FRACTION IN BR, ITS EXPONENT IN FE & SC,
						; 6606	; THE OTHER OP IN AR WITH ITS EXPONENT IN AR0-8
						; 6607	; RETURN THE LARGER EXPONENT IN FE, AND 36-(MAGNITUDE OF DIFFERENCE)
						; 6608	; IN SC.  RETURN 4 IF SC POSITIVE, 5 IF NEGATIVE.
						; 6609	
U 2216, 2222,0001,0000,5202,1020,5122,0200	; 6610	EXPD:	SC_EXP-SC,EXP_SIGN,SKP SCAD0	;COMPARE MAGNITUDES
						; 6611	=0	AR_BR,ARX_BRX,BR/AR,BRX/ARX,	;AC OP IS LARGER MAGNITUDE
U 2222, 2227,3242,2660,2001,0000,0010,0000	; 6612			FE_FE+SC,J/EXPD1	;ITS EXP TO FE
						; 6613		MQ_ARX,SC_#+SC,#/36.,		;CHECK FOR EXP DIFF > 36
U 2223, 0004,4001,0010,2302,2020,5103,0044	; 6614			SKP SCAD0,RETURN4
						; 6615	EXPD1:	MQ_ARX,SC_#-SC,#/36.,		;AC EXP .GE. MEM
U 2227, 0004,0001,0010,5302,2020,5103,0044	; 6616			SKP SCAD0,RETURN4	;SHIFT MEM OP
						; 6617	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16
; FP.MIC[10,5351]	19:52 24-Jul-85			DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV

						; 6618	;DFMP
						; 6619	; DO TESTS FOR STICKY BITS FIRST THEN
						; 6620	; GET HERE WITH MEM OPERAND (M'CAND) IN BR!BRX
						; 6621	; AR HAS (AC1), LOW HALF OF M'IER
						; 6622	
						; 6623	.IF/MODEL.B
						; 6624	=0
U 2230, 0640,3240,2001,0000,0020,0010,0000	; 6625	DFMP:	AR_AC1,J/DFMP1			;NO STICKY BIT
U 2231, 2232,3240,2001,0000,0020,5610,0000	; 6626		AR_AC1,SKP AD NE		;GET AC LOW AND TEST
U 2232, 0640,0001,0000,0000,0000,0010,0000	; 6627	=0	J/DFMP1				;NO STICKY BIT
U 2233, 0640,0001,0000,0000,0000,1610,0061	; 6628		SET SR3				;WORRY ABOUT IT IN NORM
						; 6629	=00*
						; 6630	DFMP1:	MQ_AR,AR_0S,ARX_0S,		;SETUP LOW M'IER
						; 6631			SC_#+SC,#/-200,		;CORRECT EXPONENT
U 0640, 1542,3401,2210,2302,1000,0050,0600	; 6632			CALL,J/MULREE		;MULTIPLY BY THE LOW PART
						;;6633	.IFNOT/MODEL.B
						;;6634	=00*
						;;6635	DFMP:	MQ_AR,AR_0S,ARX_0S,		;SETUP LOW M'IER
						;;6636			SC_#+SC,#/-200,		;CORRECT EXPONENT
						;;6637			CALL,J/MULREE		;MULTIPLY BY THE LOW PART
						; 6638	.ENDIF/MODEL.B
U 0644, 0646,0602,2604,0000,0020,0027,0000	; 6639	=10*	AR_AR+BR LONG			;OOPS, LOW SIGN WAS SET
U 0646, 2243,3240,2010,0301,1020,0010,0762	; 6640		MQ_AR,AR_AC0,FE_#,#/-14.	;READY TO CONTINUE WITH HIGH PART
						; 6641	
						; 6642	;HERE TO USE HIGH MULTIPLIER
						; 6643	
						; 6644		SC_EXP+SC,EXP_SIGN.M,		;EXTRACT EXP FROM HIGH WORD
U 2243, 1162,4001,0000,2202,1001,4510,0200	; 6645			SKP AR0			;CHECK FOR NEG M'IER
						; 6646	=010
U 1162, 1542,3721,2010,0000,1000,0050,0000	; 6647	DFMP2:	MQ_AR,AR_MQ,CALL,J/MULREE	;GO BACK IN FOR HIGH PART
U 1163, 1162,4001,0000,4400,2001,0010,0200	; 6648		EXP_1,J/DFMP2			;OOPS, NEG, MOVE SIGN TO BIT 8
						; 6649	=110
U 1166, 1240,3703,0000,0302,0040,0035,0004	; 6650	DNTRY:	SC_#,#/4,GEN AR,NORM,J/DNORM	;NORMALIZE THE ANSWER
						; 6651	=
						; 6652	
						; 6653	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 17
; FP.MIC[10,5351]	19:52 24-Jul-85			DOUBLE FLOATING ARITHMETIC -- DFAD, DFSB, DFMP, DFDV

						; 6654	
						; 6655	;DFDV
						; 6656	; GET HERE WITH DIVISOR IN BR!BRX, ITS EXP-1 IN SC
						; 6657	; SKIP IF D'SOR AND D'END SIGNS DIFFER
						; 6658	
						; 6659	=000
U 1220, 0340,3240,5001,0000,0020,0050,0000	; 6660	DFDV:	AR_AC1*2,CALL,J/DFDV1		;GET LOW D'END, GO START DIVIDE
						; 6661	.IF/MODEL.B
U 1221, 0340,3240,5001,0000,0020,1650,0062	; 6662		SET SR2,AR_AC1*2,CALL,J/DFDV1	;NOTE NEG QUO
						;;6663	.IFNOT/MODEL.B
						;;6664		SR_1,AR_AC1*2,CALL,J/DFDV1	;NOTE NEG QUO
						; 6665	.ENDIF/MODEL.B
						; 6666	
						; 6667	=011	AC1_AR,AR_MQ,ARL/AD,FE_FE+1,	;HERE FROM DDVSUB. NEW STEP CNT
U 1223, 0543,3723,2001,4001,0001,1050,0102	; 6668			MQ_0.M,CALL.M,J/DIV+	; SAVE HIGH QUO, RESUME
						; 6669	=101	AC1_AR,AR_MQ,ARL/AD,FE_FE+1,
U 1225, 0542,3721,2001,4001,0001,1050,0102	; 6670			MQ_0.M,CALL.M,J/DIV-
						; 6671	
						; 6672	=111	AR_AC1,ARX/MQ,SC_#,#/4,		;POSITIVE QUOTIENT TO AR LONG
U 1227, 1240,3200,2301,0302,0040,0035,0004	; 6673			NORM,J/DNORM		;NORMALIZE AND ROUND
						; 6674	
						; 6675	=00
						; 6676	DFDV1:	ARX_AR,AR_AC0,SKP AD0,		;TEST DIVIDEND SIGN
						; 6677			FE_#,#/26.,		;SETUP COUNT FOR HIGH QUO
U 0340, 2162,3240,2400,0301,1020,5550,0032	; 6678			CALL,J/FDVCHK		;GO CHECK DIVIDABILITY
U 0342, 0722,0001,0000,0000,0000,4210,0000	; 6679	=10	SKP BR0,J/DDVSUB		;BEGIN DIVISION (RETURN ABOVE)
U 0343, 0066,4001,0000,0000,0000,1110,0624	; 6680		SET FL NO DIV,J/IFNOP		;ABORT THE DIVISION
						; 6681	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 18
; FP.MIC[10,5351]	19:52 24-Jul-85			DOUBLE PRECISION NORMALIZATION				

						; 6682	.TOC	"DOUBLE PRECISION NORMALIZATION"
						; 6683	
						; 6684	=000
U 1240, 2250,3721,0000,0302,0040,5427,0043	; 6685	DNORM:	SKP ARX+MQ NE,SC_#,#/35.,J/DNZERO	;AR=0
						; 6686	.IF/MODEL.B
						; 6687		BR/AR,BRX/ARX,AR_MQ COMP,	;RESULT NEG, MAKE POS
U 1241, 2274,2021,2060,0000,0000,1610,0062	; 6688			SET SR2,J/DNNEG		;[241]FLAG NEGATIVE
						;;6689	.IFNOT/MODEL.B
						;;6690		BR/AR,BRX/ARX,AR_MQ COMP,	;RESULT NEG, MAKE POS
						;;6691			SR_1,J/DNNEG		;FLAG NEGATIVE
						; 6692	.ENDIF/MODEL.B
						; 6693		AR_AR*.25 LONG,MQ_MQ*.25,
U 1242, 2276,3701,7710,2031,0000,0012,0004	; 6694			FE_FE+#,#/4,J/DNHI	;MSB IN AR 1-6
						; 6695		AR_AR*.25 LONG,
U 1243, 1245,3701,7700,2031,0000,0010,0002	; 6696			FE_FE+#,#/2,J/DROUND	;MSB IN AR7
U 1244, 1245,0301,7700,4001,0020,0027,0000	; 6697		AR_AR*.5 LONG,FE_FE+1		;MSB IN AR8
						; 6698	DROUND:	AR_AR+1,ARX/ADX,NORM,		;MSB IS AR9, RIGHT ON
U 1245, 1046,4001,2600,0302,0060,0035,0043	; 6699			SC_#,#/35.,J/DRND1
U 1246, 1245,3701,5500,3001,0000,0710,0001	; 6700		(AR+ARX+MQ)*2,FE_FE-1,J/DROUND	;MSB IN AR10
U 1247, 2244,4001,4000,5001,0000,0010,0000	; 6701		AR_SHIFT,FE_FE-SC		;SOMEWHERE IN AR 11-35
						; 6702	
U 2244, 2254,4001,4340,0000,2000,0010,0000	; 6703	DNSHFT:	BR/AR,AR_ARX,ARX/MQ		;SHIFT THE WHOLE THING
U 2254, 2256,3721,6010,0000,0000,0510,0000	; 6704		MQ_SHIFT,AR_ARX (ADX),CLR ARX
						; 6705		MQ_SHIFT,ARX/MQ,AR_BR,SC_#,#/10.,
U 2256, 1240,3202,2310,0302,0040,0035,0012	; 6706			NORM,J/DNORM		;GIVE IT ANOTHER GO
						; 6707	
						; 6708	.IF/MODEL.B
U 2274, 0556,0001,0000,0000,0000,0005,0000	; 6709	DNNEG:	SR DISP				;[241]TEST FOR 1'S COMP
U 0556, 2234,4001,2000,0000,0040,5410,0000	; 6710	=1110	AR_AR+1,SKP CRY0,J/DNNEG1	;[241]COMPLETE NEGATION OF MQ
						; 6711		MQ_AR,AR_BR COMP,ARX_BRX COMP,
U 0557, 1240,2502,2610,0000,1040,0035,0000	; 6712			NORM,J/DNORM		;NORMALIZE THE POS FORM
						; 6713	=0
						; 6714	DNNEG1:	MQ_AR,AR_BR COMP,ARX_BRX COMP,
U 2234, 1240,2502,2610,0000,1040,0035,0000	; 6715			NORM,J/DNORM		;NORMALIZE THE POS FORM
U 2235, 1240,5142,2610,0000,1060,0035,0000	; 6716		MQ_AR,AR_-BR,ARX/ADX,NORM,J/DNORM
						;;6717	.IFNOT/MODEL.B
						;;6718	DNNEG:	AR_AR+1,SKP CRY0		;COMPLETE NEGATION OF MQ
						;;6719	=0	MQ_AR,AR_BR COMP,ARX_BRX COMP,
						;;6720			NORM,J/DNORM		;NORMALIZE THE POS FORM
						;;6721		MQ_AR,AR_-BR,ARX/ADX,NORM,J/DNORM
						; 6722	.ENDIF/MODEL.B
						; 6723	
U 2276, 1166,3703,7710,0000,0000,0710,0001	; 6724	DNHI:	(AR+ARX+MQ)*.25,J/DNTRY		;GO TRY AGAIN
						; 6725	
						; 6726	=0
U 2250, 0244,3401,2200,0000,0000,1610,0000	; 6727	DNZERO:	SR_0,AR_0S,ARX_0S,J/DBLST	;RESULT = 0, STORE THAT
U 2251, 2244,4001,4000,5001,0000,0010,0000	; 6728		AR_SHIFT,FE_FE-SC,J/DNSHFT	;NOT ZERO, SHIFT AND TRY AGAIN
						; 6729	
						; 6730	=110
U 1046, 1047,0301,7700,4001,0020,0027,0000	; 6731	DRND1:	AR_AR*.5 LONG,FE_FE+1		;ROUNDING BLEW THE NORM, GO RIGHT
						; 6732		EXP_FE TST,SR DISP,CLR MQ,	;STUFF EXP IN, CHECK RESULT SIGN
U 1047, 0575,4041,0220,0000,2020,0705,0413	; 6733			BRX/ARX,ARX_1		;READY IF NEGATION NECESSARY
						; 6734	
						; 6735	.IF/MODEL.B
						; 6736	=1101					;[241]
						;;6737	.IFNOT/MODEL.B; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 18-1
; FP.MIC[10,5351]	19:52 24-Jul-85			DOUBLE PRECISION NORMALIZATION				

						;;6738	=0
						; 6739	.ENDIF/MODEL.B
						; 6740		AC0_AR,AR_SHIFT,ARX_BRX,	;STORE HIGH WORD, READY LOW
U 0575, 0241,3202,4600,0000,0217,1010,0000	; 6741			I FETCH,J/STD1
U 0577, 2312,3002,0604,0000,0000,1610,0000	; 6742		ARX_ARX*BRX,AD/ANDCA,SR_0	;CLEAR ROUNDING BIT
U 2312, 0244,5163,7700,0000,0020,0027,0000	; 6743	CDBLST:	AR_-AR LONG,J/DBLST		;[345] NEGATE RESULT AND STORE
						; 6744	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DOUBLE PRECISION ARITHMETIC			

						; 6745	.TOC	"GFLT DOUBLE PRECISION ARITHMETIC"
						; 6746	
						; 6747	.IF/MODEL.B
						; 6748	.IF/EXTEXP
						; 6749	
						; 6750		.DCODE
D 0102, 4000,0305				; 6751	102:	R,	B/0,	J/EDFLOT	;EFAD
D 0103, 4201,0305				; 6752	103:	R,	B/2,	J/EDFLOT	;EFSB
D 0106, 4401,0305				; 6753	106:	R,	B/4,	J/EDFLOT	;EFMP
D 0107, 4600,0305				; 6754	107:	R,	B/6,	J/EDFLOT	;EFDV
						; 6755		.UCODE
						; 6756	
						; 6757	=0****00**0*
						; 6758	EDFLOT:	VMA_VMA+1, LOAD ARX,
U 0305, 0770,4001,0000,0000,0013,3662,0100	; 6759			MQ_0.S, CALL [XFERW]
U 0307, 1320,3721,0507,0000,0000,1033,0176	; 6760		FM[E0]_AR, ARX_ARX*2, B DISP	;mem high to E0, do instruction
						; 6761	=
						; 6762	
						; 6763	=000
U 1320, 2412,4001,0000,0000,0000,4550,0000	; 6764	EDFL1:	SKP AR0, CALL [ISOEXP]		;save mem high word in E0.
U 1321, 2252,4001,0007,0000,0000,1010,0172	; 6765		FM[T2]_AR, J/EF1		;save mem exp in T2.
U 1322, 1323,5143,7700,0000,0020,0027,0000	; 6766		AR_-AR LONG			;subtract now same as add.
U 1323, 1320,4001,0007,0000,0000,1010,0176	; 6767		FM[E0]_AR, J/EDFL1		;save "positive" exponent
U 1324, 2412,4001,0000,0000,0000,4550,0000	; 6768	=100	SKP AR0, CALL [ISOEXP]		;isolate mem exp in AR.
						; 6769		BR/AR, GEN ARX, SKP AD NE,	;start test for sticky bits.
U 1325, 2300,3711,0040,0000,0020,5610,0000	; 6770			J/EFMP
						; 6771	=110	BR/AR, GEN AR, SKP AD0, 	;save mem high in br.
U 1326, 2412,3703,0040,0000,0020,5550,0000	; 6772			CALL [ISOEXP]		;get mem exp
U 1327, 3162,3401,2007,0000,0000,1010,0172	; 6773		FM[T2]_AR, AR_0S, J/EFDV0	;save mem exp in T2. No sticky bits.
						; 6774	=0
U 2252, 2412,3200,2000,0000,0020,5550,0000	; 6775	EF1:	AR_AC0, SKP AD0, CALL [ISOEXP]	;get AC op
U 2253, 2314,0001,0047,0000,0000,1010,0165	; 6776		FM[E1]_AR, BR/AR		;save AC exp in E1
						; 6777	
						; 6778	;Now have positive mem exponent in T2, pos AC exp in E1.
						; 6779	;Save larger exp in T2 and exp diff if less than 340 in SC.
						; 6780		[AR]_[AR]*FM[T2], AD/A-B,	;AR gets exp diff.
U 2314, 0450,5100,2007,0000,0040,5510,0172	; 6781			SKP AD0			;AR get exp diff, BRX gets exp.
U 0450, 2454,0001,0000,0302,0000,0050,0003	; 6782	=00	SC_#, #/3, CALL [EXPDIF]	;test for exp diff >72.
U 0451, 0550,0001,0040,0000,0000,0010,0000	; 6783		BR/AR, J/EF3A			;mem op larger.
U 0452, 2354,3242,2000,0000,0000,0010,0000	; 6784		AR_BR, J/EF5			;restore exp to AR.
U 0453, 3171,3260,2007,0401,0020,0010,0165	; 6785		[AR]_FM[E1], CLR FE, J/ACNORM	;exp diff too large, norm AC op.
						; 6786	=00
U 0550, 2454,5142,2000,0302,0020,0050,0003	; 6787	EF3A:	AR_-BR, SC_#, #/3, CALL [EXPDIF];mem larger, get positive diff.
U 0552, 2316,3242,2000,0000,0000,0010,0000	; 6788	=10	AR_BR, J/EF3B			;restore exponent to AR.
U 0553, 3167,3240,2007,0401,0020,0010,0172	; 6789		[AR]_FM[T2], CLR FE, J/MEMNRM	;exp diff > 72. norm mem op.
U 2316, 2332,3243,5000,0000,0000,0010,0000	; 6790	EF3B:	AR_AR*8				;move exp difference into AR0-8.
						; 6791		FE_AR0-8,			;mem larger, op doable.
U 2332, 2260,3240,2000,2421,0020,0010,0000	; 6792			AR_AC0			;save smaller AC op in T0,T1
U 2260, 2446,0001,0007,0000,0000,1050,0166	; 6793	=0	FM[T0]_AR, CALL [EF5B]
U 2261, 2334,0001,0007,0000,0000,1010,0171	; 6794		FM[T1]_AR
U 2334, 2336,3240,2007,0000,0020,0010,0176	; 6795		[AR]_FM[E0]			;save larger mem op in AC0,AC1
U 2336, 2352,0001,4000,0000,2000,1010,0000	; 6796		AC0_AR, AR_ARX
U 2352, 2264,0001,0001,0000,0000,1010,0000	; 6797		AC1_AR, J/EF5A			;all set to shift and add.
						; 6798	
U 2354, 2367,3243,5000,0000,0000,0010,0000	; 6799	EF5:	AR_AR*8				;move exp difference into AR0-8.
U 2367, 2373,3260,2007,2421,0020,0010,0176	; 6800		FE_AR0-8, [AR]_FM[E0]		;smaller mem op to T0,T1; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-1
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DOUBLE PRECISION ARITHMETIC			

U 2373, 2262,4001,4007,0000,2000,1010,0166	; 6801		FM[T0]_AR, AR_ARX
U 2262, 2446,0001,0007,0000,0000,1050,0171	; 6802	=0	FM[T1]_AR, CALL [EF5B]
U 2263, 2436,4001,0001,0000,0000,1010,0000	; 6803		AC1_AR				;we expect AC1*2 to be saved.
U 2436, 2444,3240,2007,0000,0020,0010,0165	; 6804		[AR]_FM[E1]			;save larger AC exp in T2
U 2444, 2264,4001,0007,0000,0000,1010,0172	; 6805		FM[T2]_AR, J/EF5A
						; 6806	
U 2446, 0001,3240,5001,0000,0020,0003,0000	; 6807	EF5B:	AR_AC1*2, RETURN1
						; 6808	
						; 6809	;EXPDIF determines if the exponent difference is too large-ie >110 oct.
						; 6810	;The largest allowed value for shifting is 72 decimal. This is 110 octal.
						; 6811	;Since the exponent is in AR1-11, 110 octal has the value 11 in AR1-8.
						; 6812	;It expects the exponent difference in AR0-8.
						; 6813	;It uses AR0-8 and the BR.
						; 6814	;Returns 2 if the difference is ok (<=110).
						; 6815	;Returns 3 if the difference is too large (>110).
U 2454, 2456,4001,0040,0000,0000,0410,0000	; 6816	EXPDIF:	BR/AR, CLR AR			;zero all of those bits.
U 2456, 2461,0001,0000,0000,0000,0110,0010	; 6817		AR0-8_#, #/10			;put in 100 in AR0-11.
U 2461, 0002,5102,0004,0000,0040,5503,0000	; 6818		GEN AR-BR, SKP AD0, RETURN2	;<max diff>-<actual diff>
						; 6819	
						; 6820	;We now have:
						; 6821	; AC0	/ larger op high
						; 6822	; AC1	/ larger op low
						; 6823	; T0	/ smaller op high
						; 6824	; T1	/ smaller op low
						; 6825	; T2	/ larger exponent
						; 6826	; FE	/ exp difference
						; 6827	;We must now sign extend both high ops.
						; 6828	=0
U 2264, 2420,3240,2000,0000,0020,5550,0000	; 6829	EF5A:	AR_AC0, SKP AD0, CALL [SGNEXT]	;get larger high op
U 2265, 2270,4001,0000,0000,0000,1010,0000	; 6830		AC0_AR				;save larger extended op in AC0
						; 6831	=0	[AR]_FM[T0], SKP AD0,		;get smaller high op
U 2270, 2420,3240,2007,0000,0020,5550,0166	; 6832			CALL [SGNEXT]		; and sign extend into AR1-11.
U 2271, 2506,4001,0007,0000,0000,1010,0166	; 6833		FM[T0]_AR			;save smaller extended op in T0
						; 6834	;We are now set to shift the smaller op to align it with the larger op.
U 2506, 2511,3260,2007,0000,0020,0010,0171	; 6835		[AR]_FM[T1]
U 2511, 2513,3240,2407,0000,1020,0013,0166	; 6836		[AR]_FM[T0], ARX_AR, SC_FE	;move diff to SC for next line.
U 2513, 2272,0001,0000,5302,0020,5110,0044	; 6837		SC_#-SC, #/36., SKP SCAD0
U 2272, 2556,5441,2400,0000,1020,0016,0000	; 6838	=0	ARX_AR, AR_SIGN, J/EF10		;FE < 37.
U 2273, 2535,0001,0060,0000,0000,0013,0000	; 6839		BR/AR, BRX/ARX, SC_FE
U 2535, 2542,5441,2400,0000,1020,0016,0000	; 6840		AR_SIGN, ARX_AR
U 2542, 2546,0001,0000,5302,0000,0010,0110	; 6841		SC_#-SC, #/72.
U 2546, 0531,4001,0400,0000,0000,0010,0000	; 6842		ARX_SHIFT			;high is sign, low is sign,,high.
						; 6843	=01	AR_BR LONG, BR/AR, BRX/ARX,	;save new stuff in BR long.
U 0531, 3034,3242,2660,0000,0000,0050,0000	; 6844			CALL [EF12]		;MQ gets lowest word.
U 0533, 2551,0001,0007,0000,0000,1010,0165	; 6845		FM[E1]_AR			;save sticky bits.
U 2551, 3024,3200,2001,0000,0020,0010,0000	; 6846		AR_AC1, J/EF11			;now prepare to add.
U 2556, 2653,3240,4207,0000,0020,0010,0171	; 6847	EF10:	AR_SHIFT, [ARX]_FM[T1]		;shift high op, load low word.
U 2653, 3004,3240,2047,0000,0020,0010,0166	; 6848		BR/AR, [AR]_FM[T0]		;shift low op, load high word.
U 3004, 0561,3711,2400,0000,0000,0010,0000	; 6849		AR_ARX (AD), ARX_SHIFT		;get shifted low word into ARX.
U 0561, 3034,0001,0020,0000,0000,0550,0000	; 6850	=01	BRX/ARX, CLR ARX, CALL [EF12]	;save low word, shift end bits.
U 0563, 3007,4001,0007,0000,0000,1010,0165	; 6851		FM[E1]_AR			;save sticky bits. (word 4 of sum).
U 3007, 3024,3200,2001,0000,0020,0010,0000	; 6852		AR_AC1, J/EF11			;prepare to add.
U 3015, 3024,3200,2061,0000,0020,0010,0000	; 6853		BR/AR, BRX/ARX, AR_AC1		;get larger op in AR,ARX
U 3024, 3033,3200,2400,0301,1020,0010,0000	; 6854	EF11:	ARX_AR, AR_AC0, FE_#, #/0	;smaller op in BR,BRX
						; 6855		AR_AR+BR, ARX/ADX, SC_#, #/3,	;operation done, now normalize.
U 3033, 1520,0602,2600,0302,0060,0035,0003	; 6856			NORM, J/ENORM; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-2
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DOUBLE PRECISION ARITHMETIC			

						; 6857	
U 3034, 2662,3713,2010,0000,0000,0510,0000	; 6858	EF12:	MQ_SHIFT, AR_ARX (AD), CLR ARX,J/SHIFT
						;;6859	.IF/GFTCNV		;[273]
						;;6860	EF12A:	AR_SHIFT, RETURN10
						; 6861	.ENDIF/GFTCNV		;[273]
						; 6862	.ENDIF/EXTEXP
						; 6863	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT MULTIPLY						

						; 6864	.TOC	"GFLT MULTIPLY"
						; 6865	.IF/EXTEXP
						; 6866	=0
U 2300, 2303,4001,0000,0000,0000,0410,0000	; 6867	EFMP:	CLR AR, J/EFMP1			;mem low op is zero, no sticky bits.
U 2301, 2302,3240,5001,0000,0020,5610,0000	; 6868		AR_AC1*2, SKP AD NE		;is AC low op non-zero as well ?
U 2302, 2303,4001,0000,0000,0000,0410,0000	; 6869	=0	CLR AR				;yes, no sticky bits today.
U 2303, 3076,3441,2007,0000,0000,1010,0165	; 6870	EFMP1:	FM[E1]_AR, AR_0S		;set sticky bits.
U 3076, 3141,0001,0000,0000,0000,0110,0200	; 6871		AR0-8_#, #/200			;subtract 200.
U 3141, 3154,3202,2040,0000,0000,0010,0000	; 6872		BR/AR, AR_BR			;swap around exp and 2000.
U 3154, 2304,5102,2000,0402,0020,0010,0000	; 6873		AR_AR-BR, CLR SC		;done, and SC is cleared.
						; 6874	=0	BR/AR, AR_AC0, SKP AD0,		;save exp-2000 in BR.
U 2304, 2412,3240,2040,0000,0020,5550,0000	; 6875			CALL [ISOEXP]		;get AC high and isolate exp.
U 2305, 3155,0602,2000,0000,0020,0010,0000	; 6876		AR_AR+BR			;add exponents together.
U 3155, 2320,0001,0007,0000,0000,1010,0172	; 6877		FM[T2]_AR			;and store the sum in T2.
						; 6878	=0	[AR]_FM[E0], SKP AD0,		;get mem high op sign extended.
U 2320, 2420,3260,2007,0000,0020,5550,0176	; 6879			CALL [SGNEXT]
U 2321, 3156,0001,0000,0301,0000,0010,0756	; 6880		FE_#, #/-18.			;
U 3156, 1420,3240,2061,0000,0020,0010,0000	; 6881		BR/AR, BRX/ARX, AR_AC1		;move mem ops to BR!BRX.
						; 6882	=000	MQ_AR, AR_0S, ARX_0S,		;multiply by low word.
U 1420, 1542,3401,2210,0000,1000,0050,0000	; 6883			CALL [MULREE]
U 1424, 1426,0602,2604,0000,0020,0027,0000	; 6884	=100	AR_AR+BR LONG			;low sign was set, add results.
						; 6885	=110	MQ_AR, AR_AC0, FE_#, #/-13.,	;now continue with high part.
U 1426, 2420,3240,2010,0301,1020,5550,0763	; 6886			SKP AD0, CALL [SGNEXT]	;sign extend the ac high op.
U 1427, 2324,4001,0007,0000,0000,1010,0166	; 6887		FM[T0]_AR			;save sign extended AC op.
						; 6888	;	SKP AR0				;test sign bit to adjust FE.
						; 6889	=0
U 2324, 1440,3723,2010,0000,1000,0010,0000	; 6890	EFMPP1:	MQ_AR, AR_MQ, J/EFMPP2		;swap AR+MQ.
U 2325, 2324,0001,0000,4001,0000,0010,0000	; 6891		FE_FE+1, J/EFMPP1		;inc the FE if number is neg.
						; 6892	=000
						; 6893	EFMPP2:	AD/0S, FE_FE+1, DISP/MUL,	;now multiply by the high word.
U 1440, 0320,3401,0010,4001,0000,0070,0000	; 6894			MQ/MQ*.25, CALL [MULP]
						; 6895	;Since our last multiply step used 2 signs bits instead of a sign bit
						; 6896	;and the MSB, our answer is too low by a power of two for positive numbers
						; 6897	;and too low by a power of 4 for negative numbers.
U 1444, 3157,3701,5500,0000,0000,0710,0001	; 6898	=100	(AR+ARX+MQ)*2, J/EFMPP3		;try this correction factor.
U 1445, 3157,3703,7710,0000,0000,0710,0001	; 6899	=101	(AR+ARX+MQ)*.25, J/EFMPP3	;shouldn't ever get here.
U 1446, 3157,3701,5500,0000,0000,0710,0001	; 6900	=110	(AR+ARX+MQ)*2			;and this for postive numbers.
						; 6901	=
U 3157, 0571,2341,0260,0000,0000,0610,0030	; 6902	EFMPP3:	BR_AR LONG, AR_0.C, ARX_1S	;result to BR!BRX. Build mask.
						; 6903	=01	SC_#, #/10.,			;load SC with shift count.
U 0571, 2662,4001,0000,0302,0000,0050,0012	; 6904			CALL [SHIFT]		;Now have mask of 0,,1777
U 0573, 3160,3202,2660,0000,0000,0010,0000	; 6905		AR_BR LONG, BR_AR LONG		;mask to BR, result TO AR!ARX.
U 3160, 3161,3522,0010,0000,0000,0710,0003	; 6906		MQ_MQ*BR, AD/ANDCB		;clear the last 10 MQ bits.
						; 6907		GEN AR, SC_#, #/3,		;generate NORM bits.
U 3161, 1520,3703,0000,0302,0040,0035,0003	; 6908			NORM, J/ENORM		;conditions set for EE norm.
						; 6909	.ENDIF/EXTEXP
						; 6910	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DIVIDE						

						; 6911	.TOC	"GFLT DIVIDE"
						; 6912	.IF/EXTEXP
U 3162, 2340,0001,0007,0000,0000,1010,0165	; 6913	EFDV0:	FM[E1]_AR			;no sticky bits on divide.
U 2340, 2420,3202,2000,0000,0020,5550,0000	; 6914	=0	AR_BR, SKP AD0, CALL [SGNEXT]	;sign extend mem high.
						; 6915		GEN AR*AC0, AD/XOR, SKP AD0,	;determine sign of result.
U 2341, 1460,3100,0060,0000,0020,5510,0000	; 6916			BR_AR LONG		;mem op to BR!BRX.
U 1460, 0620,3240,5001,0000,0020,0050,0000	; 6917	=000	AR_AC1*2, CALL [EFDV1]		;start division.
U 1461, 0620,3240,5001,0000,0020,1650,0001	; 6918		SR_1, AR_AC1*2, CALL [EFDV1]	;note result if negative.
						; 6919	
						; 6920	=011	AC1_AR, AR_MQ, ARL/AD, FE_FE+1,	;set step count to 35-2.
U 1463, 0543,3723,2001,4001,0001,1050,0102	; 6921			MQ_0.M, CALL [DIV+]
						; 6922	=101	AC1_AR, AR_MQ, ARL/AD, FE_FE+1,
U 1465, 0542,3721,2001,4001,0001,1050,0102	; 6923			MQ_0.M, CALL [DIV-]
U 1467, 3163,4001,0000,0401,0000,0410,0000	; 6924	=111	CLR AR, CLR FE			;exp must be adjusted-
U 3163, 3164,4001,0000,0000,0000,0110,0200	; 6925		AR0-8_#, #/200			;  it is currently 2000 too low
U 3164, 3165,0600,2007,4000,0020,0010,0172	; 6926		[AR]_[AR]*FM[T2], AD/A+B	;add in the correction.
U 3165, 3166,4001,0007,0000,0000,1010,0172	; 6927		FM[T2]_AR			;store the corrected exp in T2.
						; 6928		AR_AC1, ARX/MQ, SC_#, #/3,	;get answer ready for norm.
U 3166, 1520,3200,2301,0302,0040,0035,0003	; 6929			NORM, J/ENORM
						; 6930	
						; 6931	=00
						; 6932	EFDV1:	ARX_AR, AR_AC0, SKP AD0, FE_#,	;AC low*2 to ARX, AC high to AR.
U 0620, 2342,3200,2400,0301,1020,5550,0027	; 6933			#/23., CALL [EDVCHK]
U 0622, 0722,0001,0000,0000,0000,4210,0000	; 6934	=10	SKP BR0, J/DDVSUB
U 0623, 0066,4001,0000,0000,0000,1110,0624	; 6935		SET FL NO DIV, J/IFNOP		;no division this time.
						; 6936	
						; 6937	=0
U 2342, 2344,0001,0010,0000,1000,0010,0000	; 6938	EDVCHK:	MQ_AR, J/EDVCH1			;go to an even address.
U 2343, 2342,5163,7700,0000,0020,0027,0000	; 6939		AR_-AR LONG, J/EDVCHK		;make ac op positive.
						; 6940	
						; 6941	=0
U 2344, 2412,4001,0000,0000,0000,4550,0000	; 6942	EDVCH1:	SKP AR0, CALL [ISOEXP]		;op saved in MQ, get exp in AR.
						; 6943		[AR]_[AR]*FM[T2], AD/A-B,	;subtract exponents.
U 2345, 2360,5100,2007,0000,0040,5510,0172	; 6944			SKP AD0			;did this cause an underflow ?
						; 6945	=0
U 2360, 2361,4001,0000,0000,0000,1610,0062	; 6946		SET SR2 			;no, let SR2 denote this.
U 2361, 2362,0001,0007,0000,0000,1010,0172	; 6947	EDVCH2:	FM[T2]_AR			;yes, save exponent in T2 for ENORM.
						; 6948	
						; 6949	=0
U 2362, 2420,3723,2000,0000,0020,5550,0000	; 6950	EDVCH3:	AR_MQ, SKP AD0, CALL [SGNEXT]	;now sign extend the op.
U 2363, 2170,0001,0000,0000,0001,4210,0100	; 6951		SKP BR0, MQ_0.M, J/FDVCK1
						; 6952	.ENDIF/EXTEXP
						; 6953	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT NORMALIZATION					

						; 6954	.TOC	"GFLT NORMALIZATION"
						; 6955	.IF/EXTEXP
						; 6956	;Normalization is done here.
						; 6957	;	The are 8 addresses the can be reached when doing a
						; 6958	;	NORM dispatch. The following table describes the
						; 6959	;	dispatching and how to normalize the fraction and
						; 6960	;	exponent.
						; 6961	;
						; 6962	;	=000	AR=0			AR is zero, check ARX,MQ
						; 6963	;	=001	AR00=1			sign bit on, complement
						; 6964	;	=010	MSB in AR 1-6		shf 4 rt.(a guess)
						; 6965	;	=011	MSB in AR07		sht 2 rt.
						; 6966	;	=100	MSB in AR08		sht 3 rt.
						; 6967	;	=101	MSB in AR09		right on!
						; 6968	;	=110	MSB in AR10		sht 1 lf.
						; 6969	;	=111	MSB in AR 11-35		sht 4 lf.(a guess)
						; 6970	;
						; 6971	;The normalization routine for double precision assumes that
						; 6972	;	the exponent can be found in the FE. As it goes through
						; 6973	;	the normalization process, it adjusts the fraction and
						; 6974	;	the FE by the correct amounts to normalize the number.
						; 6975	;	In GFLT numbers, the exponent may not fit
						; 6976	;	into the FE, so it has to be saved in an accumulator.
						; 6977	;	However, if one assumes initially that the exponent is
						; 6978	;	zero and that it is in the FE, then the same normalization
						; 6979	;	algorithm can be used as in double precision numbers
						; 6980	;	with the realization that at the end of the normalization
						; 6981	;	process the FE contains the correction (EC)  that must be
						; 6982	;	added into the saved exponent (ES)  to produce a 'bit-9'
						; 6983	;	normalized number. Once this correction value is obtained,
						; 6984	;	the 'bit-12' normalized exponent (EN)  is given by
						; 6985	;			EN = ES + EC + 3
						; 6986	
U 3167, 3170,0001,0007,0000,0000,1010,0172	; 6987	MEMNRM:	FM[T2]_AR			;save larger exponent.
U 3170, 2374,3240,2007,0000,0020,5510,0176	; 6988		[AR]_FM[E0], SKP AD0, J/ACNRM1	;get high word, sign extend it
						; 6989	
U 3171, 3172,4001,0007,0000,0000,1010,0172	; 6990	ACNORM:	FM[T2]_AR			;save larger exponent.
U 3172, 3173,3240,5001,0401,0020,0010,0000	; 6991		AR_AC1*2, CLR FE		;get low word*2 into AR.
U 3173, 2374,3200,2400,0000,1020,5510,0000	; 6992		ARX_AR, AR_AC0, SKP AD0		;get high word, sign extend it.
						; 6993	=0
						; 6994	ACNRM1:	[AR]_[AR]*FM[EXPMSK], AD/AND,	;sign extend with 0's.
U 2374, 1520,3600,2007,0000,0040,0035,0164	; 6995		NORM, J/ENORM
						; 6996		[AR]_[AR]*FM[EXPMSK], AD/ORCB,	;sign extend with 1's.
U 2375, 1520,2700,2007,0000,0040,0035,0164	; 6997			NORM			;fall into the normalize routine.
						; 6998	
						; 6999	=000
						; 7000	ENORM:	SKP ARX+MQ NE, SC_#, #/35.,	;AR=0,check ARX,+MQ.
U 1520, 2410,3723,0000,0302,0040,5427,0043	; 7001			J/ENZERO
						; 7002		BR/AR, BRX/ARX, AR_MQ COMP,	;result neg, complement.
U 1521, 3200,2023,2060,0000,0000,1610,0061	; 7003			SET SR3, J/ENNEG	;flag negative seen.
						; 7004		AR_AR*.25 LONG, MQ_MQ*.25,	;MSB in AR 1-6.
U 1522, 3201,3703,7710,2031,0000,0012,0004	; 7005			FE_FE+#, #/4, J/ENHI
						; 7006		AR_AR*.25 LONG, FE_FE+#,	;MSB in AR07.
U 1523, 1525,3703,7700,2031,0000,0010,0002	; 7007			#/2, J/EROUND		;
U 1524, 1525,0303,7700,4001,0020,0027,0000	; 7008		AR_AR*.5 LONG, FE_FE+1		;MSB in AR08.
						; 7009	EROUND:	BR_AR LONG, AR+MQ_0.S,	 	;MSB in AR09, where we want it.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4-1
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT NORMALIZATION					

U 1525, 3202,4001,0060,0000,0000,0022,0130	; 7010			J/ERND1			;put result in BR!BRX.
						; 7011		(AR+ARX+MQ)*2, FE_FE-1,		;MSB in AR10.
U 1526, 1525,3703,5500,3001,0000,0710,0001	; 7012			J/EROUND
U 1527, 3174,0001,4000,5001,0000,0010,0000	; 7013		AR_SHIFT, FE_FE-SC		;MSB somewhere in AR 11-35.
						; 7014	
U 3174, 3175,0001,4340,0000,2000,0010,0000	; 7015	ENSHFT:	BR/AR, AR_ARX, ARX/MQ		;shift everyone.
U 3175, 3176,3721,6010,0000,0000,0510,0000	; 7016		MQ_SHIFT, AR_ARX (ADX), CLR ARX
						; 7017		MQ_SHIFT, ARX/MQ, AR_BR,	;go aroung again.
						; 7018			SC_#, #/10.,
U 3176, 1520,3242,2310,0302,0040,0035,0012	; 7019			NORM, J/ENORM
						; 7020	
U 3200, 2400,3200,0007,0000,0020,5610,0165	; 7021	ENNEG:	GEN E1, SKP AD NE		;any sticky bits left around?
U 2400, 2402,4001,2000,0000,0040,5410,0000	; 7022	=0	AR_AR+1, SKP CRY0, J/ENNEG1	;no, 2's comp MQ.
						; 7023		MQ_AR, AR_BR COMP, ARX_BRX COMP,
U 2401, 1520,2542,2610,0000,1040,0035,0000	; 7024			NORM, J/ENORM		;one's complement to finish.
						; 7025	=0
						; 7026	ENNEG1:	MQ_AR, AR_BR COMP, ARX_BRX COMP,
U 2402, 1520,2542,2610,0000,1040,0035,0000	; 7027			NORM, J/ENORM		;one's complement to finish.
						; 7028		MQ_AR, AR_-BR, ARX/ADX,		;carry happened, do two's comp.
U 2403, 1520,5162,2610,0000,1060,0035,0000	; 7029			NORM, J/ENORM
						; 7030	
U 3201, 3223,3703,7710,0000,0000,0710,0001	; 7031	ENHI:	(AR+ARX+MQ)*.25, J/ENTRY	;go try again after setting SC.
						; 7032	=0
U 2410, 0244,3401,2200,0000,0000,1610,0000	; 7033	ENZERO:	SR_0, AR_0S, ARX_0S, J/DBLST	;result = 0, store in AC,AC+1.
U 2411, 3174,0001,4000,5001,0000,0010,0000	; 7034		AR_SHIFT, FE_FE-SC, J/ENSHFT	;not zero, try next 35 bits.
						; 7035	
U 3202, 3203,4061,0500,0000,0020,0010,0000	; 7036	ERND1:	ARX_2+MQ0			;[407] build rounding constant.
U 3203, 3204,3243,0600,0000,0000,0010,0000	; 7037		ARX_ARX*4			;gen a 10 in the ARX for rounding.
U 3204, 1546,0602,2604,0000,0060,0035,0000	; 7038		AR_AR+BR, ARX/ADX, NORM		;do the rounding and test norm.
U 1546, 1547,0301,7700,4001,0020,0027,0000	; 7039	=110	AR_AR*.5 LONG, FE_FE+1		;rounding blew norm, correct it.
						; 7040	
						; 7041	; When we get here the number is 'bit-9' normalized
						; 7042	; in the AR,ARX.  Add the FE + 3 to the exponent
						; 7043	; saved in T2.
						; 7044	; At this point the Extended Exponent must be put
						; 7045	; into the AR after everything is shifted right 3 bits.
						; 7046	; The double precision norm routine does this by:
						; 7047	; EXP_FE TST, SR DISP, CLR MQ, BRX/ARX, ARX_1
						; 7048	
						; 7049	
						; 7050	ERND2:	AR_AR*.25 LONG,		;shift everything 2 bits right.
						; 7051			MQ_MQ*.25,	;	"	"	"
U 1547, 3205,3701,7710,0302,0000,0012,0003	; 7052			SC_#, #/3	;add in correction to FE.
						; 7053		AR_AR*.5 LONG,		;now shift the final bit position.
U 3205, 3206,0301,7700,2002,0020,0027,0000	; 7054			SC_FE+SC	;total exponent correction.
U 3206, 3207,0001,0060,0000,0000,0410,0000	; 7055		BR/AR, BRX/ARX, CLR AR	;save answer in BR,BRX.
U 3207, 3210,0001,0000,2400,2001,0110,0000	; 7056		EXP_SC.MS		;get exp corr in AR.
						; 7057		ARX_AR, AR_SIGN,	;get exp into ARX 1-8.
U 3210, 3211,5401,2400,0302,1020,0016,0041	; 7058			SC_#,#/33.	;prepare to shift 3 places.
						; 7059		ARX_SHIFT,		;move exponent into ARX 1-11.
U 3211, 3212,3260,2407,0000,0020,0010,0164	; 7060			[AR]_FM[EXPMSK]	;prepare to build mask in AR.
U 3212, 3213,4001,0000,0000,0000,0110,0400	; 7061		AR0-8_#, #/400		;include AR00 in EXPMSK==>400077,,-1
						; 7062		AR_AR*BR, AD/AND,	;zero AR1-11 to make room for exp.
U 3213, 3214,3602,2004,0302,0000,0010,0043	; 7063			SC_#, #/35.
						; 7064	
						; 7065	; I am sure a few lines of code can be saved around here.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4-2
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT NORMALIZATION					

						; 7066	
U 3214, 3215,3260,2047,0000,0020,0010,0172	; 7067		[AR]_FM[T2], BR/AR	;save high word in BR, load larger exp.
U 3215, 3216,3202,2040,0000,0000,0010,0000	; 7068		AR_BR, BR/AR		;swap around so we can add.
						; 7069		AR_ARX+BR, BR/AR,	;have final exponent, check for problems.
U 3216, 3217,0612,2044,0302,0020,0010,0000	; 7070			SC_#,#/0
U 3217, 0643,4001,0000,0000,0020,0007,0000	; 7071		SH DISP			;any exponent problems ?
						; 7072	=0011	ARX_AR, SC_#, #/35.,	; no problems.
U 0643, 3221,0001,0400,0302,1000,0010,0043	; 7073			J/ENFNL1
						; 7074	ENFNL0:	ARX_AR, SC_#, #/35.,	; no problems.
U 0647, 3221,0001,0400,0302,1000,0010,0043	; 7075			J/ENFNL1
U 0653, 3220,0001,0000,0000,0000,1110,0620	; 7076		SET FLOV,  J/EEOV	; an overflow occurred.
						; 7077	
U 0657, 0635,0001,0000,0000,0000,0005,0000	; 7078		SR DISP			;floating underflow - is it real ?
						; 7079	=1101	;test SR2.
U 0635, 3220,4001,0000,0000,0000,1110,0630	; 7080		SET FXU, J/EEOV		;yes, it is a real underflow.
U 0637, 3220,0001,0000,0000,0000,1110,0620	; 7081		SET FLOV		;no, GFDV saw an overflow before.
						; 7082	
						; 7083	EEOV:	P_P AND #, #/37,	;turn off AR00.
U 3220, 0647,4001,0000,7130,3000,0110,0037	; 7084			J/ENFNL0
						; 7085	
U 3221, 3222,3312,2000,0000,0000,0010,0000	; 7086	ENFNL1:	AR_ARX*BR, AD/OR	;AR now has high word, BRX has low.
U 3222, 0742,4061,0200,0000,0021,0005,0100	; 7087		ARX_1, MQ_0.M, SR DISP	;incase negation of lower word needed.
						; 7088	=10	AC0_AR, AR_SHIFT,	;store high word,
						; 7089			ARX_BRX,	;move low word to ARX.
U 0742, 0241,3202,4600,0000,0217,1010,0000	; 7090			I FETCH, J/STD1	;prepare to store low word and exit.
						; 7091		ARX_ARX*BRX, AD/ANDCA,	; clear rounding bit.
U 0743, 2312,3002,0604,0000,0000,1610,0000	; 7092			SR_0,J/CDBLST	;negate result and store double result.
						; 7093	
U 3223, 1520,3703,0000,0302,0040,0035,0003	; 7094	ENTRY:	SC_#, #/3, GEN AR, NORM, J/ENORM; go normalize again.
						; 7095	.ENDIF/EXTEXP
						; 7096	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT TO INTEGER CONVERSION				

						; 7097	.TOC	"GFLT TO INTEGER CONVERSION"
						; 7098	.IF/EXTEXP
						;;7099	.IF/GFTCNV		;[273]
						;;7100	
						;;7101	;ETXIX routine is used when converting extended exponent data to
						;;7102	;single/double precision integers with rounding/truncation.
						;;7103	;This routine assumes that the AR/ARX contain the extended exponent
						;;7104	;data. It also assumes that the maximum exponent value + 1 of either
						;;7105	;36 or 70 (decimal) are already in the FE. This is the positive exponent
						;;7106	;maximum; the code adjusts for the fact that a negative number can have
						;;7107	;an exponent one greater than a positive number.
						;;7108	;It uses all of the registers in the EBOX and returns 4 if the
						;;7109	;result is positive and returns 5 if the result is negative
						;;7110	;with the AR/ARX containing the double word integer. It is the
						;;7111	;responsibility of the calling routine to determine whether
						;;7112	;rounding or truncation should be performed and how many words
						;;7113	;to store.
						;;7114	
						;;7115	ETXIX:	ARX_ARX*2		;get low word*2 into ARX.
						;;7116	=0	MQ_AR, SKP AR0,		; get a positive exp in AR.
						;;7117			CALL [ISOEXP]
						;;7118		CLR AR, BR/AR		;clear extraneous bits, save exp.
						;;7119		AR0-8_#, #/200		;test for positive exp.
						;;7120		GEN AR+BR, SKP AD0,	;skip on positive exponent(sum has AD0 on).
						;;7121			AR_0.M		;so exponent test has a clean register.
						;;7122	=0	MEM/ARL IND, CLR/AR+ARX,;exponent must be positive.
						;;7123			RETURN4		;return to caller.
						;;7124		AR0-8_#, #/212, J/ET1	;start range check of positive exponent
						;;7125	
						;;7126	;At this point the exponent is in BR 1-11 and it is positive.
						;;7127	;Now we must determine if it is a small enough positive number
						;;7128	;to make the conversion to integer meaningful.
						;;7129	ET1:	GEN AR-BR, SKP AD0	;do the exponent test.
						;;7130	=0	AR_BR*4, J/ET2		;exp fits in AR0-8, now for final test!
						;;7131		SET AROV, I FETCH, J/NOP;exponent out of range.
						;;7132	ET2:	AR_AR*2			;finish moving exponent into AR0-8.
						;;7133		SC_AR0-8, GEN MQ,	;exponent to SC.
						;;7134			SKP AD0		;max neg exponent is 1 gtr than max pos exp.
						;;7135	=0
						;;7136	ET2A:	AR_MQ, GEN FE-SC,	;shift low word into ARX00-34, caller
						;;7137			SKP SCAD0,	;put max exponent+1 in FE. range check.
						;;7138			J/ET2B
						;;7139		FE_FE+1, J/ET2A		;max neg exp is 1 gtr than max pos exp.
						;;7140	=0
						;;7141	ET2B:	FE_SC, J/ET3		;save exp in FE.
						;;7142		SET AROV, I FETCH, J/NOP;exponent is too large.
						;;7143	ET3:	SC_#, #/12.		;prepare to map AR12 into AR00.
						;;7144	
						;;7145	;We now have the high word in the AR and
						;;7146	;the low word*2 in the ARX. The SC has 12 (dec) to let the
						;;7147	;shifter strip off the sign and exponent of the high word.
						;;7148		AR_SIGN, MQ_SHIFT	;put high 36 integer bits into MQ.
						;;7149		AR_ARX, BR/AR, CLR ARX	;generate low 36 integer bits and
						;;7150		AR_BR, ARX/MQ, MQ_SHIFT,;  put in MQ. High bits to ARX.
						;;7151			SC_FE-#, #/36.,	;check the size of the exponent.
						;;7152			SKP SCAD0	;if exp<36. then high result is sign.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5-1
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT TO INTEGER CONVERSION				

						;;7153	=0	GEN SC, SKP SCAD NE,	;is exponent gtr or geq to 36 ?
						;;7154			J/ET3A
						;;7155		SC_#+SC, #/37., J/ET5	;exponent less than 36.
						;;7156	=0
						;;7157	ET3A:	(AR+ARX+MQ)*2, J/ET3B	;must shift left 1 bit.
						;;7158		BRX/ARX, SC_#+SC, #/1,	;adjust exp, save low word in BRX.
						;;7159			J/ET4
						;;7160	ET3B:	BR_AR LONG, AR_ARX,	;high and low to BR!BRX
						;;7161			SC_#, #/35.,	;get a good exponent for final shifting.
						;;7162			ARX/MQ, J/ET4A	;rest of fraction to ARX.
						;;7163	ET4:	AR_ARX (AD), ARX/MQ,	;exp gtr 36. High result has integer bits.
						;;7164			MQ_SHIFT	;high result to MQ.
						;;7165		AR_MQ, ARX_SHIFT	;put integer bits into ARX.
						;;7166		BR_AR LONG, AR_ARX (AD),;now compute fraction.
						;;7167			CLR ARX		;low integer to AR, pad with zeros in ARX.
						;;7168	ET4A:	AR_BR LONG, MQ_SHIFT,	;restore integer to AR!ARX, fraction to MQ.
						;;7169			SC_#, #/35.,	;low word must have bit 0 same as high.
						;;7170			SKP AD0, RET[4]	;  and return on sign of integer.
						;;7171	=01
						;;7172	ET5:	FM[T0]_AR, AR_ARX (AD),	;sign is high 36 bit result. Save in T0.
						;;7173			ARX/MQ,		;high 36 bits of frac to AR, low 23 to ARX.
						;;7174			MQ_SHIFT,	;low integer result to MQ.
						;;7175			CALL [SHIFT]	;high half of fraction to AR.
						;;7176	
						;;7177	;Now we have the high 36 bits of mantissa in AR, the low 23 bits if mantissa
						;;7178	;in the ARX, the high 36 bit result (the sign bits) in T0 and the low 36 bit
						;;7179	;result in the MQ. Now we compute the fraction to store.
						;;7180		BR/AR, AR_ARX, CLR ARX	;high frac to BR. Now gen low fraction bits.
						;;7181		ARX_SHIFT,		;low fraction bits to ARX.
						;;7182			SC_#, #/35.	;low word must have same sign as high.
						;;7183		GEN ARX*BR, AD/OR,	;gen composite OR of fraction into 1 word.
						;;7184			MQ_AD,		;put this funny fraction in the MQ.
						;;7185			ARX/MQ		;low integer result to ARX.
						;;7186		[AR]_FM[T0], SKP AD0,	;get high result (Sign) back in AR.
						;;7187			RET[4]		;and return to caller.
						; 7188	.ENDIF/GFTCNV		;[273]
						; 7189	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT TO INTEGER CONVERSION				

						; 7190	
						; 7191	;ISOEXP will isolate the exponent in an extended exponent data word.
						; 7192	;It will return the positive representation of the exponent.
						; 7193	;Call with AR containing high order word with "SKP AR0" to do
						; 7194	;correct things with one's complemented exponent in negative numbers.
						; 7195	;It returns 1 with the positive exponent in the AR.
						; 7196	=0
U 2412, 0001,3500,2007,4000,0020,0003,0164	; 7197	ISOEXP:	[AR]_[AR]*FM[EXPMSK],AD/ANDCB,RET[1] ;isolate pos exp in AR1-11.
U 2413, 0001,2400,2007,4000,0020,0003,0164	; 7198		[AR]_[AR]*FM[EXPMSK],AD/NOR,RET[1]   ;isolate neg exp in AR1-11.
						; 7199	
						; 7200	;SGNEXT will extend the sign bit of the AR into AR1-11. Call with
						; 7201	;SKP AR0 so the correct actions are taken for negative numbers.
						; 7202	;It will do a return 1 with either ones or zeroes in AR1-11.
						; 7203	=0
U 2420, 0001,3600,2007,4000,0020,0003,0164	; 7204	SGNEXT:	[AR]_[AR]*FM[EXPMSK], AD/AND, RET[1]  ;extend 0s into AR1-11.
U 2421, 0001,2700,2007,4000,0020,0003,0164	; 7205		[AR]_[AR]*FM[EXPMSK], AD/ORCB, RET[1] ;extend ones into AR1-11.
						; 7206	
						; 7207	;OVTEST will determine if the high order word of a double integer,
						; 7208	;as stored in the AR is all sign bits, ie either it is all zeroes
						; 7209	;or all ones. The call is via "GEN AR, SKP AD NE, J/OVTEST".
						; 7210	;It assumes that the double integer is in the AR/ARX and the SC
						; 7211	;contains 35 decimal.
						; 7212	;OVTEST will store the ARX*.5 and exit if the AR is all sign bits.
						; 7213	;It will set AROV and jump to NOP if it finds some data bits.
U 3224, 2422,3723,2000,0000,0020,5610,0000	; 7214	OVTST1:	AR_MQ, SKP AD NE		;get the sign bits from the MQ.
						; 7215	=0
U 2422, 3225,4001,4000,0000,0217,0010,0000	; 7216	OVTEST:	AR_SHIFT, I FETCH, J/OVTST2	;the high word is all zeros - ok.
U 2423, 2440,4001,0000,0000,0040,5610,0000	; 7217		GEN AR+1, SKP AD NE		;check to see if it is all ones.
U 2440, 3225,4001,4000,0000,0217,0010,0000	; 7218	=0	AR_SHIFT, I FETCH, J/OVTST2 	;this is simply a negative number.
U 2441, 0073,4001,0000,0000,0217,1110,0420	; 7219		SET AROV, I FETCH, J/NOP	;sorry, we found some data bits.
U 3225, 0073,4001,0000,0000,0000,1010,0000	; 7220	OVTST2:	AC0_AR, J/NOP			;finish the store.
						; 7221	.ENDIF/EXTEXP
						; 7222	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DATA CONVERSION INSTRUCTIONS			

						; 7223	.TOC	"GFLT DATA CONVERSION INSTRUCTIONS"
						; 7224	
						; 7225	1013:
U 1013, 1006,0001,0000,0000,0000,0010,0000	; 7226	L-XBLT:	J/LUUO
						; 7227	1014:
U 1014, 1006,0001,0000,0000,0000,0010,0000	; 7228	L-GTPI:	J/LUUO
						; 7229	.IF/EXTEXP				;[337]
						; 7230	1104:
U 1104, 1006,0001,0000,0000,0000,0010,0000	; 7231	L-SFTE:	J/LUUO
						; 7232	1105:
U 1105, 1006,0001,0000,0000,0000,0010,0000	; 7233	L-GTDI:	J/LUUO
						; 7234	1106:
U 1106, 1006,0001,0000,0000,0000,0010,0000	; 7235	L-GTSI:	J/LUUO
						; 7236	1107:
U 1107, 1006,0001,0000,0000,0000,0010,0000	; 7237	L-GTDR:	J/LUUO
						; 7238	1110:
U 1110, 1006,0001,0000,0000,0000,0010,0000	; 7239	L-GTSR:	J/LUUO
						; 7240	1111:
U 1111, 1006,0001,0000,0000,0000,0010,0000	; 7241	L-DITE:	J/LUUO
						; 7242	1112:
U 1112, 1006,0001,0000,0000,0000,0010,0000	; 7243	L-SITE:	J/LUUO
						; 7244	1113:
U 1113, 1006,0001,0000,0000,0000,0010,0000	; 7245	L-EFSC:	J/LUUO
						; 7246	.ENDIF/EXTEXP				;[337]
						; 7247	.IF/XADDR
U 3013, 3306,3200,0204,0000,0020,0010,0000	; 7248	3013:	ARX_AC2, J/XBLT		; -20-  XBLT
						; 7249	.ENDIF/XADDR
						;;7250	.IFNOT/EXTEXP				;[337]
						;;7251	3014:	J/MUUO				;[337] No GSNGL if no G floating
						; 7252	.IF/EXTEXP				;[337]
U 3014, 3226,3240,2007,0000,0020,0010,0165	; 7253	3014:	[AR]_FM[E1], J/L-GTSP	; -21-	GSNGL
U 3104, 3243,3240,2007,0000,0020,0010,0165	; 7254	3104:	[AR]_FM[E1], J/L-EDBL	; -22-	GDBLE
						; 7255	.IFNOT/GFTCNV		;[273]
U 3105, 1002,4001,0000,0000,0000,0010,0000	; 7256	3105:	J/MUUO			; -23-	DGFIX	;[273]
U 3106, 1002,4001,0000,0000,0000,0010,0000	; 7257	3106:	J/MUUO			; -24-	GFIX	;[273]
U 3107, 1002,4001,0000,0000,0000,0010,0000	; 7258	3107:	J/MUUO			; -25-	DGFIXR	;[273]
U 3110, 1002,4001,0000,0000,0000,0010,0000	; 7259	3110:	J/MUUO			; -26-	GFIXR	;[273]
						;;7260	.IF/GFTCNV		;[273]
						;;7261	3105:	[AR]_FM[E1], J/L-GTIN	; -23-	DGFIX
						;;7262	3106:	[AR]_FM[E1], J/L-GTIN	; -24-	GFIX
						;;7263	3107:	[AR]_FM[E1], J/L-GTIN	; -25-	DGFIXR
						;;7264	3110:	[AR]_FM[E1], J/L-GTIN	; -26-	GFIXR
						; 7265	.ENDIF/GFTCNV		;[273]
U 3111, 3260,3260,2007,0000,0020,0010,0165	; 7266	3111:	[AR]_FM[E1], J/L-FLTR	; -27-	DGFLTR
U 3112, 3267,3240,2007,0000,0020,0010,0165	; 7267	3112:	[AR]_FM[E1], J/L-DFLT	; -30-	GFLTR
U 3113, 2502,3240,2007,0000,0020,0010,0165	; 7268	3113:	[AR]_FM[E1], J/L-DFSC	; -31-	GFSC
						; 7269	
						; 7270	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DATA CONVERSION INSTRUCTIONS			

U 3226, 3227,3703,0000,0000,0312,0010,0000	; 7271	L-GTSP:	VMA_AR, LOAD AR		;-21- GSNGL EDPFP TO SPFP
U 3227, 3230,3200,0003,0000,0022,0022,0100	; 7272		AR_MEM, MQ_0.S		;load high word into AR.
U 3230, 2442,3703,0000,0000,0020,5610,0000	; 7273		GEN AR, SKP AD NE	;check for zeroes.
U 2442, 0072,0001,0000,0000,0217,0010,0000	; 7274	=0	I FETCH, J/STORAC	;high word zero, store it.
U 2443, 2450,4001,0000,0000,0000,3610,0000	; 7275		VMA_VMA+1		;point to mem low word.
						; 7276	=0	ARX_AR, SKP AR0,	;save high word in ARX.
U 2450, 2412,4001,0400,0000,1000,4550,0000	; 7277			CALL [ISOEXP]	;get the excess-2000 exponent.
U 2451, 3231,4001,0040,0000,0000,0410,0000	; 7278		CLR AR, BR/AR		;exp to BR.
U 3231, 3232,0001,0000,0000,0000,0110,0220	; 7279		AR0-8_#, #/220		;largest exponent allowed is 2200.
U 3232, 2452,1102,0004,0000,0040,5510,0000	; 7280		GEN AR-BR-1, SKP AD0	;range check exponent.
U 2452, 3233,4001,0000,0000,0000,0110,0157	; 7281	=0	AR0-8_#, #/157, J/L-GTS1;do lower range check now.(actually too low)
U 2453, 0073,0001,0000,0000,0217,1110,0620	; 7282		SET FLOV, I FETCH, J/NOP;tough
U 3233, 3234,3202,2040,0000,0000,0010,0000	; 7283	L-GTS1:	BR/AR, AR_BR		;swap values around for next subtract.
U 3234, 2470,5102,0000,0000,0040,5510,0000	; 7284		GEN AR-BR, SKP AD0	;do lower range check.
U 2470, 3235,0001,0040,0000,0000,0410,0000	; 7285	=0	BR/AR, CLR AR, J/L-GTS6	;passed. 10 bit path to do last checks.
U 2471, 0073,4001,0000,0000,0217,1110,0630	; 7286		SET FXU, I FETCH, J/NOP	;too low.
U 3235, 3236,0001,0000,0000,0000,0110,0160	; 7287	L-GTS6:	AR0-8_#, #/160		;subtract 1600 to get excess 200 exp.
U 3236, 3237,3202,2040,0000,0000,0010,0000	; 7288		AR_BR, BR/AR		;swap around to do subtract.
U 3237, 3240,5102,2000,0000,0020,0010,0000	; 7289		AR_AR-BR		;got it.
U 3240, 3241,3203,5000,0000,0000,0010,0000	; 7290		AR_AR*8			;move excess-200 exponent over.
						; 7291		FE_AR0-8, AR_ARX,	;put some exponent in FE. High word to AR.
U 3241, 3242,4001,4000,2421,2013,0010,0000	; 7292			LOAD ARX	;low word to ARX.
						; 7293	;This next test determines the relative size of the exponent. If the expo-
						; 7294	;nent is less than 401 then it is a positive exponent and all will be well.
						; 7295	;If the exponent is greater than 400 (actually 700), then the exponent is
						; 7296	;really negative but bit 0 of the FE is off. To correct the sign of the
						; 7297	;exponent and to prevent undeserved FXU later because of the incorrect sign
						; 7298	;bit, we must examine the value of the exponent so as to always get the
						; 7299	;correct sign during normalization.
						; 7300		ARX_MEM, GEN FE-#,	;undeserved FXU happens when FE00 should be
U 3242, 2472,3200,0003,5030,0042,5110,0500	; 7301			#/500, SKP SCAD0;set from previous subtract of 1600.
						; 7302	=0	FE_FE+#, #/777,		;set FE00. Later add will clear it.
						; 7303			ARX_ARX*2,	;low word * 2.
U 2472, 2500,3701,0500,2031,0000,0010,0777	; 7304			J/L-GTS7	;continue.
U 2473, 2500,3701,0500,3001,0000,0010,0000	; 7305		FE_FE-1, ARX_ARX*2	;adjust FE so later add gets right exp.
						; 7306	=0
U 2500, 2420,0001,0000,0000,0000,4550,0000	; 7307	L-GTS7:	SKP AR0, CALL [SGNEXT]	;sign extend high word.
						; 7308		AR_AR*.25 LONG,		;prepare for normalization
						; 7309			FE_FE+#, #/6,	;adjust exponent.
U 2501, 1120,3703,7700,2031,0040,0035,0006	; 7310			NORM, J/SNORM	;finish up.
						; 7311	
U 3243, 3244,3703,0000,0000,0312,0010,0000	; 7312	L-EDBL:	VMA_AR, LOAD AR		;-22- GDBLE SPFP to EXTENDED EXPONENT
U 3244, 3245,3200,0003,0000,0022,0710,0003	; 7313		AR_MEM, CLR MQ
U 3245, 3246,0001,0400,0202,1000,0410,0000	; 7314		SC_EXP, ARX_AR, CLR AR	;correct the expoent, save a copy in the ARX
U 3246, 3247,4001,0007,0000,0000,1010,0165	; 7315		FM[E1]_AR		;no sticky bits here.
U 3247, 3250,4001,0000,2400,2001,0010,0200	; 7316		EXP_SC			;put the "positive" exponent back IN THE AR.
U 3250, 3251,0303,7000,0000,0020,0010,0000	; 7317		AR_AR*.5		;must move exponent into AR4-11
U 3251, 3252,3703,7000,0000,0000,0010,0000	; 7318		AR_AR*.25		;  done.
U 3252, 3253,0001,0040,0000,0000,0410,0000	; 7319		BR/AR, CLR AR		;exp to BR.
U 3253, 3254,4001,0000,0000,0000,0110,0160	; 7320		AR0-8_#, #/160		;put 1600 in the AR for exp conversion
U 3254, 3255,0602,2004,0301,0020,0010,0775	; 7321		AR_AR+BR, FE_#, #/-3	;convert exp, set initial exp correction
U 3255, 3256,4001,4007,0000,2000,1010,0172	; 7322		FM[T2]_AR, AR_ARX	;save exp for ENORM, frac to AR
U 3256, 3257,4001,0000,0000,1001,0610,0240	; 7323		EXP_SIGN.C, ARX_0.M	;get rid of exp, clear low word
						; 7324		GEN AR, SC_#, #/3, NORM,;normalize an extended exponent number
U 3257, 1520,3703,0000,0302,0040,0035,0003	; 7325			J/ENORM; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DATA CONVERSION INSTRUCTIONS			

						; 7326	
						;;7327	.IF/GFTCNV		;[273]
						;;7328	L-GTIN:	VMA_AR, LOAD AR		;23-26. fetch high word.
						;;7329		AR_MEM, MQ_0.S,		;word in AR, init MQ.
						;;7330			VMA_VMA+1	;prepare to fetch low word.
						;;7331		GEN AR, SKP AD NE	;is high word all zeroes ?
						;;7332	=0	CLR ARX, EXIT DBL	;high word zero, store zeroes.
						;;7333		LOAD ARX, B DISP	;fetch low word, call appropriate routine.
						;;7334	
						;;7335	=000	ARX_MEM, J/L-G23	;do GDP to DP integer, truncate.
						;;7336	=010	ARX_MEM, J/L-G24	;do GDP to SP integer, truncate.
						;;7337	=100	ARX_MEM, J/L-G25	;do GDP to DP integer, rounded.
						;;7338	=110	ARX_MEM, J/L-G26	;do GDP to SP integer, rounded.
						;;7339	=				;terminate this dispatch block.
						;;7340	
						;;7341	;DGFIX needs the sticky bit fix.
						;;7342	=0010
						;;7343	L-G23:	FE_#, #/70.,		;-23- DGFIX GDP to double integer, truncate.
						;;7344			CALL [ETXIX]	;do the conversion
						;;7345	=0110	EXIT DBL		;store results.
						;;7346	=0111	BR_AR LONG, AR_ARX,	;save high 2 words in BR!BRX, MSB of
						;;7347			ARX/MQ,		;fraction to AR35. Rest of fraction to ARX.
						;;7348			SC_#, #/35.,	;get fraction all together.
						;;7349			CALL [EF12A]
						;;7350	=1111	GEN AR, SKP AD NE,	;any fraction bits on ?
						;;7351			MQ_0.S		;[240]CLEAR MQ00 FOR ARX_2 MACRO.
						;;7352	=0	AR_BR LONG, J/ST2AC	;no, leave answer alone.
						;;7353		CLR AR, ARX_2		;yes, add 1 to integer part.
						;;7354		AR_AR+BR LONG, J/ST2AC	;store result.
						;;7355	
						;;7356	;GFIX needs the sticky bit fix.
						;;7357	=0010
						;;7358	L-G24:	FE_#, #/35.,		;-24- GFIX GDP to single integer, truncate.
						;;7359			CALL [ETXIX]	;do the conversion
						;;7360	=0110
						;;7361	L-GTS2:	SKP AR NE, J/OVTEST	;test for sign bits in AR and store.
						;;7362	=0111	BR_AR LONG, AR_ARX,	;save in BR!BRX.
						;;7363			ARX/MQ,		;add one to integer part of negative number
						;;7364			SC_#, #/35.,	;if fraction is not zero.
						;;7365			CALL [EF12A]
						;;7366	=1111	GEN AR, SKP AD NE,	;is fraction zero ?
						;;7367			MQ_0.S		;[240]CLEAR MQ00 FOR ARX_2 MACRO.
						;;7368	=0	AR_BR LONG, SKP AD NE,	;yes, try to store the result.
						;;7369			J/OVTEST
						;;7370		CLR AR, ARX_2		;no, add one to integer part.
						;;7371		AR_AR+BR LONG, SKP AD NE,; do the add and test that the high
						;;7372			J/OVTEST	;word is all sign bits.
						;;7373	
						;;7374	=011
						;;7375	L-G25:	FE_#, #/70.,		;-25- DGFIXR GDP to double integer, rounded.
						;;7376			CALL [ETXIX]	;do the conversion
						;;7377	=111	BR_AR LONG, CLR AR,	;save in BR!BRX, round by adding one half
						;;7378			ARX_1,		;to result. Remember that the MSB of the
						;;7379			SC_#, #/35.	;store routine expects this.
						;;7380		AR_AR+BR LONG, AD FLAGS	;fraction is on ARX35.  Do the rounding and
						;;7381	;=0	; replace SKP CRY0 with AD FLAGS. Eliminates extra word.; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9-1
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DATA CONVERSION INSTRUCTIONS			

						;;7382		EXIT DBL		;  store the double result.
						;;7383	;	SET AROV, I FETCH, J/NOP;rounding caused an overflow - too bad!
						;;7384	
						;;7385	=011
						;;7386	L-G26:	FE_#, #/35.,		;-26- GFIXR GDP to single integer, rounded.
						;;7387			CALL [ETXIX]	;do the conversion.
						;;7388	=111	BR_AR LONG, CLR AR,	;save in CR!BRX, round by adding one half
						;;7389			ARX_1,		;to result. MSB of the fraction is in ARX35.
						;;7390			SC_#, #/35.	;store routine expects this.
						;;7391		AR_AR+BR LONG, SKP AD NE,;do the rounding.
						;;7392			J/OVTEST	;figure out what, if any, to store.
						; 7393	.ENDIF/GFTCNV		;[273]
						; 7394	L-FLTR:	VMA_AR, LOAD AR,	;-27- DGFLTR DP INTEGER to EDPFP
U 3260, 3261,3703,0000,0301,0312,0010,0137	; 7395			FE_#, #/137	;inital fugde factor for exp
U 3261, 1464,3200,0003,0000,0022,0022,0100	; 7396		AR_MEM, MQ_0.S		;get high word into the AR.
						; 7397	=0*	VMA_VMA+1, LOAD ARX,	;get the low word into the ARX,
U 1464, 0770,0001,0040,0000,0013,3650,0000	; 7398			BR/AR, CALL [XFERW]; and save the high word in the BR.
U 1466, 3262,3721,0500,0000,0000,0410,0000	; 7399	=1*	ARX_ARX*2, CLR AR	;ignore the sign copy.
U 3262, 3263,4001,0007,0000,0000,1010,0165	; 7400		FM[E1]_AR		;no sticky bits here.
U 3263, 3264,4001,0000,0000,0000,0110,0200	; 7401		AR0-8_#, #/200		;ENORM expects the exponent in T2.
						; 7402		FM[T2]_AR, AR_BR,	;and save it in T2.
U 3264, 3265,3202,2217,0000,2000,1010,0172	; 7403			ARX/AD, MQ_ARX	;sign to AR, high to ARX, low to MQ.
U 3265, 3266,5441,2000,0000,0020,0016,0000	; 7404		AR_SIGN 		;
U 3266, 1520,3701,0000,0000,0040,0035,0000	; 7405		GEN AR, NORM, J/ENORM	;restore high word and normalize.
						; 7406	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; EXTEXP.MIC[10,5351]	19:52 24-Jul-85			GFLT DATA CONVERSION INSTRUCTIONS			

						; 7407	
						; 7408	L-DFLT:	VMA_AR, LOAD AR,	;-30- GFLTRSP INTEGER to EDPFP
U 3267, 3270,3701,0000,0301,0312,0010,0004	; 7409			FE_#, #/4	;initial fudge factor for exp.
U 3270, 3271,3240,0003,0000,0022,0710,0003	; 7410		AR_MEM, CLR MQ		;get the single precision op.
U 3271, 3272,5441,2400,0000,1020,0016,0000	; 7411		AR_SIGN, ARX_AR		;build a dummy high word of all sign.
U 3272, 3273,4001,0040,0000,0000,0410,0000	; 7412		BR/AR, CLR AR		;save sign, prepare for exponent.
U 3273, 3274,4001,0007,0000,0000,1010,0165	; 7413		FM[E1]_AR		;no sticky bits here.
U 3274, 3275,0001,0000,0000,0000,0110,0207	; 7414		AR0-8_#, #/207		;build an initial exp of 207 for ENORM
						; 7415		FM[T2]_AR, AR_BR,	;save exp for ENORM, restore sign word.
U 3275, 1520,3202,2007,0000,0040,1035,0172	; 7416			NORM, J/ENORM	;and normalize it.
						; 7417	
						; 7418	=0
						; 7419	L-DFSC:	AR_AC0, BR/AR, SKP AD0,	;-31- GFSC EDPFP SCALE
U 2502, 2412,3240,2040,0000,0020,5550,0000	; 7420			CALL [ISOEXP]	;get the exponent into the AR.
U 2503, 3276,3202,2040,0000,0000,0010,0000	; 7421		BR/AR, AR_BR		;put exp in BR, scale factor to AR.
						; 7422		AR_AR SWAP, GEN AC0,	;put scale in left half of AR.
U 3276, 2514,3200,4000,0000,3020,5610,0000	; 7423			SKP AD NE	;is high word zero ?
U 2514, 0061,4001,0000,0000,0001,0010,0170	; 7424	=0	AR+ARX+MQ_0.M, J/ST2AC	;yes, store zero as double result.
						; 7425		AR_SIGN, ARX_AR, SC_#,	;no, move sign and scale factor together.
U 2515, 3277,5401,2400,0302,1020,0016,0042	; 7426			#/34.
U 3277, 3300,0001,4000,0401,0000,0010,0000	; 7427		AR_SHIFT, CLR FE	;sign now in AR00, scale in AR 9-19.
U 3300, 3301,4001,0000,0000,1000,0022,0200	; 7428		EXP_SIGN		;scale sign is in AR00; extend it.
U 3301, 3302,0001,0000,0302,0000,0010,0010	; 7429		SC_#, #/8.		;move scale factor into AR 1-11 and
U 3302, 3303,3240,4201,0000,0020,0010,0000	; 7430		AR_SHIFT, ARX_AC1	; put the sign to left of scale factor.
U 3303, 3304,0602,2004,0000,0020,0710,0003	; 7431		AR_AR+BR, CLR MQ	;add exponent and scale factor.
U 3304, 0663,4001,0000,0000,1020,0007,0000	; 7432		SH/AR, DISP/SH0-3	;check for over and under flovs.
						; 7433	=0011
						; 7434	L-FSC2:	[AR]_[AR]*FM[EXPMSK],	;clear out non-exponent bits.
						; 7435			AD/ANDCB,	;and AR00 in the over or under flow case.
U 0663, 3305,3500,2007,4000,0020,0010,0164	; 7436			J/L-FSC3	; and continue
						; 7437	=0111	[AR]_[AR]*FM[EXPMSK],	;clear out non-exponent bits.
						; 7438			AD/ANDCB,	;
U 0667, 3305,3500,2007,4000,0020,0010,0164	; 7439			J/L-FSC3	; and continue
U 0673, 0663,0001,0000,0000,0000,1110,0620	; 7440	=1011	SET FLOV, J/L-FSC2	;you lose
U 0677, 0663,4001,0000,0000,0000,1110,0630	; 7441	=1111	SET FXU, J/L-FSC2	;ditto
						; 7442	
U 3305, 2520,3721,0507,0000,0000,1010,0172	; 7443	L-FSC3:	FM[T2]_AR, ARX_ARX*2	;save new exponent fofr ENORM.
						; 7444	=0	AR_AC0, SKP AD0,	;get the high word.
						; 7445			SC_#, #/3,	;for ENORM.
U 2520, 2420,3200,2000,0302,0020,5550,0003	; 7446			CALL [SGNEXT]	;and sign extend it for ENORM as well.
U 2521, 1520,3701,0000,0000,0040,0035,0000	; 7447		GEN AR, NORM, J/ENORM	;put the result back together.
						; 7448	
						; 7449	.ENDIF/EXTEXP
						; 7450	.ENDIF/MODEL.B
						; 7451	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; BLT.MIC[10,5351]	19:52 24-Jul-85			XBLT							

						; 7452	.TOC	"XBLT"
						; 7453	
						; 7454	;HERE FROM EXTEND, ARX CONTAINS AC2
						; 7455	.IF/XADDR
						; 7456	
U 3306, 3307,3200,2001,0000,0020,1610,0216	; 7457	XBLT:	AR_AC1,SR_XBLT(SRC)		;[262] IN CASE OF INTERRUPT
						; 7458		BR/AR,BRX/ARX,MQ_AR,		;SRC ADDR TO BR, DST TO BRX
U 3307, 2522,3200,2070,0000,1020,5510,0000	; 7459			AR_AC0,SKP AD0,J/XBLT3	;GET LENGTH, TEST DIRECTION
						; 7460	=0
U 2522, 2560,3703,0010,0000,1040,5410,0000	; 7461	XBLT3:	SKP AR NE,MQ_AR,J/XBLT4
						; 7462		AR_MQ-1,ARX_ARX-1,		;DECR SRC & DST ADDR'S FOR DOWN
U 2523, 3310,1723,2600,0000,0352,0010,0000	; 7463			VMA/AD,LOAD AR		;GET FIRST WORD
						; 7464	
						; 7465	;HERE IS MAIN LOOP FOR XBLT, DOWNWARDS
						; 7466	
U 3310, 3311,3240,0013,0000,1022,1610,0316	; 7467	XBLTDN:	MQ_AR,AR_MEM,SR_XBLT(DST)	;WAIT FOR SOURCE WORD
U 3311, 3312,3713,0000,0000,0316,1610,0216	; 7468		VMA_ARX,STORE,SR_XBLT(SRC)	;STORE IT IN DESTINATION
U 3312, 2524,3723,2003,0000,0002,7010,0000	; 7469		MEM_AR,AR_MQ,SKP INTRPT		;WAIT, CHECK FOR INTRPT
						; 7470	=0	BR/AR,BRX/ARX,			;PUT DECREMENTED ADDR'S IN BR,X
U 2524, 2552,4660,2060,0000,0040,5410,0000	; 7471			AR_AC0+1,SKP CRY0,J/XBLTD1	;COUNT OFF LENGTH
U 2525, 3746,4640,2060,0000,0020,0010,0000	; 7472		BR/AR,BRX/ARX,AR_AC0+1,J/PGFAC0	;CLEANUP AND TAKE INTERRUPT
						; 7473	=0
						; 7474	XBLTD1:	AC0_AR,AR_MQ-1,ARX_ARX-1,	;STORE NEW LENGTH, GET NEXT ADDR
U 2552, 3310,1721,2600,0000,0352,1010,0000	; 7475			VMA/AD,LOAD AR,J/XBLTDN	; AND READ SRC WORD
						; 7476		AC0_AR,AR_ARX,ARX/MQ,		;DONE!  PUT ALL AWAY
U 2553, 3313,0001,4300,0000,2217,1010,0000	; 7477			I FETCH
U 3313, 2124,4001,4004,0000,2000,1010,0000	; 7478	XBLTX:	AC2_AR,AR_ARX,J/STRAC1
						; 7479	
						; 7480	;HERE FOR UPWARD BLT, TESTING FOR NON-ZERO LENGTH
						; 7481	
						; 7482	=0
U 2560, 0073,0001,0000,0000,0217,1610,0000	; 7483	XBLT4:	I FETCH,SR_0,J/NOP		;DO NOTHING IF AC =0
						; 7484		VMA_BR,LOAD AR,			;ELSE START RIGHT IN
U 2561, 3316,3242,0000,0000,0312,1610,0316	; 7485			SR_XBLT(DST),J/XBLTU1
						; 7486	
						; 7487	;HERE IS MAIN LOOP FOR XBLT, UPWARDS
						; 7488	
U 3314, 2562,1723,2003,0000,0042,5610,0000	; 7489	XBLTUP:	MEM_AR,AR_MQ-1,SKP AD NE	;COUNT EXHAUSTED?
						; 7490	=0	AC0_AR,ARX_BR+1,AR_BRX+1,	;YES.  GET FINAL ADDRESSES
U 2562, 3313,4642,6200,0000,0257,1010,0000	; 7491			I FETCH,J/XBLTX		; READY TO STORE
						; 7492		AC0_AR,MQ_AR,AR_BR+1,ARX_BRX+1,
U 2563, 3315,4662,2610,0000,1352,1010,0000	; 7493			VMA/AD,LOAD AR		;GET SOURCE WORD
U 3315, 3316,0001,0060,0000,0000,1610,0316	; 7494		BR/AR,BRX/ARX,SR_XBLT(DST)	;MUST BE SAVED PRIOR TO MBWAIT
U 3316, 2564,3200,0003,0000,0022,7010,0000	; 7495	XBLTU1:	AR_MEM,SKP INTRPT		;WAIT FOR SRC, TEST INTRPT
						; 7496	=0	VMA_ARX,STORE,			;COPY TO DST
U 2564, 3314,3713,0000,0000,0316,1610,0216	; 7497			SR_XBLT(SRC),J/XBLTUP	;LOOP
U 2565, 3747,3242,2600,0000,0000,1610,0000	; 7498		AR_BR LONG,SR_0,J/XBLTPF	;TAKE INTERRUPT
						; 7499	
						; 7500	.ENDIF/XADDR
						; 7501	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; BLT.MIC[10,5351]	19:52 24-Jul-85			BLT							

						; 7502	.TOC	"BLT"
						; 7503	; ENTER WITH 0,E IN AR
						; 7504	
						; 7505	;IN THE LOOP, ARX CONTAINS THE CURRENT DESTINATION ADDRESS,
						; 7506	; BRX CONTAINS THE TERMINAL ADDRESS, AND BR CONTAINS THE DIFFERENCE
						; 7507	; BETWEEN THE SOURCE AND DESTINATION ADDRESSES.
						; 7508	
						; 7509	;UNLIKE EARLIER -10 PROCESSORS, THIS CODE CHECKS FOR THE CASE IN WHICH
						; 7510	; THE DESTINATION ADDRESS IN RH(AC) IS GREATER THAN E, AND RATHER THAN
						; 7511	; STOPPING AFTER ONE WORD, COPIES DOWNWARD (EFFECTIVELY DECREMENTING
						; 7512	; AC BY 1,,1 ON EACH STEP, RATHER THAN INCREMENTING).
						; 7513	
						; 7514	;THIS CODE ALSO PROVIDES A GUARANTEED RESULT IN AC ON COMPLETION OF
						; 7515	; THE TRANSFER (EXCEPT IN THE CASE AC IS PART OF BUT NOT THE LAST WORD
						; 7516	; OF THE DESTINATION BLOCK).  WHEN AC IS NOT PART OF THE DESTINATION
						; 7517	; BLOCK, IT IS LEFT CONTAINING THE ADDRESSES OF THE FIRST WORD FOLLOWING
						; 7518	; THE SOURCE BLOCK (IN THE LH), AND THE FIRST WORD FOLLOWING THE DEST-
						; 7519	; INATION BLOCK (IN THE RH).  IF AC IS THE LAST WORD OF THE DESTINATION
						; 7520	; BLOCK, IT WILL BE A COPY OF THE LAST WORD OF THE SOURCE BLOCK.
						; 7521	
						; 7522	;IN ADDITION, A SPECIAL-CASE CHECK IS MADE FOR THE CASE IN WHICH EACH
						; 7523	; WORD STORED IS USED AS THE SOURCE OF THE NEXT TRANSFER.  IN THIS CASE,
						; 7524	; ONLY ONE READ NEED BE PERFORMED, AND THAT DATA MAY BE STORED FOR EACH
						; 7525	; TRANSFER.  THUS THE COMMON USE OF BLT TO CLEAR CORE IS SPEEDED UP.
						; 7526	
						; 7527	;BLT:	ARX_AR,MQ_AR,ARR_AC0,ARL_ARL	;END TO ARX & MQ, DEST TO AR
						; 7528	BLT1:	BR/AR,ARX_AR,BRX/ARX,		;DST TO BR & ARX, END TO BRX
U 3317, 3320,3200,2460,0000,1020,0010,0000	; 7529			AR_AC0			;SRC TO ARL
						;;7530	.IFNOT/BLT.PXCT
						;;7531		ARR_ARL,ARL_BRL			;SRC TO ARR (SAME SECTION AS E)
						;;7532		AR_AR-BR			;SRC-DST TO ARR
						; 7533	.IF/BLT.PXCT
U 3320, 2566,3202,4000,0000,3001,7410,0002	; 7534		ARR_ARL,ARL_BRL.M,SKP P!S XCT	;SRC TO ARR (SAME SECTION AS E)
U 2566, 1574,5102,2004,0000,0020,0010,0000	; 7535	=0	AR_AR-BR,J/BLT2			;SRC-DST TO ARR
U 2567, 1634,5102,2000,0000,0020,0010,0000	; 7536		AR_AR-BR,J/BLTPXCT		;TREAT PXCT OF BLT SPECIAL
						; 7537	.ENDIF/BLT.PXCT
						;;7538	.IF/BACK.BLT
						;;7539		BR/AR,SKP ARX LE BRX		;SRC-DST TO BR. UP OR DOWN?
						;;7540	=00	AR_MQ-1,CALL,J/BLTAC		;DOWN, READY WITH E-1
						;;7541		AR_MQ+1,CALL,J/BLTAC		;UP, PUT E+1 IN AR FOR AC
						;;7542	DOWN:	LOAD VMA(EA)_ARX+BR,J/DN1	;DOWN, START THE LOOP
						; 7543	.IFNOT/BACK.BLT
						; 7544	=0*
U 1574, 3321,4023,2040,0000,0020,0050,0000	; 7545	BLT2:	BR/AR,AR_MQ+1,CALL,J/BLTAC	;SRC-DST TO BR, E+1 IN AR
						; 7546	.ENDIF/BACK.BLT
U 1576, 2570,4662,0000,0000,0040,5410,0000	; 7547		SKP BR EQ -1,J/UP		;IS THIS CORE CLEARING CASE?
						; 7548	
						; 7549	
						; 7550	;HERE TO SETUP FINAL AC
						; 7551	
U 3321, 3322,0602,2000,0000,3020,0610,0004	; 7552	BLTAC:	ARL_ARR,AR_AR+BR		;FINAL DEST TO LH, SRC TO RH
U 3322, 3323,0001,4000,0000,3000,1610,0707	; 7553		AR_AR SWAP,SR_BLT(SRC)
U 3323, 0002,0001,0000,0000,0000,1003,0000	; 7554	ACSETU:	AC0_AR,RETURN2			;[334] Used below as well
						; 7555	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; BLT.MIC[10,5351]	19:52 24-Jul-85			BLT							

						; 7556	;HERE FOR UPWARD BLT (AC RH .LE. E)
						; 7557	=0
U 2570, 0761,0612,0000,0000,0131,0010,0402	; 7558	UP:	LOAD VMA(EA)_ARX+BR,J/UP1	;NOT CLEAR CORE
U 2571, 0760,0612,0004,0000,0131,7410,0402	; 7559		SKP P!S XCT,LOAD VMA(EA)_ARX+BR	;DO NOT OPTIMIZE UNDER EXT ADDR
						; 7560	
						; 7561	;USE EVEN LOC'NS OF THIS BLOCK OF 4 IN SPECIAL "CLEAR CORE" CASE
						; 7562	
U 0760, 3324,3200,0003,0000,0022,1650,0507	; 7563	=00	AR_MEM,CALL,SR_BLT(DST),J/UP2	;GET THE WORD TO STORE IN ALL
U 0761, 3324,3200,0003,0000,0022,1650,0507	; 7564	UP1:	AR_MEM,CALL,SR_BLT(DST),J/UP2	;GET SOURCE WORD
U 0762, 3324,0001,0000,0000,0000,1650,0507	; 7565		CALL,SR_BLT(DST),J/UP2		;HERE TO STORE SAME SRC AGAIN
U 0763, 0761,0612,0000,0000,0131,0010,0402	; 7566		LOAD VMA(EA)_ARX+BR,J/UP1	;HERE TO GET NEXT SRC
						; 7567	
U 3324, 2572,3711,0000,0000,0111,7010,0042	; 7568	UP2:	STORE VMA(EA)_ARX,SKP INTRPT	;OK, GET DST ADDRESS
						; 7569	=0
U 2572, 2574,5102,0000,0000,0040,5310,0000	; 7570	UP3:	SKP ARX LT BRX,J/UP4		;CHECK FOR LAST TRANSFER
U 2573, 3744,4001,0003,0000,0002,0010,0000	; 7571		MEM_AR,J/BLTPF			;FINISH THIS, GO SERVE INTRPT
						; 7572	=0
U 2574, 0073,4001,0003,0000,0217,0010,0000	; 7573	UP4:	FIN STORE,I FETCH,J/NOP		;THAT'S ALL, FOLKS
						; 7574		MEM_AR,ARX_ARX+1,		;STORE DST,
U 2575, 0002,4021,0603,0000,0022,1603,0707	; 7575			SR_BLT(SRC),RETURN2	; CONTINUE
						; 7576	
						; 7577	;BLT CONTINUED - HERE FOR DOWNWARD BLT (AC RH .GT. E)
						;;7578	.IF/BACK.BLT
						;;7579	
						;;7580	DN1:	AR_MEM,SR_BLT(DST)		;WAIT FOR SOURCE DATA
						;;7581		VMA_ARX,STORE,SKP INTRPT	;OK, START DST REF
						;;7582	=0	SKP ARX LE BRX,J/DN3		;CHECK FOR END CONDITION
						;;7583		MEM_AR,J/BLTPF			;FINISH STORE, TAKE INTRPT
						;;7584	=0
						;;7585	DN3:	MEM_AR,ARX_ARX-1,		;NOT END, LOOP
						;;7586			SR_BLT(SRC),J/DOWN
						;;7587		FIN STORE,I FETCH,J/NOP		;END
						; 7588	.ENDIF/BACK.BLT
						; 7589	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; BLT.MIC[10,5351]	19:52 24-Jul-85			EXTENDED ADDRESSING CODE FOR PXCT OF BLT		

						; 7590	.TOC "EXTENDED ADDRESSING CODE FOR PXCT OF BLT"
						; 7591	
						; 7592	;THIS MUST BE SEPERATE CODE TO MAKE PXCT WORK NOTE THAT PXCT ONLY CAN
						; 7593	;BE USED IN SECTION 0 AND IN FACT WILL EVENTUALLY BE REMOVED FROM THERE
						; 7594	;HOPEFULLY THIS CODE CAN GO AWAY THE SPEC IS THAT PXCT OF BLT IS NOT DEFINED
						; 7595	;AND WILL NOT BE USED ON EXTENDED ADDRESSING MACHINES.
						; 7596	
						; 7597	.IF/BLT.PXCT
						; 7598	=0*
U 1634, 3325,4021,2040,0000,0020,0050,0000	; 7599	BLTPXCT:BR/AR,AR_MQ+1,CALL,J/BLTPX1	;SRC-DST TO BR, E+1 IN AR
U 1636, 0771,0612,0000,0000,0332,0010,0000	; 7600	UPPX:	VMA_ARX+BR,LOAD AR,J/UP1PX	;NOT CLEAR CORE
						; 7601						;CORE CLEARING NOT LEGAL
						; 7602	
						; 7603	
						; 7604	;HERE TO SETUP FINAL AC
						; 7605	
U 3325, 3326,0602,2004,0000,3020,0610,0004	; 7606	BLTPX1:	ARL_ARR,AR_AR+BR		;FINAL DEST TO LH, SRC TO RH
						; 7607		AR_AR SWAP,SR_BLT(PXCT SRC),
U 3326, 3323,4001,4000,0000,3000,1610,0307	; 7608			J/ACSETU		;[334] Use common return above
						; 7609	
						; 7610	;USE EVEN LOC'NS OF THIS BLOCK OF 4 IN SPECIAL "CLEAR CORE" CASE
						; 7611	
						; 7612	=00
						; 7613	=01
U 0771, 3327,3240,0003,0000,0022,1650,0107	; 7614	UP1PX:	AR_MEM,CALL,SR_BLT(PXCT DST),J/UP2PX	;GET SOURCE WORD
U 0772, 3327,4001,0000,0000,0000,1650,0107	; 7615		CALL,SR_BLT(PXCT DST),J/UP2PX	;HERE TO STORE SAME SRC AGAIN
U 0773, 0771,0612,0000,0000,0332,0010,0000	; 7616		VMA_ARX+BR,LOAD AR,J/UP1PX	;HERE TO GET NEXT SRC
						; 7617	
U 3327, 2576,3713,0000,0000,0316,7010,0000	; 7618	UP2PX:	VMA_ARX,STORE,SKP INTRPT	;OK, GET DST ADDRESS
						; 7619	=0
U 2576, 2600,5102,0000,0000,0040,5310,0000	; 7620	UP3PX:	SKP ARX LT BRX,J/UP4PX		;CHECK FOR LAST TRANSFER
U 2577, 3744,4001,0003,0000,0002,0010,0000	; 7621		MEM_AR,J/BLTPF			;FINISH THIS, GO SERVE INTRPT
						; 7622	=0
U 2600, 0073,4001,0003,0000,0217,0010,0000	; 7623	UP4PX:	FIN STORE,I FETCH,J/NOP		;THAT'S ALL, FOLKS
						; 7624		MEM_AR,ARX_ARX+1,		;STORE DST,
U 2601, 0002,4001,0603,0000,0022,1603,0307	; 7625			SR_BLT(PXCT SRC),RETURN2	; CONTINUE
						; 7626	.ENDIF/BLT.PXCT
						; 7627	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  ILDB, LDB			

						; 7628		.TOC	"Single Byte Instructions:  ILDB, LDB"
						; 7629	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7630	;									;
						; 7631	;	The following code represents a complete overhauling of the	;
						; 7632	;	byte oriented PDP-10 instructions.  These instructions have	;
						; 7633	;	been reworked with one and two word global byte pointers in	;
						; 7634	;	mind.  Special emphasis has been placed on high speed oper-	;
						; 7635	;	ation of the one word byte pointers, even where	that has meant	;
						; 7636	;	spending a substantial amount of CRAM; TWGs, by contrast,	;
						; 7637	;	have just been made to work.					;
						; 7638	;									;
						; 7639	;	The approach used for OWLs has been to minimize the amount	;
						; 7640	;	of computation that is not overlapped with memory reference.	;
						; 7641	;	This has been done by carefully initializing the SC and FE	;
						; 7642	;	in such a manner that the next shift count can be computed	;
						; 7643	;	while the current shift is taking place.  The OWG code dis-	;
						; 7644	;	patches into CRAM tables which set up these counts.  This	;
						; 7645	;	requires a lot of CRAM (one word for each possible OWG for	;
						; 7646	;	both loading and depositing bytes), but it eliminates the	;
						; 7647	;	requirement for a memory access to look up that information	;
						; 7648	;	in the EPT.							;
						; 7649	;									;
						; 7650	;						--QQSV			;
						; 7651	;									;
						; 7652	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						; 7653	
						; 7654	;
						; 7655	;	ILDB--Increment a byte pointer (in memory), then load the byte
						; 7656	;	it specifies into AC.
						; 7657	;	LDB--Load byte specified by byte pointer into AC.
						; 7658	;	The constraints immediately below are necessary to make IBP
						; 7659	;	work.
						; 7660	;
						; 7661		.DCODE
D 0134, 6500,0200				; 7662	134:	RW,	AC,	J/ILDB
D 0135, 4500,0204				; 7663		R,	AC,	J/LDB		;No write test for LDB
						; 7664		.UCODE
						; 7665	
						; 7666	=0****000000
						; 7667	ILDB:	ARX_AR,SC_P-#,#/37.,		;Save word for later dispatch
U 0200, 1642,4001,0400,5132,1020,0074,0045	; 7668			BYTE DISP,CALL [INCRBP]	; Test for OWG, increment BP
						; 7669	=100
						; 7670	LDB:	MEM_AR,ARX_AR,			;Await possible pointer store
U 0204, 0214,4001,0403,5132,1022,0034,0045	; 7671			SC_P-#,#/37.,BYTE DISP	; Save P; split OWL, OWG, TWG
						; 7672	=1100	GEN AR,EXT BYTE READ,SC_FE#,	;An OWG. Start reading the word
U 0214, 0733,3701,0000,0002,1131,0007,0620	; 7673			AR0-3 DISP,J/OWGLDB	; Split the low and high OWGs
						; 7674		MEM_AR,SET FPD,FE_S,		;A simple OWL. Save S and unwind
U 0215, 1154,2341,0003,2411,0002,1176,0100	; 7675			EA MOD DISP,CALL [LDEA]	; byte pointer EA
						; 7676		GEN AR,EXT BYTE READ,SC_FE#,	;An OWG (bit 12 is irrelevant)
U 0216, 0733,3701,0000,0002,1131,0007,0620	; 7677			AR0-3 DISP,J/OWGLDB	; Split the low and high OWGs
U 0217, 0234,4001,0003,0000,0002,7510,0000	; 7678		MEM_AR,SKP -VMA SEC0		;A TWG, maybe. Not in section 0
						; 7679	=11100	FE_S,SET FPD,
U 0234, 1154,2301,0000,2411,0000,1176,0100	; 7680			EA MOD DISP,CALL [LDEA]	;No TWGs in section 0 (treat as OWL)
U 0235, 0236,4001,0000,2411,0011,3610,0610	; 7681		FE_S,READ BP2			;Real TWG. Treat as global indirect
U 0236, 1130,4001,0000,0000,0000,1150,0100	; 7682		SET FPD,CALL [LEAIND]
						; 7683	=11111	FIN LOAD,SC_#-SC,#/-1,		;Wait for byte word. SC = 36-(P+S); KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1-1
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  ILDB, LDB			

U 0237, 2602,3240,0003,5302,0237,4710,0777	; 7684			SKP SC0,I FETCH		; Does byte go off the top?
						; 7685	=
						; 7686	=0	CLR FPD,ARX_AR,AR_0S,SC_FE+SC,	;Yes. Byte is at top of word; try
U 2602, 0064,3441,2400,2002,1020,5114,0000	; 7687			SKP SCAD0,J/SHFLOD	; to truncate it. Test if P off top
						; 7688	OWGLOD:	CLR FPD,ARX_SHIFT,AR_0S,	;Normal byte. Put at top of ARX;
U 2603, 0064,3441,2400,0002,0000,0014,0000	; 7689			SC_FE#,J/SHFLOD		; SC = S. Set for final shift
						; 7690	;
						; 7691	;	Load byte from an OWG.  Split P&S in range 45-57 octal (in which
						; 7692	;	case we optimize for a byte size of 6) or 60-77 (optimize for size
						; 7693	;	7.)  (Unfortunately, we can't reasonably optimize for size 8 bytes,
						; 7694	;	as they run from 54 to 60, thus including both ranges.)  The idea
						; 7695	;	here is to set up the shift counts that will be required for the
						; 7696	;	actual byte.  Thus, SC_36.-(P+S) and FE_S.
						; 7697	;
						; 7698	=1011
U 0733, 0240,4001,0000,0301,0020,0007,0006	; 7699	OWGLDB:	FE_#,#/6,SH DISP,J/OWGLOW	;Range is 45-57. Assume size 6
U 0737, 0260,4001,0000,0301,0020,0007,0007	; 7700		FE_#,#/7,SH DISP,J/OWGHIG	;Range is 60-77. Assume size 7
						; 7701	;
						; 7702	=00000
						; 7703	OWGLOW:					;Dummy label (40-44 are OWLs)
U 0245, 0701,3200,0003,0000,0237,0014,0000	; 7704	=00101	FIN LOAD,I FETCH,CLR FPD,J/SETZ	;45 S=6, P=36 (bad). Clear the AC
U 0246, 2603,3200,0003,0402,0237,0010,0000	; 7705		FIN LOAD,I FETCH,CLR SC,J/OWGLOD;46 S=6, P=30
U 0247, 2603,3240,0003,0302,0237,0010,0006	; 7706		FIN LOAD,I FETCH,SC_#,#/6,J/OWGLOD;47 S=6, P=24
U 0250, 2603,3240,0003,0302,0237,0010,0014	; 7707		FIN LOAD,I FETCH,SC_#,#/12.,J/OWGLOD;50 S=6, P=18
U 0251, 2603,3240,0003,0302,0237,0010,0022	; 7708		FIN LOAD,I FETCH,SC_#,#/18.,J/OWGLOD;51 S=6, P=12
U 0252, 2603,3240,0003,0302,0237,0010,0030	; 7709		FIN LOAD,I FETCH,SC_#,#/24.,J/OWGLOD;52 S=6, P=6
U 0253, 2603,3240,0003,0302,0237,0010,0036	; 7710		FIN LOAD,I FETCH,SC_#,#/30.,J/OWGLOD;53 S=6, P=0
						; 7711	
U 0254, 0701,3200,0003,0000,0237,0014,0000	; 7712		FIN LOAD,I FETCH,CLR FPD,J/SETZ	;54 S=8, P=36 (bad). Treat as SETZ
U 0255, 3330,4001,0000,0402,0000,0010,0000	; 7713		CLR SC,J/SIZE8L			;55 S=8, P=28. Correct the size
U 0256, 3330,4001,0000,0302,0000,0010,0010	; 7714		SC_#,#/8,J/SIZE8L		;56 S=8, P=20
U 0257, 3330,4001,0000,0302,0000,0010,0020	; 7715		SC_#,#/16.,J/SIZE8L		;57 S=8, P=12
U 0260, 3330,0001,0000,0302,0000,0010,0030	; 7716	OWGHIG:	SC_#,#/24.,J/SIZE8L		;60 S=8, P=4
						; 7717	
U 0261, 0701,3200,0003,0000,0237,0014,0000	; 7718		FIN LOAD,I FETCH,CLR FPD,J/SETZ	;61 S=7, P=36 (bad). Treat as SETZ
U 0262, 2603,3200,0003,0402,0237,0010,0000	; 7719		FIN LOAD,I FETCH,CLR SC,J/OWGLOD;62 S=7, P=29. Ready to shift
U 0263, 2603,3200,0003,0302,0237,0010,0007	; 7720		FIN LOAD,I FETCH,SC_#,#/7,J/OWGLOD;63 S=7, P=22
U 0264, 2603,3200,0003,0302,0237,0010,0016	; 7721		FIN LOAD,I FETCH,SC_#,#/14.,J/OWGLOD;64 S=7, P=15
U 0265, 2603,3200,0003,0302,0237,0010,0025	; 7722		FIN LOAD,I FETCH,SC_#,#/21.,J/OWGLOD;65 S=7, P=8
U 0266, 2603,3200,0003,0302,0237,0010,0034	; 7723		FIN LOAD,I FETCH,SC_#,#/28.,J/OWGLOD;66 S=7, P=1
						; 7724	
U 0267, 0701,3200,0003,0000,0237,0014,0000	; 7725		FIN LOAD,I FETCH,CLR FPD,J/SETZ	;67 S=9, P=36 (bad). Clear the AC
U 0270, 3331,0001,0000,0402,0000,0010,0000	; 7726		CLR SC,J/SIZE9L			;70 S=9, P=27. Correct size
U 0271, 3331,4001,0000,0302,0000,0010,0011	; 7727		SC_#,#/9,J/SIZE9L		;71 S=9, P=18
U 0272, 3331,4001,0000,0302,0000,0010,0022	; 7728		SC_#,#/18.,J/SIZE9L		;72 S=9, P=9
U 0273, 3331,4001,0000,0302,0000,0010,0033	; 7729		SC_#,#/27.,J/SIZE9L		;73 S=9, P=0
						; 7730	
U 0274, 0701,3200,0003,0000,0237,0014,0000	; 7731		FIN LOAD,I FETCH,CLR FPD,J/SETZ	;74 S=18, P=36 (bad). Clear AC
U 0275, 0514,3200,0003,0000,0237,0014,0000	; 7732		FIN LOAD,I FETCH,CLR FPD,J/HLRZ	;75 S=18, P=18. This is HLRZ, folks
U 0276, 0414,3240,0003,0000,0237,0014,0000	; 7733		FIN LOAD,I FETCH,CLR FPD,J/HRRZ	;76 S=18, P=0. Same as HRRZ
						; 7734	
U 0277, 1137,3200,0003,0000,0022,0010,0000	; 7735		AR_MEM,J/ILLOWG			;77 Illegal. Force UUO
						; 7736	;
U 3330, 2603,3200,0003,0301,0237,0010,0010	; 7737	SIZE8L:	FIN LOAD,I FETCH,FE_#,#/8,J/OWGLOD;Fix up all size 8 bytes
U 3331, 2603,3240,0003,0301,0237,0010,0011	; 7738	SIZE9L:	FIN LOAD,I FETCH,FE_#,#/9,J/OWGLOD;Do the same for size 9
						; 7739	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  DPB, IDPB			

						; 7740		.TOC	"Single Byte Instructions:  DPB, IDPB"
						; 7741	;
						; 7742	;	IDPB--Increment a byte pointer (in memory), then store the rightmost
						; 7743	;	bits of the AC into the byte it specifies.
						; 7744	;	DPB--Store the rightmost bits of the AC into byte specified by
						; 7745	;	pointer.
						; 7746	;	The constraints immediately below are necessary to make IBP
						; 7747	;	work.
						; 7748	;
						; 7749		.DCODE
D 0136, 6601,0300				; 7750	136:	RW,	M,	J/IDPB
D 0137, 4601,0304				; 7751		R,	M,	J/DPB		;No write test if no increment
						; 7752		.UCODE
						; 7753	
						; 7754	=0****000000
						; 7755	IDPB:	ARX_AR,SC_P-#,#/37.,BYTE DISP,	;Save for dispatch later, test
U 0300, 1642,4001,0400,5132,1020,0074,0045	; 7756			CALL [INCRBP]		; for OWG, increment pointer
						; 7757	=100
						; 7758	DPB:	MEM_AR,ARX_AR,SC_P-#,#/37.,	;Await possible pointer store
U 0304, 0314,0001,0403,5132,1022,0034,0045	; 7759			BYTE DISP		; Save P; test OWL, OWG, TWG
						; 7760	=1100	GEN AR,EXT BYTE RPW,SC_FE#,	;An OWG. Start byte read; SC_2
U 0314, 0753,3701,0000,0002,1131,0007,0760	; 7761			AR0-3 DISP,J/OWGDPB	; Split into OWG groups
						; 7762		MEM_AR,SET FPD,FE_-S,		;An OWL. Save byte size
U 0315, 1234,2341,0003,5411,0002,1136,0100	; 7763			EA MOD DISP,J/DPEA	; and compute byte word address
						; 7764		GEN AR,EXT BYTE RPW,SC_FE#,	;An OWG. See above
U 0316, 0753,3701,0000,0002,1131,0007,0760	; 7765			AR0-3 DISP,J/OWGDPB	; (Bit 12 is irrelevant here)
U 0317, 2604,4001,0003,0000,0002,7510,0000	; 7766		MEM_AR,SKP -VMA SEC0,J/DEPTWG	;Maybe a TWG. Never in section 0
						; 7767	=11110
U 0336, 3333,3240,0010,1401,0020,0713,0003	; 7768	GUDSIZ:	FE_-SC-1,SC_FE,MQ_FM[AC0],J/DEPBYT;Copy byte to MQ; FE_36-P, SC_-S
U 0337, 0336,0001,0000,2301,0000,0010,0001	; 7769		FE_#+SC,#/1,J/GUDSIZ		;Size too large. Force to 36-P
						; 7770	=
						; 7771	=0
						; 7772	DEPTWG:	MEM_AR,SET FPD,FE_-S,		;No TWGs allowed in section 0
U 2604, 1234,2341,0003,5411,0002,1136,0100	; 7773			EA MOD DISP,J/DPEA
U 2605, 3332,0001,0000,5411,0011,3610,0610	; 7774		FE_-S,READ BP2			;A TWG. Start reading second word
U 3332, 1140,4001,0000,0000,0000,1110,0100	; 7775		SET FPD,J/DEAIND		;And dive into indirection loop
						; 7776	;
						; 7777	;	At this point, we have FE = 36-P and SC = -S with memory being
						; 7778	;	loaded into both AR and ARX.  Also, both S and P have been forced
						; 7779	;	into the range 0:36.  The deposit is done with three shifts:
						; 7780	;
						; 7781	;	Shift 1:  AR and ARX have memory; shift count = 36-P
						; 7782	;	Shift 2:  AR has byte to deposit, ARX has previous shift;
						; 7783	;		  shift count = 36-S
						; 7784	;	Shift 3:  AR and ARX have previous shift; shift count = P+S
						; 7785	;
						; 7786	DEPBYT:	AR_MEM,ARX_MEM,TIME/3T,		;Wait for memory load
U 3333, 3334,3200,0003,2301,0022,0013,0044	; 7787			SC_FE,FE_#+SC,#/36.	;SC_36-P, FE_36-S
						; 7788	DEPOWG:	AR_MQ,ARX_SHIFT,		;Fetch byte, do first shift
U 3334, 3335,3721,2400,5301,0000,0013,0110	; 7789			SC_FE,FE_#-SC,#/72.	;SC_36-S, FE_72-(36-P) = 36+P
U 3335, 3336,0001,4400,5002,0000,0010,0000	; 7790		AR_SHIFT,ARX_SHIFT,SC_FE-SC	;Next shift; SC_(36+P)-(36-S) = P+S
						; 7791	RELMEM: AR_SHIFT,STORE,CLR FPD,
U 3336, 0066,4001,4000,0000,0016,1614,0000	; 7792			SR_0,J/STMEM		;Last shift; store and clear FPD
						; 7793	;
						; 7794	;	Deposit byte with an OWG.  Once again, P&S gets split into the
						; 7795	;	ranges 45-57 octal (optimized for size 6) and 60-77 (optimized; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-1
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  DPB, IDPB			

						; 7796	;	for size 7).  In addition to setting SC to 36-P and FE to 36-S,
						; 7797	;	this code also copies AC to MQ.  Since MQ_FM[] uses the # field,
						; 7798	;	this is accomplished by reading the AC into ARX and then copying
						; 7799	;	it to the MQ just before the cache can step on ARX with the
						; 7800	;	byte data.  The timing for this is a tad hairy, but it seems to
						; 7801	;	work.
						; 7802	;
						; 7803	=1011
						; 7804	OWGDPB:	ARX_FM[AC0],FE_#,#/30.,SH DISP,	;Low range OWG. Assume size 6
U 0753, 0340,3240,0200,0301,0020,0007,0036	; 7805			TIME/3T,J/ODLOW		;Fetch byte to store
						; 7806		ARX_FM[AC0],FE_#,#/29.,SH DISP,	;High range. Assume size 7
U 0757, 0360,3200,0200,0301,0020,0007,0035	; 7807			TIME/3T,J/ODHIGH
						; 7808	;
						; 7809	=00000
						; 7810	ODLOW:					;Another dummy (40-44 are OWLs)
U 0345, 3336,3240,0003,0402,0022,0010,0000	; 7811	=00101	AR_MEM,CLR SC,J/RELMEM		;45 S=6, P=36 (bad). Release memory
U 0346, 3334,3240,0013,0302,2022,0010,0006	; 7812		MQ_ARX,AR_MEM,SC_#,#/6,J/DEPOWG	;46 S=6, P=30. Copy byte to MQ
U 0347, 3334,3240,0013,0302,2022,0010,0014	; 7813		MQ_ARX,AR_MEM,SC_#,#/12.,J/DEPOWG;47 S=6, P=24
U 0350, 3334,3240,0013,0302,2022,0010,0022	; 7814		MQ_ARX,AR_MEM,SC_#,#/18.,J/DEPOWG;50 S=6, P=18
U 0351, 3334,3240,0013,0302,2022,0010,0030	; 7815		MQ_ARX,AR_MEM,SC_#,#/24.,J/DEPOWG;51 S=6, P=12
U 0352, 3334,3240,0013,0302,2022,0010,0036	; 7816		MQ_ARX,AR_MEM,SC_#,#/30.,J/DEPOWG;52 S=6, P=6
U 0353, 3334,3240,0013,0302,2022,0010,0044	; 7817		MQ_ARX,AR_MEM,SC_#,#/36.,J/DEPOWG;53 S=6, P=0
						; 7818	
U 0354, 3336,3240,0003,0402,0022,0010,0000	; 7819		AR_MEM,CLR SC,J/RELMEM		;54 S=8, P=36. Just release memory
U 0355, 3337,0001,0010,0302,2000,0010,0010	; 7820		MQ_ARX,SC_#,#/8,J/SIZE8D	;55 S=8, P=28. Copy byte, fix size
U 0356, 3337,0001,0010,0302,2000,0010,0020	; 7821		MQ_ARX,SC_#,#/16.,J/SIZE8D	;56 S=8, P=20
U 0357, 3337,4001,0010,0302,2000,0010,0030	; 7822		MQ_ARX,SC_#,#/24.,J/SIZE8D	;57 S=8, P=12
U 0360, 3337,0001,0010,0302,2000,0010,0040	; 7823	ODHIGH:	MQ_ARX,SC_#,#/32.,J/SIZE8D	;60 S=8, P=4
						; 7824	
U 0361, 3336,3240,0003,0402,0022,0010,0000	; 7825		AR_MEM,CLR SC,J/RELMEM		;61 S=7, P=36 (bad). Release memory
U 0362, 3334,3200,0013,0302,2022,0010,0007	; 7826		MQ_ARX,AR_MEM,SC_#,#/7,J/DEPOWG	;62 S=7, P=29. Copy byte to MQ
U 0363, 3334,3200,0013,0302,2022,0010,0016	; 7827		MQ_ARX,AR_MEM,SC_#,#/14.,J/DEPOWG;63 S=7, P=22
U 0364, 3334,3200,0013,0302,2022,0010,0025	; 7828		MQ_ARX,AR_MEM,SC_#,#/21.,J/DEPOWG;64 S=7, P=15
U 0365, 3334,3200,0013,0302,2022,0010,0034	; 7829		MQ_ARX,AR_MEM,SC_#,#/28.,J/DEPOWG;65 S=7, P=8
U 0366, 3334,3200,0013,0302,2022,0010,0043	; 7830		MQ_ARX,AR_MEM,SC_#,#/35.,J/DEPOWG;66 S=7, P=1
						; 7831	
U 0367, 3336,3240,0003,0402,0022,0010,0000	; 7832		AR_MEM,CLR SC,J/RELMEM		;67 S=9, P=36, no good. Let go!
U 0370, 3340,4001,0010,0302,2000,0010,0011	; 7833		MQ_ARX,SC_#,#/9,J/SIZE9D	;70 S=9, P=27. Copy byte, fix size
U 0371, 3340,4001,0010,0302,2000,0010,0022	; 7834		MQ_ARX,SC_#,#/18.,J/SIZE9D	;71 S=9, P=18
U 0372, 3340,4001,0010,0302,2000,0010,0033	; 7835		MQ_ARX,SC_#,#/27.,J/SIZE9D	;72 S=9, P=9
U 0373, 3340,4001,0010,0302,2000,0010,0044	; 7836		MQ_ARX,SC_#,#/36.,J/SIZE9D	;73 S=9, P=0
						; 7837	
U 0374, 3336,3240,0003,0402,0022,0010,0000	; 7838		AR_MEM,CLR SC,J/RELMEM		;74 S=18, P=36. Just unpause memory
U 0375, 0114,3240,0003,0000,0022,0014,0000	; 7839		AR_MEM,CLR FPD,J/HRLM		;75 S=18, P=18. Treat as HRLM
U 0376, 0106,3240,0003,0000,0022,0014,0000	; 7840		AR_MEM,CLR FPD,J/HLL		;76 S=18, P=0. Treat as HRRM
						; 7841	
U 0377, 1137,3200,0003,0000,0036,0010,0000	; 7842		FIN LOAD,STORE,J/ILLOWG		;77 Illegal byte pointer. UUO it
						; 7843	;
U 3337, 3334,3200,0003,0301,0022,0010,0034	; 7844	SIZE8D:	AR_MEM,FE_#,#/28.,J/DEPOWG	;Fix FE for size 8 bytes
U 3340, 3334,3240,0003,0301,0022,0010,0033	; 7845	SIZE9D:	AR_MEM,FE_#,#/27.,J/DEPOWG	;Same for size 9
						; 7846	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  IBP, ADJBP			

						; 7847		.TOC	"Single Byte Instructions:  IBP, ADJBP"
						; 7848	;
						; 7849	;	IBP--Increment a byte pointer (in memory).
						; 7850	;	ADJBP--Adjust a one or two word byte pointer from memory by an
						; 7851	;	amount specified by the (non zero) AC.
						; 7852	;	Both of these instructions key off of the same op code (133);
						; 7853	;	they are distinguished by ADJBP having a non zero AC field.
						; 7854	;
						; 7855	;	The IBP case is rather simple.
						; 7856	;
						; 7857		.DCODE
D 0133, 4001,1503				; 7858	133:	R,		J/IBP		;IBP and ADJBP--must adjoin FSC
						; 7859		.UCODE
						; 7860	
						; 7861	1503:					;[345] In same block of 8 as FSC
U 1503, 0032,3401,0200,5132,0000,4610,0045	; 7862	IBP:	SC_P-#,#/37.,ARX_0S,SKP AC EQ 0	;[407] Test for OWG. IBP or ADJBP?
						; 7863	=11010
U 0032, 2606,4001,0010,5412,2000,4710,0000	; 7864	IBPTST:	SC_-S,MQ_ARX,SKP SC0,J/ADJBP	;[407] ADJBP. Clear MQ0. OWG?
U 0033, 1642,4001,0000,0000,0000,4750,0000	; 7865		SKP SC0,CALL [INCRBP]		;IBP. Test for OWG and do it
U 0037, 0073,0001,0003,0000,0217,0014,0000	; 7866	=11111	FIN STORE,CLR FPD,I FETCH,J/NOP	;Tidy up and leave
						; 7867	;
						; 7868	;	ADJBP is handled separately for OWGs and OWL/TWGs.  We consider
						; 7869	;	the latter case first.
						; 7870	;	Step 1:  figure out the byte capacity of a word.  This is broken
						; 7871	;	into the capacity to the left of the current byte (including the
						; 7872	;	byte itself) and the capacity to the right of the byte.  If these
						; 7873	;	add up to zero, then the byte can't fit in a word, and we return
						; 7874	;	to the user with no divide set.  If the byte size is zero, we
						; 7875	;	return with the pointer as is.
						; 7876	;	For this version, we compute the capacities by using repeated
						; 7877	;	subtraction.  Since the numbers involved are typically no greater
						; 7878	;	than five or six (and are never bigger than 36) this will be faster
						; 7879	;	than division.
						; 7880	=0
U 2606, 1050,4041,0500,0101,1020,0007,0000	; 7881	ADJBP:	FE_P,ARX_2+MQ0,AR0-3 DISP,J/ADJOWG;[407] OWG. Split on range
U 2607, 2610,3401,0200,0103,0000,4710,0000	; 7882		FE_P,SC/SCAD,ARX_0S,SKP SC0	;OWL/TWG. Is the size zero?
U 2610, 2624,4001,0000,0000,0000,7510,0000	; 7883	=0	SKP -VMA SEC0,J/OWLCPY		;Yes. Test for possible TWG
U 2611, 2612,0001,0010,5011,2020,5110,0000	; 7884		MQ_ARX,FE_FE-S,SKP SCAD0	;No. Clear MQ. Bytes to the right?
						; 7885	=0
						; 7886	CAPLOW:	ARX_ARX+1,FE_FE-S,SKP SCAD0,	;Yes. Count the byte and look
U 2612, 2612,4021,0600,5011,0020,5110,0000	; 7887			J/CAPLOW		; for another one
						; 7888		BR/AR,BRX/ARX,ARX_-2+MQ0,	;No more. Save count and pointer
U 2613, 2614,2301,0560,5300,3000,0110,0044	; 7889			P_#-SC,#/36.		; and set up next count
						; 7890	=0
						; 7891	CAPHGH:	P_P-S,ARX_ARX+1,SKP SCAD0,	;Count possible byte on left,
U 2614, 2614,4021,0600,5110,3021,5110,0200	; 7892			J/CAPHGH		; saving alignment info
U 2615, 3341,4602,6007,0000,0020,1010,0166	; 7893		T0_AR,AR_ARX+BRX+1		;All counted. Get total capacity
U 3341, 2616,3703,0000,0000,0020,5610,0000	; 7894		SKP AR NZ			;Will any bytes fit into word?
U 2616, 0413,0001,0000,0000,0217,0010,0000	; 7895	=0	I FETCH,J/NODIVD		;No. This is pretty silly
						; 7896	;
						; 7897	;	Step 2:  generate a modified adjustment count and compute the
						; 7898	;	number of words to move and the relative byte position within
						; 7899	;	the word.  All adjustments are done relative to the first byte
						; 7900	;	in the word, so that the resulting quotient is the actual
						; 7901	;	number of words to add to the base address.  If the adjustment
						; 7902	;	is negative, however, we must back up the quotient by one and; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3-1
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  IBP, ADJBP			

						; 7903	;	offset the remainder by the capacity if it is non zero.
						; 7904	;
						; 7905	;	In order to speed up the division, the absolute value of the
						; 7906	;	modified adjustment is broken into ranges of up to 63, 64 to
						; 7907	;	2**18-1, and 2**18 or greater.  This lets us use step counts of
						; 7908	;	7, 19, and 36, respectively, saving a lot of time for the most
						; 7909	;	common cases.
						; 7910	;
						; 7911	;	For this portion of the work, OWGs and OWLs are identical.
						; 7912	;
U 2617, 3342,0610,0200,0302,0020,0010,0036	; 7913	ADJOIN:	ARX_ARX+FM[AC0],SC_#,#/30.	;Compute modified adjustment
						; 7914		T1_AR,BR/AR,AR_ARX,BRX/ARX,	;Divisor is capacity. Is modified
U 3342, 0767,3401,4267,0000,2000,1032,0171	; 7915			ARX_0S,SIGNS DISP,TIME/2T; adjustment negative?
						; 7916	=0111	AC0_AR,BRX/ARX,ARX_AR (AD),	;No. Clear BRX; use adjustment as
						; 7917			ARL_ARL.S,ARR+MQ_0.S,	; dividend, and look at high order
U 0767, 3343,3703,0220,0000,0000,1022,0110	; 7918			J/POSADJ		; half of dividend for speedup
						; 7919		AC0_AR,BRX/ARX,ARX_-BRX,	;Yes. Negate adjustment for both
U 0777, 3343,5142,0620,0000,0020,1022,0116	; 7920			ARL/ADX,ARR+MQ_0.S	; dividend and test
						; 7921	POSADJ:	AR_ARX (ADX),ARX_SHIFT,		;Generate high order 30 bits of
U 3343, 2620,3703,6400,0301,0020,5610,0044	; 7922			FE_#,#/36.,SKP AR NZ	; dividend. Are high 18 bits zero?
						; 7923	=0	ARX_SHIFT,SC_FE,		;Yes. Align low six bits to top of
U 2620, 1562,3713,0400,0000,0020,5613,0000	; 7924			SKP ARX NZ,J/SMALDV	; word. Is that enough?
U 2621, 1560,0301,0200,0301,0020,0413,0041	; 7925		ARX_AR*2,CLR AR,FE_#,#/33.,SC_FE;Need long division. Align dividend
						; 7926	=000
						; 7927	ADJDIV:	DIVIDE,AR_2(AR-BR),ARX/ADX*2,	;Do first divide step
U 1560, 0462,5102,5504,3001,0020,0071,0000	; 7928			CALL [DIVS3]		; and call subroutine for the rest
						; 7929	=010
U 1562, 0454,3401,2000,0301,0000,0050,0004	; 7930	SMALDV:	AR_0S,FE_#,#/4,CALL [DIVS1]	;Very short division is adequate
						; 7931		ARX_AR SWAP,AR_0S,FE_#,#/16.,	;Medium size needed. Put significant
U 1563, 1560,3441,2400,0301,3000,0010,0020	; 7932			J/ADJDIV		; dividend bits in proper spot
						; 7933	;
						; 7934	;	Return from division is either 6 (negative dividend) or 7
						; 7935	;	(non negative dividend).  We tuck the negative offset code in
						; 7936	;	at 4 and 5 for convenience.
						; 7937	;
U 1564, 1567,5142,0600,0000,0020,0010,0000	; 7938	NEGADJ:	ARX_-BRX,J/ADJUST		;Zero remainder. Negate quotient
U 1565, 1567,0600,2007,0000,0020,0010,0171	; 7939		AR_AR+FM[T1],J/ADJUST		;Non zero. Offset by capacity
						; 7940	;
						; 7941	;	On exit from division, AR has the signed remainder and ARX and
						; 7942	;	BRX have the positive quotient.  If the dividend was negative,
						; 7943	;	we must either negate the quotient or negate and subtract one
						; 7944	;	(thus one's complementing it) depending upon whether there was
						; 7945	;	a non zero remainder.
						; 7946	;
U 1566, 1564,2542,0600,0000,0000,4510,0000	; 7947		ARX_BRX COMP,SKP AR0,J/NEGADJ	;Negative dividend. Complement
						; 7948						; quotient and test remainder
						; 7949	;
						; 7950	;	Step 3:  add the final quotient to the address, and offset the
						; 7951	;	byte into the word by the adjusted remainder.  To do this, we
						; 7952	;	must finally differentiate an OWL from a TWG.  (Recall that we
						; 7953	;	saved most of the original byte pointer (including bit 12) in
						; 7954	;	T0 before we did the division.)  In any event, for an OWL we
						; 7955	;	add the quotient to the right half of the byte pointer; for a
						; 7956	;	TWG we fetch the second word and then add the quotient to bits
						; 7957	;	6-35 if it's global, to bits 18-35 if it's local.
						; 7958	;; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3-2
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  IBP, ADJBP			

						; 7959	;	After this, we subtract the byte pointer S field from (36 - the
						; 7960	;	alignment information left in the P field) precisely remainder
						; 7961	;	times (recall that division copied SC, preloaded with 36, into
						; 7962	;	FE when it finished).  That's about it.
						; 7963	;
						; 7964	;	OWGs split off their separate way.
						; 7965	;
U 1567, 1036,3240,2017,0000,1020,0005,0166	; 7966	ADJUST:	MQ_AR,AR_T0,SR DISP		;Remainder to MQ. OWG or OWL/TWG?
						; 7967	=1110	BR/AR,SC_P+S,MQ_MQ-1,		;OWL/TWG. Copy pointer, add first
U 1036, 1165,1721,0050,2112,0020,0734,0003	; 7968			BYTE DISP,J/ADJTWG	; S, generate count. Perhaps TWG?
						; 7969		FE_P+1,BR/AR,AR_MQ,		;An OWG. Grab initial P&S and
U 1037, 3350,3723,2040,4101,0217,0010,0000	; 7970			I FETCH,J/SNATCH	; set up quotient addition
						; 7971	;
						; 7972	=101
						; 7973	ADJTWG:	FE_FE-SC,ARL_ARL,ARR_ARX+BR,	;OWL. Adjust address; initialize P
U 1165, 1624,0612,2300,5001,0020,0610,0000	; 7974			ARX/MQ,J/ADJP
						; 7975		FE_FE-SC,AR_ARX,ARX_AR (AD),	;Perhaps TWG. Move quotient to AR
U 1167, 1030,3701,4200,5001,2000,7510,0000	; 7976			SKP -VMA SEC0		; No TWGs allowed in section 0
						; 7977	=00	ARL_ARXL,ARR_AR+BR,		;Section 0. An OWL in TWG's clothing
U 1030, 1624,0602,2304,0000,2020,0022,0004	; 7978			ARX/MQ,J/ADJP
						; 7979		BR/AR,BRX/ARX,VMA_VMA+1,LOAD AR,;A real TWG. Keep quotient and
U 1031, 0770,0001,0060,0000,0012,3650,0000	; 7980			CALL [XFERW]		; remainder while fetching address
U 1033, 2622,0602,2404,0102,1020,4510,0000	; 7981	=11	SC_P,AR_AR+BR,ARX_AR,SKP AR0	;Assume global address. Is it?
U 2622, 3344,4001,0000,2400,3001,0010,0200	; 7982	=0	P_SC,J/TWGDUN			;Yes. Use 30 bit addition
U 2623, 3344,0612,2004,0000,2020,0022,0004	; 7983		ARL_ARXL,ARR_ARX+BR		;No. Foolish, but 18 bits is enough
U 3344, 1624,3242,6301,0000,0000,1010,0000	; 7984	TWGDUN:	AC1_AR,AR_BRX,ARX/MQ		;Store address; restore first word
						; 7985	;
						; 7986	;	Address has been adjusted.  Adjust P by remainder bytes.
						; 7987	;
						; 7988	=100
						; 7989	ADJP:	FE_FE-S,P_SCAD,ARX_ARX-1,	;Step to next byte and count
U 1624, 1624,1701,0600,5011,3021,4310,0200	; 7990			SKP ARX0,J/ADJP		; down the remainder
U 1625, 0072,0001,0000,0000,0217,0010,0000	; 7991	TWGCPY:	I FETCH,J/STORAC		;Adjustment done. Load AC0
						; 7992	;
						; 7993	;	If the byte size is zero, we just load AC0, or ACs 0 and 1 if it's
						; 7994	;	a TWG.
						; 7995	;
U 1627, 0120,0001,0000,0000,0013,3610,0000	; 7996	=111	VMA_VMA+1,LOAD ARX,J/DLOAD	;A TWG. Use DMOVE code to load it
						; 7997	;
						; 7998	=0
U 2624, 0072,0001,0000,0000,0217,0010,0000	; 7999	OWLCPY:	I FETCH,J/STORAC		;Section 0, an OWL. Just load AC0
U 2625, 1625,4001,0000,0000,0000,0034,0000	; 8000		BYTE DISP,TIME/2T,J/TWGCPY	;Not section 0. Test AR12 for TWG
						; 8001	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Single Byte Instructions:  IBP, ADJBP			

						; 8002	;
						; 8003	;	OWGs use the same basic algorithm as OWLs and TWGs, but the
						; 8004	;	actual implementation of steps 1 and 3 is quite different.
						; 8005	;	Step 1:  get the byte capacity of the word and current offset
						; 8006	;	of the OWG within the word.  Note that OWGs may be split into
						; 8007	;	ranges of sizes, with the capacity identical for each OWG within
						; 8008	;	a range.  The current offset within the word can be computed by
						; 8009	;	subtracting the range base + 1 from the P&S field.  The range base
						; 8010	;	is saved in the OWG for later final adjustment.  The capacity is
						; 8011	;	computed in a rather wry way:  ARX is initially loaded with the
						; 8012	;	capacity - 4; later, AR is set to -1.  When AR*4 is subtracted
						; 8013	;	from ARX, AR*4 will be -4 as long as ARX was positive.  The only
						; 8014	;	negative case is for a capacity of 2 (for 18 bit bytes); that one
						; 8015	;	is special cased.
						; 8016	;
						; 8017	=1000
						; 8018	ADJOWG:					;40:43. No OWGs here
						; 8019	=1001	ARX_2+MQ0,TIME/2T,P_#,#/45,	;44:47. Size 6: capacity 6, base 45
U 1051, 3345,4061,0500,0302,3000,0110,0045	; 8020			SC/SCAD,J/OWGCOM	;[407]
						; 8021		ARX_2+MQ0,TIME/2T,P_#,#/45,	;50:53. More size 6
U 1052, 3345,4061,0500,0302,3000,0110,0045	; 8022			SC/SCAD,J/OWGCOM	;[407]
U 1053, 3345,3441,0200,0302,3000,0110,0054	; 8023		ARX_0S,P_#,#/54,SC/SCAD,J/OWGCOM;54:57. Size 8: capacity 4, base 54
U 1054, 2630,4001,0000,5030,0020,5110,0061	; 8024		GEN FE-#,#/61,SKP SCAD0,J/EIGHT7;60:63. Either size 8 or size 7
U 1055, 2632,0001,0000,5030,0020,5110,0067	; 8025		GEN FE-#,#/67,SKP SCAD0,J/SEVEN9;64:67. Either size 7 or size 9
U 1056, 3345,3441,0200,0302,3000,0110,0067	; 8026		ARX_0S,P_#,#/67,SC/SCAD,J/OWGCOM;70:73. Size 9: capacity 4, base 67
U 1057, 2626,4001,0000,5030,0020,5110,0077	; 8027		GEN FE-#,#/77,SKP SCAD0		;74:77. Is this an illegal pointer?
U 2626, 1002,3242,2000,0000,0000,0010,0000	; 8028	=0	AR_BR,J/UUO			;77 is no good. UUO it
						; 8029		BRX/ARX,ARX_1S,P_#,#/74,SC/SCAD,;74:76. Size 18: capacity 2, base 74
U 2627, 3345,2341,0220,0302,3000,0110,0074	; 8030			J/OWGCOM		; Save size; force ARX negative
						; 8031	;
						; 8032	=0
						; 8033	EIGHT7:	ARX_1,TIME/2T,P_#,#/61,SC/SCAD,	;61:63. Size 7: capacity 5, base 61
U 2630, 3345,4041,0200,0302,3000,0110,0061	; 8034			J/OWGCOM
U 2631, 3345,3441,0200,0302,3000,0110,0054	; 8035		ARX_0S,P_#,#/54,SC/SCAD,J/OWGCOM;60 is the last size 8 byte
						; 8036	;
						; 8037	=0
U 2632, 3345,3441,0200,0302,3000,0110,0067	; 8038	SEVEN9:	ARX_0S,P_#,#/67,SC/SCAD,J/OWGCOM;67 is the first size 9 byte
U 2633, 3345,4041,0200,0302,3000,0110,0061	; 8039		ARX_1,TIME/2T,P_#,#/61,SC/SCAD	;64:66 are the last size 7 bytes
U 3345, 3346,2341,2007,1001,0000,1010,0166	; 8040	OWGCOM:	T0_AR,AR_1S,FE_FE-SC-1		;Save pointer; find initial offset
U 3346, 2634,5113,0204,0000,3021,4310,0200	; 8041		P_FE,ARX_ARX-AR*4,SKP ARX0	;Try to get capacity. Is it 2?
						; 8042	=0	BRX/ARX,ARX_AR,AR_SIGN,		;No. Save it; set up offset and
U 2634, 3347,5401,2420,0302,1020,0016,0006	; 8043			SC_#,#/6,J/OFSHFT	; shift count for offset generation
U 2635, 3347,5441,2400,0302,1020,0016,0006	; 8044		ARX_AR,AR_SIGN,SC_#,#/6		;Yes. Size was loaded above
U 3347, 2617,3202,6400,0000,0000,1610,0001	; 8045	OFSHFT:	AR_BRX,ARX_SHIFT,SR_1,J/ADJOIN	;Mark OWG and rejoin for step 2
						; 8046	;
						; 8047	;	Step 3: add the final quotient to the address, and offset the OWG
						; 8048	;	into the word by remainder bytes.  Since this becomes a simple
						; 8049	;	integer add, this portion is rather trivial.
						; 8050	;
U 3350, 3351,0612,2004,0002,0020,1613,0000	; 8051	SNATCH:	SC_EA,AR_ARX+BR,SR_0		;Grab offset; adjust address
U 3351, 0065,4001,0000,2000,3000,0610,0200	; 8052		P_FE+SC,J/STAC			;Add proper offset to P&S. Done!
						; 8053	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Subroutines for Single Byte Instructions		

						; 8054	.TOC	"Subroutines for Single Byte Instructions"
						; 8055	;
						; 8056	;	INCRBP--Subroutine to increment a byte pointer.  The first (or
						; 8057	;	only) word of the relevant pointer is in AR.  Call with SC_P-#,
						; 8058	;	#/37.,BYTE DISP, thus testing for OWG and first part done
						; 8059	;	simultaneously.  If FPD is set, this routine returns 4 without
						; 8060	;	doing anything; otherwise, the pointer will be incremented and
						; 8061	;	the store will have been started.  Return 4 if an OWL or TWG
						; 8062	;	must recompute SC or on any OWG, 15 if an OWL and SC is OK, and
						; 8063	;	17 if possibly a TWG with SC OK.  Note that ARX must have the
						; 8064	;	first byte pointer word on exit if this is an OWL or TWG.
						; 8065	;
						; 8066	=010					;Test FPD and OWG
U 1642, 1070,4001,0000,0002,1020,1107,0100	; 8067	INCRBP:	SC_FE#,SET FPD,AR0-3 DISP,J/OWGINC;OWG, no FPD. SC_2; test edges
U 1643, 1704,4001,0000,5110,3021,0034,0200	; 8068		P_P-S,BYTE DISP,J/BYTINC	;No OWG, no FPD. Check for overflow
U 1646, 0004,4001,0000,0000,0000,0003,0000	; 8069		RETURN4				;OWG, FPD. Forget it
U 1647, 0004,4001,0000,0000,0000,0003,0000	; 8070		RETURN4				;No OWG, FPD. No increment needed
						; 8071	;
						; 8072	;	Either OWL or TWG.  Check which; if no overflow, it doesn't really
						; 8073	;	matter.
						; 8074	;
						; 8075	=100
U 1704, 0015,4001,0000,5132,0016,0003,0045	; 8076	BYTINC:	SC_P-#,#/37.,STORE,RETURN15	;OWL, no overflow. Store and leave
						; 8077		FE_#,#/36.,GEN AR+1,		;OWL, overflow. Compute new P and
U 1705, 2636,4001,0000,0301,0000,0010,0044	; 8078			TIME/2T,J/OWLINC	; set up new address portion
U 1706, 0017,0001,0000,5132,0016,0003,0045	; 8079		SC_P-#,#/37.,STORE,RETURN17	;TWG, no overflow. Just like OWL
						; 8080		FE_#,#/36.,GEN AR+1,TIME/2T,	;TWG, overflow. Compute new P and
U 1707, 2636,4003,0000,0301,0000,7510,0044	; 8081			SKP -VMA SEC0		; test for valid TWG
						; 8082	=0
						; 8083	OWLINC:	P_FE-S,ARR_AR+1,TIME/2T,STORE,	;OWL. Increment address, set new P
U 2636, 3355,4001,2000,5010,3016,0610,0200	; 8084			J/SNARFP
U 2637, 3352,4001,0000,5010,3012,3622,0200	; 8085		P_FE-S.S,VMA_VMA+1,LOAD AR	;TWG. Set new P, fetch second word
U 3352, 3353,3240,0403,0000,1022,0010,0000	; 8086		ARX_AR,AR_MEM			;Save first word, await second
U 3353, 2640,4001,2040,0102,0020,4510,0000	; 8087		SC_P,BR/AR,SKP AR0,AR_AR+1	;Increment address, check I/EFIW
U 2640, 3354,0001,0000,2400,3016,0110,0000	; 8088	=0	P_SC#,STORE,J/STORTG		;EFIW. Do full global increment
U 2641, 3354,3242,0000,0000,0016,0610,0002	; 8089		ARL_BRL,STORE			;IFIW. Just increment right half
						; 8090	STORTG:	FIN STORE,AR_ARX,VMA_VMA-1,	;Finish second word
U 3354, 0004,0001,4003,0000,2016,3503,0000	; 8091			STORE,RETURN4		; and store first
						; 8092	;
U 3355, 0015,0001,0400,5132,1000,0003,0045	; 8093	SNARFP:	ARX_AR,SC_P-#,#/37.,RETURN15	;[351] Save offset P, new pointer
						; 8094	;
						; 8095	;	An OWG.  53, 60, 66, 73, 76, and 77 need special handling.
						; 8096	;	All others just tick the P&S field.
						; 8097	;
						; 8098	=1000
						; 8099	OWGINC:					;40:43. No OWGs here
U 1071, 0004,4001,0000,4100,3016,0103,0000	; 8100	=1001	P_P+1,STORE,RETURN4		;44:47. No special handling
						; 8101		GEN AR+1,GEN P-#,#/53,		;50:53. 53 becomes 46
U 1072, 2642,4001,0000,5130,0020,5210,0053	; 8102			SKP SCAD NE,J/OVER6
U 1073, 0004,4001,0000,4100,3016,0103,0000	; 8103		P_P+1,STORE,RETURN4		;54:57. No special handling
						; 8104		GEN AR+1,GEN P-#,#/60,		;60:63. 60 becomes 55
U 1074, 2646,4003,0000,5130,0020,5210,0060	; 8105			SKP SCAD NE,J/OVER8
						; 8106		GEN AR+1,GEN P-#,#/66,		;64:67. 66 becomes 62
U 1075, 2644,4001,0000,5130,0020,5210,0066	; 8107			SKP SCAD NE,J/OVER7
						; 8108		GEN AR+1,GEN P-#,#/73,		;70:73. 73 becomes 70
U 1076, 2650,4003,0000,5130,0020,5210,0073	; 8109			SKP SCAD NE,J/OVER9; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5-1
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Subroutines for Single Byte Instructions		

U 1077, 1134,4003,0000,4102,0020,0007,0000	; 8110		GEN AR+1,SC_P+1,SH DISP		;74:77. Test low P&S bits
U 1134, 0004,4001,0000,2400,3016,0103,0000	; 8111	=1100	P_SC#,STORE,RETURN4		;74 becomes 75. Store and leave
U 1135, 0004,4001,0000,2400,3016,0103,0000	; 8112	NXTOWG:	P_SC#,STORE,RETURN4		;75 becomes 76. Store and leave
						; 8113		AR_AR+1,TIME/2T,SC_#,#/75,	;76 becomes 75. Increment address
U 1136, 1135,4003,2000,0302,0000,0010,0075	; 8114			J/NXTOWG		; first
U 1137, 0313,0001,0003,0000,0002,0014,0000	; 8115	ILLOWG:	MEM_AR,CLR FPD,J/FPNO		;77 Illegal byte pointer. UUO it
						; 8116	;
						; 8117	=0
U 2642, 1135,4003,2000,0302,0000,0010,0046	; 8118	OVER6:	AR_AR+1,TIME/2T,SC_#,#/46,J/NXTOWG;53. Increment address first
U 2643, 0004,4001,0000,4100,3016,0103,0000	; 8119		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 8120	;
						; 8121	=0
U 2644, 1135,4003,2000,0302,0000,0010,0062	; 8122	OVER7:	AR_AR+1,TIME/2T,SC_#,#/62,J/NXTOWG;66. Increment address first
U 2645, 0004,4001,0000,4100,3016,0103,0000	; 8123		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 8124	;
						; 8125	=0
U 2646, 1135,4001,2000,0302,0000,0010,0055	; 8126	OVER8:	AR_AR+1,TIME/2T,SC_#,#/55,J/NXTOWG;60. Increment address first
U 2647, 0004,4001,0000,4100,3016,0103,0000	; 8127		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 8128	;
						; 8129	=0
U 2650, 1135,4003,2000,0302,0000,0010,0070	; 8130	OVER9:	AR_AR+1,TIME/2T,SC_#,#/70,J/NXTOWG;73. Increment address first
U 2651, 0004,4001,0000,4100,3016,0103,0000	; 8131		P_P+1,STORE,RETURN4		;Others just tick P&S
						; 8132	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Subroutines for Single Byte Instructions		

						; 8133	;
						; 8134	;	LDEA--Subroutine to compute the effective address of a byte from
						; 8135	;	a one word local byte pointer.  Called with the byte pointer in ARX
						; 8136	;	and EA MOD DISP on it.
						; 8137	;	LEAIND--Entry point for two word global EA calculation on the
						; 8138	;	second pointer word.  Called with READ BP2 on the second pointer
						; 8139	;	word.
						; 8140	;	Both entries return 37 with the byte being loaded into AR, and
						; 8141	;	with the FE added to SC.
						; 8142	;	Warning:  two of the words below (LDEA+1, LEAIND+5) cannot have
						; 8143	;	their parity generated directly by the assembler.  The SKP AR0 macro
						; 8144	;	can be used to force correct parity.  It will be ignored, since
						; 8145	;	J/37.
						; 8146	;
						; 8147	=1100
						; 8148	LDEA:	GEN AR,BYTE LOAD,		;No index, no indirect. Load byte
U 1154, 0037,3703,0000,2002,0111,0003,0420	; 8149			SC_FE+SC,RETURN37	; word
						; 8150		GEN AR+XR,INDEXED,BYTE LOAD,	;Index, no indirect. Add index
U 1155, 0037,0600,0002,2002,2131,0003,0420	; 8151			SC_FE+SC,RETURN37	; register to generate byte address
						; 8152		GEN AR,BYTE INDRCT,		;No index, indirect. Test for
U 1156, 1130,3703,0000,0000,0111,7010,0610	; 8153			SKP INTRPT,J/LEAIND	; interrupt
						; 8154		GEN AR+XR,INDEXED,BYTE INDRCT,	;[350] Index, indirect. Add index
U 1157, 1130,0600,0002,0000,2131,7010,0610	; 8155			SKP INTRPT		; register and test for interrupt
						; 8156	=00
U 1130, 3356,3240,0003,0000,0022,2550,0000	; 8157	LEAIND:	ARX_MEM,LONG EN,CALL [BYTIND]	;No interrupt. Unwind indirection
U 1131, 0144,3200,0003,0000,0022,7710,0000	; 8158		ARX_MEM,TAKE INTRPT		;Interrupted. Blow this place
U 1132, 1154,2301,0002,0000,0020,0036,0000	; 8159		XR,EA MOD DISP,TIME/3T,J/LDEA	;Local word at end. Untangle it
U 1133, 1176,2301,0002,0000,0020,0036,0000	; 8160		XR,EA MOD DISP,TIME/3T		;Global word at end. Indexed?
						; 8161	=1110	GEN ARX,GLOBAL,BYTE LOAD,	;No indexing. Read global word
U 1176, 0037,3713,0000,2002,1111,0003,0420	; 8162			SC_FE+SC,RETURN37	; and add FE to SC
						; 8163		GEN ARX+XR,GLOBAL,BYTE LOAD,	;Indexing. Add index to address
						; 8164			SC_FE+SC,RETURN37,	; and do otherwise the same
U 1177, 0037,0610,0002,2002,1131,4503,0420	; 8165			SKP AR0			; (This forces odd parity)
						; 8166	;
						; 8167	;	DPEA--Routine to compute the effective address of a byte from
						; 8168	;	a one word local byte pointer to be used in a deposit operation.
						; 8169	;	Entered with the byte pointer in ARX and EA MOD DISP on it.
						; 8170	;	DEAIND--Entry point for two word global EA calculation on the
						; 8171	;	second pointer word.  Entered with READ BP2 on the second pointer
						; 8172	;	word.
						; 8173	;	Both entries return to GUDSIZ testing the sign of FE-SC-1.
						; 8174	;	[340] This code has been desubroutinized for now, since it
						; 8175	;	must be called from an odd address.
						; 8176	;	WARNING:  two of the words below (DPEA+1, DEAIND+5) cannot
						; 8177	;	have their parity generated by the assembler.  Since the SKIP
						; 8178	;	field is already busy, we use the MQ field and set MQ/SH when
						; 8179	;	we need to generate parity.
						; 8180	;
						; 8181	=1100
						; 8182	DPEA:	GEN AR,BYTE RPW,GEN FE-SC-1,	;No index, no indirect. Load byte
U 1234, 0336,3703,0000,1000,0131,5110,0760	; 8183			SKP SCAD0,J/GUDSIZ	; word, test word underflow
						; 8184		GEN AR+XR,INDEXED,BYTE RPW,	;Index, no indirect. Add index
						; 8185			GEN FE-SC-1,SKP SCAD0,	; register, load byte, test word
U 1235, 0336,0600,0012,1000,2151,5110,0760	; 8186			MQ/SH,J/GUDSIZ		; underflow, and force odd parity
						; 8187		GEN AR,BYTE INDRCT,		;No index, indirect. Start read
U 1236, 1140,3701,0000,0000,0111,7010,0610	; 8188			SKP INTRPT,J/DEAIND	; and test for interrupt; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6-1
; BYTE.MIC[10,5351]	19:52 24-Jul-85			Subroutines for Single Byte Instructions		

						; 8189		GEN AR+XR,INDEXED,BYTE INDRCT,	;Index, indirect. Add index
U 1237, 1140,0600,0002,4000,2131,7010,0610	; 8190			SKP INTRPT		; register, read, test interrupt
						; 8191	=00
U 1140, 3356,3240,0003,0000,0022,2550,0000	; 8192	DEAIND:	ARX_MEM,LONG EN,CALL [BYTIND]	;No interrupt. Unwind indirection
U 1141, 0144,3200,0003,0000,0022,7710,0000	; 8193		ARX_MEM,TAKE INTRPT		;Interrupted. Blast out sideways
U 1142, 1234,2301,0002,0000,0020,0036,0000	; 8194		XR,EA MOD DISP,TIME/3T,J/DPEA	;Local word at end. Decode further
U 1143, 1256,2341,0002,0000,0020,0036,0000	; 8195		XR,EA MOD DISP,TIME/3T		;Global end word. Indexed?
						; 8196	=1110	GEN ARX,GLOBAL,BYTE RPW,	;No index. Read byte word
						; 8197			GEN FE-SC-1,SKP SCAD0,	; and test byte underflow
U 1256, 0336,3713,0000,1000,1131,5110,0760	; 8198			J/GUDSIZ
						; 8199		GEN ARX+XR,GLOBAL,BYTE RPW,	;Index. Add it in, read byte word,
						; 8200			GEN FE-SC-1,SKP SCAD0,	; and test byte underflow
U 1257, 0336,0610,0002,1000,1151,5110,0760	; 8201			J/GUDSIZ		;Can force odd parity here
						; 8202	;
						; 8203	;	BYTIND--Subroutine to unwind some indirection for an OWL or
						; 8204	;	a TWG.  Call with current indirect word in ARX.  Return 2 if
						; 8205	;	final word is local (possibly indirected), 3 if it is global.
						; 8206	;	Return 0 if final word is global indirect, in which case we
						; 8207	;	will dive back in again if no interrupt is pending.
						; 8208	;
U 3356, 1263,2341,4002,0000,2020,0036,0000	; 8209	BYTIND:	AR_ARX,XR,EA MOD DISP,TIME/3T	;Dispatch on global indirection
U 1263, 1336,2301,0002,0000,0020,0036,0000	; 8210	=0011	XR,EA MOD DISP,TIME/3T,J/GLBIND	;Global indirect. Test indexing
U 1267, 0003,0001,0000,0000,0000,0003,0000	; 8211		RETURN3				;Global, no indirect. Done for now
U 1273, 0170,0001,0000,0301,0000,0010,0024	; 8212		FE_#,#/24,J/PF24		;Both bits 0 and 1 set. No good
U 1277, 0002,4001,0000,0000,0000,0003,0000	; 8213		RETURN2				;Local word. Let main line handle it
						; 8214	;
						; 8215	=1110
						; 8216	GLBIND:	GEN ARX,GLOBAL,BYTE INDRCT,	;No indexing. Fetch next word in
U 1336, 0000,3711,0000,0000,1111,7003,0610	; 8217			SKP INTRPT,RETURN0	; loop, testing for interrupt
						; 8218		GEN ARX+XR,GLOBAL,BYTE INDRCT,	;Indexing. Add in index and do
U 1337, 0000,0610,0002,0000,1131,7003,0610	; 8219			SKP INTRPT,RETURN0	; similarly
						; 8220	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; BYTSUB.MIC[10,5351]	19:52 24-Jul-85			BYTE GROUP -- Some Old Style Subroutines		

						; 8221	.TOC	"BYTE GROUP -- Some Old Style Subroutines"
						; 8222	;
						; 8223	;	This file once included all of the byte instruction code.
						; 8224	;	With the coming of the new version of the byte instructions,
						; 8225	;	however, much of this stuff became unnecessary and has
						; 8226	;	been eliminated as a result.  It is hoped to be able to
						; 8227	;	eliminate more of this once we rewrite the string instructions.
						; 8228	;	[345]
						; 8229	;
						; 8230	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; BYTSUB.MIC[10,5351]	19:52 24-Jul-85			INCREMENT BYTE POINTER SUBROUTINE			

						; 8231	.TOC	"INCREMENT BYTE POINTER SUBROUTINE"
						; 8232	
						; 8233	;THIS SUBROUTINE IS CALLED BY THE INSTRUCTIONS ILDB, IDPB AS
						; 8234	;WELL AS THE MICROCODED 10/11 INTERFACE HANDLER.
						; 8235	;CALL WITH BYTE DISP TESTING FPD AND SIGN OF P-S
						; 8236	;[TIME=2+2(BP OVFLO)]
						; 8237	
						;;8238	.IFNOT/XADDR
						;;8239	=010					;BR12 IRELEVANT
						;;8240	IBPS:	STORE,RETURN4			;SIMPLE, NO OVERFLOW
						;;8241		FE_#,#/36.,GEN AR+1,TIME/2T,	;HERE IF OVRFLO OF WORD
						;;8242			ARX_AR,J/NXTWRD
						;;8243		AR_BR,RETURN4			;FPD WAS SET, RESTORE AR
						;;8244		AR_BR,RETURN4			; AND CONVERT TO LDB OR DPB
						;;8245						;TEST BR12 ONLY
						;;8246	NXTWRD:	AR_AR+1,P_FE-S,STORE,
						;;8247			TIME/2T,RETURN4
						;;8248	
						;;8249	
						;;8250	.TOC	"BYTE EFFECTIVE ADDRESS EVALUATOR - NO XADDR"
						;;8251	
						;;8252	;ENTER WITH POINTER IN AR, ARX, AND BR
						;;8253	;RETURN1 WITH (EA) LOADING INTO AR AND ARX,
						;;8254	;FPD SET, P IN SC, AND S IN FE
						;;8255	;[TIME=4+1(INDEXED)+?(INDIRECT)]
						;;8256	
						;;8257	BYTEA:	MEM_AR,FE_S,SET FPD,		;PUT AWAY UPDATED POINTER
						;;8258			EA MOD DISP		;EVAL BP ADDR
						;;8259	=1100
						;;8260	BFETCH:	GEN ARX,BYTE READ,RETURN1	;START DATA FETCH
						;;8261		GEN ARX+XR,BYTE READ,RETURN1	;ADDRESS IS INDEXED
						;;8262		GEN ARX,BYTE INDRCT,J/BYTEI	;DO INDIRECT
						;;8263		GEN ARX+XR,BYTE INDRCT,J/BYTEI	;INDIRECT INDEXED!!!
						;;8264	
						;;8265	BYTEI:	ARX_MEM,SKP INTRPT		;WAIT FOR INDIRECT WORD
						;;8266	=0	EA MOD DISP,J/BFETCH		;PROCEED IN ADDR EVAL
						;;8267		SR DISP,J/CLEAN			;INTERRUPTED, CLEAN UP AS REQ'D
						; 8268	.IF/XADDR
						; 8269	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; BYTSUB.MIC[10,5351]	19:52 24-Jul-85			INCREMENT BYTE POINTER SUBROUTINE			

						; 8270	;IBP SUBROUTINE
						; 8271	; CALL WITH BP IN AR, P_P-S, BYTE DISP
						; 8272	
						; 8273	=010
U 1722, 0004,0001,0000,0000,0016,0003,0000	; 8274	IBPS:	STORE,RETURN4			;SIMPLE CASE
						; 8275		FE_#,#/36.,GEN AR+1,TIME/2T,	;POINTER OVERFLOW, B12=0
U 1723, 2652,4003,0000,0301,0000,0010,0044	; 8276			J/NXTWRD
U 1726, 0004,3242,2000,0000,0000,0003,0000	; 8277		AR_BR,RETURN4
U 1727, 0004,3242,2000,0000,0000,0003,0000	; 8278		AR_BR,RETURN4
						; 8279	=0
						; 8280	NXTWRD:	P_FE-S,AR_AR+1,TIME/2T,		;SINGLE WORD BP
U 2652, 0004,4001,2000,5010,3016,0603,0200	; 8281			STORE,RETURN4
						; 8282	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; BYTSUB.MIC[10,5351]	19:52 24-Jul-85			BYTE EFFECTIVE ADDRESS EVALUATOR - XADDR		

						; 8283	.TOC	"BYTE EFFECTIVE ADDRESS EVALUATOR - XADDR"
						; 8284	;HERE TO EVALUATE EFFECTIVE ADDRESS OF BYTE POINTER.
						; 8285	; ENTER AT BYTEA WITH BYTE DISP (SCAD0=0), EXCEPT FOR EXTENDED
						; 8286	; INSTRUCTION SET, WHICH MUST GET SECOND PART OF POINTER FROM
						; 8287	; AC AND MUST NOT SET FPD, AND THEREFORE ENTERS AT BFETCH (FOR
						; 8288	; SINGLE-WORD POINTERS) OR BYTEI (FOR LONG POINTERS).
						; 8289	
						; 8290	=100
						; 8291	BYTEA:	MEM_AR,FE_S,SET FPD,
U 1744, 1354,2341,0003,2411,0002,1136,0100	; 8292			EA MOD DISP,J/BFETCH
U 1745, 3357,4001,0000,2411,0011,3610,0610	; 8293		READ BP2,FE_S,J/BPART2		;GET SECOND WORD
U 1746, 1744,4001,0003,0000,0002,7510,0000	; 8294		MEM_AR,SKP -VMA SEC0,J/BYTEA	;B12=1.  OBEY IF NOT SEC0
						; 8295	=
						; 8296	=0000
						; 8297	BXA:	GEN ARX,GLOBAL,BYTE INDRCT,
U 1340, 2654,3713,0000,0000,1111,7010,0610	; 8298			SKP INTRPT,J/BYTEI
						; 8299		GEN ARX+XR,GLOBAL,BYTE INDRCT,
U 1341, 2654,0610,0002,4000,1131,7010,0610	; 8300			SKP INTRPT,J/BYTEI
						; 8301		GEN ARX,GLOBAL,BYTE INDRCT,
U 1342, 2654,3713,0000,0000,1111,7010,0610	; 8302			SKP INTRPT,J/BYTEI
						; 8303		GEN ARX+XR,GLOBAL,BYTE INDRCT,
U 1343, 2654,0610,0002,4000,1131,7010,0610	; 8304			SKP INTRPT,J/BYTEI
						; 8305	
U 1344, 0001,3711,0000,0000,1111,0003,0620	; 8306		GEN ARX,GLOBAL,BYTE READ,RETURN1
U 1345, 0001,0610,0002,0000,1131,0003,0620	; 8307		GEN ARX+XR,GLOBAL,BYTE READ,RETURN1
U 1346, 0001,3711,0000,0000,1111,0003,0620	; 8308		GEN ARX,GLOBAL,BYTE READ,RETURN1
U 1347, 0001,0610,0002,0000,1131,0003,0620	; 8309		GEN ARX+XR,GLOBAL,BYTE READ,RETURN1
						; 8310	
U 1350, 0170,0001,0000,0301,0000,0010,0024	; 8311		FE_#,#/24,J/PF24		;ILLEGAL FORMAT INDIRECT WORD
U 1351, 0170,0001,0000,0301,0000,0010,0024	; 8312		FE_#,#/24,J/PF24
U 1352, 0170,0001,0000,0301,0000,0010,0024	; 8313		FE_#,#/24,J/PF24
U 1353, 0170,0001,0000,0301,0000,0010,0024	; 8314		FE_#,#/24,J/PF24
						; 8315	
						; 8316	BFETCH:
U 1354, 0001,3701,0000,0000,0111,0003,0620	; 8317		GEN AR,BYTE READ,RETURN1
U 1355, 0001,0600,0002,4000,2131,0003,0620	; 8318		GEN AR+XR,INDEXED,BYTE READ,RETURN1
						; 8319		GEN AR,BYTE INDRCT,
U 1356, 2654,3703,0000,0000,0111,7010,0610	; 8320			SKP INTRPT,J/BYTEI
						; 8321		GEN AR+XR,INDEXED,BYTE INDRCT,
U 1357, 2654,0600,0002,0000,2131,7010,0610	; 8322			SKP INTRPT,J/BYTEI
						; 8323	
U 3357, 2654,0001,0000,0000,0000,1110,0100	; 8324	BPART2:	SET FPD				;SET BEFORE FAULTING
						; 8325	=0
U 2654, 3360,3200,0003,0000,0022,2510,0000	; 8326	BYTEI:	ARX_MEM,LONG EN,J/BYTEI2
U 2655, 0144,3200,0003,0000,0022,7710,0000	; 8327		ARX_MEM,TAKE INTRPT
U 3360, 1340,2341,4002,0000,2020,0036,0000	; 8328	BYTEI2:	AR_ARX,XR,EA MOD DISP,TIME/3T,J/BXA
						; 8329	.ENDIF/XADDR
						; 8330	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; BYTSUB.MIC[10,5351]	19:52 24-Jul-85			LOAD BYTE SUBROUTINE					

						; 8331	.TOC	"LOAD BYTE SUBROUTINE"
						; 8332	;ENTER WITH S IN FE, P+S IN SC, AND AR LOAD IN PROGRESS
						; 8333	;SKP INTERRUPT AT ENTRY IS OPTIONAL
						; 8334	;RETURN2 WITH BYTE RIGHT JUSTIFIED IN AR
						; 8335	;[TIME=7]
						; 8336	=0
						; 8337	LDB1:	AR_MEM,SC_#-SC,#/36.,SKP SCAD0,	;36-(P+S)
U 2656, 2660,3200,0003,5302,0022,5110,0044	; 8338			TIME/3T,J/LDB2
U 2657, 0040,3240,0003,0000,0022,0005,0000	; 8339		AR_MEM,SR DISP,J/CLEAN		;HERE IF INTERRUPT PENDING
						; 8340	
						; 8341	=0
U 2660, 2662,3401,2400,0000,0000,0013,0000	; 8342	LDB2:	ARX_SHIFT,AR_0S,SC_FE,J/SHIFT	;BYTE IN ARX HI, READY TO SHIFT
						; 8343		ARX_AR,AR_0S,			;P+S > 36, PUT BYTE IN ARX HI
U 2661, 2662,3441,2400,2002,1020,5110,0000	; 8344			SC_FE+SC,SKP SCAD0	;ADJUST S AND SHIFT BYTE
						; 8345	
						; 8346	;PUT BYTE INTO AR RIGHT-JUSTIFIED
						; 8347	; THIS INSTRUCTION ALSO CALLED ALONE AS A SUBROUTINE
						; 8348	
						; 8349	=0
U 2662, 0002,0001,4000,0000,0000,0003,0000	; 8350	SHIFT:	AR_SHIFT,RETURN2		;RETURN WITH BYTE IN AR
U 2663, 0002,4001,0000,0000,0000,0003,0000	; 8351		RETURN2				;BYTE WAS OFF THE END, RETURN AR=0
						; 8352	
						; 8353	
						; 8354	.TOC	"DEPOSIT BYTE SUBROUTINE"
						; 8355	;ENTER WITH BYTE RIGHT JUSTIFIED IN AR, POINTER IN BR,
						; 8356	; S IN FE, 36-P IN SC, AND LOAD AR-ARX STARTED
						; 8357	; SKP IF P>36
						; 8358	;RETURN3 WITH FINAL STORE IN PROGRESS
						; 8359	;[TIME=11]
						; 8360	
						; 8361	=0
						; 8362	DPB1:	MQ_AR,AR_MEM,ARX_MEM,		;GET WORD TO ROTATE 36-P
						; 8363			GEN FE-SC-1,TIME/3T,	; [303] COMPUTE S-(36-P)-1
U 2664, 2666,3200,0013,1000,1022,5110,0000	; 8364			SKP SCAD0,J/DPB2	;CHECK THAT P+S<=36
U 2665, 0003,3200,0003,0000,0022,0003,0000	; 8365		AR_MEM,RETURN3			;[226]P>36, STORE NOTHING
						; 8366	
						; 8367	=0
U 2666, 2667,0001,0000,2401,0000,0010,0000	; 8368	DPB2:	FE_SC				;P+S>36, S_36-P
						; 8369		ARX_SHIFT,AR_MQ,SC_FE,		;ARX HAS P,X,S
U 2667, 3361,3723,2400,5301,0000,0013,0110	; 8370			FE_#-SC,#/72.		;SC_S, FE_72-(36-P)=36+P
U 3361, 3362,4001,0000,5302,0000,0010,0044	; 8371		SC_#-SC,#/36.			;SC_36-S (KNOWN .LE. P)
						; 8372		AR_SHIFT,ARX_SHIFT,		;S,P,X
U 3362, 3363,0001,4400,5002,0000,0010,0000	; 8373			SC_FE-SC		;SC_(36+P)-(36-S)=P+S
U 3363, 0003,0001,4000,0000,0016,0003,0000	; 8374		AR_SHIFT,STORE,RETURN3		;[335][345] DONE, STORE IT BACK
						; 8375	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; BYTSUB.MIC[10,5351]	19:52 24-Jul-85			DEPOSIT BYTE SUBROUTINE					

						; 8376	;SUBROUTINE TO GET CONTENTS OF SC RIGHT ALIGNED IN AR
						; 8377	;[TIME=6]
						; 8378	
U 3364, 3365,0001,0000,2400,2000,0022,0200	; 8379	GETSC:	AR0-8_SC			;PUT SC INTO AR
U 3365, 2662,0001,0400,0302,1000,0010,0011	; 8380		ARX_AR,SC_#,#/9.,J/SHIFT	;HERE WITH DATA IN AR0-8
						; 8381	;
						; 8382	;	Some one word global subroutines.
						; 8383	;
						; 8384	.IF/OWGBP
						; 8385	=0
U 2670, 2672,4001,0000,5132,0020,5110,0045	; 8386	GTST:	SC_P-#,#/45, SKP SCAD0,J/GTST1	;TEST FOR ONE WORD GLOBAL [265]
U 2671, 0001,4001,0000,0000,0000,0003,0000	; 8387		RETURN1				;NOT IN SEC 0
						; 8388	=0
U 2672, 0002,0001,0040,0000,0000,0003,0000	; 8389	GTST1:	BR/AR,RETURN2			;DO OWG CODE
U 2673, 0001,4001,0000,0000,0000,0003,0000	; 8390		RETURN1				;NOT OWG
						; 8391	;
						; 8392	;CNV2WD -- ROUTINE TO CALCULATE NEW P FIELD OF ONE WORD GLOBAL BYTE
						; 8393	;POINTER AND STORE NEW POINTER. A TABLE IS IN THE EPT STARTING AT 700
						; 8394	;AND THIS IS USED TO CONVERT THE OWGBP TO A TWO WORD GLOBAL POINTER
						; 8395	;AND TO CALCULATE THE NEW P FOR THE STORE.
						; 8396	;
						; 8397	;ENTER WITH P-45 IN SC
						; 8398	;	    BYTE POINTER IN BR
						; 8399	;
						; 8400	
U 3366, 3367,3731,2000,0000,0000,1710,0000	; 8401	CNV2WD:	AR_VMA HELD			;[326] GET FULL VMA FOR WRITE
U 3367, 3370,0001,0010,0000,1000,0010,0000	; 8402		MQ_AR				;SAVE FOR WRITE BACK
U 3370, 1654,0001,0000,2400,2000,0022,0200	; 8403		AR0-8_SC			;P-45 IN AR
						; 8404	=0*	AR_ARX (AD),ARX_AR,SC_#,#/9.,	;SWAP AROUND FOR SHIFT
U 1654, 2662,3711,2400,0302,1000,0050,0011	; 8405			CALL [SHIFT]		;NOW SHIFT IT TO BIT 35
U 1656, 3371,0303,7700,0000,0020,0027,0000	; 8406		AR_AR*.5 LONG			;MAKE IT AN OFFSET, LSB IN ARX0
U 3371, 3372,4001,0000,0000,0100,3310,0700	; 8407		VMA_#+AR32-35,#/700		;POINT TO RIGHT WORD
U 3372, 3373,4001,0000,0000,0012,0026,0111	; 8408		LOAD AR,EPT REF CACHE		;GET AND CACHE DATA FROM EPT [260]
U 3373, 2674,3711,0000,0000,0022,5510,0000	; 8409		MB WAIT,GEN ARX,SKP AD0		;TEST FOR EVEN/ODD
						; 8410	=0
U 2674, 3374,4001,0400,2411,1000,0010,0000	; 8411	CNV01:	FE_S,ARX_AR,J/CNV02		;SKIP SWAP
U 2675, 2674,4001,4000,0000,3000,0010,0000	; 8412		AR_AR SWAP,J/CNV01		;SWAP HALVES FOR ODD
U 3374, 2676,3701,0020,0000,0020,5610,0000	; 8413	CNV02:	BRX/ARX,GEN AR,SKP AD NE	;DID WE GET 0 DATA ?
U 2676, 1002,0001,0040,0000,0000,0010,0000	; 8414	=0	BR/AR,J/UUO			;P=77 OR EPT NOT SET UP
U 2677, 0001,3721,0000,0000,0103,0003,0000	; 8415		RSTR VMA_MQ,RETURN1		;[307][326]NO, RESTORE VMA AND EXIT
						; 8416	.ENDIF/OWGBP
						; 8417	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EXTENDED INSTRUCTION SET DECODING			

						; 8418	.TOC	"EXTENDED INSTRUCTION SET DECODING"
						; 8419	
						; 8420	.IF/EXTEND
						; 8421	;GET HERE WITH E0 IN BR, (E0) IN AR
						; 8422	;	(E0) IS THE OPERATION WORD, AND HAS THE NORMAL -10 INSTRUCTION
						; 8423	;	FORMAT -- BITS 0-8 ARE OPCODE, 9-12 IGNORED, 13 @, 14-17 XR,
						; 8424	;	AND 18-35 Y.  THE AC USED COMES FROM THE EXTEND INSTRUCTION.
						; 8425	; COMPUTE E1 FROM 13-35
						; 8426	
						;;8427	.IFNOT/XADDR
						;;8428	;EXTEND:FE_#+AR0-8,#/-32,SKP SCAD0,	;CHECK LEGAL OPERATION
						;;8429	;		ARX_AR,AR_BRX		;OPR TO ARX, GET AC FROM BRX
						;;8430	=0
						;;8431	EXT1:	AR_BR,J/UUO			;OPCODE > 17 or 31
						;;8432	.IF/MODEL.B				;[246]
						;;8433		AR0-8_FE+#,#/32			;PLUG OPR INTO EXTEND AC
						;;8434		GEN AR,LOAD IR,AR_ARX
						;;8435		ARL_0.M,EA MOD DISP,J/EXT2
						;;8436	=1100
						;;8437	.IFNOT/MODEL.B				;[246]
						;;8438		AR0-8_FE+#,#/20			;PLUG OPR INTO EXTEND AC
						;;8439		GEN AR,LOAD IR,AR_ARX,ARL_0.M,
						;;8440			EA MOD DISP
						;;8441	=00
						;;8442	.ENDIF/MODEL.B
						;;8443	EXT2:	E1_AR,B DISP,J/EXT5		;SAVE E1, READY TO SAVE E0
						;;8444		ARL_0.M,AR_ARX+XR,J/EXT2
						;;8445		GEN ARX,A INDRCT,SKP INTRPT,J/EXT3
						;;8446		GEN ARX+XR,A INDRCT,
						;;8447			SKP INTRPT,J/EXT3
						;;8448	=0
						;;8449	EXT3:	AR_MEM,ARX_MEM,J/EXT4
						;;8450		AR_MEM,TAKE INTRPT
						;;8451	EXT4:	ARL_0.M,EA MOD DISP,J/EXT2
						;;8452	
						;;8453	=110
						;;8454	EXT5:	AR_BR,J/EXT6			;TRANSLATE MODE, DO NOT EXTEND
						;;8455		ARL_1S.M,SKP AR18		;SIGN EXTEND E1 IF NEGATIVE
						;;8456	=0	AR_BR,J/EXT6			;POS, ALREADY OK
						;;8457		E1_AR,AR_BR			;PUT NEG RESULT IN E1
						;;8458	EXT6:	E0_AR,VMA_AR+1,IR DISP,J/2000	;ENTER EXTENDED INSTR HANDLER
						;;8459	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; EIS.MIC[10,5351]	19:52 24-Jul-85			EXTENDED INSTRUCTION SET DECODING			

						;;8460	;HERE FOR EXTENDED INSTRUCTION SET DECODING UNDER XADDR
						; 8461	.IF/XADDR
						; 8462	;EXTEND: SC_#+AR0-8,#/-32,SKP SCAD0,	;VALID EXTENDED OPERATION?
						; 8463	;		ARX_AR,AR_BR,J/EXTF1	; OPR TO ARX, AC TO AR
						; 8464	=0
U 2700, 1002,3242,2000,0000,0000,0010,0000	; 8465	EXTF1:	AR_BR,J/UUO			;Opcode is too large.
U 2701, 3375,3242,6017,0000,1000,1010,0176	; 8466		E0_AR,MQ_AR,AR_BRX		;SAVE E0.  GET AC FROM EXTEND
U 3375, 3376,4001,0000,2302,2000,0110,0032	; 8467		AR0-8_#+SC,#/32,SC/SCAD		;COMBINE EXT OP <32 WITH AC
U 3376, 2702,0001,0000,2400,0020,5210,0000	; 8468		GEN SC,SKP SCAD NE		;TEST OP CODE
U 2702, 1002,3242,2000,0000,0000,0010,0000	; 8469	=0	AR_BR,J/UUO			;OP CODE = 0 (UUO) [217][251]
U 2703, 3377,3701,0000,0000,0000,1410,0000	; 8470		GEN AR,LOAD IR			;MAP THIS OVER THE LUUO SPACE
U 3377, 1374,2301,4000,0000,2000,0036,0000	; 8471	EXTF2:	AR_ARX,EA MOD DISP,J/EXTLA	;GO EVALUATE E1
						; 8472	=0000
U 1360, 2704,3711,0000,0000,1111,7010,0230	; 8473	EXTXA:	GEN ARX,GLOBAL,EXT INDRCT,SKP INTRPT,J/EXTI
U 1361, 2704,0610,0002,0000,1131,7010,0230	; 8474		GEN ARX+XR,GLOBAL,EXT INDRCT,SKP INTRPT,J/EXTI
U 1362, 2704,3711,0000,0000,1111,7010,0230	; 8475		GEN ARX,GLOBAL,EXT INDRCT,SKP INTRPT,J/EXTI
U 1363, 2704,0610,0002,0000,1131,7010,0230	; 8476		GEN ARX+XR,GLOBAL,EXT INDRCT,SKP INTRPT,J/EXTI
						; 8477	
U 1364, 3077,3711,0300,0000,1004,0002,0400	; 8478		GEN ARX,GLOBAL,EXT INDEX,ARX/MQ,J/3077
U 1365, 3077,0610,0302,0000,1024,0002,0400	; 8479		GEN ARX+XR,GLOBAL,EXT INDEX,ARX/MQ,J/3077
U 1366, 3077,3711,0300,0000,1004,0002,0400	; 8480		GEN ARX,GLOBAL,EXT INDEX,ARX/MQ,J/3077
U 1367, 3077,0610,0302,0000,1024,0002,0400	; 8481		GEN ARX+XR,GLOBAL,EXT INDEX,ARX/MQ,J/3077
						; 8482	
U 1370, 0131,0001,0000,0000,0000,2210,0400	; 8483		GET ECL EBUS,J/ILLIND		;[234] illegal indirect word
U 1371, 0131,0001,0000,0000,0000,2210,0400	; 8484		GET ECL EBUS,J/ILLIND		;[234] illegal indirect word
U 1372, 0131,0001,0000,0000,0000,2210,0400	; 8485		GET ECL EBUS,J/ILLIND		;[234] illegal indirect word
U 1373, 0131,0001,0000,0000,0000,2210,0400	; 8486		GET ECL EBUS,J/ILLIND		;[234] illegal indirect word
						; 8487	;
						; 8488	;	[325]
						; 8489	;	The effective address dispatch logic is quite arcane.  It appears
						; 8490	;	that MEM/A RD,DISP/DRAM A RD, and SH/2 interact to get the section
						; 8491	;	number from either AD (if the AC > 777777) or from VMA section, but
						; 8492	;	in order for that to work, we must do something with the VMA, even
						; 8493	;	though we don't actually use it here if the address computation
						; 8494	;	is complete.  Thus the VMA/LOAD has been added for the index case.
						; 8495	;
U 1374, 1762,3701,0300,0000,0004,0033,0400	; 8496	EXTLA:	GEN AR,EXT ADDR,ARX/MQ,J/EXT2
U 1375, 3077,0600,0302,0000,2124,0002,0400	; 8497		GEN AR+XR,INDEXED,EXT INDEX,ARX/MQ,VMA/LOAD,J/3077 ;[325]
U 1376, 2704,3701,0000,0000,0111,7010,0230	; 8498		GEN AR,EXT INDRCT,SKP INTRPT,J/EXTI
U 1377, 2704,0600,0002,4000,2131,7010,0230	; 8499		GEN AR+XR,INDEXED,EXT INDRCT,SKP INTRPT,J/EXTI
						; 8500	
						; 8501	=0
U 2704, 3400,3240,0003,0000,0022,2510,0000	; 8502	EXTI:	ARX_MEM,LONG EN,J/EXTI2
U 2705, 0144,3200,0003,0000,0022,7710,0000	; 8503		ARX_MEM,TAKE INTRPT
						; 8504	EXTI2:	AR_ARX,XR,EA MOD DISP,
U 3400, 1360,2341,4002,0301,2020,0036,0024	; 8505			FE_#,#/24,TIME/3T,J/EXTXA
						; 8506	
						; 8507	=010
						; 8508	EXT2:	E1_AR,AR_ARX,VMA_ARX+1,		;ESTABLISH E1
U 1762, 2000,4011,4007,0000,2320,1001,0165	; 8509			IR DISP,J/2000		;GO TO SPECIFIC HANDLER
U 1763, 2706,4001,0000,0000,0001,4410,0020	; 8510		ARL_0.M,SKP AR18,J/EXT3		;OFFSET MODE.  EXTEND E1
						; 8511		E1_AR,AR_ARX,VMA_ARX+1,		;[301] Duplicate these to
U 1766, 2000,4011,4007,0000,2320,1001,0165	; 8512			IR DISP,J/2000		; distinguish GSNGL (B=5) from
						; 8513		E1_AR,AR_ARX,VMA_ARX+1,		; offset instructions (B=1)
U 1767, 2000,4011,4007,0000,2320,1001,0165	; 8514			IR DISP,J/2000
						; 8515	=0; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EXTENDED INSTRUCTION SET DECODING			

						; 8516	EXT3:	E1_AR,AR_ARX,VMA_ARX+1,		;ESTABLISH E1
U 2706, 2000,4011,4007,0000,2320,1001,0165	; 8517			IR DISP,J/2000		;GO TO SPECIFIC HANDLER
U 2707, 2706,2341,0000,0000,0000,0610,0002	; 8518		ARL_1S,J/EXT3			;NEGATIVE OFFSET
						; 8519	.ENDIF/XADDR
						; 8520	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; EIS.MIC[10,5351]	19:52 24-Jul-85			EXTENDED INSTRUCTION SET DECODING			

						; 8521	;	By using "IR DISP,J/2000" we can use the same DRAM for LUUOs as
						; 8522	;	for the EXTEND instructions with like opcodes.  The LUUOs dispatch
						; 8523	;	to addresses in the range 1000-1017; by dispatching with J/2000,
						; 8524	;	the EXTEND ops go to 3000-3017 (model B) or 2000-20017 (model A).
						;;8525	.IFNOT/MODEL.B
						;;8526	2005:	AR_AC3,J/CMPS			;HIDDEN BEHIND L-CMS
						;;8527	2006:	CLR AR,ARX_1S,SC_#,#/15.,J/EDIT	;HIDDEN BEHIND L-EDIT
						;;8528	2010:	AR_AC0 COMP,J/DBIN		;HIDDEN BEHIND L-DBIN
						;;8529	2011:	AR_AC1,ARL/AD,SC_1,ARX+MQ_0.M,
						;;8530			BYTE DISP,J/BDEC	;HIDDEN BEHIND L-BDEC
						;;8531	2012:	AR_AC3,LOAD AR,J/MVST		;HIDDEN BEHIND L-MVS
						; 8532	.IF/MODEL.B
						;;8533	.IFNOT/XADDR
						;;8534	3005:	AR_AC3,J/CMPS			;HIDDEN BEHIND L-CMS
						;;8535	3006:	CLR AR,ARX_1S,SC_#,#/15.,J/EDIT	;HIDDEN BEHIND L-EDIT
						;;8536	3010:	AR_AC0 COMP,J/DBIN		;HIDDEN BEHIND L-DBIN
						;;8537	3011:	AR_AC1,ARL/AD,SC_1,ARX+MQ_0.M,
						;;8538			BYTE DISP,J/BDEC	;HIDDEN BEHIND L-BDEC
						;;8539	3012:	AR_AC3,LOAD AR,J/MVST		;HIDDEN BEHIND L-MVS
						; 8540	.IF/XADDR
						;;8541	.IFNOT/OWGBP				;[265]
						;;8542	3005:	AR_AC3,J/CMPS			;HIDDEN BEHIND L-CMS
						;;8543	3006:	CLR AR,ARX_1S,SC_#,#/15.,J/EDIT	;HIDDEN BEHIND L-EDIT
						;;8544	3010:	AR_AC0 COMP,J/DBIN		;HIDDEN BEHIND L-DBIN
						;;8545	3011:	AR_AC1,ARL/AD,SC_1,ARX+MQ_0.M,
						;;8546			BYTE DISP,J/BDEC	;HIDDEN BEHIND L-BDEC
						;;8547	3012:	AR_AC3,LOAD AR,J/MVST		;HIDDEN BEHIND L-MVS
						; 8548	.IF/OWGBP				;[265]
						; 8549	;
						; 8550	;	[347] CMPS dispatch rewritten to test bad high length bits first.
						; 8551	;
U 3005, 3401,3200,2000,0301,0020,0050,0777	; 8552	3005:	AR_AC0,FE_#,#/777,CALL [FLGTST]	;[347] Any illegal high bits in len?
U 3025, 2020,0001,0007,0000,0000,1050,0173	; 8553	3025:	FILL_AR,CALL [EXT2WD]		;[310][347] Save fill VMA, test OWG
U 3035, 3431,3240,2015,0000,2020,0010,0000	; 8554	3035:	AR_AC3,MQ_ARX,J/CMPS		;[310][347] Get dest length and go
						; 8555	;
U 3006, 0465,0001,0000,0000,0000,0010,0000	; 8556	3006:	J/EDIT				;HIDDEN BEHIND L-EDIT
U 3010, 2734,0001,0000,0000,0000,0010,0000	; 8557	3010:	J/DBIN				;HIDDEN BEHIND L-DBIN
U 3011, 0461,4013,2000,0000,0020,0010,0000	; 8558	3011:	AR_ARX+1 (AD),J/BDEC		;[344] HIDDEN BEHIND L-BDEC
						; 8559	;
U 3012, 3401,3240,2000,0301,0020,0050,0077	; 8560	3012:	AR_AC0,FE_#,#/77,CALL [FLGTST]	;[347] MVST. Watch out for illegal
U 3032, 0660,4001,0000,0000,0012,0010,0000	; 8561	3032:	LOAD AR,J/MVST			; flags first. 
						; 8562	;
						; 8563	;	Subroutine to check for bits set that are not allowed to be.
						; 8564	;	Enter with AR containing AC0 and FE with relevant bit mask.
						; 8565	;	Return 20 if none set; sideways exit to UUO if any are.  Note
						; 8566	;	that BRX must still contain the EXTEND for this to work.
						; 8567	;
U 3401, 3402,3200,2005,7021,0020,0010,0000	; 8568	FLGTST:	AR_AC3,FE_FE AND AR0-8		;[347] Get dest length
U 3402, 2710,4001,0000,6020,0020,5210,0000	; 8569		GEN FE OR AR0-8,SKP SCAD NZ	;[347] Are any high bits set?
U 2710, 0020,4013,2000,0000,0020,0003,0000	; 8570	=0	AR_ARX+1 (AD),RETURN20		;[347] No. Start saving fill VMA
U 2711, 1002,3242,2000,0000,0000,0010,0000	; 8571		AR_BR,J/UUO			;[347] Yes. Blow out of the water
						; 8572	.ENDIF/OWGBP				;[265]
						; 8573	;3042:	AR_BR,J/UUO		;[217] INDEXING ON ILL. EXTEND OP.
						; 8574	;
						; 8575	;	As first written, locations 3044, 3045, 3046, 3050, 3051, 3052,
						; 8576	;	3144, 3145, 3146, 3147, 3150, 3151, 3152, 3153, and 3154 all were; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EXTENDED INSTRUCTION SET DECODING			

						; 8577	;	B DISP,J/EXT2.  The comment:  these are index cases because index
						; 8578	;	must do AREAD with the DISP function in order to get the correct
						; 8579	;	index value for E1.
						; 8580	;
U 3077, 1762,4001,0000,0000,0000,0033,0000	; 8581	3077:	B DISP, J/EXT2		;[251]
U 3177, 1762,4001,0000,0000,0000,0033,0000	; 8582	3177:	B DISP, J/EXT2		;[251]
						; 8583	.ENDIF/XADDR
						; 8584	.ENDIF/MODEL.B
						; 8585	.ENDIF/EXTEND
						; 8586	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; EIS.MIC[10,5351]	19:52 24-Jul-85			ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND

						; 8587	.TOC	"ONE WORD GLOBAL BYTE POINTER SUBROUTINES FOR EXTEND"
						; 8588	;
						; 8589	; HERE FOR MVST, EDIT AND CMPS INSTRUCTIONS
						; 8590	; MUST CHECK BOTH AC1 AND AC4 FOR OWGBP
						; 8591	; AND CONVERT TO TWO WORD GLOBAL POINTERS.
						; 8592	; There is also a hack in here for the CMPSx instructions.  In
						; 8593	; order to find their fill characters in the right place, we must
						; 8594	; fetch FILL (saved as E0+1) into ARX.  [310]
						; 8595	; BDEC ENTERS AT EXT01 FOR AC4 ONLY
						; 8596	.IF/EXTEND
						; 8597	.IF/OWGBP				;[265]
						; 8598	=000
U 2020, 1170,3200,2001,0000,0020,0050,0000	; 8599	EXT2WD:	AR_AC1,CALL [TST2WD]		;AC1 OWGBP ?
U 2021, 3410,0001,0000,0000,0000,0050,0000	; 8600		CALL [STR2WD]			;YES, CONVERT DONE, STORE
U 2022, 2024,0001,0000,0000,0000,0010,0000	; 8601		J/EXT01				;NO, TRY AC4
U 2023, 3406,3312,2004,0000,0000,1010,0000	; 8602		AC2_AR,AR_BR OR ARX,J/EXT02	;ADDRESS STORE
U 2024, 1170,3200,2006,0000,0020,0050,0144	; 8603	EXT01:	AR_AC4,CALL [TST2WD]		;AC4 OWGBP ?
U 2025, 3410,3240,0207,0000,0020,0050,0173	; 8604		ARX_FILL,CALL [STR2WD]		;[310] YES, CONVERT DONE, STORE
U 2026, 0010,3200,0207,0000,0020,0003,0173	; 8605		ARX_FILL,RETURN10		;[310][347] NO, CAN'T DO NO MORE
U 2027, 3403,0001,0000,0000,0000,0010,0145	; 8606		SEL DSTP2			;[310] DON'T GLITCH ON AC5 STORE
U 3403, 3404,3312,2006,0000,0000,1010,0145	; 8607		AC5_AR,AR_BR OR ARX		; (See second edit #210)
U 3404, 3405,3202,0600,0000,0000,0010,0144	; 8608		ARX_BRX,SEL AC4			;[310] RESTORE ARX AND SELECT AC4
U 3405, 0010,4001,0006,0000,0000,1003,0144	; 8609		AC4_AR,RETURN10			;[347] P,S,BIT 12 = 1 TO AC4
						; 8610	
U 3406, 2024,0001,0001,0000,0000,1010,0000	; 8611	EXT02:	AC1_AR,J/EXT01			;P,S,BIT 12 = 1 TO AC1
						; 8612	
						; 8613	; HERE FOR DBIN
						; 8614	
						; 8615	=00
U 1150, 1170,3200,2001,0000,0020,0050,0000	; 8616	DB2WD:	AR_AC1,CALL [TST2WD]		;AC1 OWGBP ?
U 1151, 3410,0001,0000,0000,0000,0050,0000	; 8617		CALL [STR2WD]			;YES, CONVRT DONE, STORE
U 1152, 0001,4001,0000,0000,0000,0003,0000	; 8618		RETURN1				;NO, GET OUT
U 1153, 3407,3312,2004,4000,0000,1010,0000	; 8619		AC2_AR,AR_BR OR ARX		;[407] ADDRESS TO AC2
U 3407, 0001,4001,0001,0000,0000,1003,0000	; 8620		AC1_AR,RETURN1			;[407] P,S,BIT 12 = 1 TO AC1
						; 8621	
						; 8622	; HERE TO TEST FOR OWGBP IN THE AR AND
						; 8623	; TO CONVERT IT IF IT'S OK
						; 8624	
						; 8625	=00
U 1170, 2670,0001,0000,0000,0000,5050,0000	; 8626	TST2WD:	SKP PC SEC0,CALL,J/GTST		;TEST FOR NOT SEC 0 AND OWGBP [266]
U 1171, 0002,4001,0000,0000,0000,0003,0000	; 8627	RET2:	RETURN2				;[260]NOT TODAY
U 1172, 3366,0001,0000,0000,0000,0050,0000	; 8628		CALL [CNV2WD]			;YES, CONVERT 
U 1173, 0001,3202,0010,0000,0000,0703,0003	; 8629		MQ_BR,RETURN1			;GET OUT
						; 8630	
						; 8631	; HERE TO GET P,S,BIT 12 = 1 AND A GOOD ADDRESS
						; 8632	; SOME VERY TRICKY STUFF GOING ON HERE
						; 8633	
U 3410, 3411,3240,2067,0000,0020,0010,0164	; 8634	STR2WD:	[AR]_FM[EXPMSK], BR/AR,BRX/ARX	;[310] P,S,JUNK TO BR, SAVE ARX
U 3411, 3412,3002,0204,0000,0000,0010,0000	; 8635		ARX_AR ANDCA BR			;P,S,0 TO ARX
U 3412, 3413,5100,2007,0000,0020,0010,0175	; 8636		[AR]_[AR]-FM[ADMSK]		;BIT 12 = 1 TO AR
						; 8637		AR_[MQ] AND FM[ADMSK],		;0,ADDRESS TO AR
U 3413, 0002,3620,2047,4000,0020,0003,0175	; 8638			BR/AR,RETURN2		;BIT 12=1 TO BR
						; 8639	.ENDIF/OWGBP				;[265]
						; 8640	.ENDIF/EXTEND
						; 8641	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- STRING MOVE					

						; 8642	.TOC	"EIS -- STRING MOVE"
						; 8643	.IF/EXTEND
						; 8644	; HERE FOR MOVE STRING, CHECK FOR OWGBP FIRST
						; 8645	;SLEN IS THE COMPLEMENT OF THE SHORTER STRING LENGTH
						; 8646	;DLEN IS <SRC LEN>-<DST LEN>
						; 8647	
						; 8648	.IF/OWGBP
						; 8649	=0*0*					;[347]
U 0660, 1171,3200,0003,0000,0022,0050,0000	; 8650	MVST:	AR_MEM,CALL [RET2]		;[260]GET FILL, WAIT FOR PARITY
U 0662, 2020,0001,0007,0000,0000,1050,0173	; 8651		FILL_AR,CALL [EXT2WD]		;SAVE FILL, CHECK FOR OWGBP
U 0672, 3414,3200,2005,0000,0020,0010,0000	; 8652	=1*1*	AR_AC3				;[347] GET DLEN
U 3414, 3415,3200,2040,0000,0020,0010,0000	; 8653		BR/AR,AR_AC0			;[347] Copy for length compare
						;;8654	.IFNOT/OWGBP
						;;8655	MVST:	BR/AR,AR_MEM,			;HOLD AC3, WAIT FOR FILLER
						;;8656			FE_AR0-8,SKP SCAD NE	;CHECK FOR FLAGS IN DEST LEN
						;;8657	=0	ARX_AC0,J/MVST1			;GET SRC LEN, FLAGS
						;;8658	NOLENS:	AR_E0,J/UUO			;NO FLAGS ALLOWED IN DST LEN
						;;8659	MVST1:	FILL_AR,AR_ARX			;SAVE FILL CHAR
						; 8660	.ENDIF/OWGBP
U 3415, 3416,0001,0000,2421,0000,0110,0000	; 8661		FE_AR0-8,AR0-8_#,#/0		;SEPARATE FLAGS OFF
U 3416, 2712,5102,2400,0000,1040,5510,0000	; 8662		ARX_AR,AR_AR-BR,SKP AD0		;COMPUTE SRC-DST LEN
U 2712, 3417,2542,2005,0000,0000,1010,0000	; 8663	=0	DLEN_AR,AR_BR COMP,J/MVST2	;SRC LONGER
U 2713, 3417,2001,6005,0000,0000,1010,0000	; 8664		DLEN_AR,AR_ARX COMP		;DST LONGER
U 3417, 3420,3401,2417,0000,1000,1010,0170	; 8665	MVST2:	SLEN_AR,ARX_AR,MQ_AR,AR_0S	;-SHORT LEN -1 TO MQ
U 3420, 3421,0001,0020,0000,2000,0022,0200	; 8666		AR0-8_FE,BRX/ARX		; AND BRX
U 3421, 2164,0001,0000,0000,0000,1033,0000	; 8667		SFLGS_AR,B DISP
U 2164, 3423,2341,0200,0302,0000,0410,0014	; 8668	=100	CLR AR,ARX_1S,SC_#,#/12.,J/MOVS2;[220]TRANSLATE, BUILD MASK
U 2165, 3422,3240,2006,0000,0020,0010,0144	; 8669		AR_DSTP,J/MVSO3			;OFFSET, MASK DEPENDS ON S
U 2166, 2240,4662,6600,0000,0020,1610,0111	; 8670		ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1	;LEFT JUSTIFY
U 2167, 1230,3240,2005,0000,0020,5510,0000	; 8671		AR_DLEN,SKP AD0,J/MOVRJ		;RIGHT JUSTIFY
						; 8672	
U 3422, 3423,2341,2000,2412,0000,0510,0000	; 8673	MVSO3:	SC_S,CLR ARX,AR_1S		;PREPARE TO BUILD MASK
U 3423, 3424,4001,4000,0000,0000,1610,0111	; 8674	MOVS2:	AR_SHIFT,SR_SRC
U 3424, 2220,0001,0007,0000,0000,1010,0167	; 8675		MSK_AR
						; 8676	=000
U 2220, 2370,4640,2007,0000,0020,0050,0170	; 8677	MOVELP:	AR_SLEN+1,CALL,J/SRCMOD		;PICK UP SOURCE BYTE
U 2221, 2241,3240,2005,0000,0020,0010,0000	; 8678		AR_DLEN,J/MOVSTX		;(1) LENGTH EXHAUSTED
						; 8679	=100
U 2224, 1540,4001,0000,0000,0000,1650,0213	; 8680	MOVPUT:	SR_SRC+DST,CALL,J/PUTDST	;(4) NORMAL, STORE DST BYTE
U 2225, 3425,3200,2005,0000,0237,0010,0000	; 8681		I FETCH,AR_DLEN,J/MVABT		;(5) ABORT
U 2226, 2220,0001,0000,0000,0000,1610,0111	; 8682	=110	SR_SRC,J/MOVELP			;(6) DPB DONE
						; 8683	=
						; 8684	;HERE TO ABORT A STRING MOVE DUE TO TRANSLATE OR OFFSET FAILURE
						; 8685	
U 3425, 2714,5160,2047,0000,0020,4510,0170	; 8686	MVABT:	BR/AR,AR_-SLEN,SKP AR0		;WHICH STRING LONGER?
						; 8687	=0
U 2714, 3426,4001,0005,0000,0002,1010,0000	; 8688	MVABT1:	AC3_AR,FETCH WAIT,J/MVABT2	;PUT AWAY DEST LEN
U 2715, 2714,5102,2000,0000,0020,0010,0000	; 8689		AR_AR-BR,J/MVABT1		;DEST LEN WAS GREATER
						; 8690	
U 3426, 2716,2540,2007,0000,0020,4210,0170	; 8691	MVABT2:	AR_SLEN COMP,SKP BR0		;GET UNDECREMENTED SLEN
U 2716, 2717,0602,2004,0000,0020,0010,0000	; 8692	=0	AR_AR+BR			;SRC LONGER BY (DLEN)
U 2717, 0065,3300,2000,4000,0020,1610,0000	; 8693	MVEND:	AR_AR*SFLGS,AD/OR,SR_0,J/STAC	;PUT BACK REMAINING LENGTH
						; 8694	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- STRING MOVE					

						; 8695	;HERE TO BEGIN RIGHT-JUSTIFIED MOVE
						; 8696	
						; 8697	=00
U 1230, 2720,3240,2401,0000,1020,1610,0111	; 8698	MOVRJ:	ARX_AR,AR_SRCP,SR_SRC,J/MVSKP	;SRC LONGER, SKIP OVER SOME
U 1231, 0652,4001,0000,0000,0000,1650,0214	; 8699		SR_DSTF,CALL,J/MOVF1		;DST LONGER, FILL IT
U 1233, 2240,4662,6600,0000,0020,1610,0111	; 8700	=11	ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1	;DONE FILLING
						; 8701	
						; 8702	=0
						; 8703	MVSKP:	ARX_ARX-1 (AD),FE_#,#/36.,
U 2720, 1436,1711,0200,0301,0040,7032,0044	; 8704			SIGNS DISP,SKP INTRPT,J/MVSK1
U 2721, 2720,4003,2000,5010,3020,0610,0200	; 8705		P_FE-S,AR_AR+1,J/MVSKP
						; 8706	=1110
U 1436, 2720,0001,0000,5110,3021,5110,0200	; 8707	MVSK1:	P_P-S,SKP SCAD0,J/MVSKP		;BUMP POINTER
U 1437, 1456,3713,0001,0000,0021,1032,0030	; 8708		SRCP_AR,GEN ARX,SIGNS DISP,AR_0.M
U 1456, 3427,2540,2227,0000,0020,0010,0170	; 8709	=1110	BRX/ARX,AR_SLEN COMP,ARX/AD,J/MVSK3	;INTERRUPTED
U 1457, 2246,4001,0005,0000,0000,1010,0000	; 8710		DLEN_AR,J/MVSK4			;DONE FILLING
						; 8711	
U 3427, 3430,4602,6005,4000,0020,1010,0000	; 8712	MVSK3:	AC3_AR,AR_ARX*BRX,AD/A+B+1	;DEST HAS SHORT LEN
U 3430, 3772,4001,0000,0000,0000,1610,0000	; 8713		SR_0,J/STRPF2			;FIX UP AC0, SERVE INTRPT
						; 8714	
						; 8715	;HERE FOR NO-MODIFICATION STRING MOVES
						; 8716	
						; 8717	;[266]	Remove edit 244
						; 8718	;;[244]	THIS ADDRESS MUST REMAIN SET FOR THE PROBLEM
						; 8719	;;	OF THE S FIELD OF THE SOURCE POINTER BEING > 36.
						; 8720	;;
						; 8721	;.IF/MODEL.B
						; 8722	;1300:					;[244]
						; 8723	;MOVST1:	SLEN_AR,BRX/ARX,		;PUT UPDATED LEN AWAY
						; 8724	;		AR+ARX+MQ_0.M,CALL.M,
						; 8725	;		SIGNS DISP,J/GSRC
						; 8726	;1301:
						; 8727	;MOVSTX:	SKP AR0,ARX_AR,AR_0S,J/MOVST2	;SHORT LEN EXHAUSTED
						; 8728	;1302:	SR_SRC+DST,CALL,J/PUTDST
						; 8729	;1306:
						; 8730	;MVSK4:	ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1
						; 8731	;.IFNOT/MODEL.B		;[244][266]
						; 8732	=000
						; 8733	MOVST1:	SLEN_AR,BRX/ARX,		;PUT UPDATED LEN AWAY
						; 8734			AR+ARX+MQ_0.M,CALL.M,
U 2240, 1753,4001,0027,0000,0021,1072,0170	; 8735			SIGNS DISP,J/GSRC
U 2241, 1250,3401,2400,0000,1000,4510,0000	; 8736	MOVSTX:	SKP AR0,ARX_AR,AR_0S,J/MOVST2	;SHORT LEN EXHAUSTED
U 2242, 1540,4001,0000,0000,0000,1650,0213	; 8737	=010	SR_SRC+DST,CALL,J/PUTDST
						; 8738	=110
U 2246, 2240,4662,6600,0000,0020,1610,0111	; 8739	MVSK4:	ARX_BRX+1,AR/ADX,SR_SRC,J/MOVST1
						; 8740	=
						; 8741	;.ENDIF/MODEL.B		;[244][266]
						; 8742	=00
						; 8743	MOVST2:	TEST ARX,TEST FETCH,		;SKIP IF BOTH LENGTHS =0
U 1250, 2717,3713,4005,0000,2246,1010,0203	; 8744			AC3_AR,AR_ARX,J/MVEND	;CLEAR DEST LEN, REBUILD SRC
U 1251, 0652,4001,0000,0000,0000,1650,0212	; 8745		SR_DST,CALL,J/MOVF1		;SOURCE GONE, FILL OUT DST
U 1253, 3524,3200,2000,0000,0220,0010,0000	; 8746	=11	AR_SFLGS,VMA_PC+1,J/SFET1	;DONE FILLING
						; 8747	
						; 8748	;NOTE -- IT AIN'T AS EASY AS IT LOOKS TO BUM A CYCLE OUT OF THIS
						; 8749	; ROUTINE, BECAUSE AN INTERRUPT, IF ANY, HAS TO BE TAKEN AFTER THE
						; 8750	; POINTER UPDATE AND BEFORE THE LENGTH UPDATE.  GOOD HUNTING!; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- STRING MOVE					

						; 8751	=01*
U 0652, 1540,3240,2007,0000,0020,0050,0173	; 8752	MOVF1:	AR_FILL,CALL,J/PUTDST
U 0656, 2722,4660,2005,0000,0020,7010,0000	; 8753		AR_DLEN+1,SKP INTRPT,J/MOVF2
						; 8754	=0
U 2722, 1473,0001,0005,0000,0020,1032,0000	; 8755	MOVF2:	DLEN_AR,SIGNS DISP,J/MOVF3	;DONE?
U 2723, 0040,4001,0000,0000,0000,0005,0000	; 8756		SR DISP,J/CLEAN			;BREAK OUT FOR INTERRUPT
						; 8757	=1011
U 1473, 0002,4001,0000,0000,0000,0003,0000	; 8758	MOVF3:	RETURN2				;YES, DONE
U 1477, 0652,0001,0000,0000,0000,0010,0000	; 8759		J/MOVF1				;NO, DO ANOTHER
						; 8760	.ENDIF/EXTEND
						; 8761	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- STRING COMPARE					

						; 8762	.TOC	"EIS -- STRING COMPARE"
						; 8763	.IF/EXTEND
						; 8764	
						; 8765	;HERE FOR CMPS, CHECK FOR OWGBP FIRST
						; 8766	; [310] E0+1 will be saved in FILL during OWG checking.  We restore it
						; 8767	; from ARX to MQ here.  This keeps us from fetching bogus fill characters.
						; 8768	
						; 8769	.IF/OWGBP
U 3431, 3432,3200,2440,0000,1020,0010,0000	; 8770	CMPS:	BR/AR,ARX_AR,AR_AC0		;[347]DEST LEN TO BR, GET SRC LEN
U 3432, 2724,3102,0000,0000,0020,5410,0000	; 8771		SKP AR GT BR			;[347] Which string is longer?
U 2724, 3433,3721,0000,0000,0300,0010,0000	; 8772	=0	VMA_MQ,J/CMPS1			;[310] Source shorter
U 2725, 3433,4023,0000,0000,0320,0010,0000	; 8773		VMA_MQ+1			;[310] SRC LONGER, GET DST FILLER
U 3433, 3434,1703,6200,0000,0032,0010,0000	; 8774	CMPS1:	LOAD AR,AR_ARX-1,ARX_AR-1,TIME/3T;[347] Decrement lengths, get fill
U 3434, 2727,3240,0063,0000,0022,0010,0000	; 8775		AR_MEM,BR/AR,BRX/ARX,J/CMPS4	;DECREMENTED LEN'S TO BR'S
						; 8776	
						;;8777	.IFNOT/OWGBP				;[347]
						;;8778	CMPS:	BR/AR,ARX_AR,FE_AR0-8,AR_AC0	;DEST LEN TO BR, GET SRC LEN
						;;8779		FE_FE OR AR0-8,			;GATHER HIGH BITS OF LEN'S
						;;8780			SKP AR GT BR		;WHICH STRING LONGER?
						;;8781	=0					;[347]
						;;8782	CMPS1:	LOAD AR,AR_ARX-1,ARX_AR-1,	;SRC SHORTER
						;;8783			GEN FE,SKP SCAD NE,J/CMPS2 ;CHECK LEN'S PURE
						;;8784		VMA_VMA+1,J/CMPS1		;SRC LONGER, GET DST FILLER
						;;8785	=0
						;;8786	CMPS2:	AR_MEM,BR/AR,BRX/ARX,J/CMPS4	;DECREMENTED LEN'S TO BR'S
						;;8787		AR_MEM,J/NOLENS			;[275] ILLEGAL BITS IN LEN'S
						; 8788	.ENDIF/OWGBP
						; 8789	
						; 8790	;HERE IS THE COMPARE LOOP.
						; 8791	; MQ CONTAINS THE FILL CHARACTER FOR THE SHORTER STRING,
						; 8792	; BR CONTAINS THE REMAINING DESTINATION LENGTH,
						; 8793	; BRX CONTAINS THE REMAINING SOURCE LENGTH
						; 8794	=0
U 2726, 1536,4321,0700,0000,0020,0010,0000	; 8795	CMPS3:	ARX0_MQ35,J/CMPSX		;WE GOT INEQUALITY.  GET SIGN
						; 8796	CMPS4:	MQ_AR,ARX_AR,FE_#,#/36.,	;FILL TO MQ & ARX
U 2727, 1530,3242,2410,0301,1000,4310,0044	; 8797			AR_BR,SKP ARX0		;MORE CHARS IN SRC STRING?
						; 8798	=1000	AR_SRCP,ARX_SRCP,		;READY WITH SRC POINTER
U 1530, 2760,3240,2201,0000,0040,1650,0101	; 8799			SR_ED(S),CALL,J/GSRC1	;GO GET SRC BYTE
U 1531, 1532,3441,4200,0000,2020,1632,0000	; 8800		AR_ARX,ARX_0S,SR_0,SIGNS DISP	;SRC DONE.  TEST DEST LEN
U 1532, 1535,3723,2007,0000,0020,1032,0166	; 8801	=1010	T0_AR,AR_MQ,SIGNS DISP,J/CMPS5	;SRC (OR SRC FILL) TO T0,
						; 8802	=1110					;TEST FOR END OF DEST STRING
U 1536, 0073,3711,0000,0000,0246,0010,0202	; 8803	CMPSX:	GEN ARX,CMS FETCH,J/NOP		;QUIT WITH COMPARE COND IN ARX
						; 8804	=
						; 8805	;HERE TO GET DESTINATION BYTE.  SRC IS IN T0, FILL CHAR IN AR
						; 8806	;HERE WITH SIGNS DISP, TO AVOID CALL ON CMPDST IF DST LEN EXHAUSTED
						; 8807	
						; 8808	=1101
U 1535, 2732,4001,0000,0000,0000,1650,0224	; 8809	CMPS5:	SR_ED(+D),CALL,J/CMPDST		;GO FOR DESTINATION BYTE
						; 8810		AR_AR*T0,AD/XOR,		;AR ZERO IF EQUAL
U 1537, 3435,3100,2307,4000,0020,0012,0166	; 8811			ARX/MQ,MQ_MQ*2		;FILL TO ARX, CRY TO MQ35
						; 8812		BR/AR,BRX/ARX,			;EQUALITY TO BR, FILL TO BRX
U 3435, 2730,3242,2660,0000,0000,4210,0000	; 8813			AR_BR,ARX_BRX,SKP BR0	;LENGTHS TO AR, ARX
						; 8814	=0	AC3_AR,ARX_AR,AR_ARX (AD),	;UPDATE DEST LEN IN AC3
U 2730, 1556,3713,2405,0000,1020,1032,0000	; 8815			SIGNS DISP,J/CMPS6	;TEST SRC LEN
U 2731, 1556,3713,2400,0000,1000,0010,0000	; 8816		ARX_AR,AR_ARX (AD)		;DEST LEN EXHAUSTED
						; 8817	=1110; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- STRING COMPARE					

U 1556, 3436,1703,6200,0000,0040,1010,0000	; 8818	CMPS6:	AC0_AR,AR_ARX-1,ARX_AR-1,J/CMPS7	;UPDATE SRC LEN IN AC0
U 1557, 3436,1701,6200,0000,0040,0010,0000	; 8819		AR_ARX-1,ARX_AR-1		;SRC EXHAUSTED PREVIOUSLY
						; 8820	
						; 8821	CMPS7:	BR/AR,BRX/ARX,			;LENGTHS TO BR'S
U 3436, 2726,7162,6060,0000,0040,5410,0000	; 8822			SKP BR EQ,AR/ADX,J/CMPS3	;CHECK FOR EQUALITY
						; 8823	
						; 8824	=0
						;;8825	.IFNOT/MODEL.B
						;;8826	CMPDST:	AR_DSTP,ARX_DSTP,FE_#,#/36.,	;GET DEST BYTE FOR COMPARE
						;;8827			CALL,J/IDST		;UPDATE DEST POINTER
						; 8828	.IF/MODEL.B
						; 8829	CMPDST:	AR_DSTP,ARX_DSTP,		;GET DEST BYTE FOR COMPARE
U 2732, 3507,3240,2206,0000,0040,0050,0144	; 8830			CALL,J/IDST		;UPDATE DEST POINTER
						; 8831	.ENDIF/MODEL.B
U 2733, 2656,4001,0000,2002,0000,7010,0000	; 8832		SC_FE+SC,SKP INTRPT,J/LDB1	;GET DEST BYTE
						; 8833	.ENDIF/EXTEND
						; 8834	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- DECIMAL TO BINARY CONVERSION			

						; 8835	.TOC	"EIS -- DECIMAL TO BINARY CONVERSION"
						; 8836	.IF/EXTEND
						; 8837	; HERE WITH AC0 (SRC LEN) IN AR COMPLEMENTED
						; 8838	; IN THE LOOP, AC3 CONTAINS 10 (DECIMAL), BR'BRX HAS ACCUMULATED BINARY
						; 8839	
						; 8840	.IF/OWGBP
						; 8841	=0
U 2734, 1150,0001,0000,0000,0000,0050,0000	; 8842	DBIN:	CALL [DB2WD]			;CHECK FOR OWGBP
U 2735, 3437,2540,2000,0000,0020,0010,0000	; 8843		AR_AC0 COMP			;FLAGS TO AR
U 3437, 3440,0001,0040,1421,0000,0110,0777	; 8844		BR/AR,FE_AR0-8 COMP,AR0-8_#,#/-1	;FORCE OUT FLAGS
						;;8845	.IFNOT/OWGBP
						;;8846	DBIN:	BR/AR,FE_AR0-8 COMP,AR0-8_#,#/-1	;FORCE OUT FLAGS
						; 8847	.ENDIF/OWGBP
U 3440, 1575,3441,2207,0000,0020,1032,0170	; 8848		SLEN_AR,AR_0S,ARX_0S,SIGNS DISP
U 1575, 3442,3200,0216,0000,2020,0110,0144	; 8849	=1101	AR0-8_FE#,MQ_ARX,ARX_AC4,J/DBS1	;BUILD SFLGS
U 1577, 2266,0001,0000,0000,0000,0033,0000	; 8850		B DISP				;OFFSET OR TRANSLATE?
U 2266, 3441,4001,0000,0000,2000,0022,0200	; 8851	=110	AR0-8_FE,J/DBST			;TRANSLATE, LET S FLAG SET LATER
U 2267, 3441,4001,0000,6030,2000,0110,0400	; 8852		AR0-8_FE OR #,#/400		;OFFSET, SET S FLAG
U 3441, 3444,3401,2200,0000,0000,1010,0000	; 8853	DBST:	SFLGS_AR,AR_0S,ARX_0S,J/DBS2	;CLEAR BINARY
						; 8854	
U 3442, 3443,3721,0500,0000,0000,1010,0000	; 8855	DBS1:	SFLGS_AR,ARX_ARX*2		;HERE WHEN SIG ALREADY ON
U 3443, 3444,3200,2005,0000,0020,0010,0000	; 8856		AR_AC3				;ACCUMULATED BINARY IN AR
U 3444, 3445,4061,2060,0000,0020,0510,0000	; 8857	DBS2:	BR_AR LONG,AR_1,CLR ARX
U 3445, 2306,0603,5004,0302,0020,0033,0004	; 8858		AR_AR*10,B DISP,SC_#,#/4	;GET CONSTANT 10 FOR COMPARE
U 2306, 3446,2301,4205,0000,2000,1010,0000	; 8859	=110	AC3_AR,AR_ARX,ARX_1S,J/DBS3	;PREPARE TO BUILD MASK
U 2307, 3446,2301,2005,0000,0000,1010,0000	; 8860		AC3_AR,AR_1S			;OFFSET
U 3446, 3447,0001,4000,0000,0000,1610,0102	; 8861	DBS3:	AR_SHIFT,SR_DB
U 3447, 0520,3202,2607,0000,0000,1010,0167	; 8862		MSK_AR,AR_BR LONG		;SAVE MASK, GET INITIAL INPUT
						; 8863	
						; 8864	=0*0
						; 8865	DBINLP:	BR_AR LONG,AR_SLEN+1,		;BINARY BACK TO BR, COUNT LENGTH
U 0520, 2370,4640,2067,0000,0020,0050,0170	; 8866			CALL,J/SRCMOD		;PICK UP A DIGIT
U 0521, 2740,3203,0000,0000,0220,5510,0000	; 8867		SKP AR2,VMA_PC+1,J/DBXIT	;(1) DONE, TEST M FLAG
						; 8868		ARX_AR,AR+MQ_0.M,GEN AR-AC3,	;(4) NORMAL, ADD IN DIGIT
U 0524, 2736,5100,0405,4000,1041,5410,0130	; 8869			SKP CRY0,J/DBIN2	;TEST FOR DIGIT >9
U 0525, 3452,2500,2007,0000,0020,0010,0170	; 8870		AR_SLEN COMP,J/DBABT		;(5) ABORT
						; 8871	
						; 8872	;HERE TO ADD IN A DIGIT
						; 8873	
						; 8874	=0
U 2736, 3450,3202,2660,0000,0000,0010,0000	; 8875	DBIN2:	BR_AR LONG,AR_BR LONG,J/DBIN3	;DIGIT TO BR LONG, BINARY TO AR LONG
U 2737, 3452,2500,2007,0000,0020,0010,0170	; 8876		AR_SLEN COMP,J/DBABT		;DIGIT >9, ABORT
						; 8877	
U 3450, 3451,0603,2604,0000,0020,0027,0000	; 8878	DBIN3:	AR_AR*5 LONG			;ALREADY HAVE BINARY *2
U 3451, 0520,0602,5500,0000,0020,0027,0000	; 8879		AR_2(AR+BR) LONG,J/DBINLP	;ADD IN DIGIT, SHIFT LEFT
						; 8880	
						; 8881	;HERE ON ABORT
						; 8882	
U 3452, 3453,3300,2000,4000,0020,0010,0000	; 8883	DBABT:	AR_AR*SFLGS,AD/OR		;[230][221]FLAGS +LEN REMAINING
						; 8884		AC0_AR,AR_BR LONG,SC_#,#/35.,	;PUT BACK UNUSED LENGTH
U 3453, 3454,3242,2600,0302,0200,1010,0043	; 8885			VMA_PC+1,J/STOR34	;END WITH NO SKIP
						; 8886	
						; 8887	;HERE AT END
						; 8888	
						; 8889	=0
						; 8890	DBXIT:	AR_BR LONG,VMA_VMA+1,		; M FLAG=0; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- DECIMAL TO BINARY CONVERSION			

U 2740, 3454,3242,2600,0302,0000,3610,0043	; 8891			SC_#,#/35.,J/STOR34	;GO FOR NEXT INSTR
						; 8892		AR_-BR LONG,VMA_VMA+1,		;NEGATE
U 2741, 3454,5142,2600,0302,0020,3627,0043	; 8893			SC_#,#/35.
U 3454, 3455,5401,2005,0000,0037,1016,0000	; 8894	STOR34:	AC3_AR,AR_SIGN,FETCH		;STORE HIGH PART
U 3455, 3456,0001,4000,0000,0000,1610,0000	; 8895		AR_SHIFT,SR_0			;GET LOW READY
						; 8896	.IF/MODEL.B
U 3456, 3457,0001,0000,0000,0000,0010,0144	; 8897		SEL AC4				;PRESEL NUMBER TO FIX HARDW GLITCH
						; 8898	.ENDIF/MODEL.B
U 3457, 0221,0001,0006,0000,0000,1010,0144	; 8899	STAC4:	AC4_AR,FINISH
						; 8900	.ENDIF/EXTEND
						; 8901	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- BINARY TO DECIMAL CONVERSION			

						; 8902	.TOC	"EIS -- BINARY TO DECIMAL CONVERSION"
						; 8903	.IF/EXTEND
						; 8904	;	AC0,AC1 = BINARY INTEGER INPUT
						; 8905	;	AC3 = FLAGS, MAX LENGTH OF DECIMAL STRING
						; 8906	;	AC4 = DESTINATION STRING POINTER
						; 8907	; TEMPS ARE USED AS FOLLOWS:
						; 8908	;	FILL = VMA of fill character (to preserve through OWGBP check) [344]
						; 8909	;	SLEN= # OF SIGNIFICANT DIGITS
						; 8910	;	T1,2= 10.**(SLEN) THE LOWEST POWER OF TEN LARGER THAN BINARY
						; 8911	;
						; 8912	;FPD IS SET IF THE INSTRUCTION WAS INTERRUPTED AFTER CONVERSION OF THE
						; 8913	; BINARY INTEGER TO FRACTION FORM (AFTER STORING FILL, IF NEEDED).
						; 8914	
						; 8915	.IF/OWGBP
						; 8916	=0***					;[347]
U 0461, 2024,4001,0007,0000,0000,1050,0173	; 8917	BDEC:	FILL_AR,CALL [EXT01]		;[344] Save fill VMA, check OWGBP
						; 8918		AR_AC1,ARL/AD,SC_1,ARX+MQ_0.M,
U 0471, 1443,3200,2001,4402,0041,0034,0142	; 8919			BYTE DISP		;GET BIN INTEGER
						; 8920	=011	ARX_SHIFT,AR_AC0,SKP AD0,	;BINARY INTEGER NOW IN AR LONG
U 1443, 2742,3200,2400,0302,0020,5510,0020	; 8921			SC_#,#/20,J/BD1		;IS IT NEGATIVE?
						;;8922	.IFNOT/OWGBP
						;;8923	=011
						;;8924	BDEC:	ARX_SHIFT,AR_AC0,SKP AD0,	;BINARY INTEGER NOW IN AR LONG
						;;8925			SC_#,#/20,J/BD1		;IS IT NEGATIVE?
						; 8926	.ENDIF/OWGBP
U 1447, 3460,3200,2405,0000,1020,1610,0010	; 8927	BDDR1:	ARX_AR,AR_AC3,SR_BDT		;RESUME WITH FRACTION IN AR LONG
						; 8928		BR/AR,CLR EXP,			;SEPARATE FLAGS & LENGTH
U 3460, 3461,3240,0260,0400,2021,0010,0200	; 8929			BRX/ARX,ARX_AC0		;LOW FRAC TO BRX, HI TO ARX
U 3461, 3462,3002,2044,0000,0000,0010,0000	; 8930		AR_AR*BR,AD/ANDCA,BR/AR		;JUST FLAGS TO AR, JUST LEN TO BR
U 3462, 3463,4001,4005,0000,2000,1010,0000	; 8931		AC3_AR,AR_ARX			;GET HI FRAC TO AR
						; 8932		BR/AR,VMA_PC+1,			;FRAC TO BR LONG, GET VMA READY
U 3463, 2746,5162,2040,0000,0240,5410,0000	; 8933			AR_-BR,SKP CRY0,J/BDDR4	;CHECK FOR MORE TO GO
						; 8934	
						; 8935	=0
U 2742, 1330,3701,0000,0000,0040,5427,0000	; 8936	BD1:	SKP AR NE,AD LONG,J/BD2		;TEST FOR ZERO LONG
U 2743, 1331,5163,7700,0302,0020,0027,0030	; 8937		AR_-AR LONG,SC_#,#/30,J/BD3	;MAKE POSITIVE, SET N&M FLAGS
						; 8938	=00
						; 8939	BD2:	BR_AR LONG,AR_1 LONG,		;BINARY RIGHT-ALIGNED IN BR,
U 1330, 2322,4041,7660,0303,0020,0010,0024	; 8940			SC_#,FE_#,#/20.,J/BD4	;LOOK FOR LARGER POWER OF TEN
						; 8941	BD3:	BR_AR LONG,AR_AC3,		;SAVE POS BINARY, GET AC FLAGS
U 1331, 3545,3240,2065,0000,0020,0050,0000	; 8942			CALL,J/SETFLG		; SET FLAGS AS NEEDED
U 1333, 1330,3201,7705,0000,0000,1010,0000	; 8943	=11	AC3_AR,AR_BR*.5 LONG,J/BD2	;SAVE NEW FLAGS, SHIFT BINARY RIGHT
						; 8944	
						; 8945	;HERE TO FIND THE SMALLEST POWER OF TEN LARGER THAN THE BINARY INTEGER.
						; 8946	;BINARY IS IN BR LONG, AND POSITIVE UNLESS IT WAS 1B0.  IN THIS CASE THE
						; 8947	;COMPARISON WILL NEVER FIND A LARGER POWER OF TEN, BUT THE COUNT IN FE
						; 8948	;WILL RUN OUT, AND WE WILL CORRECTLY COMPUTE 22 DIGITS REQUIRED.
						; 8949	
						; 8950	=010					;IGNORE BR SIGN
U 2322, 3464,0603,5504,3001,0020,0027,0000	; 8951	BD4:	AR_AR*10 LONG,FE_FE-1,J/BD6	;THIS POWER IS TOO SMALL
U 2323, 1660,4001,4007,1002,2000,1010,0171	; 8952		SC_FE-SC-1,T1_AR,AR_ARX,J/BD7	;THIS POWER IS BIG ENOUGH
U 2326, 2327,4001,0000,3001,0000,0010,0000	; 8953		FE_FE-1				;10.**21 IS TOO SMALL, USE 22
U 2327, 1660,4001,4007,1002,2000,1010,0171	; 8954		SC_FE-SC-1,T1_AR,AR_ARX,J/BD7	;10.**21 IS BIG ENOUGH
						; 8955	
U 3464, 2322,1102,0004,0000,0020,0031,0000	; 8956	BD6:	GEN AR-BR-1,DISP/DIV,J/BD4	;COMPARE BINARY TO 10**N
						; 8957	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- BINARY TO DECIMAL CONVERSION			

						; 8958	;HERE HAVING FOUND THE NUMBER OF DIGITS REQUIRED TO REPRESENT THE
						; 8959	; GIVEN INTEGER.  THE ONE'S COMPLEMENT OF THE NUMBER OF DIGITS IS NOW
						; 8960	; IN SC, AND T1/T2 IS GETTING A POWER OF TEN LARGER THAN THE INPUT.
						; 8961	
						; 8962	=0*
U 1660, 3364,2301,2007,0000,0000,1050,0172	; 8963	BD7:	T2_AR,AR_1S,CALL,J/GETSC	;SAVE (10**N), GET -# OF DIGITS
U 1662, 3465,2543,0207,0000,0000,1010,0170	; 8964		SLEN_AR,ARX_AR*4 COMP		;-# OF SIGNIFICANT DIGITS-1
U 3465, 3466,3200,2005,0000,0020,0010,0000	; 8965		AR_AC3				;GET FLAGS, LENGTH
U 3466, 3467,0001,0000,2421,0000,0110,0000	; 8966		FE_AR0-8,AR0-8_#,#/0		;LEN IN AR, FLAGS IN FE
						; 8967		AR_ARX*.25-AR-1,SKP CRY0,	;-# OF FILL CHARS -1
U 3467, 2744,1113,7000,5032,0040,5410,0400	; 8968			SC_FE-#,#/400		;SC0 SET IF S FLAG =0
U 2744, 3470,4003,0200,0000,0021,0010,0030	; 8969	=0	ARX_AR+1,AR_0.M,J/BD8		;ENOUGH SPACE. -FILL CNT TO ARX
U 2745, 0073,4001,0000,0000,0217,0010,0000	; 8970		I FETCH,J/NOP			;OVERFLOW
						; 8971	BD8:	AR0-8_FE.M,SKP SC0,		;FLAGS TO AR.  S FLAG =0?
U 3470, 1616,2013,0000,0000,2021,4732,0200	; 8972			GEN ARX COMP,SIGNS DISP	; OR EXACT LENGTH?
U 1616, 3471,3260,0007,0000,0332,0010,0173	; 8973	=1110	VMA_FM[FILL],LOAD AR,J/BDF1	;[344] Must fill. GET FILLER
U 1617, 3472,4001,0005,0000,0000,1010,0000	; 8974	BD9:	AC3_AR,J/BDDV1			;NO FILL.  FLAGS TO AC3
						; 8975	
U 3471, 1430,0001,0007,0000,0000,1010,0166	; 8976	BDF1:	T0_AR				;[344] Save flags in T0
U 1430, 3003,3200,0003,0000,0022,1650,0203	; 8977	=00	AR_MEM,SR_BDF,CALL,J/RET1	;GET FILLER, GO WAIT FOR PARITY
U 1431, 2722,0001,4007,0000,2000,1050,0173	; 8978		FILL_AR,AR_ARX,CALL,J/MOVF2	;FILL AS REQUIRED
U 1433, 1617,3200,2007,0000,0020,0010,0166	; 8979	=11	AR_T0,J/BD9			;GET FLAGS BACK
						; 8980	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- BINARY TO DECIMAL CONVERSION			

						; 8981	;SETUP FOR LONG DIVISION OF BINARY BY 10**N
						; 8982	;BR STILL HAS BINARY RIGHT ALIGNED (IE, LOW SIGN SQUEEZED OUT BY
						; 8983	; SHIFTING HIGH WORD RIGHT).  BR IS POSITIVE UNLESS INPUT INTEGER WAS
						; 8984	; 1B0, IN WHICH CASE BR IS -1B1.  T1,T2 HAS LARGER POWER OF TEN, UNLESS
						; 8985	; BINARY EXCEEDS 10**21, IN WHICH CASE T1,T2 CONTAINS 10**21. SINCE
						; 8986	; BINARY CANNOT BE AS LARGE AS 2 * 10**21, THE FIRST DIVIDE STEP
						; 8987	; IS GUARANTEED TO GENERATE A 1 IN THIS CASE ONLY, AND TO REDUCE THE
						; 8988	; BINARY TO LESS THAN 10**21.
						; 8989	
U 3472, 2346,3200,0207,0000,0020,0410,0172	; 8990	BDDV1:	ARX_T2,CLR AR			;FILL DONE.  GET 10**N
						; 8991	=110	AR_T1,MQ_AR,			;D'SOR SET IN AR, MQ CLR
U 2346, 2350,3240,2017,0000,1020,4250,0171	; 8992			SKP BR0,CALL,J/BDDV2	; CHK D'END SIGN
U 2347, 3473,3200,2400,0000,1020,1110,0100	; 8993		ARX_AR,AR_AC0,SET FPD		;DONE, GET FULL QUO IN AR LONG
U 3473, 3474,4001,2600,0000,0020,1627,0010	; 8994		AR_AR+1 LONG,SR_BDT,J/BDD1	;PREVENT 9'S DISEASE
						; 8995	
						; 8996	=000
						; 8997	BDDV2:	AR_BR LONG,BR_AR LONG,		;BEGIN LONG DIVISION
						; 8998			SC_#,FE_#,#/34.,	;STEP COUNTS FOR BOTH PARTS
U 2350, 0722,3242,2660,0303,0000,0050,0042	; 8999			CALL,J/DDVSUB
						; 9000		AR_-BR,ARX/ADX,BR_AR LONG,	;HERE IF BINARY WAS 1B0
						; 9001			SC_#,FE_#,#/34.,	; IT'S NOW 1B1
U 2351, 0722,5162,2660,0303,0020,0050,0042	; 9002			CALL,J/DDVSUB
						; 9003	=011	AC0_AR,AR_MQ,ARL/AD,MQ_0.M,	;HALF DONE WITH DIVISION
U 2353, 0720,3723,2000,2401,0001,1010,0102	; 9004			FE_SC,J/DDVLP		;RESUME WITH ADD STEP
						; 9005	=101	AC0_AR,AR_MQ,ARL/AD,MQ_0.M,
U 2355, 0722,3721,2000,2401,0001,1010,0102	; 9006			FE_SC,J/DDVSUB		;RESUME WITH SUBTRACT STEP
						; 9007	=
						; 9008	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- BINARY TO DECIMAL CONVERSION			

						; 9009	;HERE WITH QUOTIENT OF <INPUT INTEGER>/<10**N> IN AR LONG, WITH THE
						; 9010	; BINARY POINT BETWEEN BITS 0 AND 1 OF AR.  THUS, BIT 0 WILL BE SET
						; 9011	; IFF THE INPUT INTEGER WAS GREATER THAN OR EQUAL TO 10**21.
						; 9012	; SINCE THIS IS A TRUNCATED FRACTION, IT IS NOT GREATER THAN THE TRUE
						; 9013	; QUOTIENT, AND THE ERROR IS LESS THAN 2**-71. WE ADD 2**-71, TO
						; 9014	; GUARANTEE THAT OUR FRACTION IS GREATER THAN THE TRUE QUOTIENT,
						; 9015	; WITH AN ERROR NO GREATER THAN 2**-71.  WE WILL THEN MULTIPLY THIS
						; 9016	; FRACTION BY 10 N TIMES, REMOVING THE INTEGER PART AT EACH STEP
						; 9017	; TO EXTRACT THE N DIGITS.  SINCE N IS AT MOST 21, THIS IS A MULTIPLI-
						; 9018	; CATION BY AT MOST 10**21, SO THE ERROR IS AT MOST (2**-71)*(10**21).
						; 9019	; SINCE THIS IS LESS THAN ONE, THE ERROR DOES NOT INTRUDE INTO THE
						; 9020	; OUTPUT DIGIT STRING.
						; 9021	
						; 9022	;HERE IS LOOP TO EXTRACT DIGITS FROM FRACTION IN AC0,AC1
						; 9023	
						; 9024	BDD1:	BR_AR LONG,VMA_PC+1,		;START NEXT LOOP ITERATION
U 3474, 2746,4640,2067,0000,0240,5410,0170	; 9025			AR_SLEN+1,SKP CRY0	;ANY MORE DIGITS?
						; 9026	=0					;HERE TO RESUME AFTER INTERRUPT
						; 9027	BDDR4:	SLEN_AR,MQ_AR,SC_1,		;YES, SAVE LENGTH REMAINING
						; 9028			AR_BR LONG,		; AND GET FRACTION
U 2746, 1635,3202,2617,4402,1020,1032,0170	; 9029			SIGNS DISP,J/BDD2	;CHECK FOR 1ST DIGIT OF 10**21
						; 9030		AR_0S,ARX_0S,CLR FPD,		;NO, DONE.  CLEAR AC0 & AC1
U 2747, 3475,3441,2200,0000,0000,3614,0000	; 9031			VMA_VMA+1
U 3475, 2124,0001,0000,0000,0017,1010,0000	; 9032		AC0_AR,FETCH,J/STRAC1		;MOVE FETCH WHEN TIMING FIXED
						; 9033	=1101					;LOOK AT BR0 ONLY
U 1635, 1637,0603,7700,0302,0020,0027,0004	; 9034	BDD2:	AR_AR*1.25 LONG,SC_#,#/4	;NEXT DIGIT TO AR0-3
U 1637, 2750,3401,2400,0000,1000,7010,0000	; 9035		ARX_AR,AR_0S,SKP INTRPT		;READY TO SHIFT IN DIGIT
U 2750, 2752,0001,4000,0000,0000,0033,0000	; 9036	=0	AR_SHIFT,B DISP,J/BDD3		;STORE IT
U 2751, 3763,3242,2600,0000,0000,1610,0000	; 9037		AR_BR LONG,SR_0,J/B2DPF		;UPDATE REGS & QUIT
						; 9038	
						; 9039	;HERE TO STORE DIGIT IN AR FOR BDEC
						; 9040	=0
U 2752, 3476,0600,0007,4000,0332,0010,0165	; 9041	BDD3:	VMA_AR+E1,LOAD AR,J/BDD4	;TRANSLATE: GET TABLE ENTRY
U 2753, 2364,0600,2007,0000,0020,0010,0165	; 9042		AR_AR+E1,J/BDD7			;OFFSET AR AND STORE IT
						; 9043	
U 3476, 2754,6023,0000,0000,0021,5410,0040	; 9044	BDD4:	SKP MQ EQ -1,TIME/3T,ARX_0.M	;LAST DIGIT?
						; 9045	=0
U 2754, 3477,3200,0003,0000,0022,0010,0000	; 9046	BDD5:	AR_MEM,J/BDD6			;NO, STORE RH (POS DIGIT)
U 2755, 2754,3200,0205,0000,0020,0010,0000	; 9047		ARX_AC3,J/BDD5			;YES, LOOK AT M FLAG
U 3477, 2364,3243,0400,0000,3021,5310,0020	; 9048	BDD6:	SKP ARX2,ARX_AR SWAP,ARL_0.M
						; 9049	=100
U 2364, 1540,0001,0000,0000,0000,1650,0206	; 9050	BDD7:	SR_BDD,CALL,J/PUTDST
U 2365, 2364,4001,4000,0000,2001,0010,0020	; 9051		AR_ARX,ARL_0.M,J/BDD7		;M SET ON LAST DIGIT, USE LH
						; 9052	
						; 9053		AR_BR LONG,SR_BDT,		;GET FRACTION BACK
U 2366, 1655,3242,2600,0000,0020,1632,0010	; 9054			SIGNS DISP		;CHECK BR0 FOR INTEGER PART
						; 9055	=
U 1655, 1657,0603,5500,0000,0020,0027,0000	; 9056	=1101	AR_AR*10 LONG			;DISCARD PREVIOUS DIGIT
U 1657, 3474,0001,0000,7130,3000,0110,0037	; 9057		P_P AND #,#/37,J/BDD1		;CLEAR AR0, GO FOR NEXT
						; 9058	.ENDIF/EXTEND
						; 9059	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE

						; 9060	.TOC	"EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE"
						; 9061	.IF/EXTEND
						; 9062	
						; 9063	;SLEN = COMPLEMENT OF LENGTH
						; 9064	;MSK = MASK
						; 9065	;E1 = EFFECTIVE ADDRESS OF OPERATION WORD (SIGN EXTENDED IF OFFSET)
						; 9066	
						; 9067	;CALL WITH:	AR_SLEN+1,CALL,J/SRCMOD
						; 9068	;RETURNS:	1 LENGTH EXHAUSTED: FLAGS IN AR
						; 9069	;		2 (EDIT ONLY) NO SIGNIFICANCE: FLAGS IN FE
						; 9070	;		3 (EDIT ONLY) SIGNIFICANCE START: BYTE IN AR, FLAGS IN FE
						; 9071	;		4 NORMAL: BYTE IN AR
						; 9072	;		5 ABORT: OUT OF RANGE OR TRANSLATE FAILURE
						; 9073	;	BR, BRX, PRESERVED.
						; 9074	;	B=0 IF TRANSLATE, =1 IF OFFSET MODE, =2 IF EDIT, =4 IF CVTDBT
						; 9075	
						; 9076	;[266] Remove edit 244
						; 9077	;;[244]	THIS ADDRESS MUST REMAIN FOR THE PROBLEM OF THE
						; 9078	;;	S FIELD OF THE SOURCE POINTER BEING GREATER THAT 36.
						; 9079	;
						; 9080	;.IF/MODEL.B
						; 9081	;1200:					;[244]
						; 9082	;SRCMOD:	SLEN_AR,AR+ARX+MQ_0.M,CALL.M,	;PUT LENGTH AWAY, GET BYTE
						; 9083	;		SIGNS DISP,J/GSRC	;CHECK FOR LENGTH EXHAUSTION
						; 9084	;1201:	AR_SFLGS,SR_0,RETURN1		;LEN =0, DONE
						; 9085	;1202:	E1,TIME/2T,B DISP		;BYTE IN AR
						; 9086	;1206:	AR_AR*.5 LONG,E1,J/XLATE	;LOW BIT TO ARX0, BYTE/2 TO AR LOW
						; 9087	;1207:	AR_AR+E1,TIME/3T		;OFFSET, ADD OFFSET, TEST MASK
						; 9088	;	TEST AR.MSK,SKP CRY0,RETURN4	;RETURN 4 IF OK, 5 OUT OF RANGE
						; 9089	;.IFNOT/MODEL.B				;[244][266]
						; 9090	=000
						; 9091	SRCMOD:	SLEN_AR,AR+ARX+MQ_0.M,CALL.M,	;PUT LENGTH AWAY, GET BYTE
U 2370, 1753,0001,0007,0000,0021,1072,0170	; 9092			SIGNS DISP,J/GSRC	;CHECK FOR LENGTH EXHAUSTION
U 2371, 0001,3200,2000,0000,0020,1603,0000	; 9093		AR_SFLGS,SR_0,RETURN1		;LEN =0, DONE
U 2372, 2376,0001,0007,0000,0000,0033,0165	; 9094		E1,TIME/2T,B DISP		;BYTE IN AR
U 2376, 3501,0301,7707,0000,0020,0027,0165	; 9095	=110	AR_AR*.5 LONG,E1,J/XLATE	;LOW BIT TO ARX0, BYTE/2 TO AR LOW
U 2377, 3500,0600,2007,0000,0020,0010,0165	; 9096		AR_AR+E1,TIME/3T		;OFFSET, ADD OFFSET, TEST MASK
U 3500, 0004,3600,0007,4000,0040,5403,0167	; 9097		TEST AR.MSK,SKP CRY0,RETURN4	;RETURN 4 IF OK, 5 OUT OF RANGE
						; 9098	;.ENDIF/MODEL.B		;[244][266]
						; 9099	
						; 9100	;HERE ON TRANSLATE-MODE OPERATIONS, WITH THE BYTE/2 IN AR, AND
						; 9101	; THE LEAST SIGNIFICANT BIT OF THE BYTE IN ARX0.  PERFORM THE
						; 9102	; TABLE LOOKUP, AND OPERATE AS CONTROLLED BY THE HIGH THREE BITS
						; 9103	; OF THE TABLE ENTRY.
						; 9104	
U 3501, 3502,0600,0007,0000,0332,0010,0165	; 9105	XLATE:	VMA_AR+E1,LOAD AR		;GET FUNCTION FROM TABLE
						; 9106	
U 3502, 2756,3240,0003,0302,0022,4310,0022	; 9107	TRNAR:	AR_MEM,SKP ARX0,SC_#,#/18.	;WHICH HALF?
						; 9108	=0	ARX_AR,AR0-3 DISP,		;LH, MOVE TO ARX LEFT
U 2756, 1661,3240,2400,0000,1040,0007,0000	; 9109			AR_SFLGS,J/TRNFNC
						; 9110		ARX_AR SWAP,AR18-21 DISP,	;RH, MOVE THAT TO ARX LEFT
U 2757, 1661,3200,2400,0000,3040,0007,0000	; 9111			AR_SFLGS,J/TRNFNC
						; 9112	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 14
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE

						; 9113	;HERE ON TRANSLATE OPERATION TO PERFORM FUNCTIONS REQUIRED BY
						; 9114	; THE 3 HIGH ORDER BITS OF THE TRANSLATE FUNCTION HALFWORD.
						; 9115	; WE HAVE DISPATCHED ON THOSE THREE BITS, WITH THE FUNCTION
						; 9116	; HALFWORD IN LH(ARX), AND THE FLAGS FROM AC0 IN AR.
						; 9117	
						; 9118	=0001
						; 9119	TRNFNC:	SFLGS_AR,FE_P,AR_SHIFT,		;SAVE FLAGS, GET FCN IN AR RIGHT
U 1661, 1713,4001,4000,0101,0020,1032,0000	; 9120			SIGNS DISP,J/TRNRET	;WAS S FLAG ALREADY SET?
U 1663, 0005,0001,0000,7131,0000,1003,0003	; 9121	TRNABT:	SFLGS_AR,FE_P AND #,#/3,RETURN5	;ABORT
U 1665, 1661,4001,0000,7130,3000,0110,0067	; 9122		P_P AND #,#/67,J/TRNFNC		;CLEAR M FLAG
U 1667, 1661,0001,0000,6130,3000,0110,0010	; 9123		P_P OR #,#/10,J/TRNFNC		;SET M FLAG
U 1671, 1661,0001,0000,6130,3000,0110,0020	; 9124	TRNSIG:	P_P OR #,#/20,J/TRNFNC		;SET N FLAG
U 1673, 1663,4001,0000,6130,3000,0110,0020	; 9125		P_P OR #,#/20,J/TRNABT		;SET N AND ABORT
U 1675, 1671,0001,0000,7130,3000,0110,0067	; 9126		P_P AND #,#/67,J/TRNSIG		;CLEAR M, THEN SET N
U 1677, 1661,4001,0000,6130,3000,0110,0030	; 9127		P_P OR #,#/30,J/TRNFNC		;SET N AND M
						; 9128	
						; 9129	=1011
						; 9130	TRNRET:	ARX_AR*MSK,AD/AND,		;S FLAG IS 0, GET BYTE IN AR
U 1713, 2404,3600,0207,4000,0020,4433,0167	; 9131			SKP AR18,B DISP,J/TRNSS	;IS THIS EDIT?
U 1717, 0004,3600,2007,4000,0020,0003,0167	; 9132		AR_AR*MSK,AD/AND,RETURN4	;RETURN NORMAL SINCE S FLAG SET
						; 9133	
						; 9134	=100
U 2404, 0150,3240,2005,0000,0020,0033,0000	; 9135	TRNSS:	AR_DLEN,B DISP,J/TRNNS1		;NO SIG ON MOVE OR D2B
U 2405, 3504,3200,2000,0302,0020,0010,0040	; 9136		AR_SFLGS,SC_#,#/40,J/TRNSS1	;SIG START, SET FLAG
U 2406, 0002,4640,0007,0000,0332,0003,0176	; 9137		VMA_E0+1,LOAD AR,RETURN2	;EDIT NO SIG.  GET FILL
						;;9138	.IFNOT/MODEL.B
						;;9139		AR_DSTP,FE_#,#/36.,RETURN3	;EDIT SIG START
						; 9140	.IF/MODEL.B
U 2407, 0003,3200,2006,0301,0020,0003,0144	; 9141		AR_DSTP,FE_#,#/144,RETURN3	;EDIT SIG START
						; 9142	.ENDIF/MODEL.B
						; 9143	
						; 9144	=0**
U 0150, 3503,1703,2000,0000,0020,0010,0000	; 9145	TRNNS1:	AR_AR-1,J/TRNNS2		;COMPENSATE FOR IGNORING SRC
U 0154, 2370,4660,2007,0000,0020,0010,0170	; 9146		AR_SLEN+1,J/SRCMOD		;D2B HAS NO DEST LENGTH
U 3503, 1733,4001,0005,0000,0020,1032,0000	; 9147	TRNNS2:	DLEN_AR,SIGNS DISP
U 1733, 2370,3200,2007,0000,0020,0010,0170	; 9148	=1011	AR_SLEN,J/SRCMOD		;SLEN = DST LEN, DON'T CHANGE IT
U 1737, 2370,4660,2007,0000,0020,0010,0170	; 9149		AR_SLEN+1,J/SRCMOD		;SLEN REFLECTS SRC LENGTH
						; 9150						; COUNT DOWN FOR BYTE SKIPPED
U 3504, 3505,0001,0000,6100,3001,0010,0200	; 9151	TRNSS1:	P_P OR SC
U 3505, 0004,0001,4000,0000,2000,1003,0000	; 9152		SFLGS_AR,AR_ARX,RETURN4		;RETURN WITH SIG SET
						; 9153	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE

						; 9154	;SUBROUTINE TO GET BYTE FROM SOURCE STRING
						; 9155	; CALL GSRC WITH SIGNS DISP TO CHECK FOR LENGTH EXHAUSTION
						; 9156	; [TIME = 17 + 3(BP OVERFLOW)]
						; 9157	
						; 9158	=1011
U 1753, 0001,3240,2005,0000,0020,0003,0000	; 9159	GSRC:	AR_DLEN,RETURN1			;LEN RAN OUT
U 1757, 2760,3200,2201,0301,0040,0010,0044	; 9160	GETSRC:	AR_SRCP,ARX_SRCP,FE_#,#/36.
						; 9161	;[266] Remove edit 244
						; 9162	;.IF/MODEL.B
						; 9163	;	GEN FE-S,SKP SCAD0		;[244] IS S > 36 ?
						; 9164	;=0	J/GSRC1				;[244] NO, GO BELOW
						; 9165	;	DISP/RETURN,J/501		;[244] YES, TRICKY WAY TO
						; 9166	;					;[244] GET OUT
						; 9167	;;[244]	THIS IS DONE THIS WAY SO THAT WE CAN TAKE THE ERROR
						; 9168	;;	RETURN OF THE EXTEND INSTRUCTION. THE TWO PLACES THAT
						; 9169	;;	CALL GSRC ARE SET SO THAT A RETURN WITH J FIELD OF 500
						; 9170	;;	WILL GO TO HERE.
						; 9171	;1701:	RETURN5				;[244] ERROR RETURN
						; 9172	;.ENDIF/MODEL.B		;[244][266]
						; 9173	=0
						; 9174	GSRC1:	P_P-S,SC/SCAD,VMA_PC,CALL.M,	;[352] Increment pointer, init VMA
U 2760, 2414,4001,0000,5112,3121,0074,0200	; 9175			BYTE DISP,J/GSRC2	; section, test word overflow
U 2761, 2656,4001,0000,2002,0000,7010,0000	; 9176		SC_FE+SC,SKP INTRPT,J/LDB1	;GET BYTE & RETURN TO CALLER
						;;9177	.IFNOT/XADDR
						;;9178	=110
						; 9179	.IF/XADDR
						; 9180	=100
						; 9181	.ENDIF/XADDR
						; 9182	GSRC2:	SRCP_AR,ARX_AR,FE_S,		;[352] STORE POINTER,
U 2414, 1354,2341,0401,2411,1000,1036,0000	; 9183			EA MOD DISP,J/BFETCH	; GO EVALUATE THE ADDRESS
						; 9184	GSRC3:	ARR_AR+1,ARX/AD,INH CRY18,	;[352] Update address for ARX (used
U 2415, 2414,4003,2200,5012,3020,0611,0200	; 9185			P_FE-S,SC/SCAD,J/GSRC2	; in EA MOD DISP) and set P
						; 9186	.IF/XADDR
U 2416, 1452,3240,0204,0000,0020,5010,0000	; 9187		ARX_SRCP2,SKP PC SEC0,J/GSRC4	;GET ADDR PART OF POINTER
U 2417, 2762,3240,2404,0000,1020,5010,0000	; 9188		ARX_AR,AR_SRCP2,SKP PC SEC0
U 2762, 2764,4001,2000,0101,0020,4516,0000	; 9189	=0	FE_P,AR_AR+1-AR0,SKP AR0,J/GSRC5
U 2763, 2415,0001,4000,0000,2000,0010,0000	; 9190		AR_ARX,J/GSRC3			;OOPS, SEC 0 IS COMPATABLE
						; 9191	=0
U 2764, 1450,4001,0000,0000,3001,0010,0200	; 9192	GSRC5:	P_FE,J/GSRC6			;EFIW, INCR ALL BUT 0-5
U 2765, 1450,4003,2000,0000,0020,0011,0000	; 9193		AR_AR+1,INH CRY18		;IFIW, INCR RIGHT HALF ONLY
						; 9194	=00
						; 9195	GSRC6:	SRCP2_AR,AR_ARX,ARX_AR (AD),	;SAVE ADDR PART
U 1450, 3506,3701,4204,0000,2000,1050,0000	; 9196			CALL,J/RESETP		;GO SET P TO 36-S
						; 9197	=10
U 1452, 2654,4001,0001,2411,0000,1010,0000	; 9198	GSRC4:	SRCP_AR,FE_S,J/BYTEI		;[352] GO EVALUATE LONG POINTER
U 1453, 1354,2341,0401,2411,1000,1036,0000	; 9199		SRCP_AR,ARX_AR,FE_S,EA MOD DISP,J/BFETCH
						; 9200	
						; 9201	;SUBROUTINE TO LOAD P FROM 36-S
						; 9202	
U 3506, 0002,0001,0000,5312,3000,0103,0044	; 9203	RESETP:	P_#-S,#/36.,SC/SCAD,RETURN2	;START P BACK AT LEFT EDGE
						; 9204	.ENDIF/XADDR
						; 9205	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE

						; 9206	;SUBR TO STORE AR IN DEST STRING
						; 9207	; [TIME = 24 + 3(BP OVERFLOW)]
						; 9208	
						; 9209	=00
						;;9210	.IFNOT/MODEL.B
						;;9211	PUTDST:	MQ_AR,AR_DSTP,ARX_DSTP,
						;;9212			FE_#,#/36.,CALL,J/IDST
						; 9213	.IF/MODEL.B
U 1540, 3507,3240,2216,0000,1040,0050,0144	; 9214	PUTDST:	MQ_AR,AR_DSTP,ARX_DSTP,CALL,J/IDST
						; 9215	.ENDIF/MODEL.B
						; 9216		AR_MQ,SC_#-SC,#/36.,SKP SCAD0,
U 1541, 2664,3721,2000,5302,0020,5150,0044	; 9217			CALL,J/DPB1
U 1543, 0006,4001,0003,0000,0002,0003,0000	; 9218	=11	MEM_AR,RETURN6
						; 9219	
						; 9220	;SUBROUTINES TO UPDATE STRING POINTERS
						; 9221	
						; 9222	IDST:	VMA_PC,P_P-S,SC/SCAD,BYTE DISP,	;[352] Init VMA section and
U 3507, 2424,0001,0000,5112,3121,0034,0200	; 9223			J/IDST2			; TEST FOR WORD OVERFLOW
						;;9224	.IFNOT/XADDR
						;;9225	  .IFNOT/MODEL.B
						;;9226	=110
						;;9227	IDST2:	DSTP_AR,ARX_AR,FE_S,		;STORE POINTER,
						;;9228			EA MOD DISP,J/BFETCH	; GO GET THE WORD ADDRESSED
						;;9229		AR_AR+1,P_FE-S,SC/SCAD,J/IDST2
						;;9230	  .IF/MODEL.B
						;;9231	=110
						;;9232	IDST2:	SEL DSTP,ARX_AR,J/IDST2B	;[352] PRESEL #, fix ARX address
						;;9233	IDST2A:	FE_#,#/36.			;COULDN'T LOAD FE EARLIER
						;;9234		AR_AR+1,P_FE-S,SC/SCAD,J/IDST2
						;;9235	IDST2B:	DSTP_AR,ARX_AR,FE_S,		;[352] STORE POINTER,
						;;9236			EA MOD DISP,J/BFETCH	; GO GET THE WORD ADDRESSED
						;;9237	  .ENDIF/MODEL.B
						; 9238	.IF/XADDR
						; 9239	=100
U 2424, 3513,4001,0406,0000,1000,0010,0144	; 9240	IDST2:	DSTP,ARX_AR,J/IDST2B		;[352] PRESEL #, fix ARX address
U 2425, 3510,4001,2000,0000,0020,0011,0000	; 9241		AR_AR+1,INH CRY18,J/IDST3
U 2426, 1552,3240,0206,0000,0020,5010,0145	; 9242		ARX_DSTP2,SKP PC SEC0,J/IDST4	;GET ADDR PART OF POINTER
U 2427, 2766,3240,2406,0000,1020,5010,0145	; 9243		ARX_AR,AR_DSTP2,SKP PC SEC0
U 2766, 2770,4001,2000,0101,0020,4516,0000	; 9244	=0	FE_P,AR_AR+1-AR0,SKP AR0,J/IDST5
U 2767, 3510,4013,2000,0000,0020,0011,0000	; 9245		AR_ARX+1 (AD),INH CRY18
U 3510, 2424,0001,0000,5312,3000,0110,0044	; 9246	IDST3:	P_#-S,#/36.,SC/SCAD,J/IDST2	;GO STORE SHORT POINTER AWAY
						; 9247	=0
U 2770, 1550,4001,0000,0000,3000,0110,0145	; 9248	IDST5:	P_FE.C,SEL DSTP2,J/IDST6	;PRESEL # TO FIX HARDW GLITCH
U 2771, 1550,4001,2000,0000,0020,0011,0145	; 9249		AR_AR+1,INH CRY18,SEL DSTP2
						; 9250	=00
						; 9251	IDST6:	DSTP2_AR,AR_ARX,ARX_AR (AD),	;INCR ADDR PART
U 1550, 3506,3703,4206,0000,2000,1050,0145	; 9252			CALL,J/RESETP		;GET P BACK TO 36-S
						; 9253	=10
U 1552, 3511,0001,0000,0000,0000,0010,0144	; 9254	IDST4:	SEL DSTP,J/IDST7		;PRESEL # TO PREVENT HARDW GLITCH
U 1553, 3512,0001,0000,0000,0000,0010,0144	; 9255		SEL DSTP,J/IDST8		;PRESEL # TO PREVENT HARDW GLITCH
						; 9256	
U 3511, 2654,4001,0006,2411,0000,1010,0144	; 9257	IDST7:	DSTP_AR,FE_S,J/BYTEI
						; 9258	IDST8:	DSTP_AR,ARX_AR,FE_S,		;[352][300]
U 3512, 1354,2341,0406,2411,1000,1036,0144	; 9259			EA MOD DISP,J/BFETCH
						; 9260	
						; 9261	IDST2B:	DSTP_AR,ARX_AR,FE_S,		;[352][300]STORE POINTER,; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- SRCMOD SUBROUTINE TO GET MODIFIED SOURCE BYTE

U 3513, 1354,2341,0406,2411,1000,1036,0144	; 9262			EA MOD DISP,J/BFETCH	; GO GET THE WORD ADDRESSED
						; 9263	.ENDIF/XADDR
						; 9264	.ENDIF/EXTEND
						; 9265	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 17
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9266	.TOC	"EIS -- EDIT FUNCTION"
						; 9267	.IF/EXTEND
						; 9268	;	HERE WITH E0, E1 SETUP, 0 IN AR, -1 IN ARX, AND 15 IN SC
						; 9269	
						; 9270	.IF/OWGBP
						; 9271	=0***					;[347]
U 0465, 2020,0001,0000,0000,0000,0050,0000	; 9272	EDIT:	CALL [EXT2WD]			;CHECK FOR OWGBP
U 0475, 3514,2341,0200,0302,0000,0410,0017	; 9273		CLR AR,ARX_1S,SC_#,#/15.	;SETUP FOR SHIFT
U 3514, 3515,3200,4200,0000,0020,1610,0000	; 9274		AR_SHIFT,ARX_AC0,SR_ED(PAT)	;MASK TO AR, FLAGS ETC TO ARX
						;;9275	.IFNOT/OWGBP
						;;9276	EDIT:	AR_SHIFT,ARX_AC0,SR_ED(PAT)	;MASK TO AR, FLAGS ETC TO ARX
						; 9277	.ENDIF/OWGBP
						;;9278	.IFNOT/MODEL.B
						;;9279	=1*0	MSK_AR,AR_ARX (AD),		;SAVE MASK, GET FLAGS IN AR
						;;9280			VMA_ARX,LOAD AR,	;GET FIRST PATTERN OPERATOR
						;;9281			CALL,J/TRNABT		;GET PBN INTO FE
						; 9282	.IF/MODEL.B
U 3515, 3516,0001,0007,0000,0000,1010,0167	; 9283		MSK_AR				;SAVE MASK FOR TRAN FUNC
U 3516, 2772,4001,4000,0000,2001,5010,0020	; 9284		AR_ARX,ARL_0.M,SKP PC SEC0	;DO WE ALLOW SECTION #?
U 2772, 3517,3713,4000,0000,2312,0010,0000	; 9285	=0	VMA_ARX,LOAD AR,AR_ARX,J/EDIT1	;YES.  PROVIDE IT
U 2773, 3517,3701,4000,0000,2312,0010,0000	; 9286		VMA_AR,LOAD AR,AR_ARX		;NO, GIVE 0
U 3517, 3520,4001,0000,7131,0000,0010,0003	; 9287	EDIT1:	FE_P AND #,#/3			;GET PBN IN FE
						; 9288	.ENDIF/MODEL.B
						; 9289	EDITLP:	SC_# AND AR0-8,#/30,		;PBN*8 IN SC
U 3520, 3521,4001,0400,7322,1000,1010,0030	; 9290			SFLGS_AR,ARX_AR		;UPDATED AC NOW IN AC AND ARX
U 3521, 3522,3240,0003,2002,0022,0010,0000	; 9291		AR_MEM,SC_FE+SC			;PATTERN IN AR, PBN*9 IN SC
U 3522, 2001,0001,4000,0302,0020,0007,0005	; 9292		AR_SHIFT,SH DISP,SC_#,#/5	;PATTERN BYTE TO AR0-8,
						; 9293	=0001					; DISP ON HIGH 3 BITS
						; 9294	EDDISP:	GEN #+AR0-8,#/-5,
U 2001, 2774,4001,0000,2320,0020,5110,0773	; 9295			SKP SCAD0,J/EDOPR	;(0XX) OPERATE GROUP
U 2003, 1610,3203,5000,0000,0000,4310,0000	; 9296		AR_AR*8,SKP ARX0,J/EDMSG	;(1XX) MESSAGE
U 2005, 3000,4001,0000,0000,0000,0010,0000	; 9297		J/EDNOP				;(2XX) UNDEFINED
U 2007, 3000,4001,0000,0000,0000,0010,0000	; 9298		J/EDNOP				;(3XX) UNDEFINED
U 2011, 3000,4001,0000,0000,0000,0010,0000	; 9299		J/EDNOP				;(4XX) UNDEFINED
						; 9300		MQ_ARX,ARX_ARX*4,
U 2013, 3525,3203,0610,4002,2000,0010,0000	; 9301			SC_FE+1,J/EDSKPT	;(5XX) SKIP IF MINUS
						; 9302		MQ_ARX,ARX_ARX*2,
U 2015, 3525,3721,0510,4002,2000,0010,0000	; 9303			SC_FE+1,J/EDSKPT	;(6XX) SKIP IF NON-ZERO
U 2017, 3001,3203,5000,4002,0000,0010,0000	; 9304		AR_AR*8,SC_FE+1,J/EDSKP		;(7XX) SKIP ALWAYS
						; 9305	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 18
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9306	;HERE TO DECODE OPERATE GROUP
						; 9307	=0
U 2774, 3000,4001,0000,0000,0000,0010,0000	; 9308	EDOPR:	J/EDNOP				;OPR .GE. 005 UNDEFINED
U 2775, 2430,0001,0000,0000,0020,0007,0000	; 9309		SH DISP,J/OPDISP		;(00X), DISP ON LOW 3 BITS
						; 9310	=000
						; 9311	OPDISP:	AR_ARX,SC_#,#/-4,		;(000) STOP
U 2430, 3523,4001,4000,0302,2200,0010,0774	; 9312			VMA_PC+1,J/EDSTOP
U 2431, 1664,0001,0000,0000,0000,1610,0101	; 9313		SR_ED(S),J/EDSEL		;(001) SELECT
U 2432, 1570,3240,2006,0000,0020,4310,0144	; 9314		AR_DSTP,SKP ARX0,J/EDSSIG	;(002) START SIGNIFICANCE
U 2433, 3531,0001,4000,0000,2000,0010,0000	; 9315		AR_ARX,J/EDFLDS			;(003) FIELD SEPARATOR
						;;9316	.IFNOT/MODEL.B
						;;9317		VMA_AC3,LOAD ARX,		;(004) EXCH MARK AND DEST
						;;9318			MQ_ARX,J/EDEXMD
						; 9319	.IF/MODEL.B
						;;9320	.IFNOT/XADDR
						;;9321		AR_DSTP,MQ_ARX,J/EDEXMD		;(004) EXMD
						; 9322	.IF/XADDR
						; 9323		AR_DSTP,ARX/AD,MQ_ARX,		;(004) EXMD
U 2434, 3030,3240,2216,0000,2020,5010,0144	; 9324			SKP PC SEC0,J/EDEX0
						; 9325	.ENDIF/XADDR
						; 9326	.ENDIF/MODEL.B
						; 9327	=
						; 9328	;HERE TO TERMINATE EDIT INSTRUCTION
						; 9329	; SC HAS -4, FE HAS CURRENT PBN, VMA HAS PC IF ABORT, PC+1 IF DONE
						; 9330	
U 3523, 2776,4001,0000,5031,0020,5110,0003	; 9331	EDSTOP:	FE_FE-#,#/3,SKP SCAD0
						; 9332	=0	AR_AR+1,INH CRY18,
U 2776, 3524,4003,2000,7100,3021,0011,0200	; 9333			P_P AND SC,J/SFET1
U 2777, 3524,0001,0000,4100,3000,0110,0000	; 9334		P_P+1
U 3524, 0072,4001,0000,0000,0017,3610,0000	; 9335	SFET1:	FETCH+1,J/STORAC
						; 9336	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 19
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9337	;HERE FOR SKPM & SKPN, WITH APPROPRIATE BIT IN ARX0
						; 9338	
U 3525, 3000,3203,5300,0000,0000,4310,0000	; 9339	EDSKPT:	AR_AR*8,SKP ARX0,ARX/MQ		;SKIP DISTANCE TO AR0-5
						; 9340	
						; 9341	;HERE AT END OF OPERATION TO UPDATE PBN
						; 9342	=0
						; 9343	EDNOP:	FE_FE-#,#/3,SKP SCAD0,		;END OF PATTERN WORD?
U 3000, 3016,4001,4000,5031,2020,5110,0003	; 9344			AR_ARX,J/EDNXT1
U 3001, 3000,0001,0000,2101,0000,0010,0000	; 9345	EDSKP:	FE_P+SC,J/EDNOP			;ADD SKIP DISTANCE
						; 9346	=0
						;;9347	.IFNOT/XADDR
						;;9348	EDNXT1:	AR_AR+1,INH CRY18,		;BUMP TO NEXT WORD
						;;9349			FE_FE-#,#/4,		;REDUCE PBN
						;;9350			SKP SCAD0,J/EDNXT1
						;;9351		SR_ED(PAT)
						;;9352		FE_FE+#,#/4			;RESTORE PBN POS, INCR IT
						;;9353		SC_P AND #,#/74,VMA_AR,LOAD AR,	;FLAGS & EDIT BIT TO SC, GET PATTERN
						;;9354			SKP INTRPT,J/EDNXT3
						; 9355	.IF/XADDR
U 3016, 3022,4001,0000,0000,0000,5010,0000	; 9356	EDNXT1:	SKP PC SEC0,J/EDNXT2
U 3017, 3526,4001,0000,0000,0000,1610,0000	; 9357		SR_ED(PAT)
U 3526, 3020,4001,0000,2031,0000,5010,0004	; 9358		FE_FE+#,#/4,SKP PC SEC0		;RESTORE PBN POS, INCR IT
						; 9359	=0	SC_P AND #,#/74,VMA_AR,LOAD AR,	;FLAGS & EDIT BIT TO SC,
U 3020, 3026,3703,0000,7132,0312,7010,0074	; 9360			SKP INTRPT,J/EDNXT3	; GET PATTERN
U 3021, 3527,0001,0000,7132,0000,0010,0074	; 9361		SC_P AND #,#/74			;IN SEC0, MUST NOT LOAD FULL SEC
U 3527, 3530,4001,0400,0000,1001,0010,0020	; 9362		ARX_AR,ARL_0.M			;CLEAR SEC #
						; 9363		VMA_AR,LOAD AR,AR_ARX,		;GET PATTERN
U 3530, 3026,3701,4000,0000,2312,7010,0000	; 9364			SKP INTRPT,J/EDNXT3
						; 9365	
						; 9366	=0
						; 9367	EDNXT2:	AR_AR+1,FE_FE-#,#/4,		;REDUCE PBN
U 3022, 3016,4001,2000,5031,0020,5110,0004	; 9368			SKP SCAD0,J/EDNXT1
						; 9369		AR_AR+1,INH CRY18,		;BUMP TO NEXT WORD
						; 9370			FE_FE-#,#/4,		;REDUCE PBN
U 3023, 3016,4003,2000,5031,0020,5111,0004	; 9371			SKP SCAD0,J/EDNXT1
						; 9372	.ENDIF/XADDR
						; 9373	=0
U 3026, 3520,0001,0000,6000,3001,0010,0200	; 9374	EDNXT3:	P_FE OR SC,J/EDITLP		;SET NEW PBN, GO DO NEXT PATTERN
U 3027, 3746,4001,0000,6000,3001,0010,0200	; 9375		P_FE OR SC,J/PGFAC0		;GO RESTORE THINGS AND TAKE
						; 9376						; THE INTERUPT
						; 9377	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 20
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9378	;HERE FOR FIELD SEPARATOR (CLEAR FLAGS IN AC 0-2)
						; 9379	
U 3531, 1573,4001,0000,7130,3000,0110,0007	; 9380	EDFLDS:	P_P AND #,#/7,J/EDSEND		;EASY ENOUGH
						; 9381	
						; 9382	;HERE FOR SIG START
						; 9383	
						; 9384	=00
						;;9385	.IFNOT/XADDR
						;;9386	EDSSIG:	VMA_AC3,STORE,CALL,J/EDFLT1	;SAVE MARK, GET FLOAT
						; 9387	.IF/XADDR
						; 9388	EDSSIG:	ARX_AR,VMA_AC3,AR/AD,ARL_0.M,
						; 9389			BYTE DISP,SCADA EN/0S,SCAD/A,
U 1570, 2474,3240,2405,0400,1341,5074,0020	; 9390			CALL,SKP PC SEC0,J/EDFLT
						; 9391	.ENDIF/XADDR
						; 9392		FE_FE-#,#/3,SKP SCAD0,		;S FLAG ALREADY SET, NOP
U 1571, 3016,4001,4000,5031,2020,5110,0003	; 9393			AR_ARX,J/EDNXT1
						; 9394	=11
U 1573, 3000,0001,0400,7131,1000,0010,0003	; 9395	EDSEND:	FE_P AND #,#/3,ARX_AR,J/EDNOP	;READY TO DO NEXT OP
						; 9396	
						; 9397	;HERE FOR MESSAGE CHAR
						; 9398	
						; 9399	=00
U 1610, 2462,4660,0007,0000,0332,0010,0176	; 9400	EDMSG:	VMA_E0+1,LOAD AR,J/EDSFIL	;NO SIG, PUT FILLER
U 1611, 3364,3441,2000,0102,0000,0050,0000	; 9401		SC_P,AR_0S,CALL,J/GETSC		;GET MESSAGE SELECT IN AR
U 1613, 2467,4600,0007,0000,0332,0010,0176	; 9402	=11	VMA_AR+E0+1,LOAD AR,J/EDMPUT	;STORE MESSAGE
						; 9403	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 21
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9404	;HERE TO EXCHANGE MARK AND DESTINATION POINTERS
						; 9405	
						;;9406	.IFNOT/MODEL.B				;EASY CASE
						;;9407	EDEXMD:	AR_DSTP				;READY TO STORE DEST PTR
						;;9408		FIN XFER,STORE			;WAIT FOR MARK, STORE DSTP
						;;9409		MEM_AR,AR_ARX			;READY TO UPDATE DSTP
						;;9410		DSTP_AR,ARX/MQ,J/EDNOP		;DONE, GET NEXT OPR
						; 9411	.IF/MODEL.B
						; 9412	.IF/XADDR
						; 9413	=0
						; 9414	EDEX0:	VMA_AC3,LOAD AR (WR TST),	;GET MARK POINTER
U 3030, 3534,3240,0065,0000,0331,0010,0440	; 9415			BR/AR,BRX/ARX,J/EDEX2	;DSTP IN BR & BRX,
						; 9416	.ENDIF/XADDR
U 3031, 3532,3240,2045,0000,0021,0010,0020	; 9417	EDEXMD:	BR/AR,AR_AC3,ARL_0.M
U 3532, 3533,3701,0000,0000,0311,0010,0440	; 9418		VMA_AR,LOAD AR (WR TST)		;GET MARK FROM SECT 0
U 3533, 2245,3200,0003,0000,0022,0010,0000	; 9419		AR_MEM
						;;9420	.IFNOT/XADDR
						;;9421		BR/AR,AR_BR,STORE		;STORE DEST POINTER
						;;9422		MEM_AR,AR_BR,SEL DSTP		;DONE.  GET MARK AGAIN
						;;9423						;PRESELECT # TO FIX HARDWARE GLITCH
						;;9424		DSTP_AR,ARX/MQ,J/EDNOP		;MARK BECOMES DEST. GET NEXT PAT
						; 9425	.IF/XADDR
						; 9426	=101
U 2245, 3537,3202,2040,0000,0016,0010,0000	; 9427	EDDSNG:	BR/AR,AR_BR,STORE,J/EDEXX	;NEITHER POINTER IS DOUBLE
U 2247, 1002,4001,0000,0000,0000,0010,0000	; 9428		J/UUO				;SHORT DSTP, LONG MARK ILLEGAL
						; 9429	;;;FLUSH WHEN SURE THIS IS RIGHT
						; 9430	;	BR/AR,AR_BR,			;DSTP TO AR, MARK TO BR
						; 9431	;		VMA_VMA+1,LOAD ARX	;GET MARK2
						; 9432	;	FIN XFER,VMA_VMA-1,STORE,J/EDEXX;NOW STORE DSTP AS NEW MARK
						; 9433	
U 3534, 2435,3240,0003,0000,0042,0034,0000	; 9434	EDEX2:	AR_MEM,BYTE DISP		;WAIT FOR MARK, TEST DESTP
U 2435, 2245,4001,0000,0000,0020,0034,0000	; 9435	=101	BYTE DISP,J/EDDSNG		;NO, CHECK MARK
U 2437, 2445,3200,0206,0000,0040,0034,0145	; 9436		ARX_DSTP2,BYTE DISP		;YES, CHECK MARK
U 2445, 1002,4001,0000,0000,0000,0010,0000	; 9437	=101	J/UUO				;LONG DSTP SHORT MARK ABORT
						; 9438	;;;FLUSH WHEN SURE THE UUO IS RIGHT
						; 9439	;	BR/AR,AR_ARX,			;MARK TO BR, DSTP2 TO AR
						; 9440	;		VMA_VMA+1,STORE,J/EDEXM4 ; STORE DSTP2
						; 9441		BR/AR,AR_ARX,
U 2447, 3535,0001,4040,0000,2011,3610,0240	; 9442			VMA_VMA+1,LOAD ARX (WR TST)	;GET MARK2
U 3535, 3536,3240,0003,0000,0036,0010,0000	; 9443		FIN XFER,STORE			;PUT BACK DSTP2
						; 9444	;EDEXM4:
						; 9445		FIN STORE,AR_BRX,		;GET DSTP FROM BRX
U 3536, 3537,3202,6003,0000,0016,3510,0000	; 9446			VMA_VMA-1,STORE		;PUT THAT DOWN
						; 9447	EDEXX:	MEM_AR,AR_BR,SEL DSTP,		;PRESELECT # TO FIX HARDWARE GLITCH
U 3537, 3036,3242,2003,0000,0002,5010,0144	; 9448			SKP PC SEC0		;GET MARK FOR NEW DSTP
U 3036, 2455,0001,4006,0000,2020,1034,0144	; 9449	=0	DSTP_AR,AR_ARX,BYTE DISP,J/EDEX1
U 3037, 2455,4001,0006,0000,0000,1010,0144	; 9450		DSTP_AR
						; 9451	=101
						; 9452	EDEX1:	FE_FE-#,#/3,SKP SCAD0,
U 2455, 3016,3721,2000,5031,0020,5110,0003	; 9453			AR_MQ,J/EDNXT1
U 2457, 3540,0001,0000,0000,0000,0010,0145	; 9454		SEL DSTP2			;PRESELECT # TO FIX HARDWARE GLITCH
U 3540, 2455,0001,0006,0000,0000,1010,0145	; 9455		DSTP2_AR,J/EDEX1		;PUT OLD MARK2 AS DSTP2
						; 9456	.ENDIF/XADDR
						; 9457	.ENDIF/MODEL.B
						; 9458	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9459	;HERE FOR SELECT
						; 9460	
						; 9461	=0*
						; 9462	EDSEL:	AR_SRCP,ARX_SRCP,FE_#,#/36.,
U 1664, 2760,3240,2201,0301,0040,0050,0044	; 9463			CALL,J/GSRC1		;GO GET SRC BYTE
U 1666, 2460,0303,7707,0000,0020,0027,0165	; 9464		AR_AR*.5 LONG,E1		;GOT IT, DIVIDE BY 2
U 2460, 3502,0600,0007,4000,0332,0050,0165	; 9465	=000	VMA_AR+E1,LOAD AR,CALL,J/TRNAR	;GO TRANSLATE BY HALFWORDS
						; 9466	=010
U 2462, 3541,3240,0003,0000,0022,0010,0000	; 9467	EDSFIL:	AR_MEM,J/EDSF1			;(2) NO SIGNIFICANCE, STORE FILL
U 2463, 1620,0001,0020,5110,0020,5110,0000	; 9468		GEN P-S,SKP SCAD0,BRX/ARX,J/EDSFLT ;(3) SIG START, DO FLOAT CHAR
U 2464, 1540,0001,0000,0000,0000,1650,0224	; 9469	EDSPUT:	SR_ED(+D),CALL,J/PUTDST		;(4) NORMAL, STORE AT DST
U 2465, 3523,4001,0000,0302,0100,0010,0774	; 9470		VMA/PC,SC_#,#/-4,J/EDSTOP	;(5) ABORT
U 2466, 1573,3240,2000,0000,0020,0010,0000	; 9471	EDFPUT:	AR_SFLGS,J/EDSEND		;(6) BUMP PBN AND GO TO NEXT
U 2467, 2464,3200,0003,0000,0022,0010,0000	; 9472	EDMPUT:	AR_MEM,J/EDSPUT			;FILL OR MSG IN AR, STORE IT
						; 9473	
						; 9474	
						; 9475	;HERE WHEN TIME TO STORE FILL CHAR
						; 9476	
U 3541, 2466,3701,0000,0000,0040,5410,0000	; 9477	EDSF1:	SKP AR NE,J/EDFPUT		;IS THERE ONE?
						; 9478	
						; 9479	;HERE WHEN SELECT STARTS SIGNIFICANCE
						; 9480	
						; 9481	=00
						;;9482	.IFNOT/MODEL.B
						;;9483	EDSFLT:	VMA_AC3,STORE,CALL,J/EDFLT	;STORE DEST AT MARK ADDR
						; 9484	.IF/MODEL.B
						;;9485	.IFNOT/XADDR
						;;9486	EDSFLT:	ARX_AR,AR_AC3,ARL_0.M,CALL,J/EDFLT
						; 9487	.IF/XADDR
						; 9488	EDSFLT:	ARX_AR,VMA_AC3,AR/AD,ARL_0.M,
						; 9489			BYTE DISP,SCADA EN/0S,SCAD/A,
U 1620, 2474,3240,2405,0400,1341,5074,0020	; 9490			CALL,SKP PC SEC0,J/EDFLT
						; 9491	.ENDIF/XADDR
						; 9492	.ENDIF/MODEL.B
U 1621, 1620,4001,2000,0000,3021,0010,0200	; 9493		P_FE,AR_AR+1,J/EDSFLT		;FORCE STANDARD POINTER FORM
U 1623, 2464,3202,6000,0000,0000,1010,0000	; 9494	=11	SFLGS_AR,AR_BRX,J/EDSPUT	;SET S FLAG, GET BYTE, STORE IT
						; 9495	
						; 9496	;HERE IS SUBROUTINE TO STORE FLOAT CHAR
						; 9497	
						; 9498	.IF/MODEL.B
						;;9499	.IFNOT/XADDR
						;;9500	EDFLT:	VMA_AR,AR_ARX,STORE,J/EDFLT1
						; 9501	.IF/XADDR
						; 9502	=100
U 2474, 3544,0001,4000,0000,2016,0010,0000	; 9503	EDFLT:	AR_ARX,STORE,J/EDFLT1		;SHORT POINTER.  STORE IT
U 2475, 3544,3703,4000,0000,2316,0010,0000	; 9504		VMA_AR,AR_ARX,STORE,J/EDFLT1	; LIKEWISE.  FORCE SECTION 0
U 2476, 3542,0001,4000,0000,2016,0010,0000	; 9505		AR_ARX,STORE,J/EDFLTX		;LONG POINTER, DO MORE
U 2477, 3544,3703,4000,0000,2316,0010,0000	; 9506		VMA_AR,AR_ARX,STORE,J/EDFLT1	; IN SECTION 0, KEEP THERE
						; 9507	
U 3542, 3543,4001,0003,0000,0002,0010,0000	; 9508	EDFLTX:	MEM_AR				;FINISH STORE OF 1ST PART
U 3543, 3544,3200,2006,0000,0036,3610,0145	; 9509		AR_DSTP2,VMA_VMA+1,STORE	;NOW DO SECOND PART
						; 9510	.ENDIF/XADDR
						;;9511	.IFNOT/MODEL.B
						;;9512	
						;;9513	EDFLT:
						; 9514	.ENDIF/MODEL.B; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22-1
; EIS.MIC[10,5351]	19:52 24-Jul-85			EIS -- EDIT FUNCTION					

						; 9515	
U 3544, 1670,4061,5003,0000,0022,0010,0000	; 9516	EDFLT1:	MEM_AR,AR_2			;MARK STORED, READY FOR FLOAT
U 1670, 0770,0600,0007,4000,0332,0050,0176	; 9517	=0*	VMA_AR+E0,LOAD AR,CALL,J/XFERW
U 1672, 2504,3701,0000,0000,0040,5410,0000	; 9518		SKP AR NE
U 2504, 3545,3200,2000,0302,0020,0010,0040	; 9519	=100	AR_SFLGS,SC_#,#/40,J/SETFLG	;NO FLOAT CHR, SET S FLAG
U 2505, 1540,0001,0000,0000,0000,1650,0224	; 9520		SR_ED(+D),CALL,J/PUTDST		;STORE FLOAT CHR IN DST
U 2507, 3545,3200,2000,0302,0020,0010,0040	; 9521	=111	AR_SFLGS,SC_#,#/40		;SET S FLAG AND RETURN
U 3545, 0003,4001,0000,6100,3001,0003,0200	; 9522	SETFLG:	P_P OR SC,RETURN3		;NO FLOAT CHR, SET S FLAG
						; 9523	
						; 9524	.ENDIF/EXTEND
						; 9525	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 1
; IO.MIC[10,5351]	19:56 24-Jul-85			I/O INSTRUCTIONS					

						; 9526	.TOC	"I/O INSTRUCTIONS"
						; 9527	
						; 9528	; BITS 10-12 OF INSTRUCTION GET MAPPED TO IR 7-9 FOR I/O INSTRUCTIONS
						; 9529	; THE DEVICE ADDRESS IS BROKEN DOWN AS ONE OF THE FIRST 7, OR ALL OTHERS
						; 9530		.DCODE
						; 9531	
						; 9532	;DEVICE 000 (APR)
						; 9533	
D 0700, 3001,0512				; 9534	700:	W,		J/APRBI	;APRID (BLKI APR,)	OPTIONS, SERIAL #
D 0701, 3600,0510				; 9535		W,	DATAI,	J/APRDI	;DATAI APR,		ADDRESS COMPARE
D 0702, 0000,0706				; 9536		I,		J/APRBO	;WRFIL (BLKO APR,)	REFILL RAM
D 0703, 4401,0704				; 9537		R,	DATAO,	J/APRDO	;DATAO APR,		ADDRESS COMPARE
D 0704, 0401,0714				; 9538		I,	CONO,	J/APRCO	;CONO APR,		APR FLAGS
D 0705, 0601,0716				; 9539		I,	CONI,	J/APRCI	;CONI APR,
D 0706, 0100,0716				; 9540		I,	CONSZ,	J/APRCI	;CONSZ APR,
D 0707, 0501,0716				; 9541		I,	CONSO,	J/APRCI	;CONSO APR,
						; 9542	
						; 9543	;DEVICE 004 (PI)
						; 9544	
D 0710, 3600,0711				; 9545	710:	W,	M,	J/PIBI	;RDERA (BLKI PI,)	READ ERA
D 0711, 3301,0710				; 9546		W,	B/3,	J/PIDI	;DATAI PI,		Stats, or not used
D 0712, 4001,0712				; 9547		R,		J/PIBO	;SBDIAG (BLKO PI,)	SBUS DIAGNOSTIC
D 0713, 4000,0710				; 9548		R,	B/0,	J/PIDO	;DATAO PI,		More statistics
D 0714, 0400,1200				; 9549		I,	CONO,	J/PICO	;CONO PI,		PI SYSTEM CONTROL
D 0715, 0600,1204				; 9550		I,	CONI,	J/PICI	;CONI PI,		IN PROGRESS, ENABLE
D 0716, 0101,1204				; 9551		I,	CONSZ,	J/PICI
D 0717, 0500,1204				; 9552		I,	CONSO,	J/PICI
						; 9553	
						; 9554	;DEVICE 010 (PAG)
						; 9555	
D 0720, 6200,1212				; 9556	720:	RW,	BLKI,	J/PAGBI	;BLKI PAG,		UNASSIGNED
D 0721, 3600,1210				; 9557		W,	DATAI,	J/PAGDI	;DATAI PAG,		USER CONTEXT
D 0722, 0001,1100				; 9558		I,		J/PAGBO	;CLRPT (BLKO PAG,)	INVAL PAGE TABLE
D 0723, 4400,1102				; 9559		R,	DATAO,	J/PAGDO	;DATAO PAG,		USER CONTEXT
D 0724, 0400,1214				; 9560		I,	CONO,	J/PAGCO	;CONO PAG,		EXEC CONTEXT
D 0725, 0600,1216				; 9561		I,	CONI,	J/PAGCI	;CONI PAG,
D 0726, 0101,1216				; 9562		I,	CONSZ,	J/PAGCI
D 0727, 0500,1216				; 9563		I,	CONSO,	J/PAGCI
						; 9564	
						; 9565	;DEVICE 014 (CCA)
						; 9566	
D 0730, 0001,1504				; 9567	730:	I,	J/SWEEP	;BLKI CCA,	8 FUNCTIONS TO SWEEP THE CACHE
D 0731, 0001,1504				; 9568		I,	J/SWEEP	;SWPIA (DATAI CCA,)INVALIDATE CACHE, NO CORE UPDATE
D 0732, 0001,1504				; 9569		I,	J/SWEEP	;SWPVA (BLKO CCA,)VALIDATE CORE, LEAVE CACHE VALID
D 0733, 0001,1504				; 9570		I,	J/SWEEP	;SWPUA (DATAO CCA,)UNLOAD CACHE TO CORE, CLEAR CACHE
D 0734, 0001,1504				; 9571		I,	J/SWEEP	;CONO CCA,
D 0735, 0001,1504				; 9572		I,	J/SWEEP	;SWPIO (CONI CCA,)INVALIDATE ONE PAGE
D 0736, 0001,1504				; 9573		I,	J/SWEEP	;SWPVO (CONSZ CCA,)VALIDATE ONE PAGE
D 0737, 0001,1504				; 9574		I,	J/SWEEP	;SWPUO (CONSO CCA,)UNLOAD ONE PAGE
						; 9575	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 2
; IO.MIC[10,5351]	19:56 24-Jul-85			I/O INSTRUCTIONS					

						; 9576	;I/O CONT'D
						; 9577	
						; 9578	;DEVICE 020 (TIM)
						; 9579	
D 0740, 3101,1506				; 9580	740:	W,	B/1,	J/RDMTR	;RDPERF (BLKI TIM,)	PERF CNT
D 0741, 3000,1506				; 9581		W,	B/0,	J/RDMTR	;RDTIME (DATAI TIM,)	TIME BASE
D 0742, 4400,1312				; 9582		R,	DATAO,	J/TIMBO	;WRPAE (BLKO TIM,)	PA ENABLES
D 0743, 4401,1310				; 9583		R,	DATAO,	J/TIMDO	;DATAO TIM,		UNDEFINED
D 0744, 0401,1300				; 9584		I,	CONO,	J/TIMCO	;CONO TIM,		SETUP INTERVAL TIMER
D 0745, 0601,1302				; 9585		I,	CONI,	J/TIMCI	;CONI TIM,		RD INTERVAL & PERIOD
D 0746, 0100,1302				; 9586		I,	CONSZ,	J/TIMCI
D 0747, 0501,1302				; 9587		I,	CONSO,	J/TIMCI
						; 9588	
						; 9589	;DEVICE 024 (MTR)
						; 9590	
						; 9591	
D 0750, 3300,1506				; 9592	750:	W,	B/3,	J/RDMTR	;RDMACT (BLKI MTR,)	CACHE CNT
D 0751, 3201,1506				; 9593		W,	B/2,	J/RDMTR	;RDEACT (DATAI MTR,)	EBOX CNT
D 0752, 0001,1002				; 9594		I,		J/UUO	;BLKO MTR,		UNDEFINED
D 0753, 0001,1002				; 9595		I,		J/UUO	;DATAO MTR,		UNDEFINED
D 0754, 0400,1304				; 9596		I,	CONO,	J/MTRCO	;WRTIME (CONO MTR,)	ACCT & TB CTL
D 0755, 0600,1306				; 9597		I,	CONI,	J/MTRCI	;CONI MTR,		SAME
D 0756, 0101,1306				; 9598		I,	CONSZ,	J/MTRCI
D 0757, 0500,1306				; 9599		I,	CONSO,	J/MTRCI
						; 9600	
						; 9601	;DEVICE 030
						; 9602	
D 0760, 6200,0500				; 9603	760:	RW,	BLKI,	J/BLKIO
D 0761, 3601,0506				; 9604		W,	DATAI,	J/IO
D 0762, 6001,0500				; 9605		RW,	BLKO,	J/BLKIO
D 0763, 4401,0506				; 9606		R,	DATAO,	J/IO
D 0764, 0401,0502				; 9607		I,	CONO,	J/CONO
D 0765, 3601,0506				; 9608		W,	CONI,	J/IO
D 0766, 0101,0502				; 9609		I,	CONSZ,	J/CONS
D 0767, 0500,0502				; 9610		I,	CONSO,	J/CONS
						; 9611	
						; 9612	;DEVICES 034-774 (ALL OTHERS)
						; 9613	
D 0770, 6200,0500				; 9614	770:	RW,	BLKI,	J/BLKIO
D 0771, 3601,0506				; 9615		W,	DATAI,	J/IO
D 0772, 6001,0500				; 9616		RW,	BLKO,	J/BLKIO
D 0773, 4401,0506				; 9617		R,	DATAO,	J/IO
D 0774, 0401,0502				; 9618		I,	CONO,	J/CONO
D 0775, 3601,0506				; 9619		W,	CONI,	J/IO
D 0776, 0101,0502				; 9620		I,	CONSZ,	J/CONS
D 0777, 0500,0502				; 9621		I,	CONSO,	J/CONS
						; 9622	
						; 9623		.UCODE
						; 9624	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 3
; IO.MIC[10,5351]	19:56 24-Jul-85			EXTERNAL DEVICE I/O INSTRUCTIONS			

						; 9625	.TOC	"EXTERNAL DEVICE I/O INSTRUCTIONS"
						; 9626	
						; 9627	=0****00*000
U 0500, 3002,0001,0000,0000,0000,7350,0000	; 9628	BLKIO:	SKP IO LEGAL,CALL,J/IOCHK	;FIRST VERIFY INSTR VALIDITY
U 0501, 0443,0001,0000,0000,0020,0034,0000	; 9629		BYTE DISP,J/BLK1		;TEST FPD
						; 9630	CONS:					;HERE FOR CONSO, CONSZ TO LOAD
						; 9631						; BR IN CASE OF UUO
U 0502, 0506,4001,0040,0000,3000,0610,0004	; 9632	CONO:	BR/AR,ARL_ARR,ARR_ARR		;CONDITIONS TO BOTH HALVES
						; 9633	=10
U 0506, 1630,3200,0003,0000,0022,7350,0000	; 9634	IO:	AR_MEM,SKP IO LEGAL,CALL,J/GTEBUS;WAIT FOR MBOX IF BLKI/O
U 0507, 0022,0001,0000,0000,0005,2233,0000	; 9635	RELEEB:	REL ECL EBUS,B WRITE		;XFER DONE, WHAT TO DO?
						; 9636	=
						; 9637	=1*010
U 0022, 3546,4001,0003,0000,0002,0010,0000	; 9638	IOTEND:	FIN STORE,MB WAIT,J/BLK4	;BLKI/BLKO
U 0023, 0073,3602,0004,0000,0246,0010,0203	; 9639		TEST AR.BR,TEST FETCH,J/NOP	;CONSZ
U 0026, 0242,4001,0003,0000,0002,6510,0000	; 9640		MEM_AR,SKP PI CYCLE,J/IOFET	;DATA/CON I/O
U 0027, 0073,3602,0004,0000,0246,0010,0203	; 9641		TEST AR.BR,TEST FETCH,J/NOP	;CONSO
						; 9642	;BLKI/BLKO SCREWED AROUND WITH TO TRY TO STOP PI LOSSAGE
U 3546, 0020,5062,0000,0000,0246,6517,0203	; 9643	BLK4:	TEST BRL,TEST FETCH,SKP PI CYCLE,J/CLRFPD
						; 9644	
						; 9645	.IF/IPA20				;[410] Tail of PI function 7
						; 9646	=1***00
U 0240, 3552,4001,0003,0000,0002,2250,0026	; 9647	ESEND:	MEM_AR,SET DATAO,CALL [EBUSO]	;Send data out over EBUS
						; 9648	.ENDIF/IPA20				;[410]
						; 9649	=1***10					;[410]
U 0242, 0073,4001,0000,0000,0217,0010,0000	; 9650	IOFET:	I FETCH,J/NOP			;HERE IF NOT PI CYCLE
U 0243, 2104,0001,0000,0000,0000,0024,0502	; 9651		DISMISS,J/PIFET			;DISMISS INTRPT AFTER DATA/CON I/O
						; 9652	
						; 9653	=1**010
U 0442, 0125,4001,0043,0000,0002,0633,0020	; 9654	BLK2:	MEM_AR,BR/AR,ARL_0.C,B DISP,J/BLK3
U 0443, 0442,4003,2000,0000,0036,0017,0000	; 9655	BLK1:	AR_AR+1,GEN CRY18,STORE,J/BLK2	;UPDATE POINTER WORD
U 0447, 0442,0001,0000,0000,0000,6510,0000	; 9656	=111	SKP PI CYCLE,J/BLK2		;IF FPD & NOT PI, DON'T INCREMENT
						; 9657	=1*101					;DO DATAI OR DATAO
U 0125, 0506,3703,0000,0000,0312,1110,0100	; 9658	BLK3:	VMA_AR,LOAD AR,SET FPD,J/IO	;GET DATA TO OUTPUT
U 0127, 0506,3703,0000,0000,0300,1110,0100	; 9659		VMA_AR,SET FPD,J/IO		;INPUT DO BEFORE MEM
						; 9660	
						; 9661	;;;NOTE NOTE NOTE SET FPD INHIBITED BY HARDWARE IF PI CYCLE (SCD5)
						; 9662	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4
; IO.MIC[10,5351]	19:56 24-Jul-85			EXTERNAL DEVICE I/O INSTRUCTIONS			

						; 9663	;SUBROUTINES TO HANDLE EBUS
						; 9664	;CALL WITH "SKP IO LEGAL"
						; 9665	;ENTER AFTER LOADING AR IF OUTPUT FUNCTION
						; 9666	
						; 9667	=00
U 1630, 1002,3242,2000,0000,0000,0010,0000	; 9668	GTEBUS:	AR_BR,J/UUO			;HERE IF IO ILLEGAL IN THIS MODE
U 1631, 3042,4001,0000,0000,0000,2250,0200	; 9669		REQ EBUS,CALL,J/WGRANT		;ASK PI SYSTEM FOR BUS
						; 9670	=11					;RETURN TO TRANSFER ROUTINE
						; 9671	
						; 9672	;SUBROUTINE TO PERFORM EBUS TRANSFER
						; 9673	;SETUP CONTROLLER SELECT AND FUNCTION LINES BEFORE CALL
						; 9674	;IF OUTPUT, ALSO PUT AR ONTO EBUS DATA LINES
						; 9675	
						; 9676	EBUSX:	GEN AR,TIME/5T,			;WAIT AFTER ASSERTING FUNCTION
U 1633, 3041,3701,0000,0000,0060,2210,0060	; 9677			SET EBUS DEMAND,J/WXFER	; AFTER 300 NS, ASSERT DEMAND
						; 9678	=0
						; 9679	EBUSW:	AR_EBUS,GEN AR,
U 3040, 3547,3703,3000,0000,0060,2210,0020	; 9680			CLR EBUS DEMAND,J/RELEB	;STROBE DATA AND DROP DEMAND
U 3041, 3040,3701,0000,0000,0000,6710,0000	; 9681	WXFER:	GEN AR,SKP -EBUS XFER,J/EBUSW	;WAIT FOR TRANSFER
						; 9682	
						; 9683	RELEB:	GEN AR,REL EBUS,TIME/5T,	;DROP DATA, CS, AND FCN
U 3547, 0003,3701,0000,0000,0060,2203,0100	; 9684			RETURN3			;AFTER 150 NS, THEN RELEASE BUS
						; 9685	
						; 9686	
						; 9687	;SUBROUTINE TO WAIT FOR PI SYSTEM TO GRANT EBUS
						; 9688	; IT WILL EITHER SEND EBUS GRANT, OR PI READY
						; 9689	
						; 9690	=0
U 3042, 3044,4001,0000,0000,0000,6610,0000	; 9691	WGRANT:	SKP -EBUS GRANT,J/WGRNT1	;GOT IT?
U 3043, 0071,4001,0000,0000,0000,2210,0000	; 9692		DROP EBUS REQ,J/TAKINT
						; 9693	=0
U 3044, 3550,3701,0000,0000,0000,2210,0030	; 9694	WGRNT1:	IO INIT,GEN AR,J/WGRNT2		;GOT IT, SETUP CS, FCN, AND DATA
U 3045, 3042,0001,0000,0000,0000,7010,0000	; 9695		SKP INTRPT,J/WGRANT		;DIDN'T GET IT, TEST FOR INTERUPT
						; 9696	WGRNT2:	GEN AR,TIME/5T,			;JUST WAIT
U 3550, 0003,3703,0000,0000,0060,0003,0030	; 9697			EBUS CTL/IO INIT,RETURN3
						; 9698	
						; 9699	;HERE TO START PI CYCLE TRANSFER.  HOLD EBUS CTL SELECTION
						; 9700	
U 3551, 1633,0001,0000,0000,0060,0010,0027	; 9701	EBUSI:	TIME/5T,EBUS CTL/DATAI,J/EBUSX
U 3552, 1633,3701,0000,0000,0060,0010,0026	; 9702	EBUSO:	GEN AR,TIME/5T,EBUS CTL/DATAO,J/EBUSX
						; 9703	
						; 9704	;SUBROUTINES TO CHECK IO LEGALITY FOR INTERNAL I/O INSTRUCTIONS
						; 9705	
						; 9706	.IF/MODEL.B
						; 9707	3002:
						;;9708	.IFNOT/MODEL.B
						;;9709	2002:					;ACCESSIBLE ON EXTEND [0]
						; 9710	.ENDIF/MODEL.B
U 3002, 1002,3242,2000,0000,0000,0010,0000	; 9711	IOCHK:	AR_BR,J/UUO			;NAUGHTY, MUST'NT DO
						; 9712	.IF/MODEL.B
						; 9713	3003:
						;;9714	.IFNOT/MODEL.B
						;;9715	2003:
						; 9716	.ENDIF/MODEL.B
U 3003, 0001,4001,0000,0000,0000,0003,0000	; 9717	RET1:	RETURN1				;ONE-CYCLE NULL ROUTINE
						; 9718	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 4-1
; IO.MIC[10,5351]	19:56 24-Jul-85			EXTERNAL DEVICE I/O INSTRUCTIONS			

						; 9719	=0
U 3046, 1002,3242,2000,0000,0000,0010,0000	; 9720	GETEEB:	AR_BR,J/UUO			;IO ILLEGAL IN THIS MODE
U 3047, 0001,0001,0000,0000,0000,2203,0400	; 9721	GTEEB1:	GET ECL EBUS,RETURN1
						; 9722	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 5
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- APR, CCA			

						; 9723	.TOC	"INTERNAL DEVICE FUNCTIONS -- APR, CCA"
						; 9724	
						; 9725	=0****00***0
						; 9726	SWEEP:	BR/AR,SC_#,#/9.,CALL,
U 1504, 3002,0001,0040,0302,0000,7350,0011	; 9727			SKP IO LEGAL,J/IOCHK	;ALLOWED?
U 1505, 3553,0001,4000,0000,0000,0010,0000	; 9728		AR_SHIFT			;MOVE PAGE # TO PLACE
U 3553, 3554,3701,0000,0000,0307,0010,0606	; 9729	=	VMA_AR,SWEEP CACHE		;START A SWEEP
U 3554, 0066,0001,0000,0000,0002,0010,0000	; 9730	MBREL:	MB WAIT,J/IFNOP			;COMPLETE REG FUNC BEFORE FETCH
						; 9731	
						; 9732	
						; 9733	=0****00**00
U 0704, 3046,0001,0000,0000,0000,7350,0000	; 9734	APRDO:	CALL,SKP IO LEGAL,J/GETEEB	;SET ADDR BREAK
U 0705, 0507,3703,0000,0000,0060,2010,0417	; 9735		DATAO APR,J/RELEEB
U 0706, 3002,4001,0040,0000,0000,7350,0000	; 9736	APRBO:	BR/AR,CALL,SKP IO LEGAL,J/IOCHK	;SET CACHE REFILL ALGORITHM
U 0707, 3554,4001,0000,0000,0007,0010,0505	; 9737		WR REFILL RAM,J/MBREL		;INFO ALREADY IN VMA
						; 9738	=
						; 9739	
						; 9740	=0****00*000
U 0510, 3046,0001,0000,0000,0000,7350,0000	; 9741	APRDI:	CALL,SKP IO LEGAL,J/GETEEB	;READ ADDR BREAK
U 0511, 0507,0001,3000,0000,0060,2010,0513	; 9742		DATAI APR(L),J/RELEEB
						; 9743	=010
						; 9744	.IFNOT/DDT.BUG				;[346] Normal code
U 0512, 3002,0001,0000,0000,0000,7350,0000	; 9745	APRBI:	CALL,SKP IO LEGAL,J/IOCHK	;RETURN MICRO VERSION, SERIAL #
						; 9746		AR_SERIAL,TIME/3T,		;READ SERIAL NUMBER
U 0513, 0137,4001,0000,0000,0020,0750,0100	; 9747			CALL,J/UVERS		;GET MICRO-CODE VERSION IN AR
U 0517, 3555,3242,2000,0000,3000,0022,0004	; 9748	=111	ARL_ARR.S,AR_BR			;COMB SERIAL WITH VERSION
						; 9749	=	AR0-8_#,STORE,OPTIONS,		;SET OPTION FLAGS
U 3555, 0066,0001,0000,0000,0016,0110,0640	; 9750			J/STMEM
						;;9751	.IF/DDT.BUG				;[346] Gross hack to make EDDT work
						;;9752	APRBI:	AR_SERIAL,TIME/3T		;[346] Get hardware serial number
						;;9753	=
						;;9754	=0	AR_AR SWAP,FE_#,OPTIONS,	;Set to test bit 23
						;;9755			SKP IO LEGAL,CALL [IOCHK]; Is this a legal instruction here?
						;;9756		GEN P AND #,#/1,SKP SCAD NZ	;Was bit 23 set?
						;;9757	=0*0
						;;9758	SEROK:	AR_AR SWAP,CALL [UVERS]		;Maybe not. Get microcode version
						;;9759		P_P AND #,#/76,J/SERFIX		;Yes. Clean it and move it
						;;9760		ARL_ARR,ARR_BR			;It's fixed. Shuffle version to spot
						;;9761	=	AR0-8_FE,STORE,J/STMEM		;Move in options and store result
						;;9762	;
						;;9763	SERFIX:	FE_FE+#,#/10,J/SEROK		;OR in bit 5 for option
						; 9764	.ENDIF/DDT.BUG				;[346] All this code is GROSS!!
						; 9765	
						; 9766	=0****00**00
						; 9767	APRCO:	BR/AR,ARL_ARR.M,ARR_ARR,CALL.M,	;SET APR FLAGS
U 0714, 3046,4001,0040,0000,3001,7350,0004	; 9768			SKP IO LEGAL,J/GETEEB
U 0715, 3560,3701,0000,0000,0060,2010,0414	; 9769		CONO APR, J/APRCO7		;[272]
						; 9770	APRCI:	BR/AR,CALL,
U 0716, 3046,4001,0040,0000,0000,7350,0000	; 9771			SKP IO LEGAL,J/GETEEB	;READ APR FLAGS
U 0717, 3556,4001,3000,0000,0060,2010,0510	; 9772		CONI APR(R)			;GET RIGHT HALF OF APR CONDITIONS
U 3556, 3557,0001,3400,0000,3060,2010,0512	; 9773	=	ARX_AR SWAP,CONI APR(L)		;NOW LH COND TO AR LEFT
U 3557, 0507,0001,4000,0000,2000,0610,0000	; 9774		AR_ARX,ARL_ARL,J/RELEEB		;COMBINE HALVES
U 3560, 0507,3703,0000,0000,0060,2010,0414	; 9775	APRCO7:		CONO APR,J/RELEEB	;[272]
						; 9776	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 6
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- PI				

						; 9777	.TOC	"INTERNAL DEVICE FUNCTIONS -- PI"
						; 9778	
						; 9779	=0****00*000
						; 9780	.IFNOT/INSTR.STAT
						; 9781	PIDO:
U 0710, 1002,3242,2000,0000,0000,0010,0000	; 9782	PIDI:	AR_BR,J/UUO			;DATAI/O PI, UNASSIGNED
						; 9783	
						;;9784	.IF/INSTR.STAT
						;;9785	;DATAO PI, SETS UP BUFFER POINTERS FOR TRACKS
						;;9786	;DATAI PI, READS CURRENT BUFFER POINTER
						;;9787	
						;;9788	PIDI:
						;;9789	PIDO:	BR/AR,ARL+ARX_0.M,CALL.M,	;CHECK IO LEGALITY
						;;9790			SKP IO LEGAL,J/IOCHK
						;;9791		SC_#,#/9.,B DISP,SKP BR0,J/PIDX	;NOW, WHAT TO DO?
						; 9792	.ENDIF/INSTR.STAT
						; 9793	
U 0711, 0774,0001,0000,0000,0007,0010,0504	; 9794	PIBI:	READ ERA,J/RDEBRG		;GET AND STORE
						; 9795	=0
						; 9796	PIBO:	FE_#,#/7,CALL,			;NUMBER OF TIMES TO TRY
U 0712, 3002,0001,0000,0301,0000,7350,0007	; 9797			SKP IO LEGAL,J/IOCHK	;SBUS DIAGNOSTIC
U 0713, 3561,0001,0040,0000,0007,0010,0407	; 9798	DODIAG:	SBUS DIAG,BR/AR			;SEND THE DIAG FUNCTION FROM AR
						; 9799	=
						; 9800	.DEFAULT/MOS=0
						; 9801	.IFNOT/MOS
U 3561, 0066,4001,1000,0000,0016,3610,0000	; 9802		AR/CACHE,VMA_VMA+1,STORE,J/STMEM ;STORE THE RESPONSE
						;;9803	.IF/MOS
						;;9804		AR/CACHE,MB WAIT		;[225]GET THE DATA.
						;;9805		GEN AR+1,SKP AD NE		;IF MEMORY RETURNED -1 TRY AGAIN
						;;9806	=0
						;;9807		FE_FE-1,SKP SCAD0,J/SDTEST	;IT IS SEE IF TOO MANY TRIES
						;;9808	SDONE:	VMA_VMA+1,STORE,J/STMEM		;STORE THE RESPONSE
						;;9809	=0
						;;9810	SDTEST:	AR_BR,J/DIAG1			;[225]RECOVER THE FUNC AND RETRY.
						;;9811		AR_0S,J/SDONE			;TOO MANY TRIES QUIT RETURNING 0
						; 9812	.ENDIF/MOS
						; 9813	
						; 9814	=0****00*000
						; 9815	PICO:	BR/AR,ARL_ARR.M,ARR_ARR,
U 1200, 3050,0001,0040,0000,3001,7350,0004	; 9816			CALL.M,SKP IO LEGAL,J/PICOM1
U 1203, 3565,3703,0000,0000,0060,2010,0415	; 9817	=11	CONO PI,J/PICOM2		;SEND THE DATA
						; 9818	=0****00*100
U 1204, 3050,0001,0040,0000,0000,7350,0000	; 9819	PICI:	BR/AR,CALL,SKP IO LEGAL,J/PICOM1
U 1207, 3562,4001,3000,0000,0060,2010,0500	; 9820	=11	CONI PI(R)			;READ RH TO AR LEFT
						; 9821	=	ARX_AR SWAP,			;RH COND TO ARX RH
U 3562, 3563,4001,3400,0000,3060,2010,0530	; 9822			CONI PI(PAR)		; AND PARITY ENABLES TO RH
						; 9823		BRX/ARX,ARX_AR,			;READY TO COMB RH PARTS
U 3563, 3564,4001,3420,0000,1060,2010,0501	; 9824			CONI PI(L)		; AND LH TO AR LEFT
U 3564, 3565,3302,6004,0000,0000,0610,0000	; 9825		AR_ARX*BRX,AD/OR,ARL_ARL	;COMBINE THEM
U 3565, 0022,3701,0000,0000,0005,2233,0100	; 9826	PICOM2:	REL EBUS,GEN AR,B WRITE,J/IOTEND
						; 9827	
						; 9828	=0
U 3050, 1002,3242,2000,0000,0000,0010,0000	; 9829	PICOM1:	AR_BR,J/UUO			;LOSE
U 3051, 3042,0001,0000,0000,0000,2210,0200	; 9830		REQ EBUS,J/WGRANT		;OK, WAIT TO GET FULL EBUS
						; 9831	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7
; IO.MIC[10,5351]	19:56 24-Jul-85			TRACKS SUPPORT						

						; 9832	.TOC	"TRACKS SUPPORT"
						; 9833	;
						; 9834	;	According to the University of Essex, when TRACKS is used with
						; 9835	;	address break enabled, the monitor cannot disable address break
						; 9836	;	for the actual execution of the instruction unless we include
						; 9837	;	"ABORT INSTR" as part of the TRACKS loop.  Accordingly, this is
						; 9838	;	now done for all the different flavors of TRACKS.  Thanks, folks.
						; 9839	;
						; 9840	;[317]	During an attempt to implement uncounting of doubly counted op
						; 9841	;	codes for the OP.CNT conditional, we learned:  (1) EA
						; 9842	;	calculation requires that VMA be initialized to the instruction
						; 9843	;	location (usually PC) for the local/global stuff to work properly.
						; 9844	;	Beware!  (This is important if you try to do something after
						; 9845	;	fetching an instruction but before you EA calc it.)  (2) NICOND
						; 9846	;	clears the trap enable flag.  If INSTR.STAT is on, this will force
						; 9847	;	the dispatch to the statistics logic without taking the trap,
						; 9848	;	which will thus be lost and gone forever.  This is a hardware bug,
						; 9849	;	but it's rather impossible to ECO it at this late date.
						; 9850	;
						; 9851	;[321]	The solution to this is to ignore the statistics flag if a trap
						; 9852	;	is ready.  See the block at NEXT for details.  (It's not really
						; 9853	;	a hardware bug after all.)
						; 9854	;
						;;9855	.IF/INSTR.STAT
						;;9856	=00
						;;9857	PIDX:	CLR TRACKS EN,J/IFNOP		;TURN TRACKS OFF
						;;9858	.IF/TRACKS
						;;9859		ARX_SHIFT,ARL_BRL,ARR_0.S,J/PIDO2
						;;9860	.IFNOT/TRACKS
						;;9861		ARX_SHIFT,AR_2,J/PIDX1		;[315] Turn statistics on
						;;9862	.ENDIF/TRACKS
						;;9863	=11	AR_TRX				;READ BACK POINTER
						;;9864		ARL_0.M				;GET INDEX PART
						;;9865		AR_AR+TRB,STORE,J/STMEM		;DONE WITH DATAI
						;;9866	
						;;9867	.IFNOT/TRACKS
						;;9868	PIDX1:	AR_ARX+AR*4			;SAVE PAGE NUMBERS IN TRX REGISTERS
						;;9869		TRB_AR				;INITIAL GARBAGE HERE
						;;9870		VMA_ARX,LOAD AR,PHYS REF
						;;9871		AR_MEM
						;;9872		VMA_ARX+1,LOAD AR,PHYS REF
						;;9873		TRX_AR,ARX_ARX+1		;[315] Must wait one cycle for
						;;9874		AR_MEM				;parity check before writing AC
						;;9875		VMA_ARX+1,LOAD AR,PHYS REF
						;;9876		TRX1_AR,ARX_ARX+1		;[315] Note that this will always
						;;9877		AR_MEM				;make it before AR is smashed
						;;9878		VMA_ARX+1,LOAD AR,PHYS REF
						;;9879		TRX2_AR
						;;9880		AR_MEM
						;;9881		SET TRACKS EN			;[315] Might as well do this now
						;;9882		TRX3_AR,I FETCH,J/NOP		;SAVE TABLE PAGE #, TURN ON
						;;9883	.ENDIF/TRACKS
						;;9884	
						;;9885	PIDO2:	TRX_AR,AR_ARX			;SET UP INDEX
						;;9886		TRB_AR				;AND BASE
						;;9887		SET TRACKS EN,J/IFNOP		;TURN TRACKS ON; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 7-1
; IO.MIC[10,5351]	19:56 24-Jul-85			TRACKS SUPPORT						

						;;9888	
						;;9889	.IF/TRACKS
						;;9890	;HERE WHEN NICOND FINDS TRACKS ENABLED
						;;9891	=0
						;;9892	TRK1:	TRX_AR,J/TRK2			;PUT BACK UPDATED INDEX
						;;9893		AR_AR SWAP			;END OF BUFFER.  RESET
						;;9894		AR_-AR,J/TRK1			; ORIGINAL INDEX
						;;9895	
						;;9896	TRK2:	ARL_0.M
						;;9897		AR_AR+TRB			;ADDRESS TRACKS BUFFER
						;;9898		VMA_AR,PHYS REF			;TO MAKE MODEL A WORK
						;;9899		AR_PC,STORE,PHYS REF		; PUT PC THERE
						;;9900		MEM_AR,VMA/PC,CLR TRACKS EN	;PREVENT NICOND SEEING TRACKS...
						;;9901		ABORT INSTR			;[306] Make address break work by
						;;9902						; copying AD BRK CYC to AD FAIL INH
						;;9903		SET TRACKS EN			;...UNTIL NEXT TIME
						;;9904		DISP/NICOND,J/NEXT		;GO DO NEXT INSTR
						;;9905	.ENDIF/TRACKS
						;;9906	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 8
; IO.MIC[10,5351]	19:56 24-Jul-85			TRACKS SUPPORT						

						;;9907	.IF/OP.CNT
						;;9908	;HERE WHEN NICOND FINDS OPCODE COUNTING ENABLED
						;;9909	; SKIP IF USER MODE
						;;9910	;[316] Make this register usage compatible with timing version below by
						;;9911	; making it use TRX2 (for exec mode counts pointer) and TRX3 (for user mode
						;;9912	; counts pointer) instead of TRX and TRX+1.
						;;9913	;[317] A massive attempt to uncount doubly counted instructions when an
						;;9914	; interrupt was detected has been backed off.  See INSTR.STAT dispatch logic
						;;9915	; above for commentary.
						;;9916	;
						;;9917	=0
						;;9918	OPCT1:	AR_TRX2,SKP AC REF,J/OPCT2	;[316] TRX HAS PAGE # FOR EXEC TABLE
						;;9919		AR_TRX3,SKP AC REF		;[316] NEXT PAGE IS FOR USER
						;;9920	=0
						;;9921	OPCT2:	AR_SHIFT,MQ_SHIFT,		;[317] Save VMA of increment
						;;9922			CLR TRACKS EN,J/OPCT3	;OPCODE INDEXES INTO TABLE
						;;9923		ARX_FM(VMA),J/OPCT2		;GET INSTR FROM FM
						;;9924	;
						;;9925	OPCT3:	VMA_AR,LOAD AR,PHYS REF		;GET TABLE ENTRY
						;;9926		AR_MEM				;[306] Make address break work
						;;9927		BR/AR,AR_AR+1,STORE		;ADD THIS OCCURANCE TO IT, keep old
						;;9928		MEM_AR,VMA/PC
						;;9929		SET TRACKS EN			;LET US GET BACK NEXT NICOND
						;;9930		DISP/NICOND			;DO INSTR IN ARX
						;;9931	;
						;;9932	;	In an attempt to prevent an interrupt after counting an instruction,
						;;9933	;	we now fake the second NICOND in line.
						;;9934	;
						;;9935	=0000	AR_BR,CLR TRACKS EN,J/OPFIX	;Some kind of odd condition.
						;;9936	=0010	AR_BR,CLR TRACKS EN,J/OPFIX	; Uncount the instruction
						;;9937		AR_BR,CLR TRACKS EN,J/OPFIX
						;;9938		AR_BR,CLR TRACKS EN,J/OPFIX
						;;9939		AR_BR,CLR TRACKS EN,J/OPFIX
						;;9940		AR_BR,CLR TRACKS EN,J/OPFIX
						;;9941	=1010	BRX/ARX,AR_ARX,SET ACCOUNT EN,	;The usual case
						;;9942			XR,EA MOD DISP,J/COMPEA
						;;9943		AR_BR,CLR TRACKS EN,J/OPFIX
						;;9944	=1110	GEN ARX,LOAD IR,#/0,J/XCTGO	;Instruction in registers
						;;9945		AR_BR,CLR TRACKS EN
						;;9946	;
						;;9947	OPFIX:	VMA_MQ,STORE,PHYS REF		;Restore old count
						;;9948		MEM_AR,VMA/PC
						;;9949		SET TRACKS EN			;Turn TRACKS back on
						;;9950		DISP/NICOND,J/NEXT		; and do it yet again
						;;9951	.ENDIF/OP.CNT
						;;9952	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 9
; IO.MIC[10,5351]	19:56 24-Jul-85			TRACKS SUPPORT						

						;;9953	.IF/OP.TIME
						;;9954	;HERE TO ADD UP TIME SPENT IN INSTR'S
						;;9955	
						;;9956	OPTM0:	SC_#,#/9.,SKP USER,J/OPTM1
						;;9957	=0
						;;9958	OPTM1:	BR_AR LONG,AR_TRX2,SKP AC REF,	;INSTR IN ARX PAGE IN AR
						;;9959			J/OPTM2
						;;9960		BR_AR LONG,AR_TRX3,SKP AC REF,	;INSTR IN ARX PAGE IN AR
						;;9961			J/OPTM2
						;;9962	=0
						;;9963	OPTM2:	AR_SHIFT,ABORT INSTR,J/OPTM3	;[306] GENERATE ADDR TO INCREMENT
						;;9964						;NEXT GET ADDR FOR THIS
						;;9965		ARX_FM(VMA),J/OPTM2		;GET NEXT INSTR FROM FM
						;;9966	OPTM3:	VMA_AR,LOAD AR,UNCSH PHYS REF	;BUMP COUNT LOCATION
						;;9967		AR_MEM
						;;9968		AR_AR+1,STORE
						;;9969		MEM_AR,SKP USER
						;;9970	=0	AR_TRX,J/OPTM4
						;;9971		AR_TRX1
						;;9972	OPTM4:	AR_SHIFT,ARX_TRB
						;;9973		TRB_AR				;SAVE NEXT LOC TO BUMP
						;;9974		RD+CLR PA			;TIME TO AR
						;;9975		MTR CTL/CLR PERF
						;;9976		VMA_ARX,LOAD ARX,UNCSH PHYS REF	;GET TABLE ENTRY
						;;9977		AR_AR-BR,ARL_0.S		;COMPENSATE TIME FOR THIS CODE
						;;9978		BR/AR,ARX_MEM,SKP AR18		;IF THIS WAS AN ENABLED STATE,
						;;9979	=0	AR_ARX+BR,STORE			;WRITE IT BACK
						;;9980		MEM_AR,VMA/PC			;NOW SETUP NEXT INSTR AGAIN
						;;9981		ARX_BRX,SET TRK+PA EN		;RESTORE STATISTICS FLAGS
						;;9982		DISP/NICOND,J/NEXT
						;;9983	.ENDIF/OP.TIME
						;;9984	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 10
; IO.MIC[10,5351]	19:56 24-Jul-85			TRACKS SUPPORT						

						;;9985	;THIS IS THE SECOND ORDER STATISTICS GATHERING CODE
						;;9986	.IFNOT/SO2.CNT
						;;9987	.IF/SO.CNT
						;;9988	;THIS IS NON DEBUGED CODE THAT IS IN A VERY PRIMATIVE STATE
						;;9989	;IT WAS ABANDONED BETWEEN EDITS AS THE MONITOR NEEDED SOMETHING
						;;9990	;SLIGHTLY DIFFERENT
						;;9991	;THIS IS CODE TO DO A SECOND ORDER STATISTIC IN THE SECOND 128K
						;;9992	;EACH ENTRY IS A HALF WORD
						;;9993	;THE LOW ORDER BIT OF <LAST OPCODE><THIS OPCODE> CONCATENATION
						;;9994	;DETERMINES WHICH HALFWORD IS INCREMENTED. OFF IS HIGH
						;;9995	;IF A HALFWORD OVERFLOWS THE CODE TURNS ITSELF OFF AND WRITES -1
						;;9996	;AT LOCATION 400000
						;;9997	;TRX HIDES THE LOCATION PRESENTED TRB THE LAST OPCODE
						;;9998	
						;;9999	;	THIS IS IN THE NEXT LOOP COMMENTED HERE FOR DOCUMENTATION
						;;10000	;TRK0:	ARX_TRB,BRX/ARX,SKP AC REF,J/TRK1;GET PREV INSTR HIDE THIS ONE
						;;10001	
						;;10002	=0
						;;10003	TRK1:	AR_1,SC_#,#/9.,J/TRK2		;SHIFT IN FIRST OPCODE
						;;10004		ARX_FM(VMA),AR_ARX		;GET THE INSTRUCTION FROM FM
						;;10005		BRX/ARX,AR_1,SC_#,#/9.,ARX_AR	; WHEN IT HIDES THERE
						;;10006	TRK2:	AR_SHIFT,SC_#,#/8.,ARX_BRX	;SETUP TO DO THE NEXT OPCODE
						;;10007		AR_SHIFT,CLR TRACKS EN		;CONVIENT TO SHUT OFF TRACKS
						;;10008		VMA_AR,LOAD ARX,PHYS REF,AR_ARX	;LOW BIT OF INSTR WHERE CAN TEST
						;;10009		ARX_SHIFT,TRB_AR		;LOW BIT OF INSTR TO HIGH BIT
						;;10010		ARL_0.S,ARR_1S,SKP ARX0		;SEE WHICH HALFWORD TO INC
						;;10011						; ALSO SETTING UP FOR TEST IN
						;;10012						; LOW INCREMENT
						;;10013	=0	ARL_1.M,ARR_0.M,J/TRK3		;INC HIGH
						;;10014		ARX_MEM				;INC LOW
						;;10015		BR/AR,ARX_ARX+1,AR/ADX,STORE	;INCREMENT LOW HALF AND STORE IT
						;;10016		TEST AR.BR,SKP CRY0		;USING AND-1 HACK TO TEST OVFLOW
						;;10017	=0	AR_0S,J/TRKLOS			;OVERFLOWED FIXUP TO LOSE
						;;10018		MEM_AR,ARX_BRX,VMA/PC,AR_BRX	;NO OVERFLOW GET OUT ALSO
						;;10019	TRKN1:	SET TRACKS EN			; SAVE AWAY THIS OPCODE
						;;10020	TRKND:	DISP/NICOND,J/NEXT		;AND DO NEXT INSTR
						;;10021	
						;;10022	TRK3:	ARX_MEM,BR/AR			;GET WORD TO INC
						;;10023		AR_ARX+BR,STORE,SKP CRY0	;COMPUTE AND CHECK FOR OVERFLOW
						;;10024	=0	ARX_BRX,VMA/PC,MEM_AR,AR_BRX,	;SAVE AWAY THIS INSTR
						;;10025			J/TRKN1			; AND GET OUT
						;;10026		AR_0.C,ARX_BRX,MEM_AR,J/TRKLOS	;SHUT DOWN TRACKS ON OVERFLOW
						;;10027	
						;;10028	
						;;10029	TRKLOS:	AR_1,SC_#,#/17.			;WHEN LOSS PUT -1 IN 400000
						;;10030		AR_SHIFT
						;;10031		GEN AR,VMA/AD,PHYS REF		;SHUT OFF TRACKS AND QUIT
						;;10032		AR_TRB,STORE
						;;10033		MEM_AR,VMA/AD
						;;10034		J/TRKND
						;;10035	
						;;10036	.ENDIF/SO.CNT
						;;10037	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 11
; IO.MIC[10,5351]	19:56 24-Jul-85			TRACKS SUPPORT						

						;;10038	
						;;10039	.IF/SO2.CNT
						;;10040	;THIS ONE DOES THE COUNTING IN 128K STARTING AT AN ADDRESS PRESENTED IT
						;;10041	;THIS IS CODE TO DO A SECOND ORDER STATISTIC IN THE SECOND 128K
						;;10042	;EACH ENTRY IS A HALF WORD
						;;10043	;THE LOW ORDER BIT OF <LAST OPCODE><THIS OPCODE> CONCATENATION
						;;10044	;DETERMINES WHICH HALFWORD IS INCREMENTED. OFF IS HIGH
						;;10045	;IF A HALFWORD OVERFLOWS THE CODE TURNS ITSELF OFF AND WRITES -1
						;;10046	;AT LOCATION PRESENTED
						;;10047	;TRX HIDES THE LOCATION PRESENTED TRB THE LAST OPCODE
						;;10048	
						;;10049	;	THIS IS IN THE NEXT LOOP COMMENTED HERE FOR DOCUMENTATION
						;;10050	;TRK0:	ARX_TRB,BRX/ARX,SKP AC REF,J/TRK1;GET PREV INSTR HIDE THIS ONE
						;;10051	
						;;10052	=0
						;;10053	TRK1:	AR_0S,SC_#,#/9.,J/TRK2		;SHIFT IN FIRST OPCODE
						;;10054		ARX_FM(VMA),AR_ARX		;GET THE INSTRUCTION FROM FM
						;;10055		BRX/ARX,AR_0S,SC_#,#/9.,ARX_AR	; WHEN IT HIDES THERE
						;;10056	TRK2:	AR_SHIFT,SC_#,#/8.,ARX_BRX	;SETUP TO DO THE NEXT OPCODE
						;;10057		AR_SHIFT,CLR TRACKS EN		;CONVIENT TO SHUT OFF TRACKS
						;;10058		AR_AR+TRX			;BUMPS BY NUMBER FED IT
						;;10059		VMA_AR,LOAD ARX,PHYS REF,AR_ARX	;LOW BIT OF INSTR WHERE CAN TEST
						;;10060		ARX_SHIFT,TRB_AR		;LOW BIT OF INSTR TO HIGH BIT
						;;10061		ARL_0.S,ARR_1S,SKP ARX0		;SEE WHICH HALFWORD TO INC
						;;10062						; ALSO SETTING UP FOR TEST IN
						;;10063						; LOW INCREMENT
						;;10064	=0	ARL_1.M,ARR_0.M,J/TRK3		;INC HIGH
						;;10065		ARX_MEM,ABORT INSTR		;[306] INC LOW
						;;10066		BR/AR,ARX_ARX+1,AR/ADX,STORE	;INCREMENT LOW HALF AND STORE IT
						;;10067		TEST AR.BR,SKP CRY0		;USING AND-1 HACK TO TEST OVFLOW
						;;10068	=0	AR_0S,J/TRKLOS			;OVERFLOWED FIXUP TO LOSE
						;;10069		MEM_AR,ARX_BRX,VMA/PC,AR_BRX	;NO OVERFLOW GET OUT ALSO
						;;10070	TRKN1:	SET TRACKS EN			; SAVE AWAY THIS OPCODE
						;;10071	TRKND:	DISP/NICOND,J/NEXT		;AND DO NEXT INSTR
						;;10072	
						;;10073	TRK3:	ARX_MEM,BR/AR,ABORT INSTR	;[306] GET WORD TO INC
						;;10074		AR_ARX+BR,STORE,SKP CRY0	;COMPUTE AND CHECK FOR OVERFLOW
						;;10075	=0	ARX_BRX,VMA/PC,MEM_AR,AR_BRX,	;SAVE AWAY THIS INSTR
						;;10076			J/TRKN1			; AND GET OUT
						;;10077		AR_0.C,ARX_BRX,MEM_AR,J/TRKLOS	;SHUT DOWN TRACKS ON OVERFLOW
						;;10078	
						;;10079	
						;;10080	TRKLOS:	AR_TRX
						;;10081		GEN AR,VMA/AD,PHYS REF	;WRITE -1 IN PRESENTED LOCATION
						;;10082		AR_TRB,STORE
						;;10083		MEM_AR,VMA/PC		;NEED AN INSTRUCTION TO GIVE
						;;10084		J/TRKND			;TIME TO NICOND AFTER VMA/PC
						;;10085	
						;;10086	.ENDIF/SO2.CNT
						; 10087	.ENDIF/INSTR.STAT
						; 10088	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- PAG			

						; 10089	.TOC	"INTERNAL DEVICE FUNCTIONS -- PAG"
						; 10090	
						; 10091	=0****00**00
						;;10092	.IFNOT/KLPAGE
						;;10093	PAGBO:	BR/AR,CLR FE,CALL,		;INVALIDATE ONE PAGE TABLE LINE
						;;10094			SKP IO LEGAL,J/IOCHK
						;;10095	PAGBO1:	PT SEL_INVAL,J/PTLOOP		;SETUP INITIAL PT WR SELECT
						; 10096	.IF/KLPAGE
						; 10097	PAGBO:	AR_0S,BR/AR,CALL,		;CLEAR ONE PAGE TABLE ENTRY
U 1100, 3002,3441,2040,0000,0000,7350,0000	; 10098			SKP IO LEGAL,J/IOCHK
U 1101, 0066,0001,0000,0000,0020,2310,0010	; 10099		WR PT ENTRY,J/IFNOP
						; 10100	.ENDIF/KLPAGE
						; 10101	PAGDO:	ARX_AR (AD),ARR_ARL,ARL_ARL.M,	;SETUP USER CONTEXT
U 1102, 3046,3701,4200,0000,3001,7350,0000	; 10102			CALL.M,SKP IO LEGAL,J/GETEEB
U 1103, 3566,3701,4000,0000,2060,2010,0620	; 10103		DATAO PAG(L),AR_ARX		;SETUP AC BLOCKS, PREV CTXT
U 3566, 3052,3203,0000,0302,0020,5510,0011	; 10104	=	SKP AR2,SC_#,#/9.
						; 10105	=0
U 3052, 0507,4001,0000,0000,0000,0010,0007	; 10106	TIMCO1:	MTR CTL/CONO TIM,J/RELEEB	;DO NOT CHANGE UBR
						;;10107	.IF/PAGCNT				;[327] Count DATAO PAG with bit 2
						;;10108		MQ_AR,AR_TRX3+1			;[327] Do the count
						;;10109		TRX3_AR,AR_MQ
						; 10110	.ENDIF/PAGCNT				;[327]
						; 10111		FE_P AND #,#/4,MQ_SHIFT,	;[333] Save bit 3 for keep test
U 3053, 0650,3401,2010,7131,0000,4410,0004	; 10112			SKP AR18,AR_0S		;STORE ACCT?
U 0650, 3054,4001,0000,0000,0000,0150,0100	; 10113	=0*0	AR0-8_#,#/100,CALL,J/PAGD2	;YES, START WITH EBOX CNT
U 0651, 3577,3721,0000,0000,0307,0050,0602	; 10114		VMA_MQ,LOAD UBR,CALL [CLRPT]	;[333] No. Set for page table clear
						; 10115	.IF/BIG.PT				;[333]
U 0655, 2516,4001,0000,2400,0020,5210,0000	; 10116	=1*1	SKP SC NE,J/KEEPME		;[333] Might keep keep me bits
						;;10117	.IFNOT/BIG.PT
						;;10118	=1*1	PT SEL_INVAL,J/PTLOOP		;SETUP INITIAL PT WR SELECT
						; 10119	.ENDIF/BIG.PT				;[333]
						; 10120	;
						; 10121	;	PAGD2 is set up as a subroutine for addressing convenience only.
						; 10122	;
						; 10123	=0
U 3054, 2532,0001,0400,0302,1000,0050,0015	; 10124	PAGD2:	ARX_AR,SC_#,#/13.,CALL,J/EMTR	;UPDATE THE EBOX ACCT
U 3055, 3567,0001,0000,0000,0002,0110,0140	; 10125		MB WAIT,AR0-8_#,#/140		;READY TO GET CACHE ACCT
U 3567, 2533,0001,0400,0302,1000,0010,0015	; 10126		ARX_AR,SC_#,#/13.,J/CMTR	;RETURN ABOVE TO CLR PT
						; 10127	
						; 10128	
						; 10129	=0****00**00
						; 10130	PAGDI:	SC_#,#/70,SKP IO LEGAL,
U 1210, 3046,0001,0000,0302,0000,7350,0070	; 10131			CALL,J/GETEEB
						;;10132	.IFNOT/MODEL.B
						;;10133		DATAI PAG(L),CALL,J/PCTXT	;FIRST GET AC BLOCKS & CWSX
						; 10134	.IF/MODEL.B
						; 10135		DATAI PAG(L),ARX_1B17-1,	;PUT AC BLKS IN AR,
U 1211, 3570,1761,3200,0000,0060,2057,0511	; 10136			CALL,J/PCTXT		; [0,,-1] IN ARX
						; 10137	.ENDIF/MODEL.B
U 1212, 1002,3242,2000,0000,0000,0010,0000	; 10138	PAGBI:	AR_BR,J/UUO			;BLKI PAG, IS UNASSIGNED
U 1213, 0507,3242,4000,0000,0000,0610,0002	; 10139		AR_SHIFT,ARL_BRL,J/RELEEB	;COMBINE UBR WITH AC BLKS, CWSX
						; 10140	=
						;;10141	.IFNOT/MODEL.B
						;;10142	PCTXT:	P_SC				;PLUG IN LOAD-ENABLE BITS
						;;10143	=0*	BR/AR,AR_0S,READ UBR,		;ASK MBOX FOR UBR LOC'N
						;;10144			CALL,J/XFERW		;NOW READ IT; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 12-1
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- PAG			

						;;10145		AR_EBUS REG
						; 10146	.IF/MODEL.B
U 3570, 3571,3243,0500,2400,3001,0010,0200	; 10147	PCTXT:	P_SC,ARX_ARX*8			;STUFF IN LOAD EN, ARX=7,,-10
U 3571, 3572,3203,0540,0000,2000,0210,0000	; 10148		BR/AR,AR12-17_PREV SEC,ARX_ARX*8
U 3572, 1674,3012,2044,0000,0000,0010,0000	; 10149		BR/AR,AR_ARX*BR,AD/ANDCA	;PCS TO BR, LD EN, AC BLKS TO AR
						; 10150	=0*	AR_AR*BR,AD/OR,READ UBR,	;LH READY IN AR.  GET UBR
U 1674, 0770,3302,2004,0000,0007,0050,0502	; 10151			CALL,J/XFERW
U 1676, 3573,4001,3040,0000,0060,2010,0567	; 10152		BR/AR,AR_EBUS REG		;LH TO BR.  READ UBR ADDRESS
						; 10153	.ENDIF/MODEL.B
						; 10154		ARX_AR,AR_0S,SC_#,#/27.,	;READY TO MOVE INTO POSITION
U 3573, 0003,3441,2400,0302,1000,0003,0033	; 10155			RETURN3
						; 10156	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 13
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- PAG			

						; 10157	;CONI/O PAG,
						; 10158	
						; 10159	=0****00**00
						; 10160	PAGCO:	BR/AR,ARL_ARR.M,ARR_ARR,ARX_0S,	;SET EXEC CONTEXT
U 1214, 3046,3441,0240,0000,3001,7350,0004	; 10161			SKP IO LEGAL,CALL.M,J/GETEEB
U 1215, 2510,3703,0000,0000,0060,2010,0416	; 10162		CONO PAG,J/SETEBR		;SET CACHE, SEC, TRAP EN FLAGS
						; 10163	
						; 10164	PAGCI:	BR/AR,AR_0S,CALL,SKP IO LEGAL,	;READ EXEC CONTEXT
U 1216, 3046,3401,2040,0302,0000,7350,0011	; 10165			SC_#,#/9.,J/GETEEB
U 1217, 2000,0001,3000,0000,0060,2010,0531	; 10166		CONI PAG			;READ CACHE, SEC, TRAP EN
						; 10167	=
						; 10168	=0*	ARX_AR SWAP,AR_0S,READ EBR,	;SETUP EPT LOC'N TO READ
U 2000, 0770,3401,2400,0000,3007,0050,0503	; 10169			CALL,J/XFERW
						; 10170		AR_EBUS REG,			;GET EBR IN AR
U 2002, 3574,3401,3220,0000,0060,2010,0567	; 10171			BRX/ARX,ARX_0S		;SAVE FLAGS IN LH OF BRX
U 3574, 3575,4001,0400,0000,0000,2210,0000	; 10172		ARX_SHIFT,REL ECL EBUS		;MOVE EBR LOC LEFT
U 3575, 3576,3302,0000,0000,0001,0010,0016	; 10173		ARR_0.M,ADB/BR,ADA EN/EN,AD/OR,ARL/ADX	;COMBINE, THEN PUT IN RH
U 3576, 0022,4001,4000,0000,3005,0033,0000	; 10174		AR_AR SWAP,B WRITE,J/IOTEND	;STORE THE RESULT
						; 10175	
						; 10176	;HERE TO FINISH CONO PAG,
						; 10177	
						; 10178	=000					;[342]
U 2510, 2662,4001,0000,0302,0000,0050,0011	; 10179	SETEBR:	SC_#,#/9.,CALL,J/SHIFT		;MOVE EBR LOC'N TO POSITION
U 2512, 3577,3701,0000,0000,0307,0050,0603	; 10180	=010	VMA_AR,LOAD EBR,CALL [CLRPT]	;[333]SETUP EBR
						; 10181	=110					;[342]SETUP INITIAL PT WR SELECT
U 2516, 2356,0001,0000,0000,0000,2310,0001	; 10182	KEEPME:	PT SEL_INVAL,J/PTLOOP		;[342] FOR NON KLPAGE THIS CAN SEND
						; 10183						; THE USER TO PAGBO1 AND SAVE 1
						; 10184						; UCODE LOCATION
						; 10185	.IF/BIG.PT				;[333] Entry from DATAO
U 2517, 2526,0001,0000,0000,0000,2310,0041	; 10186		PT SEL_INVAL (KEEP),J/KEEPCL	;[342] Hang on to KEEP pages
						; 10187	.ENDIF/BIG.PT				;[333]
						; 10188	=
						; 10189	=110
						; 10190	PTLOOP:	AR_AR+BR,VMA/AD,FE_FE-1,	;SELECT A LINE OF PT
						; 10191			CLR PT LINE,TIME/3T,	;DO THE WORK
U 2356, 2356,0602,2004,3001,0320,2334,0031	; 10192			BYTE DISP,J/PTLOOP	;LOOP TO CLEAR ALL
U 2357, 0066,0001,0000,0000,0000,2310,0000	; 10193		PT SEL_NORMAL,J/IFNOP		;RESET PT WR SELECTION
						; 10194	.IF/BIG.PT				;[333]
						; 10195	=110
						; 10196	KEEPCL:	AR_AR+BR,VMA/AD,FE_FE-1,TIME/3T,;SELECT A LINE OF PT
						; 10197			CLR PT LINE (KEEP),	;DO THE WORK
U 2526, 2526,0602,2000,3001,0320,2334,0061	; 10198			BYTE DISP,J/KEEPCL	;Hang onto lines with KEEP ME set
U 2527, 0066,0001,0000,0000,0000,2310,0000	; 10199		PT SEL_NORMAL,J/IFNOP		;RESET PT WR SELECTION
						; 10200	.ENDIF/BIG.PT				;[333]
						; 10201	;
						; 10202	;	[333] Set up to clear hardware page table after setting EBR or
						; 10203	;	UBR.  KEEP ME pages may or may not be cleared, depending upon the
						; 10204	;	setting of DATAO PAG bit 3.  (Clear everything for CONO PAG.)
						; 10205	;
U 3577, 3056,3401,2200,0000,0000,2210,0000	; 10206	CLRPT:	AR_0S,ARX_0S,REL ECL EBUS	;[334]DON'T HANG UP BUS FOR THIS
						; 10207	=0	AR0-8_#,#/10,MB WAIT,		;WAIT FOR U/E BR LOAD
U 3056, 0676,0001,0000,0000,0002,0153,0010	; 10208			SC_FE,CALL [ARSWAP]	;[334]GET 1B23
						; 10209		BR/AR,AR_0S,VMA/AD,		;[333][334] START CLEARING AT ZERO
U 3057, 0004,3441,2040,0301,0300,0003,0077	; 10210			FE_#,#/63.,RETURN4	;SETUP LOOP COUNT
						; 10211	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 14
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- TIM & MTR			

						; 10212	.TOC	"INTERNAL DEVICE FUNCTIONS -- TIM & MTR"
						; 10213	
						; 10214	=0****00***0
						; 10215	RDMTR:	AR_BR,CALL,			;GET E TO AR
U 1506, 3046,3202,2000,0000,0000,7350,0000	; 10216			SKP IO LEGAL,J/GETEEB	;GRAB CONTROL OF EBUS
						; 10217		MQ_AR,AR_0S,			;SAVE E IN MQ
U 1507, 1650,3441,2010,0302,1000,0033,0015	; 10218			SC_#,#/13.,B DISP	;WHICH COUNTER?
						; 10219	=
U 1650, 3060,4001,3000,0000,0060,2010,0510	; 10220	=00	AR_TIME BASE,J/RDMTR1		;DATAI TIM,
U 1651, 3060,0001,3000,0000,0060,2010,0511	; 10221		AR_PERF CNT,J/RDMTR1		;BLKI TIM,
U 1652, 3060,0001,3000,0000,0060,2010,0512	; 10222		AR_EBOX CNT,J/RDMTR1		;DATAI MTR,
U 1653, 3060,4001,3000,0000,0060,2010,0513	; 10223		AR_CACHE CNT,J/RDMTR1		;BLKI MTR,
						; 10224	
						; 10225	=0
						; 10226	RDMTR1:	ARL+ARX_0.M,B DISP,		;SHIFT COUNT INTO POSITION
U 3060, 1700,0001,0000,0000,0001,0073,0060	; 10227			CALL.M,J/MTRDBL		;ADD DOUBLE WORD FROM PT
U 3061, 3600,0602,2600,0000,0020,2227,0000	; 10228		AR_AR+BR LONG,REL ECL EBUS
U 3600, 0341,3721,0000,0302,0316,0010,0043	; 10229		VMA_MQ,STORE,SC_#,#/35.,J/DMVM1	;STORE TOTAL AT E & E+1
						; 10230	
						; 10231	=0****00**00
U 1300, 3046,4001,0040,0000,0000,7350,0000	; 10232	TIMCO:	BR/AR,CALL,SKP IO LEGAL,J/GETEEB
U 1301, 3052,3701,0000,0000,0060,2023,0407	; 10233		CONO TIM,J/TIMCO1
						; 10234	TIMCI:	BR/AR,AR_0S,CALL,
U 1302, 3046,3441,2040,0000,0000,7350,0000	; 10235			SKP IO LEGAL,J/GETEEB
U 1303, 3601,4001,3000,0000,0060,2010,0514	; 10236		AR_INTERVAL			;INTERVAL GOES TO LH
U 3601, 3602,3441,2400,0000,3000,0010,0000	; 10237	=	ARX_AR SWAP,AR_0S
U 3602, 3603,4001,3000,0000,0060,2010,0515	; 10238		AR_PERIOD			;PERIOD TO RH
						; 10239	TIMBO1:	MTR CTL/LD PA LH,		;KEEP MTR DECODE FOR TIMBO
U 3603, 0507,4001,0000,0000,2000,0022,0004	; 10240			ARL_ARXL,J/RELEEB	;COMBINE PERIOD WITH INTERVAL
						; 10241	
						; 10242	=0****00**00
						; 10243	MTRCO:	BR/AR,ARL_ARR.M,ARR_ARR,
U 1304, 3046,4001,0040,0000,3001,7350,0004	; 10244			CALL.M,SKP IO LEGAL,J/GETEEB
U 1305, 3604,3703,0000,0000,0060,2023,0406	; 10245		CONO MTR,J/MTRCO1
						; 10246	MTRCI:	BR/AR,AR_0S,CALL,
U 1306, 3046,3441,2040,0000,0000,7350,0000	; 10247			SKP IO LEGAL,J/GETEEB
U 1307, 3604,0001,3000,0000,0060,2010,0516	; 10248		CONI MTR			;READ BACK CONDITIONS
						; 10249	=
U 3604, 0507,0001,0000,0000,0000,0022,0026	; 10250	MTRCO1:	ARL_0.S,MTR CTL/CONO MTR,J/RELEEB
						; 10251	
						; 10252	=0****00**00
U 1310, 1002,3242,2000,0000,0000,0010,0000	; 10253	TIMDO:	AR_BR,J/UUO			;DATAO TIM, UNDEFINED
						; 10254	=10
						; 10255	TIMBO:	ARX_AR,AR_0S,CALL,		;SAVE ENABLES, CLEAR AR
U 1312, 3046,3401,2400,0000,1000,7350,0000	; 10256			SKP IO LEGAL,J/GETEEB	;CHECK LEGALITY, GET BUS
U 1313, 3605,3703,4000,0000,2060,2023,0404	; 10257		BLKO TIM(L),AR_ARX		;TURN OFF BY CLEARING LH ENABLES
U 3605, 3606,3703,4000,0000,3060,2023,0405	; 10258	=	BLKO TIM(R),AR_AR SWAP		;SEND RH
U 3606, 3603,3703,0000,0000,0060,2023,0404	; 10259		BLKO TIM(L),J/TIMBO1		;SEND LH, TURNING ON AGAIN
						; 10260	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 15
; IO.MIC[10,5351]	19:56 24-Jul-85			INTERNAL DEVICE FUNCTIONS -- TIM & MTR			

						; 10261	;HERE WHEN METER INCREMENT REQUEST DETECTED
						; 10262	
U 3607, 3610,4001,0000,0000,0000,2210,0400	; 10263	MTRREQ:	GET ECL EBUS			;TAKE CONTROL OF BUS
U 3610, 3062,4001,3000,0000,0060,2010,0517	; 10264		AR_MTR REQ			;WHAT TYPE REQUEST?
						; 10265	=0	MQ_AR,AR_AR*2,CALL,		;GET READY TO DISP
U 3062, 3612,3701,5010,0000,1100,3050,0514	; 10266			VMA_#,#/514,J/MTRRQ0
U 3063, 3611,4001,0000,0000,0000,2210,0000	; 10267		REL ECL EBUS			;DONE
U 3611, 2104,0001,0000,0000,0002,2110,0105	; 10268		MB WAIT,SET ACCOUNT EN,J/PIFET	;FETCH NEXT INSTR
						; 10269	
						; 10270	MTRRQ0:	ARX_AR SWAP,DISP/SH0-3,AR_0S,	;DISPATCH ON REQUEST TYPE
U 3612, 2530,3401,2400,0302,3020,0007,0015	; 10271			SC_#,#/13.
						; 10272	=000
U 2530, 3615,0001,3000,0000,0060,2023,0510	; 10273		RD+CLR TB,J/TMTR1		;TIME BASE
U 2531, 3616,4001,3000,0000,0060,2023,0511	; 10274		RD+CLR PA,J/PMTR1		;PERF ANALYSIS CNT
U 2532, 3617,0001,3000,0000,0060,2023,0512	; 10275	EMTR:	RD+CLR E CNT,J/EMTR1		;EBOX CNT
U 2533, 3620,0001,3000,0000,0060,2023,0513	; 10276	CMTR:	RD+CLR C CNT,J/CMTR1		;CACHE CNT
U 2534, 3613,0001,0000,0000,0000,2210,0000	; 10277		REL ECL EBUS			;INTERVAL -- VECTOR INTERRUPT
U 3613, 3614,4001,0000,0000,0000,1510,0714	; 10278	=	SET PI CYCLE
U 3614, 2043,0001,0000,0000,0000,2110,0105	; 10279		SET ACCOUNT EN,J/PIINST
						; 10280	
U 3615, 3621,4001,0000,0000,0000,0010,0000	; 10281	TMTR1:	MTR CTL/CLR TIME,J/MTRRQ1	;HOLD SELECTS FOR
U 3616, 3621,0001,0000,0000,0000,0010,0001	; 10282	PMTR1:	MTR CTL/CLR PERF,J/MTRRQ1	;MTR CTL FUNCTION
U 3617, 3621,0001,0000,0000,0000,0010,0002	; 10283	EMTR1:	MTR CTL/CLR E CNT,J/MTRRQ1	; TO PREVENT RACE
U 3620, 3621,4001,0000,0000,0000,0010,0003	; 10284	CMTR1:	MTR CTL/CLR M CNT,J/MTRRQ1	; AND POSSIBLE GLITCHES
						; 10285	
						; 10286	;HERE WITH RELEVANT COUNT IN ARR, GARBAGE IN ARL
						; 10287	
U 3621, 3064,0001,0000,0000,0000,1510,0004	; 10288	MTRRQ1:	ABORT INSTR
						; 10289	=0	ARL+ARX_0.M,ARX0-3 DISP,	;CLEAR GARBAGE & RE-DISPATCH
U 3064, 1700,4001,0000,0000,2021,0047,0060	; 10290			CALL.M,J/MTRDBL		; TO ADD DOUBLE COUNTER FROM PT
						; 10291		AR_AR+BR LONG,SC_#,#/35.,
U 3065, 3622,0602,2604,0302,0036,3527,0043	; 10292			VMA_VMA-1,STORE		;STORE BACK IN PROCESS TABLE
U 3622, 3623,3441,2003,0000,0002,0010,0000	; 10293		MEM_AR,AR_0S			;HI PART TO MEM
U 3623, 0001,4001,4000,0000,0016,3603,0000	; 10294		AR_SHIFT,VMA_VMA+1,STORE,RETURN1
						; 10295	
						; 10296	;HERE TO PICK UP DOUBLEWORD FROM PROCESS TABLE
						; 10297	; AND ADD CURRENT CONTENTS OF APPROPRIATE METER
						; 10298	
						; 10299	=00
						; 10300	MTRDBL:	AR_0S,ARX_SHIFT,
U 1700, 3624,3401,2400,0000,0100,3010,0510	; 10301			VMA_#,#/510,J/RDEMTR	;TIME BASE IN EPT 510-511
						; 10302		AR_0S,ARX_SHIFT,
U 1701, 3624,3441,2400,0000,0100,3010,0512	; 10303			VMA_#,#/512,J/RDEMTR	;PERF CNT IN EPT 512-513
						; 10304		AR_0S,ARX_SHIFT,
U 1702, 3625,3441,2400,0000,0100,3010,0504	; 10305			VMA_#,#/504,J/RDUMTR	;EBOX ACCT IN UPT 504-505
						; 10306		AR_0S,ARX_SHIFT,
U 1703, 3625,3401,2400,0000,0100,3010,0506	; 10307			VMA_#,#/506,J/RDUMTR	;CACHE ACCT IN UPT 506-507
						; 10308	
						; 10309	RDEMTR:	BR_AR LONG,			;SAVE COUNT IN BR!BRX
U 3624, 2004,4001,0060,0000,0012,0026,0113	; 10310			LOAD AR,EPT REF,J/RDMTR2;GET HIGH WORD FROM EPT
U 3625, 2004,4001,0060,0000,0012,0026,0223	; 10311	RDUMTR:	BR_AR LONG,LOAD AR,UPT REF	; OR UPT AS APPROP
						; 10312	=0*
						; 10313	RDMTR2:	FIN XFER,VMA_VMA+1,LOAD ARX,	;NOW GET LOW WORD
U 2004, 0770,3200,0003,0000,0033,3650,0000	; 10314			CALL,J/XFERW		;GO WAIT FOR IT
U 2006, 0001,3701,0500,0000,0000,0003,0000	; 10315		ARX_ARX*2,RETURN1
						; 10316	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16
; IO.MIC[10,5351]	19:56 24-Jul-85			PRIORITY INTERRUPT PROCESSING				

						; 10317	.TOC	"PRIORITY INTERRUPT PROCESSING"
						; 10318	;HERE WHEN PRIORITY INTERRUPT REQUEST DETECTED
						; 10319	;PI LOGIC HAS DONE HANDSHAKE TO BRING FUNCTION WORD IN ON EBUS
						; 10320	; FUNCTION WORD IS NOW IN AR, SC=2
						; 10321	
						; 10322	;THE FORMAT OF THE FUNCTION WORD IS --
						; 10323	;    0-2	ADDRESS SPACE FOR THE FUNCTION
						; 10324	;	0=EPT
						; 10325	;	1=EXEC VIRTUAL
						; 10326	;	4=PHYSICAL
						; 10327	;	OTHERS UNDEFINED
						; 10328	;    3-5	FUNCTION TO PERFORM (SEE LIST BELOW AT PIDISP)
						; 10329	;      6	FUNCTION QUALIFIER
						; 10330	;   7-10	PHYSICAL DEVICE # ON EBUS
						; 10331	;  11-12	UNDEFINED
						; 10332	;  13-35	ADDRESS FOR FUNCTION
						; 10333	
U 3626, 3627,4001,0010,0000,1000,1510,0714	; 10334	PICYC1:	SET PI CYCLE,MQ_AR		;START PI CYCLE
U 3627, 3630,0001,0007,0000,0000,1010,0163	; 10335		FM[SV.IOP]_AR			;[234] save IOP function word
						; 10336						; in AC3.
U 3630, 2030,3600,0207,0000,0340,0007,0175	; 10337		VMA_AR AND ADMSK,ARX/AD,SH DISP	;EXTRACT ADDR, DISP ON FCN
						; 10338	=1000					;3-5 IS FUNCTION TO PERFORM
U 2030, 2043,0001,0000,0000,0100,3410,0040	; 10339	PIDISP:	VMA_40+PI*2,J/PIINST		;(0) STANDARD INTERRUPT
U 2031, 2043,0001,0000,0000,0100,3410,0040	; 10340		VMA_40+PI*2,J/PIINST		;(1) DITTO
U 2032, 3634,3203,2000,0302,0000,0010,0005	; 10341		AR_AR*4,SC_#,#/5,J/PIVECT	;(2) VECTOR
U 2033, 2077,4001,0000,0000,0015,0010,0000	; 10342		LOAD AR (RPW),J/PIINCR		;(3) INCREMENT [410] and interlock
U 2034, 2100,0001,0000,7310,0020,5210,0040	; 10343		SKP AR6,J/PIDATO		;(4) DATAO
U 2035, 1422,0001,0020,0000,0060,0010,0000	; 10344		BRX/ARX,TIME/5T,J/PIDATI	;(5) DATAI
U 2036, 2540,3243,2000,7310,0020,5210,0040	; 10345		AR_AR*4,SKP AR6,J/PIBYTE	;(6) BYTE TRANSFER
						;;10346	.IFNOT/IPA20				;[265]
						;;10347		VMA_40+PI*2,J/PIINST		;(7) UNDEFINED
						; 10348	.IF/IPA20				;[265]
U 2037, 3631,0001,0000,0000,0015,0026,0103	; 10349		LOAD AR (RPW),PHYS REF   	;[411] Increment word with interlock
U 3631, 3632,3200,0003,0000,0022,0010,0000	; 10350	AWAIT:	AR_MEM				;***HACK*** until real AWAIT ready
U 3632, 0240,4001,2000,0000,0036,0026,0103	; 10351		AR_AR+1,STORE,PHYS REF,J/ESEND	;[411] so SMP will work properly
						; 10352	.ENDIF/IPA20
						; 10353	
U 3633, 2043,4001,0000,0000,0100,3410,0041	; 10354	PICYC2:	VMA_41+PI*2,J/PIINST		;2ND PART OF STD INT
						; 10355	
U 3634, 2043,3441,2000,7311,0020,0007,0030	; 10356	PIVECT:	FE_# AND S,#/30,AR_0S,SH DISP	;WHAT KIND OF DEVICE?
						; 10357	=0011
U 2043, 0075,0001,0000,0000,0013,0026,0513	; 10358	PIINST:	EPT FETCH,J/XCTW		;CHAN 0-3
U 2047, 0075,0001,0000,0000,0013,0026,0513	; 10359		EPT FETCH,J/XCTW		;CHAN 4-7
U 2053, 2010,4001,0000,2030,2000,0110,0142	; 10360		AR0-8_FE+#,#/142,J/DTEVEC	;DTE 0-3
U 2057, 0075,4001,0000,0000,0013,0010,0000	; 10361		LOAD ARX,J/XCTW			;EXTERNAL DEVICE
						; 10362	=0*
U 2010, 2662,3441,2400,0302,1000,0050,0011	; 10363	DTEVEC:	ARX_AR,AR_0S,SC_#,#/9.,CALL,J/SHIFT
U 2012, 0075,3703,0000,0000,0313,0026,0513	; 10364		VMA_AR,EPT FETCH,J/XCTW
						; 10365	
						; 10366	=0101
U 2065, 0770,4001,0000,0000,0012,0026,0113	; 10367	PILD:	LOAD AR,EPT REF,J/XFERW		;GET DATUM FROM EPT
U 2067, 0770,4001,0000,0000,0012,0010,0000	; 10368		LOAD AR,J/XFERW			; OR EXEC VIRTUAL ADDR SPACE
						;;10369	.IFNOT/MODEL.B
						;;10370		VMA_ARX,LOAD AR,
						;;10371			PHYS REF,J/XFERW	; OR PHYSICAL MEMORY, AS REQUESTED
						; 10372	.IF/MODEL.B; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 16-1
; IO.MIC[10,5351]	19:56 24-Jul-85			PRIORITY INTERRUPT PROCESSING				

U 2075, 2060,3610,0207,4000,0320,0010,0175	; 10373		VMA_ARX AND ADMSK,ARX/AD,J/PHYS2;FORCE AC'S FOR 0-17
						; 10374	.ENDIF/MODEL.B
						;;10375	.IFNOT/MODEL.B			;[224]
						;;10376	IOPFIN:	SET ACCOUNT EN,J/PIDONE		;IN CASE OF EBUS PROBLEMS
						; 10377	.ENDIF/MODEL.B			;[224]
						; 10378	
						; 10379	;HERE TO PERFORM INCREMENT FUNCTION
						; 10380	
U 2077, 3066,3240,0003,7310,0042,5210,0040	; 10381	PIINCR:	AR_MEM,SKP AR6			;GET WORD, INCR OR DECR?
U 3066, 2103,4003,2000,0000,0036,0010,0000	; 10382	=0	AR_AR+1,STORE,J/PIDONE
U 3067, 2103,1701,2000,0000,0036,0010,0000	; 10383		AR_AR-1,STORE,J/PIDONE
						; 10384	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 17
; IO.MIC[10,5351]	19:56 24-Jul-85			PRIORITY INTERRUPT PROCESSING				

						; 10385	;HERE FOR DATAO (EXAMINE) FUNCTION
						; 10386	
						; 10387	=0000
U 2100, 2065,4001,0000,0000,1020,0047,0000	; 10388	PIDATO:	AR0-3 DISP,CALL.M,J/PILD	;GET DATA FROM REQUESTED ADR SPC
U 2101, 3636,3243,2000,0000,0000,0050,0000	; 10389		AR_AR*4,CALL,J/RDEX		;RESTRICTED EXAMINE
U 2102, 3552,3200,0003,0000,0022,2250,0026	; 10390	PIOUT:	AR_MEM,SET DATAO,CALL,J/EBUSO	;SEND DATA
U 2103, 2104,4001,0003,0000,0002,0024,0502	; 10391	PIDONE:	MEM_AR,DISMISS			;DONE, DISMISS & RESUME NORMAL
U 2104, 0073,4001,0000,0000,0117,0010,0000	; 10392	PIFET:	VMA/PC,FETCH,J/NOP		;RESUME AS BEFORE
U 2105, 2014,1701,0000,0000,0320,0050,0000	; 10393	=0101	VMA_AR-1,CALL,J/DEXCHK		;GO PROT/RELOC THIS EXAM
						;;10394	.IFNOT/MODEL.B
						;;10395	=0111	VMA_ARX,LOAD AR,PHYS REF,J/PIOUT;PROTECTED PHYSICAL
						; 10396	.IF/MODEL.B
U 2107, 1770,3610,0207,4000,0320,0010,0175	; 10397	=0111	VMA_ARX AND ADMSK,ARX/AD,J/PHYS1;FORCE AC'S FOR 0-17
						; 10398	.ENDIF/MODEL.B
U 2117, 2102,0001,0000,0000,0000,0010,0000	; 10399	=1111	J/PIOUT				;PROT VIOLATION.  SEND 0
						; 10400	
						; 10401	;HERE FOR DATAI (DEPOSIT) FUNCTION
						; 10402	
						; 10403	=10
U 1422, 3551,3441,2000,0000,0060,2250,0027	; 10404	PIDATI:	SET DATAI,TIME/5T,CALL,J/EBUSI	;READ THE DATA
U 1423, 3635,3721,2400,0000,1000,0010,0000	; 10405		ARX_AR,AR_MQ			;DATUM TO ARX, GET FCN WORD
U 3635, 2120,3242,0620,7310,0020,5210,0040	; 10406		BRX/ARX,ARX_BRX,SKP AR6		;RESTRICTED?
U 2120, 2145,3242,6000,0000,1020,0007,0000	; 10407	=0000	AR0-3 DISP,AR_BRX,J/PIST	;NO, STORE AS REQUESTED
U 2121, 3636,3243,2000,0000,0000,0050,0000	; 10408		AR_AR*4,CALL,J/RDEX		;YES, GET PROT/RELOC ADDR
U 2125, 2014,4003,0000,0000,0320,0050,0000	; 10409	=0101	VMA_AR+1,CALL,J/DEXCHK		;VERIFY LEGALITY
U 2127, 2155,3202,6000,0000,0000,0010,0000	; 10410	=0111	AR_BRX,J/PSTOR			;DATA IN AR, ADDR IN ARX. STORE PHYS
U 2137, 2103,4001,0000,0000,0000,0010,0000	; 10411	=1111	J/PIDONE			;PROT VIOLATION, STORE NOTHING
						; 10412	
						; 10413	=0101
U 2145, 2103,0001,0000,0000,0016,0026,0113	; 10414	PIST:	STORE,EPT REF,J/PIDONE
U 2147, 2103,0001,0000,0000,0016,0010,0000	; 10415		STORE,J/PIDONE
						;;10416	.IFNOT/MODEL.B
						;;10417	PSTOR:	VMA_ARX,STORE,PHYS REF,J/PIDONE
						; 10418	.IF/MODEL.B
U 2155, 2070,3610,0207,0000,0320,0010,0175	; 10419	PSTOR:	VMA_ARX AND ADMSK,ARX/AD,J/PHYS3;FORCE AC'S FOR 0-17
						; 10420	.ENDIF/MODEL.B
U 2157, 2103,4001,0000,0000,0000,0010,0000	; 10421		J/PIDONE
						; 10422	
U 3636, 3637,4001,4000,7311,2000,0010,0030	; 10423	RDEX:	FE_# AND S,#/30,AR_ARX		;DTE# *8 TO FE, ADDR TO AR
U 3637, 3640,0001,0040,2030,2000,0110,0145	; 10424		BR/AR,AR0-8_FE+#,#/145		;SAVE ADDR TO BR, GET EPT LOC
U 3640, 3641,3401,2400,0302,1000,0010,0011	; 10425	GTAR08:	ARX_AR,AR_0S,SC_#,#/9.
U 3641, 0004,0001,4000,0000,0000,0003,0000	; 10426		AR_SHIFT,RETURN4
						; 10427	
						; 10428	=0*
U 2014, 2065,4001,0000,0000,0000,0050,0000	; 10429	DEXCHK:	CALL,J/PILD			;PROTECTION WORD FROM EPT
U 2016, 1740,3102,0004,0000,0020,5410,0000	; 10430		SKP AR GT BR			;ALLOWED?
U 1740, 0012,3401,2000,0000,0000,0003,0000	; 10431	=00	AR_0S,RETURN12			;NO, SEND 0, STORE NOTHING
U 1741, 2065,4001,0000,0000,0000,3650,0000	; 10432		VMA_VMA+1,CALL,J/PILD		;YES, GET RELOCATION WORD
U 1743, 3642,0602,2004,0000,0020,0010,0000	; 10433	=11	AR_AR+BR			;RELOCATE TO PHYSICAL ADDR
U 3642, 0002,3600,0207,4000,0020,0003,0175	; 10434		ARX_AR AND ADMSK,RETURN2	;STRIP TO 23 BITS
						; 10435	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 18
; IO.MIC[10,5351]	19:56 24-Jul-85			PRIORITY INTERRUPT PROCESSING				

						; 10436	;FORCE AC'S FOR 0-17
						; 10437	
						; 10438	.IF/MODEL.B
						; 10439	=00
U 1770, 3643,4001,0000,0302,0000,0050,0040	; 10440	PHYS1:	SC_#,#/32.,CALL,J/CHKAC		;DATAO (EXAMINE)
U 1772, 2102,0001,0000,0000,0012,0010,0000	; 10441	=10	LOAD AR,J/PIOUT			;AC REF DONT USE PHYS REF
U 1773, 2102,4001,0000,0000,0012,0026,0103	; 10442		LOAD AR,PHYS REF,J/PIOUT	;NOT AC'S GET PHYSICAL MEMORY
						; 10443	
						; 10444	=00
U 2060, 3643,4001,0000,0302,0000,0050,0040	; 10445	PHYS2:	SC_#,#/32.,CALL,J/CHKAC		;DATAO (EXAMINE)
U 2062, 0770,4001,0000,0000,0012,0010,0000	; 10446	=10	LOAD AR,J/XFERW
U 2063, 0770,0001,0000,0000,0012,0026,0103	; 10447		LOAD AR,PHYS REF,J/XFERW	;NOT AC'S GET PHYSICAL MEMORY
						; 10448	
						; 10449	=00
U 2070, 3643,4001,0000,0302,0000,0050,0040	; 10450	PHYS3:	SC_#,#/32.,CALL,J/CHKAC
U 2072, 2103,0001,0000,0000,0016,0010,0000	; 10451	=10	STORE,J/PIDONE
U 2073, 2103,4001,0000,0000,0016,0026,0103	; 10452		STORE,PHYS REF,J/PIDONE
						; 10453	
U 3643, 3644,0001,0400,0000,0000,0010,0000	; 10454	CHKAC:	ARX_SHIFT			;GET ADDRESS WITHOUT 32-35
						; 10455		ARX_ARX AND ADMSK,		;FLUSH GARBAGE IN 0-3
U 3644, 0002,3610,0207,0000,0020,5603,0175	; 10456			SKP AD NE,RETURN2	;AND MAKE THE TEST
						; 10457	.ENDIF/MODEL.B
						; 10458	
						; 10459	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 19
; IO.MIC[10,5351]	19:56 24-Jul-85			PRIORITY INTERRUPT PROCESSING				

						; 10460	;HERE FOR BYTE TRANSFERS
						; 10461	
						; 10462	=000
U 2540, 3645,0001,0000,7311,0000,0050,0030	; 10463	PIBYTE:	FE_# AND S,#/30,CALL,J/PIBPA	;OUT... GET BP ADDR
U 2541, 3551,3441,2000,0000,0000,2250,0027	; 10464		SET DATAI,CALL,J/EBUSI		;IN ... FIRST READ THE DATA
U 2543, 2550,0321,5400,0000,1020,0010,0000	; 10465	=011	ARX_AR,AR_MQ*4,J/PIDPB		;GOT IT, GO DEPOSIT IT
						; 10466	
U 2544, 0730,3701,0000,0000,0300,0050,0000	; 10467	=100	VMA_AR,CALL,J/PIIBP		;GO INCREMENT OUTPUT BP
U 2545, 2656,4001,0000,2002,0000,0050,0000	; 10468		SC_FE+SC,CALL,J/LDB1		;GO LOAD BYTE FROM IT
U 2547, 2102,0001,0000,0000,0000,0010,0000	; 10469	=111	J/PIOUT				;THEN SEND BYTE
						; 10470	
						; 10471	=000
						; 10472	PIDPB:	BRX/ARX,FE_# AND S,#/30,	;HERE WITH INPUT DATA
U 2550, 3645,4001,0020,7311,0000,0050,0030	; 10473			CALL,J/PIBPA
U 2554, 0730,4003,0000,0000,0320,0050,0000	; 10474	=100	VMA_AR+1,CALL,J/PIIBP		;GO INCREMENT INPUT BYTE PTR
						; 10475		AR_BRX,SC_#-SC,#/36.,		;STORE BYTE WITH IT
U 2555, 2664,3202,6000,5302,0020,5150,0044	; 10476			SKP SCAD0,CALL,J/DPB1
U 2557, 2103,4001,0000,0000,0000,0010,0000	; 10477	=111	J/PIDONE
						; 10478	
U 3645, 3640,4001,0000,2030,2000,0110,0140	; 10479	PIBPA:	AR0-8_FE+#,#/140,J/GTAR08
						; 10480	
						; 10481	=00*
U 0730, 2065,4001,0000,0000,0000,0050,0000	; 10482	PIIBP:	CALL,J/PILD			;GET POINTER FROM EPT
U 0732, 1722,0001,0000,5110,3021,5150,0200	; 10483		P_P-S,SKP SCAD0,CALL.M,J/IBPS	;INCREMENT IT
U 0736, 1744,4001,0400,0102,1000,0010,0000	; 10484	=11*	ARX_AR,SC_P,J/BYTEA		;NOW EVALUATE ITS ADDR
						; 10485	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 20
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

						; 10486	.TOC	"KL-MODE PAGE REFILL LOGIC"
						; 10487	
						; 10488	.IF/KLPAGE
						; 10489	;HERE ON ANY PAGE FAILURE
						; 10490	;THE POSSIBLE CAUSES ARE --
						; 10491	;  1:	A PARITY ERROR WAS DETECTED IN AR OR ARX FOLLOWING A READ
						; 10492	;	REFERENCE.  IN THIS CASE WE SAVE THE BAD WORD IN A RESERVED
						; 10493	;	LOCATION IN FAST MEMORY BLOCK 7, AND RETURN A PAGE FAIL CODE
						; 10494	;	INDICATING THE ERROR.
						; 10495	;  2:	THE MBOX DETECTED A PROPRIETARY VIOLATION OR PAGE TABLE PARITY
						; 10496	;	ERROR, OR THE EBOX FOUND THE SELECTED ADDRESS BREAK CONDITION.
						; 10497	;	IN THIS CASE, WE RETURN THE PAGE FAIL CODE GENERATED BY THE
						; 10498	;	MBOX (SEE PRINT PAG4).
						; 10499	;  3:	A REFERENCE OCCURRED FOR A VIRTUAL PAGE FOR WHICH THE HARDWARE
						; 10500	;	PAGE TABLE DIRECTORY HAD NO VALID MATCH.  IN THIS CASE, WE
						; 10501	;	WRITE THE PAGE TABLE DIRECTORY FROM THE VMA, AND CLEAR THE
						; 10502	;	ACCESS BITS FOR ALL PAGE ENTRIES CONTROLLED BY THE SELECTED
						; 10503	;	DIRECTORY ENTRY.  WE THEN JOIN THE REFILL CODE, BELOW.
						; 10504	;  4:	A REFERENCE OCCURRED FOR A VIRTUAL PAGE FOR WHICH THE ACCESS BIT
						; 10505	;	IN THE HARDWARE PAGE TABLE WAS OFF, OR A WRITE OCCURRED TO A
						; 10506	;	PAGE WHOSE WRITABLE BIT WAS OFF.  IN THIS CASE, WE EVALUATE THE
						; 10507	;	PAGING POINTERS IN CORE TO DETERMINE WHETHER THE ACCESS SHOULD
						; 10508	;	BE ALLOWED, AND IF SO, THE PHYSICAL PAGE TO WHICH IT SHOULD BE
						; 10509	;	TRANSLATED.  WE THEN EITHER PAGE FAIL, OR WRITE A PAGE ENTRY
						; 10510	;	INTO THE HARDWARE PAGE TABLE AND RESTART THE REFERENCE.
						; 10511	;
						; 10512	;	[322] Note that in the latter case, if a page should be accessible
						; 10513	;	but not writable, it is the microcode's responsibility to turn on
						; 10514	;	bit 2 of the page fail word (the Accessible bit) if a write
						; 10515	;	reference is attempted to such a page.  Currently, this is done only
						; 10516	;	if no CST is present (TOPS-10 operating mode), since TOPS-20
						; 10517	;	retraces the page map from scratch and thus generates the correct
						; 10518	;	information.  The bit can be made correct for the CST present case
						; 10519	;	(see code near NOTWR), but since we are now quite tight on control
						; 10520	;	store, we have chosen not to implement this.
						; 10521	;
						; 10522	;	If you are looking at this code for the first time, be aware that
						; 10523	;	only AR, ARX, SC, and FE are saved and restored here; thus BRX and
						; 10524	;	MQ are strictly off limits for this code.
						; 10525	;
						;;10526	.IFNOT/MODEL.B
						;;10527	2377:	CLR ACCOUNT EN,FORCE AR-ARX,J/PF1
						; 10528	.IF/MODEL.B
U 3777, 3646,3203,7700,0000,0000,2110,0145	; 10529	3777:	CLR ACCOUNT EN,FORCE AR-ARX,J/PF1
						; 10530	.ENDIF/MODEL.B
U 1777, 3646,3203,7700,0000,0000,2110,0145	; 10531	1777:	CLR ACCOUNT EN,FORCE AR-ARX
U 3646, 3647,3701,4207,0000,2000,1010,0156	; 10532	PF1:	SV.AR_AR,AR_ARX,ARX_AR (AD)	;SAVE CURRENT AR
U 3647, 3650,3703,4207,0000,2000,1010,0157	; 10533		SV.ARX_AR,AR_ARX,ARX_AR (AD)	; AND ARX
						;;10534	.IF/PAGCNT				;[327] Page fault counting
						;;10535		AR_TRX2+1			;[327] Count this page fault
						;;10536		TRX2_AR
						;;10537		AR_SV.AR			;[346] Don't lose initial AR
						; 10538	.ENDIF/PAGCNT				;[327]
U 3650, 2174,0001,0000,0000,0000,2204,0400	; 10539		GET ECL EBUS,PF DISP,J/PF2	;PARITY ERROR?
						; 10540	=1100
						; 10541	PF2:; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 20-1
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

						; 10542	.IFNOT/PAGCNT				;[327]
U 2175, 3070,0001,0000,0301,0000,6410,0036	; 10543	=1101	FE_#,#/36,SKP RPW,J/PFPAR	;YES.  AR PARITY ERROR, CODE 36
						; 10544						;CHECK FOR MIDDLE OF RPW CYCLE
						;;10545	.IF/PAGCNT
						;;10546	=1101	AR_SV.AR,J/ARPAR		;[327] AR parity error. Get back
						; 10547	.ENDIF/PAGCNT				;[327] saved AR
U 2176, 3070,4001,4000,0301,2000,6410,0037	; 10548		AR_ARX,FE_#,#/37,SKP RPW,J/PFPAR;[307] YES, ARX PARITY. COULD BE RPW
U 2177, 3651,4001,3000,0000,0062,2010,0567	; 10549		AR_EBUS REG,MB WAIT		;NO. GET PAGE FAIL WORD
U 3651, 2207,0001,0000,0000,0000,2204,0000	; 10550		REL ECL EBUS,PF DISP,J/PF4	;EBOX HANDLING REQUIRED?
						; 10551	;
						;;10552	.IF/PAGCNT				;[327]
						;;10553	ARPAR:	FE_#,#/36,SKP RPW,J/PFPAR	;Set code 36 and check for RPW
						; 10554	.ENDIF/PAGCNT
						; 10555	
						; 10556	;HERE ON ANY PARITY ERROR
						; 10557	;SKIP IF MIDDLE OF READ-PAUSE-WRITE CYCLE, IN WHICH CASE WE
						; 10558	; MUST WRITEBACK THE DATA TO PREVENT INCOMPLETE CYCLE
						; 10559	=0
U 3070, 3652,3733,0000,0000,0303,1710,0000	; 10560	PFPAR:	VMA_VMA HELD,J/PFPAR1		;MAY HAVE CHANGED AT MBWAIT
U 3071, 3652,4001,0000,0000,0016,0010,0000	; 10561		STORE				;WRITEBACK WITH GOOD PARITY
U 3652, 3653,4001,0000,0302,0007,0010,0140	; 10562	PFPAR1:	MAP,SC_#,#/140			;GET MAP INFO ON REF
U 3653, 3654,0001,0007,0000,0002,1010,0160	; 10563	PFPAR2:	SV.PAR_AR,MB WAIT		;[234]SAVE WORD WITH BAD PARITY
U 3654, 3655,4001,3000,0000,0060,2010,0567	; 10564		AR_EBUS REG			;READ MAP INFO
U 3655, 3656,0001,0000,7102,0000,2210,0000	; 10565		REL ECL EBUS,SC_P AND SC	;GET USER BIT FROM MAP WORD
U 3656, 2207,0001,0000,6000,3001,0010,0200	; 10566		P_FE OR SC,J/PF4		;STUFF IN PARITY ERROR CODE
						; 10567	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 21
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

						; 10568	;HERE WITH PAGE FAIL WORD IN AR
						; 10569	; TESTING FOR EBOX HANDLING REQUIRED.
						; 10570	
						; 10571	=0111
U 2207, 0040,4001,0007,0000,0000,1005,0152	; 10572	PF4:	SV.PFW_AR,SR DISP,J/CLEAN	;NO, GO CLEAN UP
U 2217, 3657,3242,2047,0000,0000,1010,0152	; 10573		SV.PFW_AR,AR_BR,BR/AR		;YES, GET BR TOO
U 3657, 3660,3401,2007,0000,0000,1010,0150	; 10574		SV.BR_AR,AR_0S			;SAVE BR
U 3660, 3661,4001,0000,0000,2000,0022,0200	; 10575		AR0-8_FE			;NOW SAVE 10-BIT REGS
U 3661, 3662,3703,0200,2400,2000,0022,0200	; 10576		ARX_AR (AD),AR0-8_SC		;FE TO ARX, SC TO AR
						; 10577		ARR_ARL,ARL_ARX (ADX),		;FE IN ARL, SC IN ARR
U 3662, 3663,3733,4200,0000,3001,1710,0006	; 10578			ARX_VMA HELD		;GET VMA WHICH FAILED
						; 10579		SV.SC_AR,AR_ARX,		;HOLD SC & FE
U 3663, 2236,3241,4007,0000,2020,1032,0151	; 10580			GEN BR*2,SIGNS DISP	;TEST FOR PT DIR MATCH
						; 10581	=1110
						; 10582	PGRF1:	SV.VMA_AR,ARX_AR SWAP,ARR_ARL,	;GET SEC # TO AR32-35
U 2236, 2255,0001,4407,0000,3000,1036,0145	; 10583			DISP/EA MOD,J/PGRF2	; SEC < 20?
						; 10584	
						; 10585	;	HERE TO WRITE PT DIR, & CLR 4 PAGE ENTRIES.  If the expanded
						; 10586	;	page table ECO has been installed, this will only clear two entries
						; 10587	;	(since they go in pairs for that case), but in either case the
						; 10588	;	right thing will happen. [333]
						; 10589	;
						; 10590		AR_0S,COND/MBOX CTL,MBOX CTL/2,	;READY TO CLEAR EVEN PAIR
U 2237, 3664,3401,2000,7133,0000,2310,0002	; 10591			FE_P AND #,#/2,SC/SCAD	;GET WRITE REF BIT TO FE & SC
						; 10592		COND/MBOX CTL,MBOX CTL/33,	;CLR EVEN, WR DIR, SEL ODD
U 3664, 3665,0001,0000,2003,0020,2310,0033	; 10593			TIME/3T,FE_FE+SC,SC/SCAD; WR REF = 4 NOW
						; 10594		COND/MBOX CTL,MBOX CTL/10,	;CLR ODD, RESET NORMAL SELECT
U 3665, 3666,3202,2000,2001,0020,2310,0010	; 10595			TIME/3T,FE_FE+SC,AR_BR	;GET PFW BACK, WR REF = 10
U 3666, 3667,4001,0000,7322,0000,0010,0401	; 10596		SC_# AND AR0-8,#/401		;GET USER & PAGED REF BITS
U 3667, 3670,4001,0000,6000,2001,0010,0200	; 10597		AR0-8_FE OR SC			;COMBINE WITH WR REF BIT
U 3670, 2236,0001,4047,0000,2000,1010,0152	; 10598		SV.PFW_AR,BR/AR,AR_ARX,J/PGRF1	;REJOIN MAIN PATH
						; 10599	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

						; 10600	;	HERE TO TRACE PAGE POINTERS FOR THIS ADDRESS
						; 10601	;	VMA WHICH FAILED IS IN ARX AND AR WITH THE HALVES SWAPPED
						; 10602	;	PAGE FAIL WORD IS IN BR
						; 10603	;	[333] Bit 5 of all access pointers is implemented as "Keep" for
						; 10604	;	pages which should not be swept from the page map on DATAO PAG
						; 10605	;	UBR reload unless bit 3 is also set.
						; 10606	;
						; 10607	
						;;10608	.IFNOT/EPT540
						;;10609	=1101
						;;10610	PGRF2:	VMA_#+AR32-35,#/440,		;YES.
						;;10611			SIGNS DISP,J/PGRF3	; USER REF?
						;;10612		VMA_#+AR32-35,#/460,		;NO
						;;10613			SIGNS DISP,J/PGRF3
						; 10614	.IF/EPT540
						; 10615	=1101
						; 10616	PGRF2:	VMA_#+AR32-35,#/540,		;YES.
U 2255, 2275,4001,0000,0000,0120,3332,0540	; 10617			SIGNS DISP,J/PGRF3	; USER REF?
						; 10618		VMA_#+AR32-35,#/560,		;NO
U 2257, 2275,0001,0000,0000,0120,3332,0560	; 10619			SIGNS DISP,J/PGRF3
						; 10620	.ENDIF/EPT540
						; 10621	=1101
U 2275, 3671,4001,0000,3401,0012,0026,0113	; 10622	PGRF3:	LOAD AR,EPT REF,FE_-1,J/SECPTR	;Initialize APWKC bits, get section
U 2277, 3671,4001,0000,3401,0012,0026,0223	; 10623		LOAD AR,UPT REF,FE_-1		; pointer from EPT or UPT
						; 10624	
						; 10625	;HERE TO FIND PAGE MAP WITH SECTION POINTER
						; 10626	
U 3671, 2310,3240,0003,0302,0022,7010,0011	; 10627	SECPTR:	AR_MEM,SC_#,#/9,SKP INTRPT	;GET SECTION POINTER
						; 10628	=1000	FE_FE AND AR0-8,BR/AR,		;COMBINE ACCESS BITS
U 2310, 2311,0001,0040,7021,1020,0007,0000	; 10629			AR0-3 DISP		;SPT INDEX IN ARR, DISP ON TYPE
U 2311, 3726,3240,2007,0000,0020,0010,0150	; 10630	=1001	AR_SV.BR,J/PFT			;NO ACCESS TO SECTION (OR INTRPT)
						; 10631	=1011
U 2313, 3701,0001,0400,0000,0001,0010,0020	; 10632	SECIMM:	ARX_SHIFT,ARL_0.M,J/PGRF5	;IMMEDIATE
U 2315, 3074,4001,0000,0000,0001,0010,0020	; 10633	=1101	ARL_0.M,J/LDIND			;SHARED
U 2317, 3672,4001,0000,0000,0001,0010,0020	; 10634	=1111	ARL_0.M				;INDIRECT SECTION POINTER
						; 10635	;
						; 10636	;	WARNING:  do not use the technique at LDIND to allow
						; 10637	;	interrupts out of section pointer loops, as that will have
						; 10638	;	adverse effects on byte transfers and console executes.
						; 10639	;
U 3672, 3673,0600,4007,4000,2320,0010,0143	; 10640		VMA_AR+SBR,AR_ARX		;LOOK IN SPT
U 3673, 3674,3242,2040,0000,0012,0026,0103	; 10641		BR/AR,AR_BR,LOAD AR,PHYS REF	;CALL FOR SPT ENTRY
U 3674, 3675,3240,0403,0000,0022,0010,0000	; 10642		ARX_SHIFT,AR_MEM		;SEC PTR INDEX TO ARX0-8
U 3675, 3676,3242,4200,0000,0000,0010,0000	; 10643		AR_SHIFT,ARX_BR			;NEW SEC PTR ADDR TO AR
						; 10644		GEN # AND AR0-8,#/77,SKP SCAD NE,
U 3676, 3072,3701,0000,7320,0320,5210,0077	; 10645			VMA_AR
U 3072, 3671,0001,0000,0000,0012,0026,0103	; 10646	=0	LOAD AR,PHYS REF,J/SECPTR
U 3073, 3726,3240,2007,0000,0020,0010,0150	; 10647		AR_SV.BR,J/PFT			;TRAP, SEC MAP NOT IN CORE
						; 10648	;
						; 10649	;	We must turn off special cycle for indirect pointers so that
						; 10650	;	we can take an interrupt.  However, we can't do it if PXCT or
						; 10651	;	SXCT might be active.  Thus, a kernel mode program can get into
						; 10652	;	a page fail loop that the microcode cannot exit.
						; 10653	;
						; 10654	=0
U 3074, 3677,0600,0007,0000,0320,0010,0143	; 10655	LDIND:	VMA_AR+SBR,J/LDIND1		;FOR INDIRECT PAGE POINTERS; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22-1
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

U 3075, 3074,0001,0000,0000,0000,1510,0000	; 10656		CLR SPECIAL CYCLE,J/LDIND
						; 10657	;
						; 10658	;	This fixes the glitch that INSTR FETCH (NICOND) doesn't
						; 10659	;	clear 'CON4 INT DISABLE L' before fetching user's instruction.
						; 10660	;					;SHARED SEC = INDRCT PAG
U 3677, 3700,4001,0000,0000,0012,0026,0103	; 10661	LDIND1:	LOAD AR,PHYS REF		;GET PAGE MAP ADDR
U 3700, 2313,3240,0003,0000,0022,0010,0000	; 10662		AR_MEM,J/SECIMM
						; 10663	
						; 10664	
						; 10665	;HERE WITH PAGE NO OF PAGE MAP IN AR,
						; 10666	; VIRTUAL PAGE NO WITHIN SECTION IN ARX0-8
						; 10667	
						; 10668	.IF/NOCST
U 3701, 3100,3240,0007,0000,0020,5610,0142	; 10669	PGRF5:	TEST CBR			;[247] CBR = 0 MEANS NO CST UPDATE
						; 10670	.IF/BIG.PT				;[346]
						; 10671	=0	AR_ARX,FE_FE AND #,#/174,	;[247][333] NO CST UPDATE
U 3100, 3706,4001,4000,7031,2000,0010,0174	; 10672			J/NO.CST		;[247]
						;;10673	.IFNOT/BIG.PT				;[346]
						;;10674	=0	AR_ARX,FE_FE AND #,#/164,	;[247] NO CST UPDATE. Eat bit 5
						;;10675			J/NO.CST		;[247] if no big page table
						; 10676	.ENDIF/BIG.PT				;[346]
						;;10677	.IFNOT/NOCST				;[247]
						;;10678	PGRF5:
						; 10679	.ENDIF/NOCST				;[247]
U 3101, 3702,0600,4007,4000,2320,0010,0142	; 10680		VMA_AR+CBR,AR_ARX		;GENERATE CST ADDRESS
						; 10681		GEN # AND AR0-8,#/77,		;IS PAGE MAP IN CORE?
U 3702, 3102,4001,0000,7320,0020,5210,0077	; 10682			SKP SCAD NE
						; 10683	=0	LOAD AR,PHYS REF,		;GET CST ENTRY FOR PAGE MAP
U 3102, 3703,0001,0040,0000,0012,0026,0103	; 10684			BR/AR,J/PGRF6		;SAVE PAGE PTR ADDR IN BR
U 3103, 3726,3240,2007,0000,0020,0010,0150	; 10685		AR_SV.BR,J/PFT			;NOT IN CORE, PAGE FAIL TRAP
						; 10686	;
						; 10687	.IF/BIG.PT				;[346]
U 3703, 3704,3200,0003,7031,0022,0010,0174	; 10688	PGRF6:	AR_MEM,FE_FE AND #,#/174	;[333]HERE IF CST FOR PAGE MAP
						;;10689	.IFNOT/BIG.PT				;[346]
						;;10690	PGRF6:	AR_MEM,FE_FE AND #,#/164	;HERE IF CST FOR PAGE MAP. No K bit
						; 10691	.ENDIF/BIG.PT				;[346] if no big page table
U 3704, 3114,3600,2007,0100,0040,5210,0140	; 10692		AR_AR AND CSMSK,SKP P NE	;BEGIN CST UPDATE
U 3114, 3726,3240,2007,0000,0020,0010,0150	; 10693	=0	AR_SV.BR,J/PFT			;AGE TRAP, MAP BEING SWAPPED
U 3115, 3705,3300,2007,0000,0036,0010,0141	; 10694		AR_AR OR PUR,STORE		;PUT CST WORD BACK
						;;10695	.IFNOT/NOCST				;[247]
						;;10696		MEM_AR,VMA_BR,PHYS REF		;PHYS REF MAKES MODEL.A LOAD
						;;10697						;LONG VMA
						;;10698		LOAD AR,PHYS REF		;GET PAGE MAP ENTRY
						;;10699		AR_MEM,FE_FE OR #,#/100,	;PAGE POINTER
						;;10700			SKP INTRPT		;CHECK FOR LONG INDIRECT
						;;10701						;POINTER
						; 10702	.IF/NOCST				;[247]
U 3705, 3116,3202,0003,0000,0302,0026,0103	; 10703		MEM_AR,VMA_BR,PHYS REF,J/NOCST0	;PHYS REF MAKES MODEL.A LOAD
						; 10704						;LONG VMA
						; 10705	NO.CST:	GEN # AND AR0-8, #/77,		;[247] page map in core ?
U 3706, 3116,3701,0000,7320,0320,5210,0077	; 10706			VMA_AR,SKP SCAD NE	;[247]
						; 10707	=0
U 3116, 3707,0001,0000,0000,0012,0026,0103	; 10708	NOCST0:	LOAD AR,PHYS REF, J/NOCST1	;[247] GET PAGE MAP ENTRY
U 3117, 3726,3240,2007,0000,0020,0010,0150	; 10709		AR_SV.BR, J/PFT			;[247] not in core, pf trap
						; 10710	NOCST1:	AR_MEM,FE_FE OR #,#/100,	;[247] PAGE POINTER
U 3707, 2330,3240,0003,6031,0022,7010,0100	; 10711			SKP INTRPT		;[247] CHECK FOR LONG INDIRECT; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22-2
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

						; 10712	.ENDIF/NOCST				;[247] POINTER
						; 10713	;
						; 10714	;	HERE WITH PAGE MAP ENTRY IN AR
						; 10715	;	FE HAS ACCUMULATED ACCESS BITS -- APWKC*4
						; 10716	;	SC CONTAINS 9.
						; 10717	;
						; 10718	=1000	FE_FE AND AR0-8,AR0-3 DISP,	;COMBINE PWKC, DISP ON TYPE
U 2330, 2331,3200,0207,7021,1020,0007,0145	; 10719			ARX_SV.VMA,TIME/3T	;[346] GET BACK SAVED VMA
U 2331, 3726,3240,2007,0000,0020,0010,0150	; 10720	=1001	AR_SV.BR,J/PFT			;0=NO ACCESS (OR HERE ON INTRPT)
						; 10721	=1011
						; 10722	LDIMM:	ARL_SHIFT,FE_FE SHRT,		;1=IMMEDIATE, LOAD PT
						; 10723			ARX_ARX*2 COMP,		; GET -WR REF TO ARX03
U 2333, 3713,2021,0500,0302,0001,1210,0004	; 10724			SC_#,#/4,J/LDPT		;[333] Set to move K bit
U 2335, 3710,4001,0000,0000,0001,0010,0020	; 10725	=1101	ARL_0.M,J/LDSHR			;2=SHARED, GET SPT ENTRY
						; 10726	=1111	ARL_0.M,ARX_SHIFT,SKP USER,	;3=INDIRECT, LOOP
U 2337, 3074,4001,0400,0000,0001,6210,0020	; 10727			J/LDIND
						; 10728	;
						; 10729	;	HERE TO GET SHARED PAGE POINTER OUT OF SPT
						; 10730	;
U 3710, 3711,0600,0007,4000,0320,0010,0143	; 10731	LDSHR:	VMA_AR+SBR			;ADDRESS OF SPT ENTRY
U 3711, 3712,4001,0000,0000,0012,0026,0103	; 10732		LOAD AR,PHYS REF
U 3712, 2333,3200,0003,6031,0022,0010,0100	; 10733		AR_MEM,FE_FE OR #,#/100,J/LDIMM	;TREAT SPT ENTRY AS IMMED
						; 10734	
						; 10735	;
						; 10736	;	HERE WITH IMMEDIATE PAGE NO IN AR TO LOAD INTO PT
						; 10737	;
						; 10738	
U 3713, 3120,4001,0000,7320,0020,5210,0077	; 10739	LDPT:	GEN # AND AR0-8,#/77,SKP SCAD NE;Test storage medium
						; 10740	.IF/BIG.PT				;[333]
						; 10741	=0	ARL_0.M,GEN FE AND SC,		;[333]In core. Is Keep bit set?
U 3120, 3122,4001,0000,7000,0021,5210,0020	; 10742			SKP SCAD NE,J/LDPT1
						;;10743	.IFNOT/BIG.PT
						;;10744	=0	ARL_0S,J/LDPT1
						; 10745	.ENDIF/BIG.PT				;[333]
U 3121, 3726,3240,2007,0000,0020,0010,0150	; 10746		AR_SV.BR,J/PFT			;PAGE NOT IN CORE
						; 10747	;
						; 10748	.IF/BIG.PT				;[333]
						; 10749	=0
						; 10750	.ENDIF/BIG.PT
U 3122, 3714,3711,7400,4402,1000,0010,0000	; 10751	LDPT1:	ARX_AR,AR_ARX*.25,SC_1,J/KMOVED	;[333]No K. GET -WR REF TO AR05
						; 10752	.IF/BIG.PT
U 3123, 3122,4001,0000,5001,0000,0110,0010	; 10753		AR0-8_#,#/10,FE_FE-SC,J/LDPT1	;[333]K set. Move to bit 5 for now
						; 10754	.ENDIF/BIG.PT
						; 10755	;
						; 10756	.IF/NOCST				;[247]
U 3714, 3124,3200,0007,7102,0020,5610,0142	; 10757	KMOVED:	TEST CBR, SC_P AND SC		;[247][333]CBR = 0? (-WR REF TO SC)
						; 10758	=0	SC_-SC,AR_ARX,ARX_AR (AD),
U 3124, 3717,3701,4200,5402,2000,0010,0000	; 10759			J/NOUPDT		;[247] YES, SKIP SOME
						;;10760	.IFNOT/NOCST				;[333]
						;;10761	KMOVED:					;[333]
						; 10762	.ENDIF/NOCST				;[247]
U 3125, 3715,0610,4007,7102,2320,0010,0542	; 10763		VMA_ARX+CBR,AR_ARX,SC_P AND SC	;PAGE IN CORE. SC_-WR REF
						; 10764	;
						; 10765	;	NOW GET CST ENTRY FOR THIS PAGE.
						; 10766	;
						; 10767	GTCST:	LOAD AR,PHYS REF,ARX_AR SWAP,	;Shuffle K over to bit 23; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22-3
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

U 3715, 3716,4061,2400,5402,3032,0026,0103	; 10768			SC_-SC,AR_1		;SC=0 IF WR REF, ELSE -1
						; 10769		GEN FE AND #,#/10,SKP SCAD NE,	;SKIP IF WRITABLE
U 3716, 3126,4001,0040,7030,0022,5210,0010	; 10770			BR/AR,MB WAIT		;GET CST ENTRY & BIT FOR TESTING
						; 10771	=0	GEN P AND SC,SKP SCAD NE,	;FAIL IF WRITING OR AGE=0
U 3126, 3132,3600,2007,7100,0040,5210,0140	; 10772			AR_AR AND CSMSK,J/NOTWR	;STRIP OLD AGE FROM CST
						; 10773	;
						; 10774	;	[303] Looks like it's writable.  Make one final check by looking
						; 10775	;	at bit 18 of the CST entry, and abort if it's not set.
						; 10776	;
						; 10777		TEST AR.BR,SKP CRY0,		;POSSIBLY WRITABLE--SKIP IF CST
U 3127, 2536,3602,0004,5300,0040,5434,0777	; 10778			GEN #-SC,#/-1,BYTE DISP	; WRITTEN OR THIS IS WRITE REF
U 2536, 3131,4001,0000,5031,0000,0010,0004	; 10779	=110	FE_FE-#,#/4,J/STCST		;[305] TEMPORARILY UNWRITABLE, SET S
						; 10780	.IF/CST.WRITE				;[314]
						; 10781		AR_AR*BR,AD/OR,FE_FE OR #,#/4,	;SET CST WRITTEN AND SOFT BITS
U 2537, 3130,3302,2004,6031,0000,4410,0004	; 10782			SKP AR18		;IS IT REALLY WRITABLE? [303]
U 3130, 3726,3240,2007,0000,0020,0010,0150	; 10783	=0	AR_SV.BR,J/PFT			;NOT REALLY. BAIL OUT
						;;10784	.IFNOT/CST.WRITE			;[314]
						;;10785		AR_AR*BR,AD/OR,FE_FE OR #,#/4	;[314] Set CST written and soft bits
						; 10786	.ENDIF/CST.WRITE			;[314]
U 3131, 3132,3600,2007,0120,0040,5210,0140	; 10787	STCST:	AR_AR AND CSMSK,SKP P NE	;[305] WRITABLE. CLEAR, TEST OLD AGE
						; 10788	;
						; 10789	;	[322] At this point we should check whether we got here as a result
						; 10790	;	of an age trap (in which case we just take the page failure) or not,
						; 10791	;	in which case the failure was due to a write reference and we should
						; 10792	;	set bit 2 (the A bit) in the PFW.  This is not currently done
						; 10793	;	because (1) nobody needs it now, and (2) we are very short on CRAM
						; 10794	;	space.
						; 10795	;
						; 10796	=0
U 3132, 3726,3240,2007,0000,0020,0010,0150	; 10797	NOTWR:	AR_SV.BR,J/PFT			;WRITE OR AGE TRAP
						;;10798	.IFNOT/NOCST				;[247]
						;;10799		AR_AR OR PUR,STORE		;SET USE BITS, STORE BACK CST
						;;10800		MB WAIT,VMA_SV.VMA,		;RELOAD VMA FOR ORIGINAL REF
						;;10801			AR_SV.VMA,SC_1		;READY TO TEST VMA USER
						;;10802		GEN P AND SC,SKP SCAD NE,	;TEST VMA USER, copy page # to ARL,
						;;10803			AR_ARX,			;[333] K to AR bit 23, APMWC0 TO SC
						;;10804			SC_FE,ARX_AR (AD)	; MAP BIT TO ARX
						;;10805	=0	P_P OR SC#,EXEC REF,J/WRHPT	;BUILD PT ENTRY, CLEAR VMA USER
						;;10806		P_P OR SC#,USER REF		; OR SET USER, AS NECESSARY
						;;10807	WRHPT:	WR PT ENTRY,FE_#,#/10,AR_ARX*4	;UPDATE HARDWARE TABLE
						; 10808	.IF/NOCST
U 3133, 3137,3300,2007,4000,0036,0010,0141	; 10809		AR_AR OR PUR,STORE,J/WRHPT	;[247]SET USE BITS,
						; 10810						; STORE BACK CST
						; 10811	;
						; 10812	NOUPDT:	GEN FE AND #, #/10, ARX_AR SWAP,
U 3717, 3134,3713,2400,7030,3020,5210,0010	; 10813			AR_ARX (AD),SKP SCAD NE	;[247] SKIP IF WRITABLE
						; 10814	=0	AR_SV.PFW,GEN P AND SC,		;[322] Get saved PFW and
U 3134, 3136,3200,2007,7100,0040,5210,0152	; 10815			SKP SCAD NE,J/NOT.WR	;[247]FAIL IF WRITING
U 3135, 3137,4001,0000,6031,0000,0010,0004	; 10816		FE_FE OR #, #/4, J/WRHPT	;[247]SET WRITABLE BIT
						; 10817	;
U 3720, 3132,4001,0007,0000,0000,1010,0152	; 10818	WRFAIL:	SV.PFW_AR,J/NOTWR		;[322]Restore PFW and page fail
						; 10819	;
						; 10820	=0					;[323]
U 3136, 3720,4001,0000,6320,2000,0110,0100	; 10821	NOT.WR:	AR0-8_# OR AR0-8,#/100,J/WRFAIL	;[322]Write failure. Set A in PFW
U 3137, 3721,3240,4007,0000,2123,0013,0145	; 10822	WRHPT:	RSTR VMA_SV.VMA,AR_ARX,SC_FE	;RELOAD ORIGINAL VMA
U 3721, 3722,4001,0000,6100,3001,0010,0200	; 10823		P_P OR SC			;[333]COMBINE APMWC WITH PAGE #, K; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 22-4
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

U 3722, 3723,0001,0000,0000,0020,2310,0010	; 10824		WR PT ENTRY			;UPDATE HARDWARE PAGE TABLE
						; 10825	.ENDIF/NOCST
						; 10826	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 23
; IO.MIC[10,5351]	19:56 24-Jul-85			KL-MODE PAGE REFILL LOGIC				

						; 10827	;HERE WHEN MAP INFO WRITTEN INTO HARDWARE PAGE TABLE
						; 10828	; WE NOW NEED ONLY RESTORE THE REGISTERS WE HAVE USED, AND RESTART THE
						; 10829	; MEMORY REFERENCE WHICH FAILED, RETURNING TO THE MICROINSTRUCTION
						; 10830	; WHICH WAITS FOR ITS COMPLETION.  (EXCEPT FOR MAP)
						; 10831	
						;;10832	.IFNOT/MODEL.B
						;;10833		GEN FE AND S,SKP SCAD NE,	;TEST FOR MAP INSTR
						;;10834			AR_SV.SC
						; 10835	.IF/MODEL.B
U 3723, 3140,3200,2007,0000,0020,0010,0151	; 10836		AR_SV.SC
						; 10837	.ENDIF/MODEL.B
						; 10838	
						; 10839	;HERE TO RESTORE REGISTERS AND RESTART REFERENCE WHICH FAILED
						; 10840	
						; 10841	=0	SC_EXP,FE_EXP,SKP AR0,		;RESTORE FE
U 3140, 3142,3240,4207,0203,3020,4510,0150	; 10842			AR_AR SWAP,ARX_SV.BR,J/PGRST1
						;;10843	.IFNOT/MODEL.B
						;;10844		AR_SV.VMA,ARX_SV.VMA,J/COMPEA	;*MAP* RESTART FROM AREAD
						; 10845	.IF/MODEL.B
						; 10846	=
						; 10847	.ENDIF/MODEL.B
						; 10848	=0
						; 10849	PGRST1:	SC_EXP,SKP AR0,			;RESTORE SC
U 3142, 3144,3240,4207,0202,2020,4510,0156	; 10850			AR_ARX,ARX_SV.AR,J/PGRST2
U 3143, 3142,4001,0000,1401,0000,0010,0000	; 10851		FE_-SC-1,J/PGRST1		;MAKE FE NEG
						; 10852	=0
						; 10853	PGRST2:	BR/AR,AR_ARX,			;RESTORE BR AND AR
U 3144, 3724,3200,4247,0000,2020,0010,0157	; 10854			ARX_SV.ARX,J/PGRST3	; AND ARX
						; 10855		SC_-SC-1,BR/AR,AR_ARX,		;NEGATE SC
U 3145, 3724,3240,4247,1402,2020,0010,0157	; 10856			ARX_SV.ARX
						; 10857	
						; 10858	;HERE RETURN TO POINT OF FAULT.  THERE MUST BE EXACTLY ONE MICRO-
						; 10859	; INSTRUCTION, OF 2 OR 3 TICKS, BETWEEN THE REQUEST AND THE RETURN.
						; 10860	; AT LEAST ONE IS REQUIRED TO GET NICOND LOGIC SET UP CORRECTLY IN
						; 10861	; CASE THIS IS A FETCH, BUT THERE MUST NOT BE TIME FOR A READ TO
						; 10862	; READ REFERENCE TO COMPLETE, BECAUSE THE MB WAIT INSTRUCTION TO WHICH
						; 10863	; WE RETURN MAY EXPECT TO GET SOMETHING OUT OF AR OR ARX BEFORE THE
						; 10864	; MBOX RESPONSE.  SEE DPB1.
						; 10865	
U 3724, 3725,3240,0007,0000,0130,0010,0145	; 10866	PGRST3:	REQ SV.VMA			;RESTART FAULTED REQUEST
U 3725, 0000,4001,0000,0000,0000,2103,0105	; 10867		SET ACCOUNT EN,RETURN0		;RETURN TO POINT OF FAILURE
						; 10868	
						; 10869	
						; 10870	;HERE ON A TRAP CONDITION DETECTED BY REFILL LOGIC
						; 10871	;AR CONTAINS SAVED BR
						; 10872	
						; 10873	PFT:	BR/AR,VMA_SV.VMA,		;RESTORE BR & VMA
U 3726, 0040,3200,0047,0000,0320,0005,0145	; 10874			SR DISP,J/CLEAN		;TAKE TRAP
						; 10875	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 24
; IO.MIC[10,5351]	19:56 24-Jul-85			KI-MODE PAGE FAIL HANDLING				

						; 10876	.TOC	"KI-MODE PAGE FAIL HANDLING"
						; 10877	
						;;10878	.IFNOT/KLPAGE
						;;10879	.IFNOT/MODEL.B
						;;10880	2377:	CLR ACCOUNT EN,FORCE AR-ARX,J/PFSAVE
						;;10881	.IF/MODEL.B
						;;10882	3777:	CLR ACCOUNT EN,FORCE AR-ARX,J/PFSAVE
						;;10883	.ENDIF/MODEL.B
						;;10884	1777:	CLR ACCOUNT EN,FORCE AR-ARX	;DON'T CHARGE PAGE FAIL TO USER
						;;10885	PFSAVE:	SV.AR_AR,AR_ARX			;PRESERVE AR, ARX
						;;10886		SV.ARX_AR,SR DISP,J/CLEAN	;NOW CHECK FOR CLEANUP REQUIRED
						;;10887	
						; 10888	.ENDIF/KLPAGE
						; 10889	;HERE ON PAGE FAIL OR INTERRUPT WHICH REQUIRES CLEANUP IN ORDER
						; 10890	; TO BE CORRECTLY RESTARTABLE AFTER SERVICE...
						; 10891	
						; 10892	=1*0000
U 0040, 3146,0001,0000,0301,0000,7010,0037	; 10893	CLEAN:	FE_#,#/37,SKP INTRPT,J/PGF1	;HERE FOR INTRPT OR PGF?
						; 10894	
						; 10895	;(1) HERE ON EDIT SOURCE FAIL
						; 10896	
U 0041, 3751,3240,2001,0000,0020,1610,0000	; 10897		AR_SRCP,SR_0,J/BACKS		;BACK UP SRC POINTER
						; 10898	
						; 10899	;(2) HERE ON ANY FAILURE IN DECIMAL TO BINARY
						; 10900	
U 0042, 3756,3202,2600,0000,0000,1610,0001	; 10901		AR_BR LONG,SR_1,J/D2BPF		;GET ACCUMULATED BINARY
						; 10902	
						; 10903	;(3) HERE ON DST FAIL IN BINARY TO DECIMAL FILL
						; 10904	
U 0043, 3762,2540,2005,0000,0020,1610,0004	; 10905		AR_DLEN COMP,SR_#,#/4,J/B2DFPF
						; 10906	
						; 10907	;(4) HERE ON EDIT DST FAIL WITH NO SRC POINTER UPDATE
						; 10908	
U 0044, 3753,4001,0000,0000,0000,1610,0000	; 10909		SR_0,J/BACKD			;BACK UP DST POINTER ONLY
						; 10910	
						; 10911	;(5) HERE ON EDIT DST FAIL AFTER UPDATING SRC POINTER
						; 10912	
U 0045, 3753,0001,0000,0000,0000,1610,0001	; 10913		SR_1,J/BACKD			;BACK UP DST, THEN SRC
						; 10914	
						; 10915	;(6) HERE ON DESTINATION FAILURE IN BINARY TO DECIMAL
						; 10916	
U 0046, 3753,0001,0000,0000,0000,1610,0010	; 10917		SR_BDT,J/BACKD			;BACK UP DST, THEN SAVE FRACTION
						; 10918	
						; 10919	;(7) HERE ON BLT FAILURE
						; 10920	.IF/KLPAGE
U 0047, 3744,3240,0207,0000,0020,0010,0157	; 10921		ARX_SV.ARX,J/BLTPF		;GET DEST ADDR AGAIN
						;;10922	.IFNOT/KLPAGE
						;;10923	BLTPF:	AR_ARX+BR,SR_0,J/BLTPF1		;CURRENT SRC ADDR
						; 10924	.ENDIF/KLPAGE
						; 10925	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 25
; IO.MIC[10,5351]	19:56 24-Jul-85			KI-MODE PAGE FAIL HANDLING				

						; 10926	;(10) HERE ON TRANSLATION FAILURE IN BINARY TO DECIMAL
						; 10927	
U 0050, 3763,3242,2600,0000,0000,1610,0000	; 10928		AR_BR LONG,SR_0,J/B2DPF		;GET BINARY FRACTION
						; 10929	
						; 10930	;(11) HERE ON SRC FAILURE IN COMPARE OR MOVE STRING
						; 10931	
U 0051, 3767,3240,2005,0000,0020,1610,0001	; 10932		AR_DLEN,SR_1,J/STRPF		;PUT LENGTHS BACK, THEN BACK SRC
						; 10933	
						; 10934	;(12) HERE ON DST FAILURE IN COMPARE OR MOVE STRING
						; 10935	
U 0052, 3767,3240,2005,0000,0020,1610,0004	; 10936		AR_DLEN,SR_#,#/4,J/STRPF
						; 10937	
						; 10938	;(13) HERE ON DST FAILURE AFTER UPDATING SRC IN COMPARE OR MOVE
						; 10939	
U 0053, 3753,0001,0000,0000,0000,1610,0111	; 10940		SR_SRC,J/BACKD			;BACK DST, THEN HANDLE AS SRC FAIL
						; 10941	
						; 10942	;(14) HERE ON DST FILL FAILURE IN MOVRJ
						; 10943	
U 0054, 3773,3240,2005,0000,0020,1610,0004	; 10944		AR_DLEN,SR_#,#/4,J/STRPF4
						; 10945	.IF/KLPAGE
						; 10946	;(15) HERE ON PAGE FAILURE IN MAP INSTRUCTION.  RETURN PAGE FAIL WORD
						; 10947	
U 0055, 1020,3240,2007,0000,0020,7310,0152	; 10948		AR_SV.PFW,SKP IO LEGAL,J/MAP2	;RETURN PFW IN AC
						; 10949	.ENDIF/KLPAGE
						; 10950	.IF/XADDR
						; 10951	;(16) HERE ON PAGE FAIL IN XBLT
						; 10952	
U 0056, 3747,3242,2600,0000,0000,1610,0000	; 10953		AR_BR LONG,SR_0,J/XBLTPF
						; 10954	.ENDIF/XADDR
						; 10955	.IF/MODEL.B
						; 10956	;(17)	HERE ON FAILURE IN WORD MOVE STRING
						; 10957	
U 0057, 3773,3200,2005,0000,0020,1610,0000	; 10958		AR_DLEN,SR_0,J/STRPF4
						; 10959	.ENDIF/MODEL.B
						; 10960	=
						; 10961	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 26
; IO.MIC[10,5351]	19:56 24-Jul-85			KI-MODE PAGE FAIL HANDLING				

						; 10962	;HERE ON ANY PAGE FAILURE OR PI REQUEST IN LONG INSTRUCTIONS
						; 10963	; SKIP IF PI REQUEST, WHICH TAKES PRIORITY
						; 10964	
						; 10965	.IF/KLPAGE
						; 10966	=0
						; 10967	.IFNOT/SHIFT.MUUO
U 3146, 3727,0001,0000,0000,0102,3010,0500	; 10968	PGF1:	MB WAIT,VMA_#,#/500,J/PGF2
						;;10969	.IF/SHIFT.MUUO
						;;10970	PGF1:	MB WAIT,VMA_#,#/501,J/PGF2	;SHIFT TO CLOSER MATCH XADDR FORM
						; 10971	.ENDIF/SHIFT.MUUO
U 3147, 0071,0001,0000,0000,0000,2110,0105	; 10972		SET ACCOUNT EN,J/TAKINT		;CLEANUP DONE, SERVE INTRPT
U 3727, 2110,3240,2007,0000,0020,6510,0152	; 10973	PGF2:	AR_SV.PFW,SKP PI CYCLE,J/PGF4	;GET BACK PAGE FAIL WORD
						;;10974	.IFNOT/KLPAGE
						;;10975	=0
						;;10976	PGF1:	GET ECL EBUS,CLR SC,
						;;10977			PF DISP,J/PGF2
						;;10978		SET ACCOUNT EN,J/TAKINT		;HERE TO SERVE INTRPT, DO IT
						;;10979	=1100
						;;10980	PGF2:
						;;10981	=01	AR_SV.AR,FE_FE-1,		;AR PARITY ERROR
						;;10982			SKP RPW,J/PGF6		;DO WE NEED TO RESTART RPW?
						;;10983	=10	VMA_VMA HELD,MAP,SC_FE		;ARX PARITY ERROR, PF CODE 37
						;;10984	PGF3:	MB WAIT,VMA_#,#/500
						;;10985		AR_EBUS REG			;READ PAGE FAIL WORD
						;;10986		REL ECL EBUS
						;;10987		P_P OR SC,SKP PI CYCLE		;STUFF ERROR CODE IF PARITY
						; 10988	.ENDIF/KLPAGE
						; 10989	=00
						; 10990	PGF4:	ARX_AR,AR_VMA HELD,SC_#,#/13.,	;READY TO COMBINE PF WORD
U 2110, 1454,3733,2400,0302,1000,1750,0015	; 10991			CALL,J/ROTS		; WITH ADDRESS
						; 10992		ARX_AR,AR_VMA HELD,SC_#,#/13.,	;READY TO COMBINE PF WORD
U 2111, 3740,3733,2400,0302,1000,1710,0015	; 10993			J/IOPGF			; WITH ADDRESS
U 2113, 3730,4001,4000,0000,0000,1510,0004	; 10994	=11	AR_SHIFT,ABORT INSTR		;RECOVER TRAP FLAGS, IF ANY
						;;10995	.IFNOT/XADDR
						;;10996		STORE,UPT REF			;PUT PAGE FAIL WORD AT 500
						;;10997		FM[HARDPFW]_AR			;[406] Protect from soft page fail
						;;10998		FIN STORE,AR_PC,
						;;10999			VMA_VMA+1,STORE		;STORE OLD PC AT 501 OR 502
						;;11000		FIN STORE,AR_0S,
						;;11001			VMA_VMA+1,LOAD AR	;GET NEW PC FROM 502 OR 503
						;;11002		SET ACCOUNT EN,J/NEWPC
						; 11003	.IF/XADDR
U 3730, 3731,3731,0200,0000,0016,0026,0223	; 11004		STORE,UPT REF,ARX_PC		;PAGE FAULT WORD TO 500
U 3731, 3732,0001,0007,0000,0000,1010,0177	; 11005		FM[HARDPFW]_AR			;[406] Protect from soft page fail
						; 11006		AR_ARX ANDC ADMSK,MB WAIT,	;GET PC FLAGS FOR STORING
U 3732, 3733,3510,2227,0000,0022,0010,0175	; 11007			BRX/ARX,ARX/AD		;FULL PC IN BRX, FLAGS IN ARX
U 3733, 3150,0001,0000,0000,0000,6222,0030	; 11008		AR_0.S,SKP USER
U 3150, 0676,4001,0000,0000,2000,0250,0000	; 11009	=0	AR12-17_PREV SEC,CALL [ARSWAP]	;[334] GET PCS IF EXEC MODE
						; 11010		ARL_ARXL,ARR_ARR,		;FLAGS WORD IN AR
U 3151, 3734,4001,0000,0000,2016,3622,0004	; 11011			VMA_VMA+1,STORE		;STORE FLAGS WORD IN 501
						; 11012		AR_ARX*BRX,AD/ANDCA,		;GET PC ADDRESS
U 3734, 3735,3002,6004,0000,0016,3610,0000	; 11013			VMA_VMA+1,STORE		; STORE IT IN 502
U 3735, 3736,3401,2000,0000,0012,3610,0000	; 11014		AR_0S,VMA_VMA+1,LOAD AR		;GET NEW PC ADDRESS FROM 503
U 3736, 3737,4001,0000,0000,0000,0024,0020	; 11015		SET FLAGS_AR			;CLEAR ALL FLAGS
U 3737, 0751,3200,0003,0000,0022,2110,0105	; 11016		AR_MEM,SET ACCOUNT EN,J/ARJMP	;NEW ADDRESS FOR PC
						; 11017	.ENDIF/XADDR; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 26-1
; IO.MIC[10,5351]	19:56 24-Jul-85			KI-MODE PAGE FAIL HANDLING				

						; 11018	
						;;11019	.IFNOT/KLPAGE
						;;11020	
						;;11021	;HERE ON PARITY ERROR IN AR.  SKIP IF READ-PAUSE-WRITE IN PROGRESS
						;;11022	=0
						;;11023	PGF6:	VMA_VMA HELD,MAP,		;AR PARITY ERROR
						;;11024			SC_FE,J/PGF3		;PF CODE 36
						;;11025		STORE,J/PGF6			;COMPLETE CYCLE, WITH GOOD PAR
						; 11026	.ENDIF/KLPAGE
						; 11027	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 27
; IO.MIC[10,5351]	19:56 24-Jul-85			PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS

						; 11028	.TOC	"PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS"
						; 11029	
						; 11030	;HERE ON PAGE FAIL DURING PI CYCLE
						; 11031	
						;;11032	.IFNOT/MODEL.B			 	;[224][274]
						;;11033	IOPGF:	AR_SHIFT,ARX_SHIFT,SC_#-SC,	;TROUBLE... CREATE PAGE FAIL WORD
						;;11034			#/36.
						;;11035		AR_SHIFT,SET IO PF
						;;11036		SV.IOPF_AR,J/IOPFIN		;FINISH  IOPF ROUTINE
						; 11037	.IF/MODEL.B				;[224] DO FANCY STUFF FOR DTE.
U 3740, 3741,3200,2007,0000,0020,0010,0163	; 11038	IOPGF:	AR_FM[SV.IOP]			;[274] GET THE SAVED API WORD
U 3741, 3742,0001,0007,0000,0000,1010,0162	; 11039		SV.IOPF_AR			;[224] SAVE IT IN AC BLK 7.
U 3742, 3743,3200,2047,0000,0020,0010,0156	; 11040		BR/AR,AR_SV.AR			;[224] KEEP IOPF WORD AROUND,
U 3743, 2122,4001,0000,0000,0000,2310,0100	; 11041		SET IO PF			;[224][274] HANDLE DTE.
						; 11042	
						; 11043	;[223] THIS RESTARTS THE DTE'S CLOCK TO PREVENT A DEX FAILURE.
U 2122, 3552,0001,0000,0000,0000,2250,0026	; 11044	=10	SET DATAO,CALL,J/EBUSO		;[223] SEND THE DATA TO THE DTE.
U 2123, 2103,3202,2000,0000,0000,2110,0105	; 11045		AR_BR,SET ACCOUNT EN,J/PIDONE	;[223] AND FINISH THE INTRPT.
						; 11046	.ENDIF/MODEL.B				;[224]
						; 11047	
						; 11048	
						; 11049		;HERE ON BLT PAGE FAIL
						; 11050	
						; 11051	.IF/KLPAGE
U 3744, 3745,0612,2004,0000,0020,1610,0000	; 11052	BLTPF:	AR_ARX+BR,SR_0			;CURRENT SRC ADDR
						; 11053	.ENDIF/KLPAGE
U 3745, 3746,3713,2000,0000,3000,0610,0004	; 11054	BLTPF1:	AR_ARX (AD),ARL_ARR
U 3746, 0040,0001,0000,0000,0000,1005,0000	; 11055	PGFAC0:	AC0_AR,SR DISP,J/CLEAN		;BEGIN NORMAL PF WORK
						; 11056	
						; 11057	;HERE ON XBLT PAGE FAIL OR INTERRUPT
						; 11058	.IF/XADDR
						; 11059	
U 3747, 3750,0001,4001,0000,2000,1010,0000	; 11060	XBLTPF:	AC1_AR,AR_ARX
U 3750, 0040,0001,0004,0000,0000,1010,0000	; 11061		AC2_AR,J/CLEAN
						; 11062	.ENDIF/XADDR
						; 11063	
						; 11064	;HERE ON VARIOUS CASES OF STRING/EDIT FAILURE
						; 11065	
U 3751, 3752,0001,0000,2110,3001,0010,0200	; 11066	BACKS:	P_P+S
U 3752, 0040,4001,0001,0000,0000,1005,0000	; 11067		SRCP_AR,SR DISP,J/CLEAN		;RE-DISPATCH FOR MORE
						; 11068	
U 3753, 3754,3200,2006,0000,0020,0010,0144	; 11069	BACKD:	AR_DSTP
						; 11070	.IF/MODEL.B
U 3754, 3755,4001,0000,2110,3000,0110,0144	; 11071		P_P+S.C,SEL DSTP		;PRESEL NUM FIELD FOR HARDW GLITCH
						;;11072	.IFNOT/MODEL.B
						;;11073		P_P+S
						; 11074	.ENDIF/MODEL.B
U 3755, 0040,4001,0006,0000,0000,1005,0144	; 11075		DSTP_AR,SR DISP,J/CLEAN
						; 11076	
U 3756, 3757,3401,2005,0302,0000,1010,0043	; 11077	D2BPF:	AC3_AR,AR_0S,SC_#,#/35.		;PUT AWAY HIGH BINARY
U 3757, 3760,5160,4207,0000,0020,0010,0170	; 11078		AR_SHIFT,ARX_-SLEN		;LOW TO AR, REMAINING LEN TO ARX
						; 11079	.IF/MODEL.B
U 3760, 3761,0001,0000,0000,0000,0010,0144	; 11080		SEL DSTP			;PRESEL NUM FIELD FOR HARDW GLITCH
						; 11081	.ENDIF/MODEL.B
U 3761, 3772,0001,4006,0000,2000,1010,0144	; 11082		AC4_AR,AR_ARX,J/STRPF2		;PUT LOW AWAY
						; 11083	; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page 27-1
; IO.MIC[10,5351]	19:56 24-Jul-85			PAGE FAIL/INTERRUPT CLEANUP FOR SPECIAL INSTRUCTIONS

U 3762, 3766,3300,2007,4000,0020,0010,0166	; 11084	B2DFPF:	AR_AR*T0,AD/OR,J/B2DPF2
						; 11085	
U 3763, 3764,0001,4000,0000,2000,1010,0000	; 11086	B2DPF:	AC0_AR,AR_ARX			;HIGH FRACTION TO AC
U 3764, 3765,0001,0001,0000,0000,1010,0000	; 11087		AC1_AR				;LOW TO AC1
U 3765, 3766,3200,2005,0000,0020,0010,0000	; 11088		AR_AC3				;GET FLAGS
U 3766, 3771,5100,2007,4000,0020,0010,0170	; 11089	B2DPF2:	AR_AR*SLEN,AD/A-B,J/STRPF3	;REBUILD FLAGS+LEN
						; 11090	
U 3767, 3152,5160,2047,0000,0020,4510,0170	; 11091	STRPF:	BR/AR,AR_-SLEN,SKP AR0		;WHICH IS LONGER?
						; 11092	=0
U 3152, 3772,0602,2005,4000,0020,1010,0000	; 11093	STRPF1:	AC3_AR,AR_AR+BR,J/STRPF2	;SRC LONGER
U 3153, 3770,3300,2400,0000,1020,0010,0000	; 11094		ARX_AR,AR_AR*SFLGS,AD/OR	;DST.  BUILD SRC LEN+FLAGS
U 3770, 3771,5112,2000,4000,0020,1010,0000	; 11095		AC0_AR,AR_ARX-BR		;THAT'S AWAY, MAKE DST LEN
U 3771, 0040,0001,0005,0000,0000,1005,0000	; 11096	STRPF3:	AC3_AR,SR DISP,J/CLEAN		;OK, NOW BACK UP SRC IF REQ'D
						; 11097	
U 3772, 3746,3300,2000,0000,0020,0010,0000	; 11098	STRPF2:	AR_AR*SFLGS,AD/OR,J/PGFAC0
						; 11099	
U 3773, 3152,2500,2047,0000,0020,4510,0170	; 11100	STRPF4:	BR/AR,AR_SLEN COMP,SKP AR0,J/STRPF1
						; 11101	


; Number of microwords used: 
;	D words= 512
;	U words= 2045, Highest= 2047

	END
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-1
; 							Cross Reference Listing					

(D) A				2250 #
	ADDR			2254 #	4206	4222	4438	4805	5003	5005	5101	5102	5159	5160	5161
				5162	5164	5165	5166	5167	5169	5170	5171	5172	5174	5175	5176
				5177	5180	5181	5183	5184	5185	5186	5187	5188	5189	5190	5201
				5202	5203	5204	5205	5206	5210	5211	5212	5213	5214	5215	5216
				5217	5218	5219	5220	5221	5222	5223	5224	5225	5226	5227	5228
				5229	5230	5231	5232	5233	5234	5235	5236	5237	5238	5239	5240
				5241	5245	5246	5392	5505	6276	6277
	IMMED			2251 #	4150	4408	4409	4503	4504	4554	4555	4566	4567	4568	4569
				4570	4571	4582	4583	4584	4585	4586	4587	4588	4589	4600	4601
				4602	4603	4604	4605	4606	4607	4618	4619	4620	4621	4622	4623
				4624	4625	4674	4675	4676	4677	4678	4679	4680	4681	4749	4750
				4751	4752	4753	4754	4755	4756	4764	4765	4766	4767	4768	4769
				4770	4771	4779	4780	4781	4782	4783	4784	4785	4786	4794	4795
				4806	4975	5006	5103	5104	5393	5498	5499	5500	5501	5502	5503
				5504	5663	5695	5768	5773	6048	6058	6121	6150	6279	9536	9538
				9539	9540	9541	9549	9550	9551	9552	9558	9560	9561	9562	9563
				9567	9568	9569	9570	9571	9572	9573	9574	9584	9585	9586	9587
				9594	9595	9596	9597	9598	9599	9607	9609	9610	9618	9620	9621
	IMMED-PF		2252 #	4146	4155	4160	4165	4230	4235	4240	4245	4250	4255	4260
				4267	4272	4277	4282	4287	4292	4297	4302	4406	4407	4417	4427
				4446	4456	4462	4472	4482	4492	4501	4502	4513	4523	4533	4543
				4552	4553	4564	4565	4573	4574	5638	5650
	RD-P-WR			2260 #	4152	4157	4162	4167	4205	4226	4227	4231	4232	4237	4242
				4247	4252	4257	4262	4268	4269	4273	4274	4279	4284	4289	4294
				4299	4304	4418	4419	4428	4429	4442	4443	4447	4448	4463	4464
				4473	4474	4483	4484	4493	4494	4514	4515	4524	4525	4534	4535
				4544	4545	4717	4718	4719	4720	4721	4722	4723	4724	4732	4733
				4734	4735	4736	4737	4738	4739	5639	5640	5651	5652
	RD-WR			2259 #	5664	5665	5696	5697	5769	5770	5774	5775	6044	6045	6049
				6050	6054	6055	6059	6060	6117	6118	6122	6123	6146	6147	6151
				6152	7662	7750	9556	9603	9605	9614	9616
	READ			2257 #	4367	4368	4575	4576	4577	4578	4579	4580	4591	4592	4593
				4594	4595	4596	4597	4598	4609	4610	4611	4612	4613	4614	4615
				4616	4627	4628	4629	4630	4631	4632	4633	4634	4683	4684	4685
				4686	4687	4688	4689	4690	4701	4702	4703	4704	4705	4706	4707
				4708	4974	5004	5662	5694	5767	5772	5921	5922	5923	5924	6042
				6043	6047	6052	6053	6057	6115	6116	6120	6144	6145	6149	6323
				6324	6326	6327	6534	6535	6536	6537	6751	6752	6753	6754	7663
				7751	7858	9537	9547	9548	9559	9582	9583	9606	9617
	READ-PF			2258 #	4145	4154	4159	4164	4220	4229	4234	4239	4244	4249	4254
				4259	4266	4271	4276	4281	4286	4291	4296	4301	4416	4426	4436
				4445	4455	4461	4471	4481	4491	4512	4522	4532	4542	5637	5649
	WR-TST			2256 #	4156	4161	4166	4236	4241	4246	4251	4256	4261	4278	4283
				4288	4293	4298	4303	4387	4388	4457	4458	9534	9535	9545	9546
				9557	9580	9581	9592	9593	9604	9608	9615	9619
(U) AC#				1983 #	3890	3941	4865	4880	5027	5118	5123	5272	5353	5358	5362
				5366	5985	6001	6003	6023	6760	6765	6767	6773	6776	6780	6785
				6789	6793	6794	6795	6800	6801	6802	6804	6805	6831	6833	6835
				6836	6845	6847	6848	6851	6870	6877	6878	6887	6913	6926	6927
				6943	6947	6987	6988	6990	6994	6996	7021	7060	7067	7197	7198
				7204	7205	7253	7254	7266	7267	7268	7315	7322	7400	7402	7413
				7415	7434	7437	7443	7893	7914	7939	7966	8040	8466	8508	8511
				8513	8516	8553	8603	8604	8605	8606	8607	8608	8609	8634	8636
				8637	8651	8665	8669	8675	8677	8686	8691	8709	8733	8752	8801
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-2
; 							Cross Reference Listing					

				8810	8829	8829	8848	8849	8862	8865	8870	8876	8897	8899	8917
				8952	8954	8963	8964	8973	8976	8978	8979	8990	8991	9025	9027
				9041	9042	9091	9094	9095	9096	9097	9105	9130	9132	9137	9141
				9146	9148	9149	9214	9214	9240	9242	9243	9248	9249	9251	9254
				9255	9257	9258	9261	9283	9314	9323	9400	9402	9436	9447	9449
				9450	9454	9455	9464	9465	9509	9517	10335	10337	10373	10397	10419
				10434	10455	10532	10533	10563	10572	10573	10574	10579	10582	10598	10630
				10640	10647	10655	10669	10680	10685	10692	10693	10694	10709	10719	10720
				10731	10746	10757	10763	10772	10783	10787	10797	10809	10814	10818	10822
				10836	10842	10850	10854	10856	10866	10873	10921	10948	10973	11005	11006
				11038	11039	11040	11069	11071	11075	11078	11080	11082	11084	11089	11091
				11100
(U) AC-OP			1998 #
	AC+#			1999 #	8603	8606	8607	8608	8609	8669	8829	8829	8849	8897	8899
				9141	9214	9214	9240	9242	9243	9248	9249	9251	9254	9255	9257
				9258	9261	9314	9323	9436	9447	9449	9450	9454	9455	9509	11069
				11071	11075	11080	11082
	OR			2001 #
	#			2000 #
(U) ACB				1979 #
	MICROB			1981 #	3890	3941	4865	4880	5027	5118	5123	5272	5353	5358	5362
				5366	5985	6001	6003	6023	6760	6765	6767	6773	6776	6780	6785
				6789	6793	6794	6795	6800	6801	6802	6804	6805	6831	6833	6835
				6836	6845	6847	6848	6851	6870	6877	6878	6887	6913	6926	6927
				6943	6947	6987	6988	6990	6994	6996	7021	7060	7067	7197	7198
				7204	7205	7253	7254	7266	7267	7268	7315	7322	7400	7402	7413
				7415	7434	7437	7443	7893	7914	7939	7966	8040	8466	8508	8511
				8513	8516	8553	8604	8605	8634	8636	8637	8651	8665	8675	8677
				8686	8691	8709	8733	8752	8801	8810	8848	8862	8865	8870	8876
				8917	8952	8954	8963	8964	8973	8976	8978	8979	8990	8991	9025
				9027	9041	9042	9091	9094	9095	9096	9097	9105	9130	9132	9137
				9146	9148	9149	9283	9400	9402	9464	9465	9517	10335	10337	10373
				10397	10419	10434	10455	10563	11005	11006	11038	11039	11078	11084	11089
				11091	11100
	PAGB			1980 #	10532	10533	10572	10573	10574	10579	10582	10598	10630	10640	10647
				10655	10669	10680	10685	10692	10693	10694	10709	10719	10720	10731	10746
				10757	10763	10772	10783	10787	10797	10809	10814	10818	10822	10836	10842
				10850	10854	10856	10866	10873	10921	10948	10973	11040
(U) AD				1570 #
	A			1608 #	3890	3953	3957	3962	3964	3975	3977	4008	4401	4712	4914
				4918	4923	4937	5031	5081	5082	5277	5342	5348	5428	5543	5543
				5558	5559	5560	5560	5563	5565	5566	5574	5576	5579	5581	5728
				5729	5730	5732	5734	5745	5746	5747	5749	5757	5907	5908	5909
				5910	5936	5956	5962	5977	6024	6178	6178	6190	6378	6384	6387
				6415	6418	6443	6445	6464	6544	6552	6576	6589	6595	6598	6647
				6650	6667	6669	6693	6695	6700	6704	6724	6760	6769	6771	6849
				6858	6890	6898	6899	6900	6907	6920	6922	6950	7004	7006	7011
				7016	7031	7050	7094	7214	7271	7273	7303	7305	7308	7312	7318
				7324	7394	7399	7405	7408	7443	7447	7468	7469	7496	7568	7618
				7672	7676	7760	7764	7788	7894	7916	7921	7922	7924	7969	7975
				8148	8152	8161	8182	8187	8196	8216	8297	8301	8306	8308	8317
				8319	8369	8401	8404	8409	8413	8415	8470	8473	8475	8478	8480
				8496	8498	8708	8772	8801	8803	8814	8816	8855	9003	9005	9195
				9216	9251	9285	9286	9302	9359	9363	9418	9453	9504	9506	9658
				9659	9676	9679	9681	9683	9694	9696	9702	9729	9735	9769	9775
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-3
; 							Cross Reference Listing					

				9817	9826	10101	10103	10114	10162	10180	10229	10233	10245	10257	10258
				10259	10265	10315	10364	10405	10467	10532	10533	10560	10576	10577	10578
				10645	10706	10751	10758	10813	10990	10992	11004	11054
	A*2			1575 #	6501	6697	6731	7008	7039	7053	7317	7925	8406	9095	9464
				10465
	A*2+1			1576 #	8795
	A+1			1571 #	3879	3940	4193	4728	4821	4832	5025	5114	5116	5146	5356
				5365	5406	5527	5528	5783	6350	6698	6710	6733	7022	7036	7087
				7217	7545	7574	7599	7624	7881	7886	7891	8019	8021	8033	8039
				8077	8080	8083	8087	8101	8104	8106	8108	8110	8113	8118	8122
				8126	8130	8275	8280	8508	8511	8513	8516	8558	8570	8705	8773
				8857	8939	8969	8994	9184	9189	9193	9241	9244	9245	9249	9332
				9367	9369	9493	9516	9655	10351	10382	10409	10474	10768
	A+B			1579 #	3955	3959	3963	3965	3976	3979	5409	5412	5644	5735	5751
				5753	5842	5847	5872	5875	5876	5879	5889	5903	5906	5943	5955
				6016	6109	6266	6388	6498	6592	6639	6855	6876	6884	6926	7038
				7069	7321	7431	7552	7558	7559	7566	7600	7606	7616	7913	7939
				7973	7977	7981	7983	8051	8150	8154	8163	8184	8189	8199	8218
				8299	8303	8307	8309	8318	8321	8474	8476	8479	8481	8497	8499
				8692	8858	8878	8879	8951	9034	9041	9042	9056	9096	9105	9465
				9517	10190	10196	10228	10291	10433	10640	10655	10680	10731	10763	11052
				11093
	A+B+1			1580 #	4775	4799	5023	5028	7471	7472	7490	7490	7492	7492	7547
				7893	8670	8677	8700	8712	8739	8753	8865	9025	9137	9146	9149
				9400	9402
	A+XCRY			1572 #	5686
	A-1			1590 #	4743	4791	5072	5091	5346	5408	5523	5529	7462	7462	7474
				7474	7489	7967	7989	8703	8774	8774	8818	8818	8819	8819	9145
				10135	10383	10393
	A-B			1584 #	4183	4379	4395	5525	5657	5737	5739	5755	5796	5802	5839
				5841	5844	5851	5856	5861	5873	5874	5877	5878	5892	5904	5905
				5945	5996	5999	6014	6030	6034	6086	6262	6264	6414	6516	6549
				6716	6743	6766	6780	6787	6818	6873	6939	6943	7028	7284	7289
				7535	7536	7570	7620	7919	7927	7938	8041	8636	8662	8686	8689
				8868	8892	8933	8937	9000	11078	11089	11091	11095
	A-B-1			1583 #	7280	8956	8967
	AND			1607 #	4423	5027	5118	5123	5353	5366	5704	5964	6994	7062	7204
				8637	9130	9132	10337	10373	10397	10419	10434	10455	10692	10772	10787
	ANDC			1596 #	4488
	ANDCA			1601 #	4452	4666	4887	5415	6436	6742	7091	8635	8930	10149	11012
	ANDCB			1606 #	4433	4865	4880	4939	5272	5358	5362	5417	6503	6906	7197
				7435	7438	11006
	B			1603 #	3927	3938	4002	4003	4036	4047	4058	4069	4080	4199	4211
				4214	4313	4315	4318	4321	4324	4328	4377	4378	4393	4394	4760
				4790	4850	4861	4884	4892	4903	4904	4905	4908	4920	4934	5033
				5046	5046	5069	5071	5074	5075	5077	5078	5083	5093	5094	5095
				5129	5139	5269	5335	5339	5349	5352	5513	5516	5534	5534	5554
				5555	5556	5571	5572	5589	5591	5599	5607	5609	5613	5619	5622
				5631	5656	5679	5702	5780	5785	5798	5803	5805	5809	5894	5931
				5933	5937	5961	5974	5975	5978	5985	5987	5989	5990	5992	5994
				5994	6003	6004	6006	6023	6026	6027	6029	6033	6069	6092	6094
				6135	6138	6165	6317	6367	6379	6420	6422	6441	6449	6451	6454
				6497	6547	6568	6582	6582	6591	6597	6599	6600	6611	6611	6625
				6626	6640	6660	6662	6672	6676	6705	6740	6775	6784	6785	6788
				6789	6790	6792	6795	6799	6800	6804	6807	6829	6831	6835	6836
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-4
; 							Cross Reference Listing					

				6843	6846	6847	6848	6852	6853	6854	6868	6872	6874	6878	6881
				6885	6905	6914	6917	6918	6928	6932	6988	6991	6992	7017	7021
				7037	7060	7067	7068	7089	7248	7253	7254	7266	7267	7268	7272
				7283	7288	7290	7300	7313	7396	7402	7410	7415	7419	7421	7422
				7430	7444	7457	7459	7467	7484	7495	7498	7529	7534	7563	7564
				7614	7683	7704	7705	7706	7707	7708	7709	7710	7712	7718	7719
				7720	7721	7722	7723	7725	7731	7732	7733	7735	7737	7738	7768
				7786	7786	7804	7806	7811	7812	7813	7814	7815	7816	7817	7819
				7825	7826	7827	7828	7829	7830	7832	7838	7839	7840	7842	7844
				7845	7966	7984	8028	8045	8086	8089	8157	8158	8192	8193	8277
				8278	8326	8327	8337	8339	8362	8362	8365	8465	8466	8469	8502
				8503	8552	8554	8560	8568	8571	8599	8603	8604	8605	8608	8616
				8629	8634	8650	8652	8653	8669	8671	8678	8681	8698	8746	8752
				8770	8775	8797	8798	8798	8813	8813	8829	8829	8849	8856	8862
				8867	8875	8884	8890	8918	8920	8927	8929	8941	8943	8965	8973
				8977	8979	8990	8991	8993	8997	9028	9037	9046	9047	9048	9053
				9093	9107	9109	9111	9135	9136	9141	9148	9159	9160	9160	9187
				9188	9214	9214	9242	9243	9274	9291	9296	9300	9304	9314	9323
				9339	9388	9414	9417	9419	9427	9434	9436	9443	9445	9447	9462
				9462	9467	9471	9472	9488	9494	9509	9519	9521	9634	9668	9711
				9720	9748	9782	9829	10104	10138	10139	10147	10148	10215	10253	10313
				10341	10345	10350	10381	10389	10390	10406	10407	10408	10410	10475	10529
				10531	10573	10580	10595	10627	10630	10641	10642	10643	10647	10662	10669
				10685	10688	10693	10703	10709	10710	10719	10720	10733	10746	10757	10783
				10797	10814	10822	10836	10842	10850	10854	10856	10866	10873	10897	10901
				10921	10928	10932	10936	10944	10948	10953	10958	10973	11016	11038	11040
				11045	11069	11088
	CRY A EQ -1		1610 #	9044
	CRY A GE B		1613 #	6177	8822
	CRY A#0			1612 #	6412	6685	7000	7461	8743	8936	9477	9518
	CRY A.B#0		1611 #	4663	4938	9097	9639	9641	10777
	EQV			1599 #	4498
	NOR			1597 #	7198
	OR			1604 #	4478	4668	5283	5361	7086	8602	8607	8619	8693	8883	9825
				10150	10173	10694	10781	10809	11084	11094	11098
	ORC			1593 #	4549
	ORCA			1594 #	4539
	ORCB			1600 #	4519	6996	7205
	ORCB+1			1582 #	4968	9643
	SETCA			1592 #	4529	4655	4658	6687	7002	8664	8972	10723
	SETCB			1598 #	4508	5891	5998	6711	6711	6714	6714	7023	7023	7026	7026
				7947	8663	8691	8709	8843	8870	8876	8964	10905	11100
	XCRY-1			1587 #	4091	4124	5411	5594	5595	5606	5690	5816	5819	5966	6031
				6100	6101	6333	6354	6381	6570	6573	6838	6840	7057	7404	7411
				7425	8042	8044	8894
	XOR			1602 #	4468	4667	4694	5611	6559	6915	8771	8810	10430
	0S			1605 #	4345	4359	4381	4398	4413	4935	4950	4987	5344	5533	5617
				5723	5723	5762	5947	5947	5954	6001	6083	6085	6133	6191	6386
				6478	6596	6630	6630	6727	6727	6773	6870	6882	6882	6893	7033
				7033	7686	7688	7862	7882	7915	7930	7931	8023	8026	8035	8038
				8342	8343	8665	8736	8800	8848	8848	8853	8853	9030	9030	9035
				9401	10097	10112	10154	10160	10164	10168	10171	10206	10206	10209	10217
				10234	10237	10246	10255	10270	10293	10300	10302	10304	10306	10356	10363
				10404	10425	10431	10464	10574	10590	11014	11077
	1S			1595 #	3881	3925	4005	4339	4346	4353	4360	4559	4815	4862	6481
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-5
; 							Cross Reference Listing					

				6902	7675	7680	7763	7773	7888	8029	8040	8159	8160	8194	8195
				8209	8210	8292	8328	8471	8504	8518	8668	8673	8859	8860	8963
				9183	9199	9259	9262	9273
(U) ADA				1616 #
	AR			1617 #	3890	3940	3975	3976	3977	3979	4423	4433	4452	4468	4478
				4488	4498	4508	4519	4529	4539	4549	4663	4666	4667	4668	4694
				4712	4728	4743	4791	4887	4914	4918	4938	4939	5027	5031	5072
				5082	5091	5118	5123	5283	5353	5361	5409	5412	5415	5417	5428
				5527	5543	5558	5560	5565	5576	5611	5644	5657	5686	5704	5728
				5729	5730	5732	5734	5735	5737	5739	5745	5746	5747	5749	5751
				5753	5755	5757	5802	5839	5842	5847	5856	5872	5873	5874	5875
				5876	5877	5878	5879	5889	5892	5903	5904	5905	5906	5943	5945
				5955	5964	6014	6016	6109	6190	6264	6266	6388	6415	6418	6464
				6498	6501	6503	6559	6592	6598	6639	6650	6693	6695	6697	6698
				6700	6710	6724	6731	6742	6771	6780	6818	6855	6873	6876	6884
				6898	6899	6900	6907	6915	6926	6943	6994	6996	7004	7006	7008
				7011	7022	7031	7038	7039	7050	7053	7062	7091	7094	7197	7198
				7204	7205	7217	7271	7273	7280	7284	7289	7308	7312	7317	7318
				7321	7324	7394	7405	7408	7431	7434	7437	7447	7461	7535	7536
				7552	7606	7672	7676	7760	7764	7894	7916	7922	7925	7927	7939
				7975	7977	7981	8077	8080	8083	8087	8101	8104	8106	8108	8110
				8113	8118	8122	8126	8130	8148	8150	8152	8154	8182	8184	8187
				8189	8275	8280	8317	8318	8319	8321	8406	8413	8470	8496	8497
				8498	8499	8635	8636	8662	8689	8692	8693	8705	8712	8771	8774
				8810	8818	8819	8858	8868	8878	8879	8883	8930	8936	8951	8956
				8969	8994	9034	9041	9042	9056	9095	9096	9097	9105	9130	9132
				9145	9184	9189	9193	9195	9241	9244	9249	9251	9286	9332	9359
				9363	9367	9369	9402	9418	9464	9465	9477	9493	9504	9506	9517
				9518	9639	9641	9655	9658	9659	9676	9679	9681	9683	9694	9696
				9702	9729	9735	9769	9775	9817	9825	9826	10101	10103	10150	10162
				10180	10190	10196	10228	10233	10245	10257	10258	10259	10265	10291	10337
				10351	10364	10382	10383	10393	10409	10430	10433	10434	10467	10474	10532
				10533	10576	10640	10645	10655	10680	10692	10694	10706	10731	10758	10772
				10777	10781	10787	10809	11012	11084	11089	11093	11094	11098
	ARX			1618 #	3953	3955	3957	3959	3962	3963	3964	3965	4865	4880	4923
				5081	5272	5277	5358	5366	5408	6178	6412	6552	6769	6849	6858
				7069	7086	7468	7496	7558	7559	7566	7568	7600	7616	7618	7913
				7924	7973	7983	8041	8051	8161	8163	8196	8199	8216	8218	8297
				8299	8301	8303	8306	8307	8308	8309	8404	8409	8473	8474	8475
				8476	8478	8479	8480	8481	8508	8511	8513	8516	8558	8570	8602
				8607	8619	8703	8708	8743	8803	8814	8816	8967	8972	9245	9285
				10149	10373	10397	10419	10455	10751	10763	10813	11006	11052	11054	11095
	MQ			1619 #	3879	5563	5579	5907	5908	5909	5910	5956	5962	5977	6024
				6387	6445	6595	6647	6667	6669	6685	6687	6890	6906	6920	6922
				6950	7000	7002	7214	7462	7469	7474	7489	7545	7599	7788	7967
				7969	8369	8415	8637	8772	8773	8795	8801	9003	9005	9044	9216
				9453	10114	10229	10405	10465
	PC			1620 #	4008	4821	4832	4937	5025	5114	5116	5146	5342	5348	5362
				5365	8401	10560	10578	10990	10992	11004
(U) ADA EN			1621 #
	EN			1622 #	4401	5523	5525	5528	5529	5543	5559	5560	5566	5574	5581
				5936	6178	6378	6384	6443	6544	6576	6589	6704	6760	7016	7303
				7305	7399	7443	7462	7474	7570	7574	7620	7624	7886	7891	7893
				7921	7989	8664	8774	8818	8819	8855	9302	10173	10315	10577	10723
	0S			1623 #	4183	4193	4379	4395	4775	4799	4968	5023	5028	5346	5356
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-6
; 							Cross Reference Listing					

				5406	5783	5796	5841	5844	5851	5861	5996	5999	6030	6034	6086
				6177	6262	6350	6414	6436	6516	6549	6716	6733	6743	6766	6785
				6787	6789	6795	6800	6804	6831	6835	6836	6847	6848	6878	6939
				6988	7028	7036	7060	7067	7087	7253	7254	7266	7267	7268	7471
				7472	7490	7490	7492	7492	7547	7881	7919	7938	8019	8021	8033
				8039	8634	8670	8677	8686	8700	8739	8753	8822	8857	8865	8892
				8933	8937	8939	8973	9000	9025	9137	9146	9149	9400	9516	9643
				10135	10768	11078	11091
(U) ADB				1625 #
	AR*4			1629 #	4968	5525	5841	5844	5985	6086	6138	6262	6414	6420	6422
				6436	6441	6454	6516	6549	6743	6766	6790	6799	6939	7037	7290
				8041	8858	8867	8878	8937	8951	8964	8967	9034	9048	9056	9296
				9300	9304	9339	10104	10147	10148	10341	10345	10389	10408	10529	10531
	BR			1628 #	4183	4379	4395	4861	4884	4887	4934	4938	4939	5033	5083
				5095	5269	5283	5335	5349	5352	5361	5599	5609	5611	5613	5619
				5622	5631	5657	5704	5735	5739	5751	5755	5796	5798	5802	5805
				5839	5842	5847	5851	5856	5861	5872	5873	5874	5875	5876	5877
				5878	5879	5889	5892	5903	5904	5905	5906	5943	5945	5955	5964
				5974	5978	5994	5994	5996	5998	5999	6004	6006	6014	6016	6029
				6030	6033	6034	6069	6177	6367	6379	6388	6497	6498	6503	6582
				6582	6592	6611	6611	6639	6705	6711	6711	6714	6714	6716	6740
				6742	6784	6787	6788	6818	6843	6855	6872	6873	6876	6884	6905
				6906	6914	7017	7023	7023	7026	7026	7028	7038	7062	7068	7069
				7086	7089	7091	7280	7283	7284	7288	7289	7321	7402	7415	7421
				7431	7484	7490	7490	7492	7492	7498	7534	7535	7536	7547	7552
				7558	7559	7566	7570	7600	7606	7616	7620	7893	7919	7927	7938
				7947	7973	7977	7981	7983	7984	8028	8045	8051	8089	8277	8278
				8465	8466	8469	8571	8602	8607	8608	8619	8629	8635	8662	8663
				8670	8689	8692	8700	8712	8739	8771	8797	8813	8813	8822	8862
				8875	8879	8884	8890	8892	8930	8933	8956	8997	9000	9028	9037
				9053	9427	9445	9447	9494	9639	9641	9643	9668	9711	9720	9748
				9782	9825	9829	10138	10139	10149	10150	10173	10190	10196	10215	10228
				10253	10291	10406	10407	10410	10430	10433	10475	10573	10595	10641	10643
				10703	10777	10781	10901	10928	10953	11012	11045	11052	11093	11095
	BR*2			1627 #	5607	5737	5753	5990	6094	6109	6264	6266	6449	6451	8943
				10580
	FM			1626 #	3927	3938	3955	3959	3963	3965	3976	3979	4002	4003	4036
				4047	4058	4069	4080	4199	4211	4214	4313	4315	4318	4321	4324
				4328	4377	4378	4393	4394	4423	4433	4452	4468	4478	4488	4498
				4508	4519	4539	4549	4663	4666	4667	4668	4694	4760	4775	4790
				4799	4850	4865	4880	4892	4903	4904	4905	4908	4920	5023	5027
				5028	5046	5046	5069	5071	5074	5075	5077	5078	5093	5094	5118
				5123	5129	5139	5272	5339	5353	5358	5362	5366	5409	5412	5415
				5417	5513	5516	5534	5534	5554	5555	5556	5571	5572	5589	5591
				5644	5656	5679	5702	5780	5785	5803	5809	5891	5894	5931	5933
				5937	5961	5975	5987	5989	5992	6003	6023	6026	6027	6092	6135
				6165	6317	6547	6559	6568	6591	6597	6599	6600	6625	6626	6640
				6660	6662	6672	6676	6775	6780	6785	6789	6792	6795	6800	6804
				6807	6829	6831	6835	6836	6846	6847	6848	6852	6853	6854	6868
				6874	6878	6881	6885	6915	6917	6918	6926	6928	6932	6943	6988
				6991	6992	6994	6996	7021	7060	7067	7197	7198	7204	7205	7248
				7253	7254	7266	7267	7268	7272	7300	7313	7396	7410	7419	7422
				7430	7434	7437	7444	7457	7459	7467	7471	7472	7495	7529	7563
				7564	7614	7683	7704	7705	7706	7707	7708	7709	7710	7712	7718
				7719	7720	7721	7722	7723	7725	7731	7732	7733	7735	7737	7738
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-7
; 							Cross Reference Listing					

				7768	7786	7786	7804	7806	7811	7812	7813	7814	7815	7816	7817
				7819	7825	7826	7827	7828	7829	7830	7832	7838	7839	7840	7842
				7844	7845	7913	7939	7966	8086	8150	8154	8157	8158	8163	8184
				8189	8192	8193	8199	8218	8299	8303	8307	8309	8318	8321	8326
				8327	8337	8339	8362	8362	8365	8474	8476	8479	8481	8497	8499
				8502	8503	8552	8554	8560	8568	8599	8603	8604	8605	8616	8634
				8636	8637	8650	8652	8653	8669	8671	8677	8678	8681	8686	8691
				8693	8698	8709	8746	8752	8753	8770	8775	8798	8798	8810	8829
				8829	8843	8849	8856	8865	8868	8870	8876	8883	8918	8920	8927
				8929	8941	8965	8973	8977	8979	8990	8991	8993	9025	9041	9042
				9046	9047	9093	9096	9097	9105	9107	9109	9111	9130	9132	9135
				9136	9137	9141	9146	9148	9149	9159	9160	9160	9187	9188	9214
				9214	9242	9243	9274	9291	9314	9323	9388	9400	9402	9414	9417
				9419	9434	9436	9443	9462	9462	9465	9467	9471	9472	9488	9509
				9517	9519	9521	9634	10313	10337	10350	10373	10381	10390	10397	10419
				10434	10455	10627	10630	10640	10642	10647	10655	10662	10669	10680	10685
				10688	10692	10693	10694	10709	10710	10719	10720	10731	10733	10746	10757
				10763	10772	10783	10787	10797	10809	10814	10822	10836	10842	10850	10854
				10856	10866	10873	10897	10905	10921	10932	10936	10944	10948	10958	10973
				11006	11016	11038	11040	11069	11078	11084	11088	11089	11091	11094	11098
				11100
(U) AR				1637 #
	AD			1642 #	3879	3940	3953	3955	3957	3959	3962	3963	3964	3965	4036
				4091	4124	4183	4199	4211	4214	4315	4318	4324	4345	4346	4359
				4360	4379	4381	4395	4398	4413	4423	4433	4452	4468	4478	4488
				4498	4508	4519	4529	4539	4549	4559	4666	4667	4668	4728	4743
				4760	4775	4790	4791	4799	4850	4880	4937	4939	4950	4987	5025
				5027	5069	5071	5072	5083	5114	5116	5118	5123	5129	5139	5146
				5272	5283	5342	5344	5358	5361	5362	5366	5409	5412	5513	5516
				5533	5534	5556	5563	5572	5579	5589	5591	5594	5595	5606	5607
				5609	5622	5644	5656	5657	5679	5690	5702	5723	5783	5785	5803
				5805	5809	5816	5819	5851	5861	5876	5877	5878	5879	5889	5892
				5907	5908	5909	5910	5937	5943	5945	5947	5955	5956	5961	5962
				5966	5974	5977	5978	5987	5992	5996	5998	5999	6003	6004	6006
				6023	6024	6026	6027	6029	6030	6031	6033	6034	6069	6092	6094
				6100	6101	6135	6165	6317	6333	6367	6379	6381	6386	6387	6388
				6420	6449	6478	6497	6498	6568	6570	6573	6582	6591	6592	6595
				6599	6600	6611	6625	6626	6630	6639	6640	6647	6667	6669	6672
				6676	6687	6698	6705	6710	6711	6714	6716	6727	6773	6775	6780
				6784	6785	6787	6788	6789	6792	6795	6800	6804	6829	6831	6835
				6836	6838	6840	6843	6846	6848	6849	6852	6853	6854	6855	6858
				6870	6872	6873	6874	6876	6878	6881	6882	6884	6885	6890	6905
				6914	6920	6922	6926	6928	6932	6943	6950	6988	6992	6994	6996
				7002	7017	7022	7023	7026	7028	7033	7038	7057	7060	7062	7067
				7068	7069	7086	7197	7198	7204	7205	7214	7253	7254	7266	7267
				7268	7283	7288	7289	7321	7402	7404	7411	7415	7419	7421	7425
				7431	7434	7437	7444	7457	7459	7462	7469	7471	7472	7474	7489
				7492	7498	7529	7535	7536	7545	7552	7599	7606	7686	7688	7788
				7930	7931	7939	7966	7969	7973	7977	7981	7983	8028	8040	8042
				8044	8051	8083	8087	8113	8118	8122	8126	8130	8277	8278	8280
				8342	8343	8369	8401	8404	8465	8469	8552	8554	8558	8560	8568
				8570	8571	8599	8602	8603	8607	8616	8619	8634	8636	8637	8652
				8653	8662	8663	8665	8669	8671	8673	8677	8678	8681	8686	8689
				8691	8692	8693	8698	8705	8709	8736	8746	8752	8753	8770	8797
				8798	8801	8810	8813	8814	8816	8829	8843	8848	8853	8856	8857
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-8
; 							Cross Reference Listing					

				8860	8862	8865	8870	8875	8876	8878	8883	8884	8890	8892	8894
				8918	8920	8927	8930	8933	8941	8963	8965	8979	8991	8993	8994
				8997	9000	9003	9005	9025	9028	9030	9035	9037	9042	9053	9093
				9096	9109	9111	9132	9135	9136	9141	9145	9146	9148	9149	9159
				9160	9184	9188	9189	9193	9214	9216	9241	9243	9244	9245	9249
				9314	9323	9332	9367	9369	9388	9401	9417	9427	9447	9453	9462
				9471	9488	9493	9509	9519	9521	9655	9668	9711	9720	9748	9782
				9829	10097	10112	10138	10149	10150	10154	10164	10168	10190	10196	10206
				10209	10215	10217	10228	10234	10237	10246	10253	10255	10270	10291	10293
				10300	10302	10304	10306	10341	10345	10351	10356	10363	10382	10383	10389
				10404	10405	10408	10425	10431	10433	10464	10573	10574	10590	10595	10630
				10641	10647	10685	10692	10693	10694	10709	10720	10746	10768	10772	10781
				10783	10787	10797	10809	10813	10814	10836	10897	10901	10905	10928	10932
				10936	10944	10948	10953	10958	10973	10990	10992	11006	11014	11038	11040
				11045	11052	11054	11069	11077	11084	11088	11089	11091	11093	11094	11095
				11098	11100
	AD*.25			1647 #	5734	5735	5737	5739	5751	5753	5755	5757	5841	5844	6086
				6109	6178	6190	6262	6414	6415	6445	6464	6501	6516	6549	6693
				6695	6697	6724	6731	6743	6766	6899	6939	7004	7006	7008	7031
				7039	7050	7053	7308	7317	7318	8406	8937	8939	8943	8967	9034
				9095	9464	10529	10531	10751
	AD*2			1645 #	3890	5527	5554	5728	5729	5730	5732	5745	5746	5747	5749
				5780	5839	5842	5847	5856	5872	5873	5874	5875	5903	5904	5905
				5906	5933	6014	6016	6418	6422	6437	6441	6451	6454	6547	6660
				6662	6700	6790	6799	6807	6868	6898	6900	6917	6918	6991	7011
				7290	7927	8858	8879	8951	9056	9296	9304	9339	9516	10265	10465
	ADX			1646 #	4887	5352	5529	5543	5559	5560	5566	5574	5581	5619	5796
				5798	5994	6384	6443	6576	6589	6704	7016	7490	7893	7921	7984
				8045	8466	8664	8670	8700	8712	8739	8774	8818	8819	8822	9445
				9494	9825	10407	10410	10475	11012
	AR			1638 #	4193	4313	4325	4338	4339	4867	4884	5406	5411	9632	9767
				9815	10160	10243	11010
	ARMM			1639 #	4140	4866	4883	5137	5142	5360	5373	5374	5375	6502	6732
				6817	6871	6925	7061	7083	7279	7281	7287	7320	7401	7414	7889
				8019	8021	8023	8026	8029	8033	8035	8038	8039	8088	8100	8103
				8111	8112	8119	8123	8127	8131	8467	8661	8844	8849	8852	8966
				9057	9122	9123	9124	9125	9126	9127	9203	9246	9248	9334	9380
				9746	9749	10113	10125	10148	10207	10360	10424	10479	10753	10821	11009
				11071
	CACHE			1641 #	9802
	EBUS			1643 #	3894	3896	4989	5346	9679	9742	9772	9773	9820	9822	9824
				10135	10152	10166	10170	10220	10221	10222	10223	10236	10238	10248	10264
				10273	10274	10275	10276	10549	10564
	MEM			1640 #	4047	4069	4080	4904	4905	4908	4920	5046	5074	5077	5093
				5094	7272	7313	7396	7410	7467	7495	7563	7564	7614	7735	7786
				7811	7812	7813	7814	7815	7816	7817	7819	7825	7826	7827	7828
				7829	7830	7832	7838	7839	7840	7844	7845	8086	8337	8339	8362
				8365	8650	8775	8977	9046	9107	9291	9419	9434	9467	9472	9634
				10350	10381	10390	10627	10642	10662	10688	10710	10733	11016
	SH			1644 #	3924	4005	4095	4125	4172	4321	4328	4329	4352	4353	4399
				4658	4661	4849	4931	4935	5034	5039	5041	5140	5141	5299	5349
				5363	5368	5520	5522	5541	5546	5567	5582	5613	5617	5630	5631
				5689	5972	5973	5975	5985	5988	5990	6032	6081	6106	6132	6161
				6176	6412	6585	6587	6588	6596	6597	6701	6703	6728	6740	6796
				6801	6847	7013	7015	7034	7088	7216	7218	7291	7322	7422	7427
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-9
; 							Cross Reference Listing					

				7430	7476	7478	7534	7553	7607	7790	7791	7914	7975	8090	8209
				8328	8350	8372	8374	8412	8471	8504	8508	8511	8513	8516	8674
				8744	8800	8859	8861	8895	8931	8952	8954	8978	9036	9051	9119
				9152	9190	9195	9251	9274	9284	9285	9286	9292	9311	9315	9344
				9363	9393	9441	9449	9503	9504	9505	9506	9728	9774	10101	10103
				10139	10174	10257	10258	10294	10423	10426	10532	10533	10548	10577	10579
				10582	10598	10640	10643	10671	10680	10758	10763	10822	10842	10850	10853
				10855	10994	11060	11078	11082	11086
(U) AR CTL			2039 #
	AR0-8 LOAD		2040 #	6502	6732
	AR9-17 LOAD		2041 #
	ARL LOAD		2043 #
	ARR LOAD		2042 #	9746
(U) AR0-8			2014 #
	LOAD			2015 #	4194	5357	5524	6082	6085	6093	6133	6136	6162	6261	6318
				6541	6610	6644	6648	7316	7323	7428	7891	7982	7989	8041	8052
				8068	8083	8085	8280	8379	8403	8666	8705	8707	8851	8928	8971
				9151	9174	9185	9192	9222	9333	9374	9375	9493	9522	10147	10483
				10566	10575	10576	10597	10823	11066
(U) ARL				2029 #
	AD			2033 #	4193	4313	4321	4328	4339	4353	4884	5349	5356	5411	5513
				5579	5785	6317	6582	6667	6669	6920	6922	7534	8089	8518	8918
				9003	9005	10139
	AD*.25			2038 #
	AD*2			2036 #	5780	5848	5857	5933
	ADX			2037 #	7920	10173	10577
	ARL			2030 #	4199	4214	4315	4329	4359	4360	5363	7917	7973	9774	9825
				10101
	ARMM			2031 #	4194	5357	5524	6082	6085	6093	6133	6136	6162	6261	6318
				6541	6610	6644	6648	7056	7316	7323	7428	7891	7982	7989	8041
				8052	8068	8083	8085	8280	8379	8403	8666	8705	8707	8851	8928
				8971	9151	9174	9185	9192	9222	9333	9374	9375	9493	9522	10147
				10483	10566	10575	10576	10597	10823	11066
	CACHE			2032 #
	EBUS			2034 #
	SH			2035 #	4318	4324	4325	4345	4346	4867	5146	5406	5520	6106	6412
				6585	7552	7606	7977	7983	9632	9748	9767	9815	10160	10240	10243
				10722	11010	11054
(U) ARMM			1727 #
	EXP_SIGN		1729 #	6082	6085	6093	6133	6136	6162	6261	6318	6541	6610	6644
				7323	7428
	SCAD EXP		1730 #	6502	6648	6732	7056	7316	8379	8403	8467	8666	8849	8851
				8852	8928	8971	10360	10424	10479	10575	10576	10597	10821
	SCAD POS		1731 #	4194	5357	5524	7083	7889	7891	7982	7989	8019	8021	8023
				8026	8029	8033	8035	8038	8039	8041	8052	8068	8083	8085	8088
				8100	8103	8111	8112	8119	8123	8127	8131	8280	8705	8707	9057
				9122	9123	9124	9125	9126	9127	9151	9174	9185	9192	9203	9222
				9246	9248	9333	9334	9374	9375	9380	9493	9522	10147	10483	10566
				10823	11066	11071
	#			1728 #	4140	5373	5374	5375	6817	6871	6925	7061	7279	7281	7287
				7320	7401	7414	8661	8844	8966	9749	10113	10125	10207	10753
(U) ARX				1648 #
	AD			1652 #	3881	3927	4393	4394	4821	4832	4865	4881	4935	5023	5028
				5091	5346	5365	5406	5410	5428	5534	5543	5555	5558	5560	5565
				5571	5576	5599	5617	5723	5931	5947	5975	5989	5990	5994	6001
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-10
; 							Cross Reference Listing					

				6083	6085	6133	6191	6354	6481	6596	6597	6630	6727	6733	6847
				6882	6902	7033	7087	7248	7403	7430	7490	7804	7806	7862	7882
				7913	7915	7916	7925	7975	8023	8026	8029	8033	8035	8038	8039
				8041	8604	8605	8635	8668	8703	8709	8774	8798	8800	8818	8819
				8829	8848	8849	8853	8859	8929	8964	8969	8990	9030	9047	9130
				9160	9184	9187	9195	9214	9242	9251	9273	9274	9323	9436	9462
				10101	10135	10160	10171	10206	10337	10373	10397	10419	10434	10455	10532
				10533	10576	10578	10643	10719	10758	10842	10850	10854	10856	10921	11004
				11007	11078
	ADX			1656 #	4379	4395	4934	5269	5335	5523	5528	5607	5613	5631	5876
				5877	5878	5879	5889	5892	5943	5945	5955	5996	5998	5999	6004
				6006	6029	6030	6420	6449	6503	6582	6592	6611	6639	6698	6711
				6714	6716	6740	6742	6843	6855	6884	6905	7023	7026	7028	7037
				7038	7089	7091	7462	7474	7492	7498	7574	7624	7886	7891	7919
				7938	7947	7989	8608	8670	8700	8739	8813	8862	8875	8878	8884
				8890	8892	8939	8994	8997	9000	9028	9037	9053	9300	10228	10291
				10406	10901	10928	10953
	ADX*.25			1657 #	5734	5735	5737	5739	5751	5753	5755	5757	5841	5844	6109
				6178	6190	6262	6350	6414	6415	6464	6549	6693	6695	6697	6724
				6731	6743	6766	6899	6939	7004	7006	7008	7031	7039	7050	7053
				7308	8406	8795	8937	8943	9034	9095	9464	10529	10531
	ADX*2			1655 #	4401	5525	5728	5729	5730	5732	5745	5746	5747	5749	5840
				5843	5847	5856	5872	5873	5874	5875	5903	5904	5905	5906	5936
				5985	6014	6016	6033	6034	6378	6418	6422	6437	6441	6451	6454
				6544	6700	6760	6898	6900	7011	7036	7303	7305	7399	7443	7881
				7888	7927	8019	8021	8855	8879	8951	9056	9302	10147	10148	10315
				10723
	ARX			1649 #
	CACHE			1651 #
	MEM			1650 #	3938	4002	4003	4377	4378	5046	5075	5078	5339	7300	7786
				8157	8158	8192	8193	8326	8327	8362	8502	8503
	MQ			1653 #	5367	5575	5581	5622	5890	5893	5973	6100	6106	6577	6585
				6588	6591	6672	6703	6705	6928	7015	7017	7476	7974	7978	7984
				8478	8479	8480	8481	8496	8497	8811	9339
	SH			1654 #	4211	4213	4906	4909	4958	5129	5299	5362	5563	5579	5589
				5591	5594	5595	5609	5625	5796	5798	5809	5816	5819	5937	5974
				5978	5986	5992	6003	6101	6103	6104	6332	6367	6381	6387	6445
				6568	6570	6573	6676	6836	6838	6840	6842	6849	6854	6932	6992
				7057	7059	7072	7074	7276	7314	7411	7425	7528	7667	7670	7686
				7688	7755	7758	7788	7790	7921	7923	7931	7981	8042	8044	8045
				8086	8093	8342	8343	8369	8372	8380	8404	8411	8662	8665	8698
				8736	8770	8796	8814	8816	8868	8920	8927	8993	9035	9048	9108
				9110	9182	9188	9199	9240	9243	9258	9261	9290	9362	9388	9395
				9488	9773	9821	9823	10124	10126	10154	10168	10172	10237	10255	10270
				10300	10302	10304	10306	10363	10405	10425	10454	10465	10484	10582	10632
				10642	10726	10751	10767	10812	10990	10992	11094
(D) B				2262 #	4150	4205	4367	4368	5004	5005	5164	5169	5170	5171	5172
				5174	5175	5176	5177	5184	5185	5186	5187	5393	5498	5499	5500
				5502	5921	5922	5923	6534	6535	6536	6537	6751	6752	6753	6754
				9535	9537	9538	9539	9546	9548	9549	9550	9557	9559	9560	9561
				9580	9581	9582	9583	9584	9585	9592	9593	9596	9597	9604	9606
				9607	9608	9615	9617	9618	9619
	AC			2266 #	4145	4146	4154	4155	4159	4160	4164	4165	4220	4222	4229
				4230	4234	4235	4239	4240	4244	4245	4249	4250	4254	4255	4259
				4260	4266	4267	4271	4272	4276	4277	4281	4282	4286	4287	4291
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-11
; 							Cross Reference Listing					

				4292	4296	4297	4301	4302	4406	4407	4416	4417	4426	4427	4436
				4438	4445	4446	4461	4462	4471	4472	4481	4482	4491	4492	4501
				4502	4512	4513	4522	4523	4532	4533	4542	4543	4552	4553	4975
				5181	5637	5638	5649	5650	5662	5663	7662	7663
	BOTH			2268 #	4409	4419	4429	4443	4448	4464	4474	4484	4494	4504	4515
				4525	4535	4545	4555	5640	5652	5665
	DBL AC			2263 #	5694	5695	5767	5768	5772	5773
	DBL BOTH		2264 #	5697	5770	5775
	MEM			2267 #	4156	4161	4166	4226	4231	4236	4241	4246	4251	4256	4261
				4268	4273	4278	4283	4288	4293	4298	4303	4408	4418	4428	4442
				4447	4457	4458	4463	4473	4483	4493	4503	4514	4524	4534	4544
				4554	5639	5651	5664	5696	5769	5774	7750	7751	9545
	SELF			2265 #	4152	4157	4162	4167	4227	4232	4237	4242	4247	4252	4257
				4262	4269	4274	4279	4284	4289	4294	4299	4304
	SJC-			2270 #	4674	4683	4701	4717	4732	4749	4764	4779
	SJCA			2274 #	4678	4687	4705	4721	4736	4753	4768	4783
	SJCE			2272 #	4676	4685	4703	4719	4734	4751	4766	4781	5161
	SJCG			2277 #	4681	4690	4708	4724	4739	4756	4771	4786	5167
	SJCGE			2275 #	4679	4688	4706	4722	4737	4754	4769	4784	4794	5165
	SJCL			2271 #	4675	4684	4702	4718	4733	4750	4765	4780	4795	5160
	SJCLE			2273 #	4677	4686	4704	4720	4735	4752	4767	4782	5162
	SJCN			2276 #	4680	4689	4707	4723	4738	4755	4770	4785	5166
(D) B0				2278 #	6042	6043	6044	6045	6048	6052	6053	6054	6055	6058
	CRY0(0)			2279 #	4566	4567	4568	4569	4575	4576	4577	4578	4584	4585	4586
				4587	4593	4594	4595	4596	4602	4603	4604	4605	4611	4612	4613
				4614	4620	4621	4622	4623	4629	4630	4631	4632	9540	9551	9556
				9562	9586	9598	9603	9605	9609	9614	9616	9620
	CRY0(1)			2280 #	4570	4571	4579	4580	4582	4583	4588	4589	4591	4592	4597
				4598	4600	4601	4606	4607	4609	4610	4615	4616	4618	4619	4624
				4625	4627	4628	4633	4634	4806	9541	9552	9563	9587	9599	9610
				9621
(D) B1-2			2281 #	4566	4567	4568	4569	4570	4571	4575	4576	4577	4578	4579
				4580	4582	4583	4584	4585	4586	4587	4588	4589	4591	4592	4593
				4594	4595	4596	4597	4598	4600	4601	4602	4603	4604	4605	4606
				4607	4609	4610	4611	4612	4613	4614	4615	4616	4618	4619	4620
				4621	4622	4623	4624	4625	4627	4628	4629	4630	4631	4632	4633
				4634	4806	9540	9541	9551	9552	9556	9562	9563	9586	9587	9598
				9599	9603	9605	9609	9610	9614	9616	9620	9621
	AC			2282 #	6042	6047	6048	6052	6057	6058	6115	6120	6121	6144	6145
				6149	6150	6279	6327
	BOTH			2284 #	6045	6050	6055	6060	6118	6123	6147	6152
	MEM			2283 #	6044	6049	6054	6059	6117	6122	6146	6151
(U) BR				1658 #
	AR			1659 #	4036	4047	4058	4069	4080	4140	4175	4178	4402	4813	4815
				4858	4882	4937	4981	5025	5069	5074	5077	5139	5272	5359	5594
				5606	5607	5656	5723	5779	5783	5784	5890	5893	5933	5972	5988
				5990	5994	5996	5998	5999	6004	6006	6029	6030	6092	6094	6164
				6378	6379	6441	6454	6478	6481	6547	6552	6560	6611	6687	6703
				6769	6771	6776	6783	6816	6839	6843	6848	6853	6872	6874	6881
				6902	6905	6916	7002	7009	7015	7055	7067	7068	7069	7278	7283
				7285	7288	7319	7398	7412	7419	7421	7458	7470	7472	7494	7528
				7545	7599	7888	7914	7967	7969	7979	8087	8389	8414	8634	8638
				8653	8686	8770	8775	8812	8821	8844	8857	8865	8875	8928	8930
				8932	8939	8941	8997	9000	9024	9415	9417	9427	9441	9632	9654
				9726	9736	9767	9770	9798	9815	9819	10097	10148	10149	10152	10160
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-12
; 							Cross Reference Listing					

				10164	10209	10232	10234	10243	10246	10309	10311	10424	10573	10598	10628
				10641	10684	10770	10853	10855	10873	11040	11091	11100
(U) BRX				1660 #
	ARX			1661 #	3924	4402	4880	5340	5599	5606	5609	5622	5723	5809	5816
				5819	5851	5852	5860	5861	5890	5893	5937	5986	5994	5996	5998
				5999	6004	6006	6029	6030	6092	6164	6379	6441	6454	6478	6481
				6547	6552	6560	6611	6687	6733	6839	6843	6850	6853	6881	6902
				6905	6916	7002	7009	7055	7458	7470	7472	7494	7528	7888	7914
				7916	7919	7979	8029	8042	8413	8634	8666	8709	8733	8775	8812
				8821	8857	8865	8875	8929	8939	8941	8997	9000	9024	9415	9468
				9823	10171	10309	10311	10344	10406	10472	11007
(U) CALL			1871 #
	CALL			1872 #	3882	4378	4394	4866	4883	4916	4936	4988	5031	5271	5276
				5277	5336	5341	5345	5347	5610	5681	5703	5781	5785	5848	5857
				5935	5954	5962	5979	6002	6018	6024	6137	6166	6168	6543	6568
				6632	6647	6660	6662	6668	6670	6678	6759	6764	6768	6772	6775
				6782	6787	6793	6802	6829	6832	6844	6850	6875	6879	6883	6886
				6894	6904	6914	6917	6918	6921	6923	6933	6942	6950	7277	7307
				7398	7420	7446	7545	7563	7564	7565	7599	7614	7615	7668	7675
				7680	7682	7756	7865	7928	7930	7980	8157	8192	8405	8552	8553
				8560	8599	8600	8603	8604	8616	8617	8626	8628	8650	8651	8677
				8680	8699	8734	8737	8745	8752	8799	8809	8830	8842	8866	8917
				8942	8963	8977	8978	8992	8999	9002	9050	9091	9174	9196	9214
				9217	9252	9272	9390	9401	9463	9465	9469	9490	9517	9520	9628
				9634	9647	9669	9726	9734	9736	9741	9745	9747	9767	9770	9796
				9816	9819	10097	10102	10113	10114	10124	10131	10136	10151	10161	10164
				10169	10179	10180	10208	10215	10227	10232	10234	10244	10246	10255	10265
				10290	10314	10363	10388	10389	10390	10393	10404	10408	10409	10429	10432
				10440	10445	10450	10463	10464	10467	10468	10473	10474	10476	10482	10483
				10991	11009	11044
(U) CLR				2016 #
	AR			2021 #	4832	4882	5359	5576	6902	8708	8969	11008
	AR+ARX			2022 #
	AR+ARX+MQ		2025 #	6462	7424	8734	9091
	AR+MQ			2023 #	7009	8868
	ARL			2019 #	4338	4352	5074	5077	5140	5141	8510	9048	9051	9284	9362
				9388	9417	9488	9654	10250	10632	10633	10634	10725	10726	10741
	ARL+ARX			2026 #	10226	10289
	ARL+ARX+MQ		2027 #
	ARR			2020 #	5524	10173
	ARR+MQ			2028 #	7917	7920
	ARX			2018 #	5513	6413	7323	9044
	ARX+MQ			2024 #	5519	5779	5784	6162	6316	8918
	MQ			2017 #	4378	4394	5571	5579	5935	6103	6104	6107	6541	6583	6586
				6668	6670	6759	6921	6923	6951	7087	7272	7396	9003	9005
(U) COND			1831 #
	AD FLAGS		1845 #	4183	4379	4395	4728	4743	4775	4791	5644	5657	5943	5945
	AR CLR			1837 #	3881	6378	6481	6816	6867	6869	6924	7055	7278	7285	7314
				7319	7399	7412	7925	8668	8990	9273
	ARL IND			1839 #	4214	4313	4315	4318	4321	4324	4325	4328	4329	4338	4339
				4345	4346	4352	4353	4359	4360	4832	4884	5363	5406	5411	6902
				7323	7552	7606	7973	8052	8083	8089	8280	8518	8705	9185	9632
				9654	9774	9825	10139	11054
	ARX CLR			1838 #	5619	5680	5701	5803	6379	6384	6444	6497	6498	6590	6704
				6850	6858	7016	8673	8857
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-13
; 							Cross Reference Listing					

	DIAG FUNC		1850 #	4923	4989	5346	5348	9735	9742	9769	9772	9773	9775	9817
				9820	9822	9824	10103	10135	10152	10162	10166	10170	10220	10221	10222
				10223	10233	10236	10238	10245	10248	10257	10258	10259	10264	10273	10274
				10275	10276	10549	10564
	EBOX STATE		1851 #	3893	3924	4997	10268	10279	10529	10531	10867	10972	11016	11045
	EBUS CTL		1852 #	3967	3968	3969	3970	4922	4924	4990	5351	8483	8484	8485
				8486	9635	9647	9669	9677	9680	9683	9692	9694	9721	9826	9830
				10172	10206	10228	10263	10267	10277	10390	10404	10464	10539	10550	10565
				11044
	FE SHRT			1844 #	10722
	FM WRITE		1842 #	3890	3941	4091	4096	4124	4134	4381	5124	5518	5548	5567
				5582	5960	5966	5973	5977	5980	5985	6001	6020	6021	6031	6033
				6034	6576	6588	6596	6667	6669	6740	6760	6765	6767	6773	6776
				6793	6794	6796	6797	6801	6802	6803	6805	6830	6833	6845	6851
				6870	6877	6887	6913	6920	6922	6927	6947	6987	6990	7088	7220
				7315	7322	7400	7402	7413	7415	7443	7474	7476	7478	7490	7492
				7554	7893	7914	7916	7919	7984	8040	8466	8508	8511	8513	8516
				8553	8602	8607	8609	8611	8619	8620	8651	8663	8664	8665	8667
				8675	8688	8708	8710	8712	8733	8744	8755	8801	8814	8818	8848
				8853	8855	8859	8860	8862	8884	8894	8899	8917	8931	8943	8952
				8954	8963	8964	8974	8976	8978	9003	9005	9027	9032	9091	9119
				9121	9147	9152	9182	9195	9198	9199	9251	9257	9258	9261	9283
				9290	9449	9450	9455	9494	10335	10532	10533	10563	10572	10573	10574
				10579	10582	10598	10818	11005	11039	11055	11060	11061	11067	11075	11077
				11082	11086	11087	11093	11095	11096
	LD AR0-8		1834 #	4140	5373	5374	5375	6817	6871	6925	7056	7061	7083	7279
				7281	7287	7320	7401	7414	7889	8019	8021	8023	8026	8029	8033
				8035	8038	8039	8088	8100	8103	8111	8112	8119	8123	8127	8131
				8467	8661	8844	8849	8852	8966	9057	9122	9123	9124	9125	9126
				9127	9203	9246	9248	9334	9380	9749	10113	10125	10207	10360	10424
				10479	10753	10821	11071
	LD AR18-35		1836 #
	LD AR9-17		1835 #	4866	4883	5137	5142	5360	10148	11009
	LD VMA HELD		1867 #
	LOAD IR			1846 #	3927	3938	5428	8470
	LONG EN			1856 #	4002	8157	8192	8326	8502
	MBOX CTL		1853 #	10099	10182	10186	10191	10193	10197	10199	10590	10592	10594	10824
				11041
	PCF_#			1843 #	5035	5040	5088	5089	5093	5615	5690	5707	5794	5968	6169
				6377	6680	6935	7076	7080	7081	7219	7282	7286	7440	7441	7674
				7679	7682	7762	7772	7775	8067	8291	8324	8993	9658	9659
	REG CTL			1840 #	5728	5729	5730	5732	5745	5746	5747	5749	5931	6502	6502
				6700	6724	6732	6732	6732	6898	6899	6900	6906	7011	7031	7313
				7410	7431	7768	7967	8629	9746
	SEL VMA			1849 #	4008	8401	10560	10578	10990	10992
	SPEC INSTR		1847 #	3880	3933	4950	4961	4967	5427	10278	10288	10334	10656	10994
	SR_#			1848 #	4108	4109	4113	4125	4920	4984	4993	5352	6006	6026	6066
				6067	6068	6190	6414	6512	6516	6526	6599	6600	6628	6662	6688
				6727	6742	6918	6946	7003	7033	7092	7457	7467	7468	7483	7485
				7494	7497	7498	7553	7563	7564	7565	7575	7607	7614	7615	7625
				7792	8045	8051	8670	8674	8680	8682	8693	8698	8699	8700	8713
				8737	8739	8745	8799	8800	8809	8861	8895	8927	8977	8994	9037
				9050	9053	9093	9274	9313	9357	9469	9520	10897	10901	10905	10909
				10913	10917	10928	10932	10936	10940	10944	10953	10958	11052
	VMA DEC			1865 #	8090	9446	10292
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-14
; 							Cross Reference Listing					

	VMA INC			1866 #	4374	4398	4888	4892	4902	4903	5120	5145	5278	5286	5343
				5367	5369	5934	5989	6542	6758	7275	7397	7681	7774	7979	7996
				8085	8293	8890	8892	9031	9335	9442	9509	9802	10294	10313	10432
				11011	11013	11014
	VMA_#			1860 #	5274	5283	5339	10266	10301	10303	10305	10307	10968
	VMA_#+AR32-35		1863 #	8407	10616	10618
	VMA_#+MODE		1862 #	5337
	VMA_#+PI*2		1864 #	10339	10340	10354
	VMA_#+TRAP		1861 #	3926	3928
(U) DIAG FUNC			2213 #
	CONI APR(L)		2232 #	9773
	CONI APR(R)		2228 #	9772
	CONI MTR		2238 #	10248
	CONI PAG		2241 #	10166
	CONI PI(L)		2227 #	9824
	CONI PI(PAR)		2240 #	9822
	CONI PI(R)		2226 #	9820
	CONO APR		2219 #	9769	9775
	CONO MTR		2217 #	10245
	CONO PAG		2221 #	10162
	CONO PI			2220 #	9817
	CONO TIM		2218 #	10233
	DATAI APR		2234 #	9742
	DATAI PAG(L)		2230 #	5346	10135
	DATAO APR		2222 #	9735
	DATAO PAG		2223 #	10103
	LD AC BLKS		2224 #
	LD PA LEFT		2215 #	10257	10259
	LD PA RIGHT		2216 #	10258
	LD PCS+CWSX		2225 #	4923	5348
	RD CACHE CNT		2235 #	10223	10276
	RD EBOX CNT		2233 #	10222	10275
	RD EBUS REG		2242 #	4989	10152	10170	10549	10564
	RD INTRVL		2236 #	10236
	RD MTR REQ		2239 #	10264
	RD PERF CNT		2231 #	10221	10274
	RD PERIOD		2237 #	10238
	RD TIME			2229 #	10220	10273
	.5 USEC			2214 #
(U) DISP			1877 #
	BYTE			1890 #	4961	7668	7671	7755	7759	7968	8000	8068	8919	9175	9222
				9389	9434	9435	9436	9449	9489	9629	10192	10198	10778
	DIAG			1878 #
	DIV			1887 #	5839	5842	5847	5856	5872	5873	5874	5875	5876	5877	5878
				5879	5903	5904	5905	5906	7927	8956
	DRAM A RD		1880 #	3962	3963	3964	3965	3975	3976	8478	8479	8480	8481	8497
	DRAM B			1889 #	4172	4181	4313	4315	4318	4321	4325	4329	4338	4339	4345
				4346	4352	4353	4359	4360	4374	4413	4423	4433	4452	4468	4478
				4488	4498	4508	4519	4529	4539	4549	4559	4656	4659	4663	4990
				5039	5545	5600	5620	5624	5644	5657	5689	5706	5797	5799	5804
				5938	6066	6081	6512	6516	6544	6760	8496	8581	8582	8667	8850
				8858	9036	9094	9131	9135	9635	9654	9826	10174	10218	10226
	DRAM J			1879 #	4037	4048	4059	4070	4081	4859	4885	4893	8509	8512	8514
				8517
	EA MOD			1892 #	3925	4005	4815	4862	7675	7680	7763	7773	8159	8160	8194
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-15
; 							Cross Reference Listing					

				8195	8209	8210	8292	8328	8471	8504	9183	9199	9259	9262	10583
	MUL			1886 #	5724	5734	5736	5738	5740	5752	5754	5756	5757	5762	5954
				6893
	NICOND			1884 #	4096	4118
	NORM			1891 #	6110	6138	6178	6190	6414	6416	6442	6447	6455	6465	6498
				6593	6650	6673	6698	6706	6712	6715	6716	6856	6908	6929	6995
				6997	7019	7024	7027	7029	7038	7094	7310	7324	7405	7416	7447
	PG FAIL			1882 #	10539	10550
	RETURN			1881 #	4402	4925	4931	5046	5299	5363	5369	5728	5729	5730	5732
				5745	5746	5747	5749	5849	5851	5852	5858	5860	5861	5891	5894
				5907	5908	5909	5910	6265	6267	6614	6616	6807	6818	7197	7198
				7204	7205	7554	7575	7625	8069	8070	8076	8079	8091	8093	8100
				8103	8111	8112	8119	8123	8127	8131	8149	8151	8162	8164	8211
				8213	8217	8219	8274	8277	8278	8281	8306	8307	8308	8309	8317
				8318	8350	8351	8365	8374	8387	8389	8390	8415	8570	8605	8609
				8618	8620	8627	8629	8638	8758	9093	9097	9121	9132	9137	9141
				9152	9159	9203	9218	9522	9684	9697	9717	9721	10155	10210	10294
				10315	10426	10431	10434	10456	10867
	SH0-3			1885 #	5344	7071	7432	7673	7677	7699	7700	7761	7765	7804	7806
				7881	8067	8110	9108	9110	9292	9309	10270	10289	10337	10356	10388
				10407	10629	10718
	SIGNS			1888 #	5811	6005	6007	7915	8704	8708	8735	8755	8800	8801	8815
				8848	8972	9029	9054	9092	9120	9147	10580	10617	10619
	SR			1883 #	6026	6027	6032	6417	6419	6421	6423	6450	6452	6453	6463
				6482	6502	6709	6732	7078	7087	7966	8339	8756	10572	10874	11055
				11067	11075	11096
(U) EA CALC			2100 #
	A IND			2110 #	3953	3955	3957	3959	3977	3979	8473	8474	8475	8476	8498
				8499
	BYTE IND		2118 #	7681	7774	8152	8154	8187	8189	8216	8218	8293	8297	8299
				8301	8303	8319	8321
	BYTE LD			2114 #	8148	8150	8161	8163
	BYTE RD			2115 #	7672	7676	8306	8307	8308	8309	8317	8318
	BYTE RD PC		2116 #
	BYTE RPW		2117 #	7760	7764	8182	8184	8196	8199
	LD ARX+WR		2126 #	9442
	LD AR(EA)		2124 #	7558	7559	7566
	LD AR+WR		2125 #	9414	9418
	POP AR			2120 #	5069
	POP AR-ARX		2122 #	5071
	POP ARX			2121 #
	PUSH			2119 #	5023	5028
	WRITE(E)		2123 #	5095	7568
(U) EBUS CTL			2197 #	4924	5351	9692
	DATAI			2211 #	9701	10404	10464
	DATAO			2210 #	9647	9702	10390	11044
	EBUS DEMAND		2201 #	9677
	EBUS NODEMAND		2202 #	9680
	GRAB EEBUS		2198 #	3967	3968	3969	3970	4922	8483	8484	8485	8486	9721	10263
				10539
	IO INIT			2207 #	9694	9697
	REL EBUS		2200 #	9683	9826
	REL EEBUS		2212 #	4990	9635	10172	10206	10228	10267	10277	10550	10565
	REQ EBUS		2199 #	9669	9830
(U) EXP TST			2044 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-16
; 							Cross Reference Listing					

	AR_EXP			2045 #	6502	6732
(U) FE				1716 #	10722
	SCAD			1717 #	4006	4008	4047	4069	4096	4118	4950	4958	4967	5519	5522
				5572	5575	5680	5702	5724	5728	5729	5730	5732	5734	5736	5738
				5740	5745	5746	5747	5749	5752	5754	5756	5757	5762	5810	5817
				5820	5839	5842	5847	5856	5872	5873	5874	5875	5876	5877	5878
				5879	5890	5893	5903	5904	5905	5906	5907	5908	5909	5910	5948
				5954	5963	5991	5997	6018	6082	6085	6094	6103	6104	6107	6135
				6138	6165	6176	6179	6318	6332	6349	6353	6415	6418	6420	6422
				6442	6446	6449	6451	6455	6462	6464	6501	6541	6553	6612	6640
				6667	6669	6677	6694	6696	6697	6700	6701	6728	6731	6785	6789
				6791	6800	6854	6880	6885	6891	6893	6920	6922	6924	6932	6991
				7005	7006	7008	7011	7013	7034	7039	7291	7302	7305	7309	7321
				7395	7409	7427	7674	7679	7681	7699	7700	7737	7738	7762	7768
				7769	7772	7774	7787	7789	7804	7806	7844	7845	7881	7882	7884
				7886	7922	7925	7927	7930	7931	7969	7973	7975	7989	8040	8077
				8080	8212	8275	8291	8293	8311	8312	8313	8314	8368	8370	8411
				8505	8552	8560	8568	8661	8703	8796	8844	8940	8951	8953	8966
				8998	9001	9004	9006	9119	9121	9141	9160	9182	9189	9198	9199
				9244	9257	9258	9261	9287	9331	9343	9345	9358	9367	9370	9392
				9395	9452	9462	9796	10111	10190	10196	10210	10356	10423	10463	10472
				10543	10548	10591	10593	10595	10622	10623	10628	10671	10688	10710	10718
				10733	10753	10779	10781	10816	10841	10851	10893
(U) FETCH			2088 #
	COMP			2091 #	4694
	JFCL			2095 #	4938
	JUMP			2094 #	4760	4775	4791	4799
	SKIP			2092 #	4712	8803
	TEST			2093 #	4655	4658	4663	8743	9639	9641	9643
	UNCOND			2089 #
(U) FLAG CTL			2067 #
	DISMISS			2071 #	4858	4910	4911	9651	10391
	DISMISS+LD		2073 #
	HALT			2074 #	4948
	JFCL			2069 #	4937
	JFCL+LD			2070 #	4940
	PORTAL			2076 #	4813
	RSTR FLAGS		2068 #	4854	4904	4905	4908
	SET FLAGS		2075 #	3822	5372	11015
(U) FMADR			1672 #
	AC+#			1683 #	8603	8607	8609	8669	8829	8829	8849	8899	9141	9214	9214
				9240	9242	9243	9251	9257	9258	9261	9314	9323	9436	9449	9450
				9455	9509	11069	11075	11082
	AC0			1673 #	4036	4091	4096	4124	4199	4211	4214	4313	4315	4318	4321
				4324	4328	4381	4423	4433	4452	4468	4478	4488	4498	4508	4519
				4539	4549	4663	4666	4667	4668	4694	4760	4775	4790	4799	5023
				5028	5069	5071	5073	5091	5124	5129	5139	5407	5409	5412	5415
				5417	5513	5516	5534	5534	5548	5556	5567	5572	5582	5589	5591
				5644	5656	5679	5702	5785	5803	5809	5891	5894	5937	5961	5966
				5992	6092	6135	6165	6317	6559	6568	6576	6588	6591	6599	6600
				6640	6676	6740	6775	6792	6796	6829	6830	6854	6874	6885	6915
				6932	6992	7088	7220	7419	7422	7444	7459	7471	7472	7474	7476
				7490	7492	7529	7554	7768	7804	7806	7913	7916	7919	8552	8560
				8653	8667	8693	8746	8770	8818	8843	8853	8855	8883	8884	8920
				8929	8993	9003	9005	9032	9093	9109	9111	9119	9121	9136	9152
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-17
; 							Cross Reference Listing					

				9274	9290	9471	9494	9519	9521	11055	11086	11094	11095	11098
	AC1			1674 #	4134	4393	4394	5518	5554	5555	5571	5780	5933	5973	5989
				6020	6021	6026	6027	6547	6596	6597	6625	6626	6660	6662	6667
				6669	6672	6797	6803	6807	6846	6852	6853	6868	6881	6917	6918
				6920	6922	6928	6991	7430	7457	7984	8599	8611	8616	8620	8698
				8708	8798	8798	8918	9160	9160	9182	9198	9199	9462	9462	10897
				11060	11067	11087
	AC2			1677 #	5977	5987	6031	7248	7478	8602	8619	9187	9188	9195	11061
	AC3			1678 #	5931	5960	5975	5980	6033	6034	8554	8568	8652	8663	8664
				8671	8678	8681	8688	8710	8712	8744	8753	8755	8814	8856	8859
				8860	8868	8894	8927	8931	8941	8943	8965	8974	9047	9135	9147
				9159	9388	9414	9417	9488	10905	10932	10936	10944	10958	11077	11088
				11093	11096
	VMA			1676 #	3927	3938	4002	4003	4047	4058	4069	4080	4092	4093	4099
				4101	4377	4378	4398	4712	4887	4890	4892	4902	4903	4904	4905
				4908	4920	5033	5035	5038	5040	5046	5046	5074	5075	5077	5078
				5093	5094	5120	5145	5278	5286	5339	5342	5352	5365	5368	7272
				7300	7313	7396	7410	7467	7469	7489	7495	7563	7564	7571	7573
				7574	7614	7621	7623	7624	7670	7674	7678	7683	7704	7705	7706
				7707	7708	7709	7710	7712	7718	7719	7720	7721	7722	7723	7725
				7731	7732	7733	7735	7737	7738	7758	7762	7766	7772	7786	7786
				7811	7812	7813	7814	7815	7816	7817	7819	7825	7826	7827	7828
				7829	7830	7832	7838	7839	7840	7842	7844	7845	7866	8086	8090
				8115	8157	8158	8192	8193	8291	8294	8326	8327	8337	8339	8362
				8362	8365	8502	8503	8650	8775	8977	9046	9107	9218	9291	9419
				9434	9443	9445	9447	9467	9472	9508	9516	9634	9638	9640	9647
				9654	10293	10313	10350	10381	10390	10391	10627	10642	10662	10688	10703
				10710	10733	11016
	XR			1675 #	3925	3955	3959	3963	3965	3976	3979	4005	4850	8150	8154
				8159	8160	8163	8184	8189	8194	8195	8199	8209	8210	8218	8299
				8303	8307	8309	8318	8321	8328	8474	8476	8479	8481	8497	8499
				8504
	#B#			1685 #	3890	3941	4865	4880	5027	5118	5123	5272	5353	5358	5362
				5366	5985	6001	6003	6023	6760	6765	6767	6773	6776	6780	6785
				6789	6793	6794	6795	6800	6801	6802	6804	6805	6831	6833	6835
				6836	6845	6847	6848	6851	6870	6877	6878	6887	6913	6926	6927
				6943	6947	6987	6988	6990	6994	6996	7021	7060	7067	7197	7198
				7204	7205	7253	7254	7266	7267	7268	7315	7322	7400	7402	7413
				7415	7434	7437	7443	7893	7914	7939	7966	8040	8466	8508	8511
				8513	8516	8553	8604	8605	8634	8636	8637	8651	8665	8675	8677
				8686	8691	8709	8733	8752	8801	8810	8848	8862	8865	8870	8876
				8917	8952	8954	8963	8964	8973	8976	8978	8979	8990	8991	9025
				9027	9041	9042	9091	9094	9095	9096	9097	9105	9130	9132	9137
				9146	9148	9149	9283	9400	9402	9464	9465	9517	10335	10337	10373
				10397	10419	10434	10455	10532	10533	10563	10572	10573	10574	10579	10582
				10598	10630	10640	10647	10655	10669	10680	10685	10692	10693	10694	10709
				10719	10720	10731	10746	10757	10763	10772	10783	10787	10797	10809	10814
				10818	10822	10836	10842	10850	10854	10856	10866	10873	10921	10948	10973
				11005	11006	11038	11039	11040	11078	11084	11089	11091	11100
(U) ISTAT			1972 #
	OPTIONS			1976 #	9749
(U) J				1563 #	3962	3963	3964	3965	3975	3976	4037	4048	4059	4070	4081
				4402	4820	4859	4885	4893	4925	4931	5046	5299	5363	5369	5728
				5729	5730	5732	5745	5746	5747	5749	5849	5851	5852	5858	5860
				5861	5891	5894	5907	5908	5909	5910	6265	6267	6614	6616	6807
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-18
; 							Cross Reference Listing					

				6818	7197	7198	7204	7205	7554	7575	7625	8069	8070	8076	8079
				8091	8093	8100	8103	8111	8112	8119	8123	8127	8131	8149	8151
				8162	8164	8211	8213	8217	8219	8274	8277	8278	8281	8306	8307
				8308	8309	8317	8318	8350	8351	8365	8374	8387	8389	8390	8415
				8478	8479	8480	8481	8497	8509	8512	8514	8517	8570	8605	8609
				8618	8620	8627	8629	8638	8758	9093	9097	9121	9132	9137	9141
				9152	9159	9203	9218	9522	9684	9697	9717	9721	10155	10210	10294
				10315	10426	10431	10434	10456	10867
	ACNORM			6990 #	6785
	ACNRM1			6994 #	6988
	ACSETU			7554 #	7608
	ADD			5644 #	5637	5638	5639	5640
	ADJBP			7881 #	7864
	ADJDIV			7927 #	7932
	ADJOIN			7913 #	8045
	ADJOWG			8018 #	7881
	ADJP			7989 #	7974	7978	7990
	ADJSP			5406 #	5393
	ADJSP1			5415 #	5410
	ADJTWG			7973 #	7968
	ADJUST			7966 #	7938	7939
	AND			4423 #	4416	4417	4418	4419
	ANDCA			4433 #	4426	4427	4428	4429
	ANDCB			4488 #	4481	4482	4483	4484
	ANDCM			4452 #	4445	4446	4447	4448
	AOBJ			4799 #	4794	4795
	AOJ			4775 #	4764	4765	4766	4767	4768	4769	4770	4771
	AOS			4728 #	4717	4718	4719	4720	4721	4722	4723	4724
	APRBI			9745 #	9534
	APRBO			9736 #	9536
	APRCI			9770 #	9539	9540	9541
	APRCO			9767 #	9538
	APRCO7			9775 #	9769
	APRDI			9741 #	9535
	APRDO			9734 #	9537
	ARJMP			4918 #	4904	4911	4920	11016
	ARSWAP			4931 #	4866	4883	10208	11009
	ASH			5532 #	5498
	ASHC			5553 #	5502
	ASHL			5589 #	5533	5554
	ASHL1			5605 #	5590
	ASHL2			5609 #	5618
	ASHL3			5613 #	5615
	ASHL4			5617 #	5614
	ASHL5			5622 #	5625
	ASHR1			5594 #	5597
	ASHR2			5599 #	5594
	ASHX			5630 #	5600	5624
	AWAIT			10350 #
	B2DFPF			11084 #	10905
	B2DPF			11086 #	9037	10928
	B2DPF2			11089 #	11084
	BACKD			11069 #	10909	10913	10917	10940
	BACKS			11066 #	10897
	BD1			8936 #	8921
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-19
; 							Cross Reference Listing					

	BD2			8939 #	8936	8943
	BD3			8941 #	8937
	BD4			8951 #	8940	8956
	BD6			8956 #	8951
	BD7			8963 #	8952	8954
	BD8			8971 #	8969
	BD9			8974 #	8979
	BDD1			9024 #	8994	9057
	BDD2			9034 #	9029
	BDD3			9041 #	9036
	BDD4			9044 #	9041
	BDD5			9046 #	9047
	BDD6			9048 #	9046
	BDD7			9050 #	9042	9051
	BDDR1			8927 #
	BDDR4			9027 #	8933
	BDDV1			8990 #	8974
	BDDV2			8997 #	8992
	BDEC			8917 #	8558
	BDF1			8976 #	8973
	BFETCH			8316 #	8292	9183	9199	9259	9262
	BLK1			9655 #	9629
	BLK2			9654 #	9655	9656
	BLK3			9658 #	9654
	BLK4			9643 #	9638
	BLKIO			9628 #	9603	9605	9614	9616
	BLT			4213 #	4206
	BLT1			7528 #	4214
	BLT2			7545 #	7535
	BLTAC			7552 #	7545
	BLTPF			11052 #	7571	7621	10921
	BLTPF1			11054 #
	BLTPXCT			7599 #	7536
	BLTPX1			7606 #	7599
	BPART2			8324 #	8293
	BRJMP			4861 #	4813	4854
	BXA			8297 #	8328
	BYTEA			8291 #	8294	10484
	BYTEI			8326 #	8298	8300	8302	8304	8320	8322	9198	9257
	BYTEI2			8328 #	8326
	BYTINC			8076 #	8068
	BYTIND			8209 #	8157	8192
	CAIM			4694 #	4674	4675	4676	4677	4678	4679	4680	4681	4683	4684	4685
				4686	4687	4688	4689	4690
	CAPHGH			7891 #	7892
	CAPLOW			7886 #	7887
	CDBLST			6743 #	7092
	CHALT			4950 #	3941
	CHKAC			10454 #	10440	10445	10450
	CLEAN			10893 #	8339	8756	10572	10874	11055	11061	11067	11075	11096
	CLRFPD			4112 #	9643
	CLRPT			10206 #	10114	10180
	CMPDST			8829 #	8809
	CMPS			8770 #	8554
	CMPS1			8774 #	8772
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-20
; 							Cross Reference Listing					

	CMPS3			8795 #	8822
	CMPS4			8796 #	8775
	CMPS5			8809 #	8801
	CMPS6			8818 #	8815
	CMPS7			8821 #	8818
	CMPSX			8803 #	8795
	CMTR			10276 #	10126
	CMTR1			10284 #	10276
	CNV01			8411 #	8412
	CNV02			8413 #	8411
	CNV2WD			8401 #	8628
	COMPEA			3975 #	3925
	CONO			9632 #	9607	9618
	CONS			9630 #	9609	9610	9620	9621
	CONT			3825 #
	D2BPF			11077 #	10901
	DASMD			5933 #	5921	5922	5923
	DB2WD			8616 #	8842
	DBABT			8883 #	8870	8876
	DBIN			8842 #	8557
	DBIN2			8875 #	8869
	DBIN3			8878 #	8875
	DBINLP			8865 #	8879
	DBLST			4381 #	6727	6743	7033
	DBS1			8855 #	8849
	DBS2			8857 #	8853
	DBS3			8861 #	8859
	DBST			8853 #	8851
	DBXIT			8890 #	8867
	DDIV			5931 #	5924
	DDIV0			5985 #	5931
	DDIV1			5994 #	5998	5999
	DDIV2			6001 #	5995
	DDIV3			6014 #	6005	6007
	DDIV4			6018 #	6015
	DDIV6			6023 #	6020
	DDVLP			5903 #	5903	5904	5905	5906	6018	9004
	DDVSUB			5905 #	6679	6934	8999	9002	9006
	DDVX1			6029 #	6026
	DDVX2			6031 #	6029	6030
	DEAIND			8192 #	7775	8188
	DEPBYT			7786 #	7768
	DEPOWG			7788 #	7812	7813	7814	7815	7816	7817	7826	7827	7828	7829	7830
				7844	7845
	DEPTWG			7772 #	7766
	DEXCHK			10429 #	10393	10409
	DFAS			6547 #	6549
	DFAS1			6568 #	6547
	DFAS2			6570 #
	DFAS3			6585 #	6572
	DFAS4			6591 #
	DFAS5			6592 #	6586	6599	6600
	DFAS6			6595 #	6577
	DFDV			6660 #	6561
	DFDV1			6676 #	6660	6662
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-21
; 							Cross Reference Listing					

	DFLOAT			6541 #	6534	6535	6536	6537
	DFMP			6625 #	6553
	DFMP1			6630 #	6625	6627
	DFMP2			6647 #	6648
	DISM			4857 #	4827	4829
	DIV			5779 #	5772	5773	5774	5775
	DIV+			5875 #	6668	6921
	DIV-			5874 #	6168	6670	6923
	DIV1			5809 #	5781
	DIV2			5816 #	5786
	DIVLP			5872 #	5848	5857	5872	5873	5874	5875	6025
	DIVS1			5839 #	5811	5818	5841	7930
	DIVS2			5842 #	5821	5844
	DIVS3			5847 #	5840	7928
	DIVS4			5856 #	5843
	DIVX			5889 #	5876	5877	5878	5879
	DLOAD			4377 #	7996
	DMOVEM			4393 #	4387
	DMOVE			4374 #	4367
	DMOVNM			4394 #	4388
	DMOVN			4373 #	4368
	DMUL1			5966 #	5968
	DMUL2			5972 #	5967
	DMULT			5954 #	5948
	DMVM1			4398 #	4393	10229
	DNHI			6724 #	6694
	DNNEG			6709 #	6688
	DNNEG1			6714 #	6710
	DNORM			6685 #	6593	6650	6673	6706	6712	6715	6716
	DNSHFT			6703 #	6728
	DNTRY			6650 #	6583	6724
	DNZERO			6727 #	6685
	DODIAG			9798 #
	DPB			7758 #	7751
	DPB1			8362 #	9217	10476
	DPB2			8368 #	8364
	DPEA			8182 #	7763	7773	8194
	DRND1			6731 #	6699
	DROUND			6698 #	6696	6700
	DSTAC			4124 #
	DTEVEC			10363 #	10360
	EBUSI			9701 #	10404	10464
	EBUSO			9702 #	9647	10390	11044
	EBUSW			9679 #	9681
	EBUSX			9676 #	9701	9702
	EDDISP			9294 #
	EDDSNG			9427 #	9435
	EDEX0			9414 #	9324
	EDEX1			9452 #	9449	9455
	EDEX2			9434 #	9415
	EDEXMD			9417 #
	EDEXX			9447 #	9427
	EDFL1			6764 #	6767
	EDFLDS			9380 #	9315
	EDFLOT			6758 #	6751	6752	6753	6754
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-22
; 							Cross Reference Listing					

	EDFLTX			9508 #	9505
	EDFLT			9503 #	9390	9490
	EDFLT1			9516 #	9503	9504	9506
	EDFPUT			9471 #	9477
	EDIT			9272 #	8556
	EDIT1			9287 #	9285
	EDITLP			9289 #	9374
	EDMPUT			9472 #	9402
	EDMSG			9400 #	9296
	EDNOP			9343 #	9297	9298	9299	9308	9345	9395
	EDNXT1			9356 #	9344	9368	9371	9393	9453
	EDNXT2			9367 #	9356
	EDNXT3			9374 #	9360	9364
	EDOPR			9308 #	9295
	EDSEL			9462 #	9313
	EDSEND			9395 #	9380	9471
	EDSF1			9477 #	9467
	EDSFIL			9467 #	9400
	EDSFLT			9488 #	9468	9493
	EDSKPT			9339 #	9301	9303
	EDSKP			9345 #	9304
	EDSPUT			9469 #	9472	9494
	EDSSIG			9388 #	9314
	EDSTOP			9331 #	9312	9470
	EDVCHK			6938 #	6933	6939
	EDVCH1			6942 #	6938
	EDVCH2			6947 #
	EDVCH3			6950 #
	EEOV			7083 #	7076	7080
	EF1			6775 #	6765
	EF10			6847 #	6838
	EF11			6854 #	6846	6852
	EF12			6858 #	6844	6850
	EF3A			6787 #	6783
	EF3B			6790 #	6788
	EF5			6799 #	6784
	EF5A			6829 #	6797	6805
	EF5B			6807 #	6793	6802
	EFDV0			6913 #	6773
	EFDV1			6932 #	6917	6918
	EFIW			3953 #	4006
	EFMP			6867 #	6770
	EFMP1			6870 #	6867
	EFMPP1			6890 #	6891
	EFMPP2			6893 #	6890
	EFMPP3			6902 #	6898	6899
	EIGHT7			8033 #	8024
	EMTR			10275 #	10124
	EMTR1			10283 #	10275
	ENFNL0			7074 #	7084
	ENFNL1			7086 #	7073	7075
	ENHI			7031 #	7005
	ENNEG			7021 #	7003
	ENNEG1			7026 #	7022
	ENORM			7000 #	6856	6908	6929	6995	7019	7024	7027	7029	7094	7325	7405
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-23
; 							Cross Reference Listing					

				7416	7447
	ENSHFT			7015 #	7034
	ENTRY			7094 #	7031
	ENZERO			7033 #	7001
	EQV			4498 #	4491	4492	4493	4494
	ERND1			7036 #	7010
	ERND2			7050 #
	EROUND			7009 #	7007	7012
	ESEND			9647 #	10351
	EXCH			4211 #	4205
	EXMSK			3940 #	3890
	EXPD			6610 #	6568
	EXPD1			6615 #	6612
	EXPDIF			6816 #	6782	6787
	EXT01			8603 #	8601	8611	8917
	EXT02			8611 #	8602
	EXT2			8508 #	8496	8581	8582
	EXT2WD			8599 #	8553	8651	9272
	EXT3			8516 #	8510	8518
	EXTEND			6366 #	6324
	EXTF1			8465 #	6367
	EXTF2			8471 #
	EXTI			8502 #	8473	8474	8475	8476	8498	8499
	EXTI2			8504 #	8502
	EXTLA			8496 #	8471
	EXTXA			8473 #	8505
	FAD			6065 #	6042	6044	6045
	FADR			6082 #	6047	6049	6050	6066
	FADRI			6080 #	6048
	FAS			6092 #	6083	6086
	FAS1			6098 #	6094
	FAS2			6100 #	6096
	FAS3			6106 #	6100
	FAS5			6109 #	6103	6104
	FDV			6068 #	6144	6146	6147
	FDVCHK			6261 #	6166	6262	6678
	FDVCK1			6264 #	6261	6951
	FDVNEG			6190 #	6177	6191
	FDVR			6162 #	6068	6149	6151	6152
	FDVRI			6161 #	6150
	FINI			4118 #	4109	4112	4113	4134	4665	4812	4940	5124	5980	8899
	FIX			6353 #	6323
	FIX1			6377 #	6350	6354
	FIX2			6387 #	6385
	FIXR			6349 #	6326
	FLGTST			8568 #	8552	8560
	FLTR			6332 #	6327
	FMP			6067 #	6115	6117	6118
	FMPR			6133 #	6067	6120	6122	6123
	FMPRI			6132 #	6121
	FPNO			6069 #	6043	6053	6116	6145	8115
	FSB			6066 #	6052	6054	6055
	FSBR			6085 #	6057	6059	6060
	FSBRI			6081 #	6058
	FSC			6316 #	6279
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-24
; 							Cross Reference Listing					

	GETEEB			9720 #	4988	9734	9741	9768	9771	10102	10131	10161	10165	10216	10232
				10235	10244	10247	10256
	GETSC			8379 #	8963	9401
	GETSRC			9160 #
	GLBIND			8216 #	8210
	GSRC			9159 #	8735	9092
	GSRC1			9174 #	8799	9463
	GSRC2			9182 #	9175	9185
	GSRC3			9184 #	9190
	GSRC4			9198 #	9187
	GSRC5			9192 #	9189
	GSRC6			9195 #	9192
	GTAR08			10425 #	4140	10479
	GTCST			10767 #
	GTDBR			4401 #	4378	4394
	GTEBUS			9668 #	9634
	GTEEB1			9721 #	5372
	GTST			8386 #	8626
	GTST1			8389 #	8386
	GUDSIZ			7768 #	7769	8183	8186	8198	8201
	HALT1			4957 #	4962
	HALT2			4961 #	4958
	HLL			4315 #	4220	4268	7840
	HLLE			4356 #	4254	4255	4256	4257
	HLLO			4360 #	4244	4245	4246	4247
	HLLZ			4359 #	4234	4235	4236	4237
	HLR			4321 #	4271	4272
	HLRE			4349 #	4301	4302	4303	4304
	HLRM			4328 #	4273
	HLRO			4353 #	4291	4292	4293	4294
	HLRS			4329 #	4274
	HLRZ			4352 #	4281	4282	4283	4284	7732
	HRL			4318 #	4229	4230
	HRLE			4342 #	4259	4260	4261	4262
	HRLM			4324 #	4231	7839
	HRLO			4346 #	4249	4250	4251	4252
	HRLS			4325 #	4232
	HRLZ			4345 #	4239	4240	4241	4242
	HRR			4313 #	4226	4266	4267
	HRRE			4335 #	4296	4297	4298	4299
	HRRO			4339 #	4286	4287	4288	4289
	HRRZ			4338 #	4276	4277	4278	4279	7733
	IBP			7862 #	7858
	IBPS			8274 #	10483
	IBPTST			7864 #
	IDIV			5783 #	5767	5768	5769	5770
	IDIV1			5784 #	5805
	IDIV2			5802 #	5783
	IDPB			7755 #	7750
	IDST			9222 #	8830	9214
	IDST2B			9261 #	9240
	IDST2			9240 #	9223	9246
	IDST3			9246 #	9241
	IDST4			9254 #	9242
	IDST5			9248 #	9244
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-25
; 							Cross Reference Listing					

	IDST6			9251 #	9248
	IDST7			9257 #	9254
	IDST8			9258 #	9255
	IFNOP			4098 #	6169	6377	6680	6935	9730	10099	10193	10199
	IFSTAC			4100 #	5041
	IHALT			4947 #	4817
	ILDB			7667 #	7662
	ILLIND			4008 #	3967	3968	3969	3970	8483	8484	8485	8486
	ILLOWG			8115 #	7735	7842
	IMUL			5679 #	5662	5664	5665
	IMUL2			5689 #	5690
	IMULI			5670 #	5663
	INCRBP			8067 #	7668	7756	7865
	INDR1			4005 #	4002
	INDRCT			4002 #	3954	3956	3958	3960	3978	3980
	INTRPT			3896 #
	IO			9634 #	9604	9606	9608	9615	9617	9619	9658	9659
	IOCHK			9711 #	9628	9727	9736	9745	9797	10098
	IOFET			9650 #	9640
	IOPGF			11038 #	10993
	IOR			4478 #	4471	4472	4473	4474
	IOTEND			9638 #	9826	10174
	ISOEXP			7197 #	6764	6768	6772	6775	6875	6942	7277	7420
	JFCL			4934 #	4806
	JFFO			5516 #	5501
	JFFO1			5522 #	5523
	JFFO2			5527 #	5528
	JRA			5131 #	5104
	JRA1			5031 #	5141	5142
	JRAB			5142 #	5140
	JRST			4812 #	4805
	JRST2			4815 #
	JRSTF			4849 #	4815	4862
	JSA			5129 #	5103
	JSA1			5145 #	5129
	JSP			5114 #	5102
	JSP1			5123 #	5114
	JSR			5116 #	5101
	JSR1			5120 #	5118
	JSTAC			5033 #	5028	5035
	JUMP			4760 #	4749	4750	4751	4752	4753	4754	4755	4756
	KEEPCL			10196 #	10186	10198
	KEEPME			10182 #	10116
	KMOVED			10757 #	10751
	L-BDEC			5292 #	5171	5172
	L-CMS			5259 #	5160	5161	5162	5165	5166	5167
	L-DBIN			5290 #	5169	5170
	L-DFLT			7408 #	7267
	L-DFSC			7419 #	7268
	L-DITE			7241 #	5188
	L-EDBL			7312 #	7254
	L-EDIT			5261 #	5164
	L-EFSC			7245 #	5190
	L-FLTR			7394 #	7266
	L-FSC2			7434 #	7440	7441
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-26
; 							Cross Reference Listing					

	L-FSC3			7443 #	7436	7439
	L-GTDI			7233 #	5184
	L-GTDR			7237 #	5186
	L-GTPI			7228 #	5181
	L-GTSI			7235 #	5185
	L-GTSP			7271 #	7253
	L-GTSR			7239 #	5187
	L-GTS1			7283 #	7281
	L-GTS6			7287 #	7285
	L-GTS7			7307 #	7304
	L-MVS			5294 #	5174	5175	5176	5177
	L-SFTE			7231 #	5183
	L-SITE			7243 #	5189
	L-XBLT			7226 #	5180
	LDB			7670 #	7663
	LDB1			8337 #	8832	9176	10468
	LDB2			8342 #	8338
	LDBRL			4402 #
	LDEA			8148 #	7675	7680	8159
	LDIMM			10722 #	10733
	LDIND			10655 #	10633	10656	10727
	LDIND1			10661 #	10655
	LDPCS			4922 #	4916
	LDPT			10739 #	10724
	LDPT1			10751 #	10742	10753
	LDSHR			10731 #	10725
	LEAIND			8157 #	7682	8153
	LSH			5513 #	5500
	LSH1			5572 #
	LSH2			5574 #	5577
	LSH3			5579 #	5575	5581
	LSHC			5571 #	5504
	LUUO			5269 #	5201	5202	5203	5204	5205	5206	5259	5290	5292	5294	7226
				7228	7231	7233	7235	7237	7239	7241	7243	7245
	LUUO1			5271 #	5269
	LUUO2			5283 #	5273
	LUUO3			5286 #	5284
	MAP			4981 #	4975
	MAP2			4993 #	10948
	MBREL			9730 #	9737
	MEMNRM			6987 #	6789
	MOVE			4181 #	4145	4146	4152	4175	4183	4227	4269	4436	4442	4443	4457
				4458
	MOVELP			8677 #	8682
	MOVEM			4210 #	4150
	MOVF1			8752 #	8699	8745	8759
	MOVF2			8755 #	8753	8978
	MOVF3			8758 #	8755
	MOVM			4175 #	4164	4165	4166	4167
	MOVN			4178 #	4159	4160	4161	4162
	MOVNEG			4183 #	4178
	MOVPUT			8680 #
	MOVRJ			8698 #	8671
	MOVS			4172 #	4154	4155	4156	4157	4324	4328
	MOVS2			8674 #	8668
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-27
; 							Cross Reference Listing					

	MOVSTX			8736 #	8678
	MOVST1			8733 #	8670	8700	8739
	MOVST2			8743 #	8736
	MTRCI			10246 #	9597	9598	9599
	MTRCO			10243 #	9596
	MTRCO1			10250 #	10245
	MTRDBL			10300 #	10227	10290
	MTRINT			3893 #	4003	4959	5285	5300	5425	8158	8193	8327	8503
	MTRREQ			10263 #	3893
	MTRRQ0			10270 #	10266
	MTRRQ1			10288 #	10281	10282	10283	10284
	MUL			5701 #	5694	5695	5696	5697
	MUL1			5706 #	5707
	MULM			5743 #	5738	5740	5756	5757
	MULP			5726 #	5724	5734	5736	5752	5754	5762	5954	6894
	MULREE			5762 #	5963	6632	6647	6883
	MULSUB			5723 #	5681	5703	6137
	MUUO			5335 #	5210	5211	5212	5213	5214	5215	5216	5217	5218	5219	5220
				5221	5222	5223	5224	5225	5226	5227	5228	5229	5230	5231	5232
				5233	5234	5235	5236	5237	5238	5239	5240	5241	7256	7257	7258
				7259
	MUUOF			5372 #	5345	5373	5374	5375
	MVABT			8686 #	8681
	MVABT1			8688 #	8689
	MVABT2			8691 #	8688
	MVEND			8693 #	8744
	MVSK1			8707 #	8704
	MVSK3			8712 #	8709
	MVSK4			8739 #	8710
	MVSKP			8703 #	8698	8705	8707
	MVSO3			8673 #	8669
	MVST			8650 #	8561
	MVST2			8665 #	8663
	NEGADJ			7938 #	7947
	NEXT			3879 #	4096	4118
	NO.CST			10705 #	10672
	NOCST0			10708 #	10703
	NOCST1			10710 #	10708
	NODIVD			5794 #	6019	7895
	NOP			4109 #	4099	4694	4760	4861	4890	4914	4918	5120	5353	5518	5548
				5605	5794	7219	7220	7282	7286	7483	7573	7623	7866	8803	8970
				9639	9641	9650	10392
	NOP2			4113 #
	NOT.WR			10821 #	10815
	NOTWR			10797 #	10772	10818
	NOUPDT			10812 #	10759
	NXTOWG			8112 #	8114	8118	8122	8126	8130
	NXTWRD			8280 #	8276
	ODHIGH			7823 #	7807
	ODLOW			7810 #	7805
	OFSHFT			8045 #	8043
	OPDISP			9311 #	9309
	ORCA			4519 #	4512	4513	4514	4515
	ORCB			4549 #	4542	4543	4544	4545
	ORCM			4539 #	4532	4533	4534	4535
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-28
; 							Cross Reference Listing					

	OVER6			8118 #	8102
	OVER7			8122 #	8107
	OVER8			8126 #	8105
	OVER9			8130 #	8109
	OVTEST			7216 #
	OVTST1			7214 #
	OVTST2			7220 #	7216	7218
	OWGCOM			8040 #	8020	8022	8023	8026	8030	8034	8035	8038
	OWGDPB			7804 #	7761	7765
	OWGHIG			7716 #	7700
	OWGINC			8099 #	8067
	OWGLDB			7699 #	7673	7677
	OWGLOD			7688 #	7705	7706	7707	7708	7709	7710	7719	7720	7721	7722	7723
				7737	7738
	OWGLOW			7703 #	7699
	OWLCPY			7999 #	7883
	OWLINC			8083 #	8078
	PAGBI			10138 #	9556
	PAGBO			10097 #	9558
	PAGCI			10164 #	9561	9562	9563
	PAGCO			10160 #	9560
	PAGD2			10124 #	10113
	PAGDI			10130 #	9557
	PAGDO			10101 #	9559
	PCA			4920 #	4833	5279
	PCTXT			10147 #	5347	10136
	PF1			10532 #	10529
	PF2			10541 #	10539
	PF24			3967 #	8212	8311	8312	8313	8314
	PF4			10572 #	10550	10566
	PFPAR			10560 #	10543	10548
	PFPAR1			10562 #	10560
	PFPAR2			10563 #	4011
	PFT			10873 #	10630	10647	10685	10693	10709	10720	10746	10783	10797
	PGF1			10968 #	10893
	PGF2			10973 #	10968
	PGF4			10990 #	10973
	PGFAC0			11055 #	7472	9375	11098
	PGRF1			10582 #	10598
	PGRF2			10616 #	10583
	PGRF3			10622 #	10617	10619
	PGRF5			10669 #	10632
	PGRF6			10688 #	10684
	PGRST1			10849 #	10842	10851
	PGRST2			10853 #	10850
	PGRST3			10866 #	10854
	PHYS1			10440 #	10397
	PHYS2			10445 #	10373
	PHYS3			10450 #	10419
	PIBI			9794 #	9545
	PIBO			9796 #	9547
	PIBPA			10479 #	10463	10473
	PIBYTE			10463 #	10345
	PICI			9819 #	9550	9551	9552
	PICO			9815 #	9549
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-29
; 							Cross Reference Listing					

	PICOM1			9829 #	9816	9819
	PICOM2			9826 #	9817
	PICYC1			10334 #	3894	3896
	PICYC2			10354 #	3880
	PIDATI			10404 #	10344
	PIDATO			10388 #	10343
	PIDI			9782 #	9546
	PIDISP			10339 #
	PIDO			9781 #	9548
	PIDONE			10391 #	10382	10383	10411	10414	10415	10421	10451	10452	10477	11045
	PIDPB			10472 #	10465
	PIFET			10392 #	9651	10268
	PIIBP			10482 #	10467	10474
	PIINCR			10381 #	10342
	PIINST			10358 #	10279	10339	10340	10354
	PILD			10367 #	10388	10429	10432	10482
	PIOUT			10390 #	10399	10441	10442	10469
	PIST			10414 #	10407
	PIVECT			10356 #	10341
	PMTR1			10282 #	10274
	POP			5069 #	5005
	POP2			5091 #	5069
	POP3			5095 #	5093
	POPJ			5071 #	5006
	POPJ2			5081 #	5088
	POPJ3			5082 #	5089
	POPJ4			5083 #	5081
	POPJT			5088 #	5076
	POSADJ			7921 #	7918
	PSTOR			10419 #	10410
	PTLOOP			10190 #	10182	10192
	PUSH			5023 #	5004
	PUSHJ			5025 #	5003
	PUTDST			9214 #	8680	8737	8752	9050	9469	9520
	PXCT			5427 #	5424
	RDEBRG			4987 #	9794
	RDEMTR			10309 #	10301	10303
	RDEX			10423 #	10389	10408
	RDMTR			10215 #	9580	9581	9592	9593
	RDMTR1			10226 #	10220	10221	10222	10223
	RDMTR2			10313 #	10310
	RDUMTR			10311 #	10305	10307
	RELEB			9683 #	9680
	RELEEB			9635 #	9735	9742	9774	9775	10106	10139	10240	10250
	RELMEM			7791 #	7811	7819	7825	7832	7838
	RESETP			9203 #	9196	9252
	RET1			9717 #	8977
	RET2			8627 #	8650
	ROT			5534 #	5499
	ROT3			5558 #	5561
	ROT4			5563 #	5559	5566
	ROTC			5555 #	5503
	ROTS			5299 #	3882	10991
	RSTF			4854 #
	RSTF0			4852 #	4849
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-30
; 							Cross Reference Listing					

	SECIMM			10632 #	10662
	SECPTR			10627 #	10622	10646
	SETCA			4508 #	4501	4502	4503	4504
	SETCM			4529 #	4522	4523	4524	4525
	SETEBR			10179 #	10162
	SETFLG			9522 #	8942	9519
	SETO			4559 #	4552	4553	4554	4555
	SETPC			5353 #	3822
	SETZ			4413 #	4406	4407	4408	4409	7704	7712	7718	7725	7731
	SEVEN9			8038 #	8025
	SFET1			9335 #	8746	9333
	SGNEXT			7204 #	6829	6832	6879	6886	6914	6950	7307	7446
	SHFLOD			4095 #	7687	7689
	SHIFT			8350 #	4936	5610	5979	6858	6904	8342	8380	8405	10179	10363
	SHR1			5541 #	5514	5545
	SHR2			5546 #	5542	5547
	SIZE8D			7844 #	7820	7821	7822	7823
	SIZE8L			7737 #	7713	7714	7715	7716
	SIZE9D			7845 #	7833	7834	7835	7836
	SIZE9L			7738 #	7726	7727	7728	7729
	SKIP			4712 #	4701	4702	4703	4704	4705	4706	4707	4708	4728	4743
	SMALDV			7930 #	7924
	SN1			6449 #	6442	6455
	SNARFP			8093 #	8084
	SNATCH			8051 #	7970
	SNORM			6412 #	6110	6138	6179	6190	6333	6414	6416	6447	6465	7310
	SNR2			6415 #	6318
	SNZERO			6462 #	6413
	SOJ			4790 #	4779	4780	4781	4782	4783	4784	4785	4786
	SOS			4743 #	4732	4733	4734	4735	4736	4737	4738	4739
	SRCMOD			9091 #	8677	8866	9146	9148	9149
	SRND2			6498 #	6478
	SRND3			6501 #
	SRND4			6502 #	6497
	SRND5			6512 #	6463
	SROUND			6478 #	6417	6419	6421	6423	6450	6452	6453
	ST0			4089 #	4172	4181	4313	4315	4318	4321	4325	4329	4338	4339	4345
				4346	4352	4353	4359	4360	4413	4423	4433	4452	4468	4478	4488
				4498	4508	4519	4529	4539	4549	4559	5644	5657	5706	5797	5799
				5804
	ST2AC			4091 #	4092	5632	5943	5945	6033	6035	7424
	ST6			4097 #	4990	5689	6512	6516
	STAC			4096 #	4108	4193	4194	4666	4667	4668	5083	5123	5146	5412	6388
				8052	8693
	STAC1			4134 #	6526
	STAC4			8899 #
	START			3822 #	4968
	STBOTH			4101 #
	STCST			10787 #	10779
	STD1			4125 #	4091	4381	6741	7090
	STDAC			5567 #	4377
	STMAC			5038 #	4211	5023	5095	5416	5418
	STMEM			4099 #	4399	7792	9750	9802
	STOR34			8894 #	8885	8891
	STORAC			4108 #	4101	4775	4791	4799	4996	5034	5630	6386	7274	7991	7999
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-31
; 							Cross Reference Listing					

				9335
	STORTG			8090 #	8088
	STR2WD			8634 #	8600	8604	8617
	STRAC1			6526 #	5529	5567	5582	7478	9032
	STRAC3			5980 #
	STRNC			6497 #	6482
	STRPF			11091 #	10932	10936
	STRPF1			11093 #	11100
	STRPF2			11098 #	8713	11082	11093
	STRPF3			11096 #	11089
	STRPF4			11100 #	10944	10958
	STSELF			4107 #	4094	4713	5039
	SUB			5656 #	5649	5650	5651	5652
	SWEEP			9726 #	9567	9568	9569	9570	9571	9572	9573	9574
	TAKINT			4003 #	4997	9692	10972
	TDC			4667 #
	TDN			4665 #	4455	4456	4564	4565	4573	4574	4656	4659
	TDO			4668 #
	TDX			4655 #	4568	4577	4582	4586	4591	4595	4600	4604	4609	4613	4618
				4622	4627	4631
	TDXX			4663 #	4566	4570	4575	4579	4584	4588	4593	4597	4602	4606	4611
				4615	4620	4624	4629	4633
	TDZ			4666 #
	TIMBO			10255 #	9582
	TIMBO1			10239 #	10259
	TIMCI			10234 #	9585	9586	9587
	TIMCO			10232 #	9584
	TIMCO1			10106 #	10233
	TIMDO			10253 #	9583
	TMTR1			10281 #	10273
	TRAP			3926 #
	TRAPX			3932 #	3926	3928
	TRNABT			9121 #	9125
	TRNAR			9107 #	9465
	TRNFNC			9119 #	9109	9111	9122	9123	9124	9127
	TRNNS1			9145 #	9135
	TRNNS2			9147 #	9145
	TRNRET			9130 #	9120
	TRNSIG			9124 #	9126
	TRNSS			9135 #	9131
	TRNSS1			9151 #	9136
	TST2WD			8626 #	8599	8603	8616
	TSX			4658 #	4569	4578	4583	4587	4592	4596	4601	4605	4610	4614	4619
				4623	4628	4632
	TSXX			4661 #	4567	4571	4576	4580	4585	4589	4594	4598	4603	4607	4612
				4616	4621	4625	4630	4634
	TWGCPY			7991 #	8000
	TWGDUN			7984 #	7982
	UNHALT			4967 #	4961
	UP			7558 #	7547
	UP1			7564 #	7558	7566
	UP1PX			7614 #	7600	7616
	UP2			7568 #	7563	7564	7565
	UP2PX			7618 #	7614	7615
	UP3			7570 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-32
; 							Cross Reference Listing					

	UP3PX			7620 #
	UP4			7573 #	7570
	UP4PX			7623 #	7620
	UPPX			7600 #
	UUO			5334 #	4816	4828	4830	4838	4839	4852	4857	4879	4947	4993	5159
				5245	5246	5392	5505	6069	6276	6277	8028	8414	8465	8469	8571
				9428	9437	9594	9595	9668	9711	9720	9782	9829	10138	10253
	UUO107			4946 #
	UUOC1			5358 #	5356
	UUOC2			5365 #	5277	5341
	UUOCOM			5356 #	5271	5336
	UVERS			4140 #	9747
	UXCT			5428 #	4967
	WGRANT			9691 #	9669	9695	9830
	WGRNT1			9694 #	9691
	WGRNT2			9696 #	9694
	WRFAIL			10818 #	10821
	WRHPT			10822 #	10809	10816
	WXFER			9681 #	9677
	XBLT			7457 #	7248
	XBLT3			7461 #	7459
	XBLT4			7483 #	7461
	XBLTDN			7467 #	7475
	XBLTD1			7474 #	7471
	XBLTPF			11060 #	7498	10953
	XBLTUP			7489 #	7497
	XBLTU1			7495 #	7485
	XBLTX			7478 #	7491
	XCT			4979 #	4974
	XCT1			5424 #	4979
	XCTGO			3924 #	3927	3938	5428
	XCTW			3938 #	3825	5286	10358	10359	10361	10364
	XFERW			5046 #	5031	5276	5935	6002	6543	6759	7398	7980	9517	10151	10169
				10314	10367	10368	10446	10447
	XHLLI			4198 #	4222
	XJRSTF1			4892 #	4819
	XJRSTF2			4913 #	4907	4910
	XJRSTF3			4916 #	4913
	XLATE			9105 #	9095
	XMOVEI			4191 #	4438
	XMOVEI1			4193 #	4199
	XOR			4468 #	4461	4462	4463	4464
	XPCW1			4879 #	4821
	XPCW2			4902 #	4888
	XSFM1			4865 #	4832
	XSFM2			4890 #	4868
(D) J				2288 #
(U) KLPAGE			1944 #
	OPTIONS			1946 #	9749
(U) LONGPC			1951 #
	OPTIONS			1953 #	9749
(U) MACRO%
	A INDRCT		2315 #	3953	3955	3957	3959	3977	3979
	A READ			2316 #	3962	3963	3964	3965	3975	3976
	ABORT INSTR		2321 #	10288	10994
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-33
; 							Cross Reference Listing					

	AC0			2322 #	5073	5091	5407	7768	7804	7806	7913
	AC0_AR			2323 #	4091	4096	4124	4381	5124	5548	5567	5582	5966	6576	6588
				6740	6796	6830	7088	7220	7474	7476	7490	7492	7554	7916	7919
				8818	8884	9003	9005	9032	11055	11086	11095
	AC1_AR			2324 #	4134	5518	5973	6020	6021	6596	6667	6669	6797	6803	6920
				6922	7984	8611	8620	11060	11087
	AC2_AR			2325 #	5977	6031	7478	8602	8619	11061
	AC3_AR			2326 #	5960	5980	6033	6034	8688	8712	8744	8814	8859	8860	8894
				8931	8943	8974	11077	11093	11096
	AC4			2328 #	8603	8609	8849	8899	11082
	AC4_AR			2332 #	8609	8899	11082
	AC5			2334 #	8607
	AC5_AR			2338 #	8607
	AD FLAGS		2339 #	4183	4379	4395	4728	4743	4775	4791	5644	5657	5943	5945
	AD LONG			2340 #	6034	6388	8936
	ADMSK			2341 #	3890	4865	4880	5027	5118	5123	5272	5353	5358	5362	5366
				8636	8637	10337	10373	10397	10419	10434	10455	11006
	AR+ARX+MQ_0.M		2578 #	6462	7424	8734	9091
	AR+MQ_0.M		2579 #	8868
	AR+MQ_0.S		2580 #	7009
	AR0-3 DISP		2582 #	7673	7677	7761	7765	7881	8067	9108	10388	10407	10629	10718
	AR0-8_#			2583 #	4140	5373	5374	5375	6817	6871	6925	7061	7279	7281	7287
				7320	7401	7414	8661	8844	8966	9749	10113	10125	10207	10753
	AR0-8_# AND AR0-8	2584 #
	AR0-8_# OR AR0-8	2585 #	10821
	AR0-8_#+SC		2586 #	8467
	AR0-8_-SC-1		2587 #
	AR0-8_FE		2588 #	8666	8851	10575
	AR0-8_FE OR #		2589 #	8852
	AR0-8_FE OR SC		2590 #	10597
	AR0-8_FE#		2591 #	8849
	AR0-8_FE+#		2592 #	10360	10424	10479
	AR0-8_FE+1		2593 #
	AR0-8_FE+SC		2594 #
	AR0-8_FE-SC		2595 #
	AR0-8_FE.M		2596 #	8971
	AR0-8_FE.R		2597 #
	AR0-8_SC		2598 #	8379	8403	10576
	AR0-8_SCAD		2599 #	8379	8403	8666	8851	10575	10576
	AR0-8_SCAD#		2600 #	8467	8852	10360	10424	10479	10821
	AR0-8_SCAD.M		2601 #	8971	10597
	AR0-8_SCAD.R		2602 #
	AR12-17_PC SEC		2603 #	5137	5142
	AR12-17_PREV SEC	2604 #	4866	4883	5360	10148	11009
	AR18-21 DISP		2605 #	9110
	ARL+ARX+MQ_0.M		2629 #
	ARL+ARX_0.M		2630 #	10226	10289
	ARL_0S			2610 #	4338	4352
	ARL_0.C			2607 #	9654
	ARL_0.M			2608 #	5140	5141	8510	9048	9051	9284	9362	9388	9417	9488	10632
				10633	10634	10725	10726	10741
	ARL_0.S			2609 #	5074	5077	10250
	ARL_1S			2612 #	4339	4353	8518
	ARL_1S.M		2613 #
	ARL_1.M			2611 #	4193	5356
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-34
; 							Cross Reference Listing					

	ARL_AC0			2614 #	4313	4321	4328
	ARL_ARL			2615 #	4214	4315	4329	4359	4360	5363	7973	9774	9825
	ARL_ARL.M		2616 #	10101
	ARL_ARL.S		2617 #	4199	7917
	ARL_ARR			2618 #	4318	4324	4325	4345	4346	5406	7552	7606	9632	11054
	ARL_ARR.M		2619 #	9767	9815	10160	10243
	ARL_ARR.S		2620 #	9748
	ARL_ARX (ADX)		2621 #	10577
	ARL_ARXL		2622 #	4867	5146	7977	7983	10240	11010
	ARL_ARXL.M		2623 #
	ARL_BRL			2624 #	4884	8089	10139
	ARL_BRL.M		2625 #	7534
	ARL_BRL.S		2626 #	5349
	ARL_SHIFT		2627 #	10722
	ARL_SIGN		2628 #	5411
	ARR+MQ_0.S		2650 #	7917	7920
	ARR_0S			2635 #	4345	4359
	ARR_0.C			2632 #
	ARR_0.M			2633 #	10173
	ARR_0.S			2634 #	5524
	ARR_1S			2636 #	4346	4360
	ARR_AC0			2637 #	4214	4315	4318	4324
	ARR_AC0.S		2638 #	4199
	ARR_AR+1		2639 #	8083	9184
	ARR_AR+BR		2640 #	7977
	ARR_ARL			2641 #	4321	4328	4329	4352	4353	5140	5141	7534	10101	10577	10582
	ARR_ARR			2642 #	4193	4313	4325	4338	4339	4867	4884	5406	5411	9632	9767
				9815	10160	10243	11010
	ARR_ARX			2643 #
	ARR_ARX+1		2644 #
	ARR_ARX+BRX		2645 #
	ARR_ARX+BR		2646 #	7973	7983
	ARR_ARX-1		2647 #
	ARR_BR			2648 #
	ARR_PC+1		2649 #	5146
	ARX+MQ_0.M		2756 #	5519	5779	5784	6316	8918
	ARX+MQ_0.S		2757 #	6162
	ARX0-3 DISP		2760 #	10289
	ARX0_AR35		2758 #
	ARX0_MQ35		2759 #	8795
	ARX_-AC0		2655 #
	ARX_-BRX		2656 #	7919	7938
	ARX_-FM[]		2657 #
	ARX_-SLEN		2658 #	11078
	ARX_-2+MQ0		2654 #	7888
	ARX_0S			2662 #	4935	5617	5723	5947	6001	6083	6085	6133	6191	6596	6630
				6727	6882	7033	7862	7882	7915	8023	8026	8035	8038	8800	8848
				8853	9030	10160	10171	10206
	ARX_0.C			2659 #
	ARX_0.M			2660 #	5513	6413	7323	9044
	ARX_0.S			2661 #
	ARX_1B1			2664 #	6350
	ARX_1B17-1		2665 #	5346	10135
	ARX_1S			2666 #	3881	6481	6902	8029	8668	8859	9273
	ARX_1			2663 #	5406	6733	7087	8033	8039
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-35
; 							Cross Reference Listing					

	ARX_2+MQ0		2667 #	7036	7881	8019	8021
	ARX_AC0			2669 #	5534	8929	9274
	ARX_AC0 COMP		2670 #
	ARX_AC0+1		2671 #	5023	5028
	ARX_AC1			2672 #	4393	4394	5555	5571	5989	6597	7430
	ARX_AC2			2673 #	7248
	ARX_AC3			2674 #	5931	5975	9047
	ARX_AC4			2675 #	8849
	ARX_AR			2677 #	4211	4213	4958	5589	5591	5595	5609	5796	5798	5809	5937
				6101	6104	6332	6367	6568	6570	6573	6676	6836	6838	6840	6854
				6932	6992	7057	7072	7074	7276	7314	7411	7425	7528	7667	7670
				7686	7755	7758	7981	8042	8044	8086	8093	8343	8380	8404	8411
				8662	8665	8698	8736	8770	8796	8814	8816	8868	8927	8993	9035
				9108	9182	9188	9199	9240	9243	9258	9261	9290	9362	9388	9395
				9488	9823	10124	10126	10154	10255	10363	10405	10425	10465	10484	10751
				10990	10992	11094
	ARX_AR (AD)		2678 #	5428	5543	5558	5560	5565	5576	7916	7975	9195	9251	10101
				10532	10533	10576	10758
	ARX_AR ANDCA BR		2680 #	8635
	ARX_AR AND ADMSK	2679 #	10434
	ARX_AR SIGN		2681 #	6354
	ARX_AR SWAP		2682 #	4906	4909	5129	5362	7931	9048	9110	9773	9821	10168	10237
				10270	10582	10767	10812
	ARX_AR*2		2683 #	7925
	ARX_AR*4 COMP		2684 #	8964
	ARX_AR*MSK		2685 #	9130
	ARX_AR+1		2686 #	8969
	ARX_AR+CBR		2688 #
	ARX_AR+FM[]		2690 #
	ARX_AR-1		2691 #	5091	8774	8818	8819
	ARX_AR-BR		2692 #
	ARX_AR-FM[]		2693 #
	ARX_AR-FM[]-1		2694 #
	ARX_ARX AND ADMSK	2696 #	10455
	ARX_ARX ANDC ADMSK	2697 #	4865
	ARX_ARX*-6		2698 #	5525
	ARX_ARX*.25		2699 #	6178
	ARX_ARX*.5		2700 #
	ARX_ARX*2		2701 #	4401	5936	6378	6544	6760	7303	7305	7399	7443	8855	9302
				10315
	ARX_ARX*2 COMP		2702 #	10723
	ARX_ARX*4		2703 #	7037	9300
	ARX_ARX*4 COMP		2704 #
	ARX_ARX*8		2705 #	5985	10147	10148
	ARX_ARX*BRX		2706 #	6503	6742	7091
	ARX_ARX*EXPMSK		2707 #
	ARX_ARX+1		2708 #	5528	7574	7624	7886	7891
	ARX_ARX+AC0		2709 #
	ARX_ARX+CBR		2711 #
	ARX_ARX+FM[]		2713 #	7913
	ARX_ARX-1		2714 #	5523	7462	7474	7989
	ARX_ARX-1 (AD)		2715 #	8703
	ARX_ARX-AR*4		2716 #	8041
	ARX_ARX-FM[]		2717 #
	ARX_ARX-FM[]-1		2718 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-36
; 							Cross Reference Listing					

	ARX_BR			2720 #	5599	5994	10643
	ARX_BR*2		2721 #	5990
	ARX_BR+1		2722 #	7490
	ARX_BRX			2723 #	4934	5269	5335	5613	5631	6582	6611	6740	7089	8608	8813
				10406
	ARX_BRX COMP		2724 #	6711	6714	7023	7026	7947
	ARX_BRX+1		2725 #	7492	8670	8700	8739
	ARX_DSTP		2726 #	8829	9214
	ARX_DSTP2		2727 #	9242	9436
	ARX_E1			2728 #
	ARX_FILL		2729 #	8604	8605
	ARX_FM			2730 #	8604	8605	8798	8829	8990	9160	9187	9214	9242	9436	9462
				10719	10842	10850	10854	10856	10921
	ARX_FM(VMA)		2733 #	3927
	ARX_FM[]		2731 #	7804	7806
	ARX_FM[]+1		2732 #
	ARX_MEM			2734 #	3938	4002	4003	4377	4378	5046	5075	5078	5339	7300	7786
				8157	8158	8192	8193	8326	8327	8362	8502	8503
	ARX_MQ-1		2735 #
	ARX_MQ-FM[]		2736 #
	ARX_MQ-FM[]-1		2737 #
	ARX_PC			2739 #	10578	11004
	ARX_PC+1		2740 #	4821	4832	5365
	ARX_SHIFT		2741 #	5299	5563	5579	5594	5625	5816	5819	5974	5978	5986	5992
				6003	6103	6381	6387	6445	6842	6849	7059	7688	7788	7790	7921
				7923	8045	8342	8369	8372	8920	10172	10300	10302	10304	10306	10454
				10632	10642	10726
	ARX_SRCP		2742 #	8798	9160	9462
	ARX_SRCP2		2743 #	9187
	ARX_SV.AR		2745 #	10850
	ARX_SV.ARX		2746 #	10854	10856	10921
	ARX_SV.BR		2747 #	10842
	ARX_SV.VMA		2748 #	10719
	ARX_T0			2750 #
	ARX_T2			2751 #	8990
	ARX_VMA HELD		2755 #	10578
	AR_(AR+2BR)*.25		2346 #	5753	6109
	AR_(AR+BR)*.25		2347 #	5735	5751
	AR_(AR-2BR)*.25		2348 #	5737
	AR_(AR-BR)*.25		2349 #	5739	5755
	AR_(ARX OR AR*4)*.25	2350 #
	AR_-AC0			2351 #
	AR_-AR			2352 #	6086	6516
	AR_-AR LONG		2353 #	5841	5844	6262	6549	6743	6766	6939	8937
	AR_-BR			2354 #	4183	5851	5861	6034	6716	6787	7028	8933	9000
	AR_-BR LONG		2355 #	4379	4395	5996	5999	6030	8892
	AR_-BR*2 LONG		2356 #
	AR_-BRX			2357 #	5796
	AR_-DLEN		2358 #
	AR_-FM[]		2359 #
	AR_-SLEN		2360 #	8686	11091
	AR_0.C			2361 #	4832	6902
	AR_0.M			2362 #	5576	8708	8969
	AR_0.S			2363 #	4882	5359	11008
	AR_0S			2364 #	4345	4359	4381	4398	4413	4950	4987	5344	5533	5723	5947
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-37
; 							Cross Reference Listing					

				6386	6478	6630	6727	6773	6870	6882	7033	7686	7688	7930	7931
				8342	8343	8665	8736	8848	8853	9030	9035	9401	10097	10112	10154
				10164	10168	10206	10209	10217	10234	10237	10246	10255	10270	10293	10300
				10302	10304	10306	10356	10363	10425	10431	10574	10590	11014	11077
	AR_1			2365 #	5783	8857	10768
	AR_1 LONG		2366 #	8939
	AR_1S			2367 #	4346	4360	4559	8040	8673	8860	8963
	AR_2			2368 #	9516
	AR_2(AR*BR)		2369 #	5839	5842	5847	5856	5872	5873	5874	5875	5903	5904	5905
				5906	6014	6016	7927	8879
	AR_2(AR+1)		2370 #	5527
	AR_2(AR+BR)		2371 #	5842	5847	5872	5875	5903	5906	6016
	AR_2(AR+BR) LONG	2372 #	8879
	AR_2(AR-BR)		2373 #	5839	5856	5873	5874	5904	5905	6014	7927
	AR_AC0			2375 #	4036	4211	4214	4315	4318	4324	4760	4790	5069	5071	5129
				5139	5513	5516	5534	5556	5572	5589	5591	5656	5679	5702	5785
				5803	5809	5937	5961	5992	6092	6135	6165	6317	6568	6591	6599
				6600	6640	6676	6775	6792	6829	6854	6874	6885	6932	6992	7419
				7444	7459	7529	8552	8560	8653	8770	8920	8993
	AR_AC0 COMP		2376 #	8843
	AR_AC0+1		2377 #	4775	4799	7471	7472
	AR_AC1			2378 #	6026	6027	6625	6626	6672	6846	6852	6853	6881	6928	7457
				8599	8616	8918
	AR_AC1 COMP		2379 #
	AR_AC1*2		2380 #	5554	5780	5933	6547	6660	6662	6807	6868	6917	6918	6991
	AR_AC2			2381 #	5987
	AR_AC3			2382 #	8554	8568	8652	8856	8927	8941	8965	9417	11088
	AR_AC3*2		2383 #
	AR_AC4			2384 #	8603
	AR_ADMSK AND VMA HEL	2386 #
	AR_AD*.25 LONG		2385 #	5841	5844	6262	6549	6743	6766	6939	8937	9034
	AR_ARX			2442 #	3924	4005	4849	5034	5039	5041	5363	5368	5567	5582	5613
				5617	5973	5985	5990	6176	6412	6585	6588	6596	6703	6796	6801
				7015	7291	7322	7476	7478	7914	7975	8090	8209	8328	8471	8504
				8508	8511	8513	8516	8744	8800	8859	8931	8952	8954	8978	9051
				9152	9190	9195	9251	9284	9285	9286	9311	9315	9344	9363	9393
				9441	9449	9503	9504	9505	9506	9774	10103	10257	10423	10532	10533
				10548	10579	10598	10640	10671	10680	10758	10763	10822	10850	10853	10855
				11060	11082	11086
	AR_ARX (ADX)		2444 #	5543	5559	5560	5566	5574	5581	6384	6443	6576	6589	6704
				7016	7921
	AR_ARX (AD)		2443 #	3953	3957	3962	3964	6849	6858	8404	8814	8816	10813	11054
	AR_ARX ANDC ADMSK	2446 #	4880	5272	5358	11006
	AR_ARX AND ADMSK	2445 #	5366
	AR_ARX COMP		2447 #	8664
	AR_ARX OR PUR		2449 #
	AR_ARX*.25		2451 #	6178	10751
	AR_ARX*.25-AR-1		2452 #	8967
	AR_ARX*2		2453 #
	AR_ARX*4		2454 #
	AR_ARX*4 COMP		2455 #
	AR_ARX*AC1		2456 #
	AR_ARX*BR		2457 #	7086	10149
	AR_ARX*BRX		2458 #	4887	8712	9825	11012
	AR_ARX*E1		2459 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-38
; 							Cross Reference Listing					

	AR_ARX+1		2460 #
	AR_ARX+1 (AD)		2461 #	8558	8570	9245
	AR_ARX+AC0		2462 #
	AR_ARX+AR*4		2463 #
	AR_ARX+BR		2464 #	7069	7973	7983	8051	11052
	AR_ARX+BRX		2465 #
	AR_ARX+BRX+1		2466 #	7893
	AR_ARX+FM[]		2467 #
	AR_ARX+XR		2468 #	3955	3959	3963	3965
	AR_ARX-1		2469 #	5529	8774	8818	8819
	AR_ARX-AC3		2470 #
	AR_ARX-BR		2471 #	11095
	AR_AR AND ADMSK		2388 #	5027	5118	5123
	AR_AR AND CSMSK		2390 #	10692	10772	10787
	AR_AR OR PUR		2391 #	10694	10809
	AR_AR SWAP		2393 #	4172	4658	4661	4931	6081	6132	6161	7422	7553	7607	8412
				10174	10258	10842
	AR_AR*.25		2395 #	7318
	AR_AR*.25 LONG		2396 #	5734	5757	6190	6415	6464	6693	6695	7004	7006	7050	7308
	AR_AR*.5		2397 #	6501	7317
	AR_AR*.5 LONG		2398 #	6697	6731	7008	7039	7053	8406	9095	9464
	AR_AR*1.25 LONG		2399 #	9034
	AR_AR*10		2400 #	8858
	AR_AR*10 LONG		2401 #	8951	9056
	AR_AR*2			2402 #	3890	10265
	AR_AR*2 LONG		2403 #	6418
	AR_AR*4			2404 #	10341	10345	10389	10408
	AR_AR*4 LONG		2405 #	6420
	AR_AR*5 LONG		2406 #	8878
	AR_AR*8			2407 #	6790	6799	7290	9296	9304	9339
	AR_AR*8 LONG		2408 #	6422	6441	6454
	AR_AR*AC0		2409 #	4423	4433	4452	4468	4478	4488	4498	4508	4519	4539	4549
				4666	4667	4668	5409	5412	5644
	AR_AR*AC1		2410 #
	AR_AR*BR		2411 #	4939	5283	5361	7062	8930	10150	10781
	AR_AR*EXPMSK		2412 #
	AR_AR*MSK		2413 #	9132
	AR_AR*SFLGS		2414 #	8693	8883	11094	11098
	AR_AR*SLEN		2415 #	11089
	AR_AR*T0		2416 #	8810	11084
	AR_AR+1			2418 #	3940	4728	6698	6710	7022	8083	8087	8113	8118	8122	8126
				8130	8280	8705	8994	9184	9193	9241	9249	9332	9367	9369	9493
				9655	10351	10382
	AR_AR+1 LONG		2419 #	8994
	AR_AR+1-AR0		2420 #	9189	9244
	AR_AR+BR		2421 #	5876	5879	5889	5943	5955	6388	6498	6592	6639	6855	6876
				6884	7038	7321	7431	7552	7606	7977	7981	8692	10190	10196	10228
				10291	10433	11093
	AR_AR+BR LONG		2422 #	5889	5943	5955	6639	6884	10228	10291
	AR_AR+E1		2423 #	9042	9096
	AR_AR+FM[]		2424 #	7939
	AR_AR+SBR		2426 #
	AR_AR+T0		2428 #
	AR_AR+T1		2429 #
	AR_AR+XR		2434 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-39
; 							Cross Reference Listing					

	AR_AR-1			2435 #	4743	4791	5072	9145	10383
	AR_AR-BR		2436 #	5657	5877	5878	5892	5945	6873	7289	7535	7536	8662	8689
	AR_AR-BR LONG		2437 #	5892	5945
	AR_AR-BR-1		2438 #
	AR_AR-FM[]		2439 #
	AR_AR-T0		2440 #
	AR_BRX			2486 #	5352	5619	5798	5994	7984	8045	8466	9445	9494	10407	10410
				10475
	AR_BRX+1		2487 #	7490
	AR_BR			2473 #	5083	5609	5622	5805	5974	5978	6033	6069	6367	6379	6497
				6582	6611	6705	6784	6788	6872	6914	7017	7068	7283	7288	7402
				7415	7421	8028	8277	8278	8465	8469	8571	8797	8813	9427	9447
				9668	9711	9720	9748	9782	9829	10138	10215	10253	10573	10595	10641
				11045
	AR_BR COMP		2474 #	6711	6714	7023	7026	8663
	AR_BR COMP LONG		2475 #	5998
	AR_BR LONG		2476 #	6004	6006	6029	6843	6905	7498	8862	8875	8884	8890	8997
				9028	9037	9053	10901	10928	10953
	AR_BR OR ARX		2477 #	8602	8607	8619
	AR_BR*.5		2478 #
	AR_BR*.5 LONG		2479 #	8943
	AR_BR*2			2480 #	6094
	AR_BR*2 LONG		2481 #	5607	6449
	AR_BR*4			2482 #
	AR_BR*4 LONG		2483 #	6451
	AR_BR+1			2484 #	7492
	AR_BR+1 LONG		2485 #
	AR_CACHE CNT		2489 #	10223	10276
	AR_DLEN			2490 #	8671	8678	8681	9135	9159	10932	10936	10944	10958
	AR_DLEN COMP		2491 #	10905
	AR_DLEN+1		2492 #	8753
	AR_DSTP			2493 #	8669	8829	9141	9214	9314	9323	11069
	AR_DSTP+1		2494 #
	AR_DSTP2		2495 #	9243	9509
	AR_DSTP2+1		2496 #
	AR_DSTW			2497 #
	AR_E0			2498 #
	AR_E1			2499 #
	AR_EBOX CNT		2500 #	10222	10275
	AR_EBUS			2501 #	3894	3896	9679
	AR_EBUS REG		2502 #	4989	10152	10170	10549	10564
	AR_FILL			2503 #	8752
	AR_FM[]			2505 #	11038
	AR_FM[]+1		2506 #
	AR_FM			2504 #	6003	6023	7966	8669	8671	8678	8681	8698	8746	8752	8798
				8829	8979	8991	9093	9109	9111	9135	9136	9141	9148	9159	9160
				9188	9214	9243	9314	9323	9462	9471	9509	9519	9521	10630	10647
				10685	10693	10709	10720	10746	10783	10797	10814	10836	10897	10932	10936
				10944	10948	10958	10973	11040	11069
	AR_FM(#)		2508 #
	AR_FM(VMA)		2510 #
	AR_INTERVAL		2511 #	10236
	AR_MEM			2513 #	4047	4069	4080	4904	4905	4908	4920	5046	5074	5077	5093
				5094	7272	7313	7396	7410	7467	7495	7563	7564	7614	7735	7786
				7811	7812	7813	7814	7815	7816	7817	7819	7825	7826	7827	7828
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-40
; 							Cross Reference Listing					

				7829	7830	7832	7838	7839	7840	7844	7845	8086	8337	8339	8362
				8365	8650	8775	8977	9046	9107	9291	9419	9434	9467	9472	9634
				10350	10381	10390	10627	10642	10662	10688	10710	10733	11016
	AR_MQ			2514 #	5563	5579	5907	5908	5909	5910	5956	5962	5977	6024	6387
				6595	6647	6667	6669	6890	6920	6922	6950	7214	7469	7788	7969
				8369	8801	9003	9005	9216	9453	10405
	AR_MQ COMP		2515 #	6687	7002
	AR_MQ*.25		2516 #	6445
	AR_MQ*2			2517 #
	AR_MQ*4			2518 #	10465
	AR_MQ*AC1		2519 #
	AR_MQ*AC2		2520 #
	AR_MQ*AC3		2521 #
	AR_MQ+1			2522 #	3879	7545	7599
	AR_MQ+AC0		2523 #
	AR_MQ+BR		2524 #
	AR_MQ-1			2525 #	7462	7474	7489
	AR_MQ-AC3		2526 #
	AR_MQ-BR		2527 #
	AR_MTR REQ		2528 #	10264
	AR_PC			2529 #	4937	5342	8401	10990	10992
	AR_PC FLAGS		2530 #	5362
	AR_PC+1			2531 #	5025	5114	5116
	AR_PERF CNT		2532 #	10221	10274
	AR_PERIOD		2533 #	10238
	AR_PUR+AR0		2535 #
	AR_SERIAL		2538 #	9746
	AR_SFLGS		2539 #	8746	9093	9109	9111	9136	9471	9519	9521
	AR_SHIFT		2540 #	4095	4125	4399	4935	5299	5349	5520	5522	5541	5546	5630
				5631	5689	5972	5975	5988	6032	6106	6587	6597	6701	6728	6740
				6847	7013	7034	7088	7216	7218	7427	7430	7790	7791	8350	8372
				8374	8674	8861	8895	9036	9119	9274	9292	9728	10139	10294	10426
				10643	10994	11078
	AR_SIGN			2541 #	4091	4124	5594	5595	5606	5690	5816	5819	5966	6031	6100
				6101	6333	6381	6570	6573	6838	6840	7057	7404	7411	7425	8042
				8044	8894
	AR_SLEN			2542 #	9148
	AR_SLEN COMP		2543 #	8691	8709	8870	8876	11100
	AR_SLEN+1		2544 #	8677	8865	9025	9146	9149
	AR_SRCP			2545 #	8698	8798	9160	9462	10897
	AR_SRCP+1		2546 #
	AR_SRCP2		2547 #	9188
	AR_SRCP2+1		2548 #
	AR_SV.AR		2549 #	11040
	AR_SV.ARX		2551 #
	AR_SV.BR		2552 #	10630	10647	10685	10693	10709	10720	10746	10783	10797
	AR_SV.PFW		2553 #	10814	10948	10973
	AR_SV.SC		2554 #	10836
	AR_SV.VMA		2555 #
	AR_SWD			2557 #
	AR_T0			2559 #	6003	7966	8979
	AR_T1			2560 #	6023	8991
	AR_T2			2561 #
	AR_TIME BASE		2562 #	10220	10273
	AR_VMA HELD		2573 #	8401	10990	10992
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-41
; 							Cross Reference Listing					

	AR_XR			2574 #	4850
	AR_[] AND FM[]		2345 #	8637
	B DISP			2764 #	4374	4656	4659	4663	5039	5545	5600	5620	5624	5938	6066
				6081	6544	6760	8581	8582	8667	8850	8858	9036	9094	9131	9135
				9654	10218	10226
	B WRITE			2765 #	4990	5689	6512	6516	9635	9826	10174
	BLKO TIM(L)		2766 #	10257	10259
	BLKO TIM(R)		2767 #	10258
	BR_AR LONG		2768 #	4402	5606	5723	5890	5893	5994	5996	5998	5999	6004	6006
				6029	6030	6379	6441	6454	6478	6481	6547	6552	6560	6902	6905
				6916	7009	8857	8865	8875	8939	8941	8997	9000	9024	10309	10311
	BYTE DISP		2769 #	4961	7668	7671	7755	7759	7968	8000	8068	8919	9175	9222
				9389	9434	9435	9436	9449	9489	9629	10192	10198	10778
	BYTE INDRCT		2773 #	8152	8154	8187	8189	8216	8218	8297	8299	8301	8303	8319
				8321
	BYTE LOAD		2774 #	8148	8150	8161	8163
	BYTE PREV & CLR SR3	2776 #
	BYTE PREV & SET SR2	2777 #
	BYTE PREV & SET SR3	2778 #
	BYTE READ		2780 #	8306	8307	8308	8309	8317	8318
	BYTE READ PC		2781 #
	BYTE RPW		2782 #	8182	8184	8196	8199
	CALL			2789 #	3882	4866	4883	4916	4936	4988	5031	5271	5276	5277	5336
				5341	5345	5347	5610	5681	5703	5962	5979	6002	6018	6024	6166
				6168	6568	6632	6647	6660	6662	6678	6759	6764	6768	6772	6775
				6782	6787	6793	6802	6829	6832	6844	6850	6875	6879	6883	6886
				6894	6904	6914	6917	6918	6921	6923	6933	6942	6950	7277	7307
				7398	7420	7446	7545	7563	7564	7565	7599	7614	7615	7668	7675
				7680	7682	7756	7865	7928	7930	7980	8157	8192	8405	8552	8553
				8560	8599	8600	8603	8604	8616	8617	8626	8628	8650	8651	8677
				8680	8699	8737	8745	8752	8799	8809	8830	8842	8866	8917	8942
				8963	8977	8978	8992	8999	9002	9050	9196	9214	9217	9252	9272
				9390	9401	9463	9465	9469	9490	9517	9520	9628	9634	9647	9669
				9726	9734	9736	9741	9745	9747	9770	9796	9819	10097	10113	10114
				10124	10131	10136	10151	10164	10169	10179	10180	10208	10215	10232	10234
				10246	10255	10265	10314	10363	10389	10390	10393	10404	10408	10409	10429
				10432	10440	10445	10450	10463	10464	10467	10468	10473	10474	10476	10482
				10991	11009	11044
	CALL []			2793 #	4866	4883	6759	6764	6768	6772	6775	6782	6787	6793	6802
				6829	6832	6844	6850	6875	6879	6883	6886	6894	6904	6914	6917
				6918	6921	6923	6933	6942	6950	7277	7307	7398	7420	7446	7668
				7675	7680	7682	7756	7865	7928	7930	7980	8157	8192	8405	8552
				8553	8560	8599	8600	8603	8604	8616	8617	8628	8650	8651	8842
				8917	9272	9647	10114	10180	10208	11009
	CALL.C			2796 #
	CALL.M			2797 #	5781	5785	5848	5857	5954	6668	6670	8734	9091	9174	9767
				9816	10102	10161	10227	10244	10290	10388	10483
	CALL.S			2798 #	4378	4394	5935	6137	6543
	CALL[]			2794 #
	CBR			2805 #	10669	10680	10757	10763
	CLR ACC+SET UCODE	2807 #
	CLR ACCOUNT EN		2808 #	3893	10529	10531
	CLR AR			2809 #	3881	6378	6481	6816	6867	6869	6924	7055	7278	7285	7314
				7319	7399	7412	7925	8668	8990	9273
	CLR ARX			2810 #	5619	5680	5701	5803	6379	6384	6444	6497	6498	6590	6704
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-42
; 							Cross Reference Listing					

				6850	6858	7016	8673	8857
	CLR EBUS DEMAND		2811 #	9680
	CLR EXP			2812 #	8928
	CLR FE			2813 #	4096	4118	4950	4967	6462	6785	6789	6924	6991	7427
	CLR FPD			2814 #	4112	7686	7688	7704	7712	7718	7725	7731	7732	7733	7791
				7839	7840	7866	8115	9030
	CLR MQ			2815 #	5931	6732	7313	7410	7431
	CLR MTR PA EN		2816 #
	CLR PT LINE		2818 #	10191
	CLR PT LINE (KEEP)	2819 #	10197
	CLR P			2817 #	4194	5357
	CLR SC			2820 #	4096	4118	4950	6020	6873	7705	7713	7719	7726	7811	7819
				7825	7832	7838
	CLR SPECIAL CYCLE	2821 #	10656
	CLR SR2			2822 #
	CLR SR3			2823 #	6599
	CLR TRACKS EN		2824 #
	CLR TRK+PA EN		2825 #
	CMS FETCH		2826 #	8803
	COMP FETCH		2827 #	4694
	CONI APR(L)		2828 #	9773
	CONI APR(R)		2829 #	9772
	CONI MTR		2830 #	10248
	CONI PAG		2831 #	10166
	CONI PI(L)		2832 #	9824
	CONI PI(PAR)		2833 #	9822
	CONI PI(R)		2834 #	9820
	CONO APR		2835 #	9769	9775
	CONO MTR		2836 #	10245
	CONO PAG		2837 #	10162
	CONO PI			2838 #	9817
	CONO TIM		2839 #	10233
	CONTINUE		2840 #	4961
	CSMSK			2842 #	10692	10772	10787
	DATAI APR(L)		2845 #	9742
	DATAI PAG(L)		2846 #	5346	10135
	DATAO APR		2847 #	9735
	DATAO PAG(L)		2848 #	10103
	DIAG IN			2849 #	4989	5346	9742	9772	9773	9820	9822	9824	10135	10152	10166
				10170	10220	10221	10222	10223	10236	10238	10248	10264	10273	10274	10275
				10276	10549	10564
	DIAG OUT		2850 #	9735	9769	9775	9817	10103	10162	10233	10245	10257	10258	10259
	DISMISS			2851 #	4858	4910	4911	9651	10391
	DIVIDE			2852 #	5839	5842	5847	5856	5872	5873	5874	5875	5876	5877	5878
				5879	5903	5904	5905	5906	7927
	DLEN			2853 #	8663	8664	8671	8678	8681	8710	8753	8755	9135	9147	9159
				10905	10932	10936	10944	10958
	DLEN_AR			2854 #	8663	8664	8710	8755	9147
	DROP EBUS REQ		2855 #	9692
	DSTP			2857 #	8669	8829	8829	9141	9214	9214	9240	9257	9258	9261	9314
				9323	9449	9450	11069	11075
	DSTP2_AR		2865 #	9251	9455
	DSTP2			2863 #	9242	9243	9251	9436	9455	9509
	DSTP_AR			2861 #	9257	9258	9261	9449	9450	11075
	DSTW			2866 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-43
; 							Cross Reference Listing					

	DSTW_AR			2867 #
	E0			2871 #	6760	6767	6795	6800	6878	6988	8466	9137	9400	9402	9517
	E0_AR			2872 #	8466
	E1			2873 #	6776	6785	6804	6845	6851	6870	6913	7021	7253	7254	7266
				7267	7268	7315	7400	7413	8508	8511	8513	8516	9041	9042	9094
				9095	9096	9105	9464	9465
	E1_AR			2874 #	8508	8511	8513	8516
	EA MOD DISP		2876 #	3925	4005	4815	4862	7675	7680	7763	7773	8159	8160	8194
				8195	8209	8210	8292	8328	8471	8504	9183	9199	9259	9262
	EPT FETCH		2881 #	10358	10359	10364
	EPT REF			2882 #	10310	10367	10414	10622
	EPT REF CACHE		2883 #	8408
	EXEC REF		2884 #
	EXIT			2885 #	4172	4181	4313	4315	4318	4321	4325	4329	4338	4339	4345
				4346	4352	4353	4359	4360	4413	4423	4433	4452	4468	4478	4488
				4498	4508	4519	4529	4539	4549	4559	5644	5657	5706	5797	5799
				5804
	EXIT DBL		2886 #	4092	5943	5945	6033	6035
	EXP TEST		2887 #
	EXP TST			2888 #	6502	6732
	EXPMSK			2889 #	3941	6994	6996	7060	7197	7198	7204	7205	7434	7437	8634
	EXP_-SC-1		2890 #
	EXP_-SC-1 TST		2891 #
	EXP_1			2892 #	6648
	EXP_FE TST		2893 #	6502	6732
	EXP_SC			2894 #	7316
	EXP_SC.MS		2895 #	7056
	EXP_SCAD		2896 #	6648	7316	8928
	EXP_SCAD.C		2897 #	6502	6732
	EXP_SCAD.MS		2898 #	7056
	EXP_SIGN		2899 #	6082	6085	6093	6133	6136	6162	6261	6318	6541	6610	7428
	EXP_SIGN.C		2900 #	7323
	EXP_SIGN.MS		2902 #
	EXP_SIGN.M		2901 #	6644
	EXT ADDR		2904 #	8496
	EXT BYTE READ		2905 #	7672	7676
	EXT BYTE RPW		2906 #	7760	7764
	EXT INDEX		2907 #	8478	8479	8480	8481	8497
	EXT INDRCT		2908 #	8473	8474	8475	8476	8498	8499
	FETCH			2956 #	3825	4861	4914	4918	5033	5081	5082	5114	5120	5145	5353
				5529	8894	9032	10392
	FETCH WAIT		2962 #	4183	8688
	FETCH+1			2957 #	9335
	FE_#			2911 #	4006	4008	4047	4069	5572	5575	5680	5702	5810	5817	5820
				5907	5908	5909	5910	5948	5963	6018	6135	6165	6332	6553	6640
				6677	6854	6880	6885	6932	7321	7395	7409	7699	7700	7737	7738
				7804	7806	7844	7845	7922	7925	7930	7931	8077	8080	8212	8275
				8311	8312	8313	8314	8505	8552	8560	8703	8796	8940	8998	9001
				9141	9160	9462	9796	10210	10543	10548	10893
	FE_# AND S		2912 #	10356	10423	10463	10472
	FE_#+AR0-8		2913 #
	FE_#+SC			2914 #	6138	7769	7787
	FE_#-SC			2915 #	7789	8370
	FE_+#			2916 #
	FE_-1			2917 #	5997	10622	10623
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-44
; 							Cross Reference Listing					

	FE_-SC			2919 #
	FE_-SC-1		2920 #	7768	10851
	FE_-S			2918 #	7762	7772	7774
	FE_1			2921 #	5991
	FE_AR0-8		2922 #	4958	6791	6800	7291	8661	8966
	FE_AR0-8 AND #		2923 #
	FE_AR0-8 COMP		2924 #	8844
	FE_EXP			2925 #	6082	6085	6541	10841
	FE_EXP+1		2926 #
	FE_EXP+SC		2927 #	6318
	FE_EXP-#		2928 #	6349	6353
	FE_EXP-1		2929 #
	FE_FE AND AR0-8		2931 #	8568	10628	10718
	FE_FE AND #		2930 #	10671	10688
	FE_FE OR #		2932 #	10710	10733	10781	10816
	FE_FE OR AR0-8		2933 #
	FE_FE SHRT		2934 #	10722
	FE_FE+#			2935 #	6176	6179	6415	6449	6694	6696	7005	7006	7302	7309	9358
	FE_FE+1			2936 #	5724	5734	5736	5738	5740	5752	5754	5756	5757	5762	5954
				6103	6104	6107	6451	6501	6667	6669	6697	6731	6891	6893	6920
				6922	7008	7039
	FE_FE+S			2937 #
	FE_FE+SC		2938 #	6094	6612	10593	10595
	FE_FE-#			2939 #	6420	6422	6442	6446	6455	6464	9331	9343	9367	9370	9392
				9452	10779
	FE_FE-1			2940 #	5839	5842	5847	5856	5872	5873	5874	5875	5876	5877	5878
				5879	5903	5904	5905	5906	6418	6700	7011	7305	7927	8951	8953
				10190	10196
	FE_FE-S			2941 #	7884	7886	7989
	FE_FE-SC		2942 #	6701	6728	7013	7034	7973	7975	10753
	FE_FE-SC-1		2943 #	8040
	FE_P			2944 #	5519	5522	7881	7882	9119	9189	9244
	FE_P AND #		2945 #	9121	9287	9395	10111	10591
	FE_P AND SC		2946 #
	FE_P OR #		2947 #
	FE_P+SC			2949 #	9345
	FE_P+1			2948 #	7969
	FE_P-S			2950 #
	FE_S			2951 #	7674	7679	7681	8291	8293	8411	9182	9198	9199	9257	9258
				9261
	FE_S+#			2952 #
	FE_SC			2953 #	5728	5729	5730	5732	5745	5746	5747	5749	5890	5893	8368
				9004	9006
	FILL			2963 #	8553	8604	8605	8651	8752	8917	8973	8978
	FILL_AR			2964 #	8553	8651	8917	8978
	FIN LOAD		2965 #	7683	7704	7705	7706	7707	7708	7709	7710	7712	7718	7719
				7720	7721	7722	7723	7725	7731	7732	7733	7737	7738	7842
	FIN STORE		2966 #	4092	4093	4099	4101	4712	4887	4890	4902	5033	5038	5120
				5145	5278	5286	5368	7573	7623	7866	8090	9445	9638
	FIN XFER		2967 #	4058	4892	4903	9443	10313
	FINISH			2968 #	4134	5980	8899
	FM(#)_AR		2972 #
	FM[]_AR			2970 #	3890	3941	6760	6765	6767	6773	6776	6793	6794	6801	6802
				6805	6833	6845	6851	6870	6877	6887	6913	6927	6947	6987	6990
				7315	7322	7400	7402	7413	7415	7443	10335	11005
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-45
; 							Cross Reference Listing					

	FM_AR			2969 #	3890	3941	5985	6001	6760	6765	6767	6773	6776	6793	6794
				6801	6802	6805	6833	6845	6851	6870	6877	6887	6913	6927	6947
				6987	6990	7315	7322	7400	7402	7413	7415	7443	7893	7914	8040
				8466	8508	8511	8513	8516	8553	8651	8663	8664	8665	8667	8675
				8708	8710	8733	8755	8801	8848	8853	8855	8862	8917	8952	8954
				8963	8964	8976	8978	9027	9091	9119	9121	9147	9152	9182	9195
				9198	9199	9251	9257	9258	9261	9283	9290	9449	9450	9455	9494
				10335	11005	11067	11075
	FORCE AR-ARX		2974 #	10529	10531
	GEN # AND AR0-8		2978 #	10644	10681	10705	10739
	GEN #+AR0-8		2979 #	9294
	GEN #+SC		2980 #	5614	6571
	GEN #-SC		2981 #	10778
	GEN -AR LONG		2982 #	5841	5844	6262	6549	6743	6766	6939	8937
	GEN -AR*4		2983 #
	GEN -SC			2984 #
	GEN -SC-1		2985 #
	GEN 0S			2986 #
	GEN 2AR			2987 #
	GEN AC0			2989 #	7422
	GEN AC0+1		2990 #
	GEN AR			2991 #	3975	3977	6598	6650	6771	6907	7094	7273	7324	7405	7447
				7672	7676	7760	7764	8148	8152	8182	8187	8317	8319	8413	8470
				8496	8498	9676	9679	9681	9683	9694	9696	9702	9826
	GEN AR*AC0		2992 #	4694	5415	5417	6559	6915
	GEN AR*BR		2993 #	5704	5964	7280	8956
	GEN AR*T0		2994 #
	GEN AR+1		2995 #	7217	8077	8080	8101	8104	8106	8108	8110	8275
	GEN AR+2BR		2996 #	6266
	GEN AR+BR		2997 #
	GEN AR+E1		2998 #
	GEN AR+FM[]		2999 #
	GEN AR+XR		3000 #	3976	3979	8150	8154	8184	8189	8318	8321	8497	8499
	GEN AR-2BR		3001 #	6264
	GEN AR-AC3		3002 #	8868
	GEN AR-BR		3003 #	5802	6818	7284
	GEN AR-BR-1		3004 #	7280	8956
	GEN AR-FM[]		3005 #
	GEN AR0-8		3006 #
	GEN ARX			3007 #	6552	6769	8161	8196	8216	8297	8301	8306	8308	8409	8473
				8475	8478	8480	8708	8803
	GEN ARX COMP		3008 #	8972
	GEN ARX*BRX		3010 #
	GEN ARX*BR		3009 #
	GEN ARX+1		3011 #
	GEN ARX+XR		3012 #	3955	3959	3963	3965	8163	8199	8218	8299	8303	8307	8309
				8474	8476	8479	8481
	GEN ARX-1		3013 #	5408
	GEN BR			3015 #	5095
	GEN BR*2		3016 #	10580
	GEN BR+ARX		3017 #
	GEN BRX+1		3018 #
	GEN CRY18		3019 #	4799	6439	9643	9655
	GEN E1			3020 #	7021
	GEN FE			3021 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-46
; 							Cross Reference Listing					

	GEN FE AND #		3022 #	10769	10812
	GEN FE AND AR0-8	3023 #
	GEN FE AND S		3024 #
	GEN FE AND SC		3025 #	10741
	GEN FE OR AR0-8		3026 #	8569
	GEN FE+#		3027 #
	GEN FE-#		3028 #	7300	8024	8025	8027
	GEN FE-1		3029 #	4961
	GEN FE-S		3030 #
	GEN FE-SC		3031 #
	GEN FE-SC-1		3032 #	8182	8185	8197	8200	8363
	GEN MQ			3033 #
	GEN MQ-AR		3034 #
	GEN P AND SC		3036 #	10771	10814
	GEN P AND #		3035 #
	GEN P+SC		3037 #
	GEN P-#			3038 #	8101	8104	8106	8108
	GEN P-S			3039 #	9468
	GEN P-SC		3040 #
	GEN SC			3041 #	8468
	GEN SCAD 0S		3042 #
	GEN T1			3043 #
	GEN T2			3044 #
	GET ECL EBUS		3045 #	3967	3968	3969	3970	4922	8483	8484	8485	8486	9721	10263
				10539
	GLOBAL			3046 #	3953	3955	3957	3959	3962	3963	3964	3965	7672	7676	7760
				7764	8161	8163	8196	8199	8216	8218	8297	8299	8301	8303	8306
				8307	8308	8309	8473	8474	8475	8476	8478	8479	8480	8481
	HALT			3048 #	4948
	HARDPFW			3049 #	11005
	I FETCH			3052 #	4058	4091	4093	4099	4101	4191	4198	4381	4890	4996	5038
				5411	5518	5548	5567	5582	5605	5630	5849	5858	5978	6019	6385
				6386	6741	7090	7216	7218	7219	7274	7282	7286	7477	7483	7491
				7573	7623	7684	7704	7705	7706	7707	7708	7709	7710	7712	7718
				7719	7720	7721	7722	7723	7725	7731	7732	7733	7737	7738	7866
				7895	7970	7991	7999	8681	8970	9650
	INDEXED			3056 #	3976	3979	8150	8154	8184	8189	8318	8321	8497	8499
	INH CRY18		3057 #	5409	9184	9193	9241	9245	9249	9332	9369
	IO INIT			3058 #	9694
	IR DISP			3059 #	4037	4048	4059	4070	4081	4859	4885	4893	8509	8512	8514
				8517
	JFCL FETCH		3061 #	4938
	JFCL S			3062 #	4940
	JFCL T			3063 #	4937
	JUMP FETCH		3064 #	4760	4775	4791	4799
	LD PCS			3066 #	4923
	LD PREV CTXT		3067 #	5348
	LOAD AR			3068 #	4819	4833	4892	4902	4903	5278	5995	7271	7312	7394	7408
				7463	7475	7484	7493	7600	7616	7979	8085	8408	8561	8774	8973
				9041	9105	9137	9285	9286	9359	9363	9400	9402	9465	9517	9658
				10310	10311	10367	10368	10441	10442	10446	10447	10622	10623	10641	10646
				10661	10683	10708	10732	10767	11014
	LOAD AR (RPW)		3070 #	10342	10349
	LOAD AR (WR TST)	3071 #	9414	9418
	LOAD ARX		3073 #	3932	4374	5031	5276	5286	5338	5934	6542	6758	7292	7397
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-47
; 							Cross Reference Listing					

				7996	10313	10361
	LOAD ARX (WR TST)	3075 #	9442
	LOAD EBR		3077 #	10180
	LOAD IR			3079 #	3927	3938	5428	8470
	LOAD UBR		3083 #	10114
	LOAD VMA(EA)_ARX+BR	3085 #	7558	7559	7566
	LONG EN			3090 #	4002	8157	8192	8326	8502
	MAP			3095 #	4011	4981	10562
	MB WAIT			3096 #	4036	4092	4987	5943	5945	6033	6035	8409	9638	9730	10125
				10207	10268	10549	10563	10770	10968	11006
	MEM_AR			3097 #	4398	5035	5040	5342	5352	5365	7469	7489	7571	7574	7621
				7624	7670	7674	7678	7758	7762	7766	7772	8115	8291	8294	9218
				9447	9508	9516	9640	9647	9654	10293	10391	10703
	MQ_0.C			3099 #
	MQ_0.M			3100 #	5571	5579	6103	6104	6107	6583	6586	6668	6670	6921	6923
				6951	7087	9003	9005
	MQ_0.S			3101 #	4378	4394	5935	6541	6759	7272	7396
	MQ_1			3102 #
	MQ_1S			3103 #
	MQ_AD			3104 #	6906	7768	7967	8629
	MQ_ARX			3109 #	5576	6001	6613	6615	7403	7812	7813	7814	7815	7816	7817
				7820	7821	7822	7823	7826	7827	7828	7829	7830	7833	7834	7835
				7836	7864	7884	8554	8849	9300	9302	9323
	MQ_ARX COMP		3110 #
	MQ_AR			3105 #	4213	5358	5679	5701	5907	5908	5909	5910	5956	5962	6024
				6095	6098	6135	6630	6640	6647	6711	6714	6716	6882	6885	6890
				6938	7023	7026	7028	7458	7461	7467	7492	7966	8362	8402	8466
				8665	8796	8991	9027	9214	10217	10265	10334
	MQ_AR (AD)		3106 #
	MQ_AR COMP		3107 #
	MQ_AR SWAP		3108 #
	MQ_BR			3111 #	8629
	MQ_BR COMP		3112 #
	MQ_FM[]			3113 #	7768
	MQ_MQ*.25		3114 #	5976	6693	7004	7051
	MQ_MQ*2			3115 #	5974	6014	6016	8811
	MQ_MQ*BR		3116 #	6906
	MQ_MQ-1			3117 #	7967
	MQ_SHIFT		3118 #	5558	5565	5574	5581	5619	5947	6100	6383	6443	6576	6589
				6591	6595	6704	6705	6858	7016	7017	10111
	MSK			3120 #	8675	8862	9097	9130	9132	9283
	MSK_AR			3121 #	8675	8862	9283
	MUL			3122 #	5724	5734	5736	5738	5740	5752	5754	5756	5757	5762	5954
	NO CRY			3125 #	4655	4658
	NORM			3126 #	6110	6178	6190	6416	6442	6447	6455	6465	6498	6593	6650
				6673	6698	6706	6712	6715	6716	6856	6908	6929	6995	6997	7019
				7024	7027	7029	7038	7094	7310	7324	7405	7416	7447
	NORM AR			3128 #	6138
	NORM -AR		3127 #	6414
	NXT INSTR		3129 #	4096	4118
	OPTIONS			3131 #	9749
	P0			3162 #	10692	10772	10787
	P1			3163 #	10694	10809
	P10			3164 #	10574	10630	10647	10685	10693	10709	10720	10746	10783	10797	10842
	P11			3165 #	10579	10836
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-48
; 							Cross Reference Listing					

	P12			3166 #	10572	10573	10598	10814	10818	10948	10973
	P13			3167 #
	P14			3168 #
	P15			3169 #
	P16			3170 #	10532	10850	11040
	P17			3171 #	10533	10854	10856	10921
	P2			3172 #	10669	10680	10757	10763
	P3			3173 #	10640	10655	10731
	P4			3174 #
	P5			3175 #	10582	10719	10822	10866	10873
	P6			3176 #
	P7			3177 #
	PC_VMA			3179 #	4951
	PF DISP			3180 #	10539	10550
	PFA			3182 #
	PFA_AR			3183 #
	PHYS REF		3185 #	10349	10351	10442	10447	10452	10641	10646	10661	10683	10703	10708
				10732	10767
	PHYS REF CACHE		3186 #
	POP AR			3188 #	5069
	POP AR-ARX		3189 #	5071
	POP ARX			3190 #
	PORTAL			3192 #	4813
	PT FETCH		3193 #
	PT REF			3194 #	3932	5276
	PT SEL_INVAL		3195 #	10182
	PT SEL_INVAL (KEEP)	3196 #	10186
	PT SEL_NORMAL		3197 #	10193	10199
	PUR			3199 #	10694	10809
	PUSH			3202 #	5023	5028
	P_#			3133 #	8019	8021	8023	8026	8029	8033	8035	8038	8039
	P_#-SC			3135 #	7889
	P_#-S			3134 #	9203	9246
	P_-SC			3136 #
	P_0			3137 #
	P_1S			3138 #
	P_FE			3139 #	5524	8041	9192	9493
	P_FE OR SC		3140 #	9374	9375	10566
	P_FE+SC			3141 #	8052
	P_FE-S			3142 #	8083	8280	8705	9185
	P_FE-S.S		3143 #	8085
	P_FE.C			3144 #	9248
	P_P AND #		3145 #	7083	9057	9122	9126	9380
	P_P AND SC		3146 #	9333
	P_P OR #		3147 #	9123	9124	9125	9127
	P_P OR SC		3148 #	9151	9522	10823
	P_P OR SC#		3149 #
	P_P+#			3150 #
	P_P+1			3151 #	8100	8103	8119	8123	8127	8131	9334
	P_P+S			3152 #	11066
	P_P+S.C			3153 #	11071
	P_P-S			3154 #	7891	8068	8707	9174	9222	10483
	P_SC			3155 #	7982	10147
	P_SC#			3156 #	8088	8111	8112
	P_SCAD			3157 #	4194	5357	5524	7891	7982	7989	8041	8068	8707	9151	9174
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-49
; 							Cross Reference Listing					

				9192	9222	9333	9374	9375	9493	9522	10147	10483	10566	10823	11066
	P_SCAD#			3158 #	7083	7889	8019	8021	8023	8026	8029	8033	8035	8038	8039
				8088	8100	8103	8111	8112	8119	8123	8127	8131	9057	9122	9123
				9124	9125	9126	9127	9203	9246	9248	9334	9380	11071
	P_SCAD.C		3159 #	8052	8083	8280	8705	9185
	P_SCAD.S		3160 #	8085
	R0			3207 #	10563
	R1			3208 #
	R10			3209 #	8665	8677	8686	8691	8709	8733	8848	8865	8870	8876	8964
				9025	9027	9091	9146	9148	9149	11078	11089	11091	11100
	R11			3210 #	6001	6023	6794	6802	6835	6847	7914	7939	8952	8954	8991
	R12			3211 #	6765	6773	6780	6789	6805	6877	6926	6927	6943	6947	6987
				6990	7067	7322	7402	7415	7443	8963	8990
	R13			3212 #	8553	8604	8605	8651	8752	8917	8973	8978
	R14			3213 #
	R15			3214 #	3890	4865	4880	5027	5118	5123	5272	5353	5358	5362	5366
				8636	8637	10337	10373	10397	10419	10434	10455	11006
	R16			3215 #	6760	6767	6795	6800	6878	6988	8466	9137	9400	9402	9517
	R17			3216 #	11005
	R2			3217 #	11039
	R3			3218 #	10335	11038
	R4			3219 #	3941	6994	6996	7060	7197	7198	7204	7205	7434	7437	8634
	R5			3220 #	6776	6785	6804	6845	6851	6870	6913	7021	7253	7254	7266
				7267	7268	7315	7400	7413	8508	8511	8513	8516	9041	9042	9094
				9095	9096	9105	9464	9465
	R6			3221 #	5985	6003	6793	6801	6831	6833	6836	6848	6887	7893	7966
				8040	8801	8810	8976	8979	11084
	R7			3222 #	8675	8862	9097	9130	9132	9283
	RD+CLR C CNT		3224 #	10276
	RD+CLR E CNT		3225 #	10275
	RD+CLR PA		3226 #	10274
	RD+CLR TB		3227 #	10273
	READ BP2		3229 #	7681	7774	8293
	READ EBR		3231 #	10168
	READ ERA		3232 #	9794
	READ UBR		3233 #	10150
	REL EBUS		3234 #	9683	9826
	REL ECL EBUS		3235 #	4990	9635	10172	10206	10228	10267	10277	10550	10565
	REQ EBUS		3236 #	9669	9830
	REQ SV.VMA		3238 #	10866
	REQ VMA HELD		3239 #
	RETURN []		3243 #
	RETURN0			3244 #	8217	8219	10867
	RETURN1			3245 #	4925	4931	5369	6807	8306	8307	8308	8309	8317	8318	8387
				8390	8415	8618	8620	8629	9093	9159	9717	9721	10294	10315
	RETURN10		3246 #	8605	8609
	RETURN12		3247 #	10431
	RETURN15		3248 #	8076	8093
	RETURN16		3249 #
	RETURN17		3250 #	8079
	RETURN2			3251 #	5046	6265	6267	6818	7554	7575	7625	8213	8350	8351	8389
				8627	8638	8758	9137	9203	10434	10456
	RETURN20		3252 #	5363	8570
	RETURN3			3253 #	4402	5299	5849	5858	5907	5910	8211	8365	8374	9141	9522
				9684	9697	10155
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-50
; 							Cross Reference Listing					

	RETURN30		3254 #
	RETURN37		3255 #	8149	8151	8162	8164
	RETURN4			3256 #	5745	5746	5747	5749	6614	6616	8069	8070	8091	8100	8103
				8111	8112	8119	8123	8127	8131	8274	8277	8278	8281	9097	9132
				9152	10210	10426
	RETURN5			3257 #	5908	5909	9121
	RETURN6			3258 #	5728	5729	5730	5732	5851	5860	5891	5894	9218
	RETURN7			3259 #	5852	5861
	RET[]			3242 #	7197	7198	7204	7205
	RSTR FLAGS_AR		3260 #	4854	4904	4905	4908
	RSTR VMA_ARX		3262 #
	RSTR VMA_MQ		3263 #	8415
	RSTR VMA_SV.VMA		3264 #	10822
	SBR			3269 #	10640	10655	10731
	SBUS DIAG		3271 #	9798
	SC_#			3273 #	3881	3894	3896	4009	4380	4393	4396	4934	4936	5342	5343
				5516	5623	5632	5686	5706	5797	5799	5803	5938	5967	5987	6031
				6380	6438	6444	6592	6650	6672	6685	6699	6705	6782	6787	6855
				6903	6907	6928	7000	7018	7052	7058	7063	7070	7072	7074	7094
				7324	7425	7429	7445	7706	7707	7708	7709	7710	7714	7715	7716
				7720	7721	7722	7723	7727	7728	7729	7812	7813	7814	7815	7816
				7817	7820	7821	7822	7823	7826	7827	7828	7829	7830	7833	7834
				7835	7836	7913	8043	8044	8113	8118	8122	8126	8130	8380	8404
				8668	8858	8884	8891	8893	8921	8937	8940	8998	9001	9034	9107
				9136	9273	9292	9311	9470	9519	9521	9726	10104	10124	10126	10130
				10154	10165	10179	10218	10229	10271	10291	10341	10363	10425	10440	10445
				10450	10562	10627	10724	10990	10992	11077
	SC_# AND AR0-8		3274 #	9289	10596
	SC_# AND S		3275 #
	SC_# OR SC		3276 #
	SC_#+AR0-8		3277 #	6366
	SC_#+SC			3278 #	5541	5544	5546	5561	5564	5592	5596	5618	5620	6095	6102
				6265	6267	6574	6613	6631
	SC_#-S			3279 #
	SC_#-SC			3280 #	5299	6098	6615	6837	6841	7683	8337	8371	9216	10475
	SC_#-SC-1		3281 #
	SC_-SC			3283 #	10758	10768
	SC_-SC-1		3284 #	10855
	SC_-S			3282 #	7864
	SC_0			3285 #
	SC_1			3286 #	5784	5985	5991	8918	9027	10751
	SC_1S			3287 #	6021
	SC_AR0-8		3288 #
	SC_EA			3289 #	5513	5532	5534	5553	5556	5572	6316	8051
	SC_EXP			3290 #	6133	7314	10841	10849
	SC_EXP+1		3291 #	6162
	SC_EXP+SC		3292 #	6136	6644
	SC_EXP-#		3293 #
	SC_EXP-1		3294 #
	SC_EXP-SC		3295 #	6093	6261	6610
	SC_FE			3296 #	6836	6839	7768	7787	7789	7923	7925	8342	8369	10208	10822
	SC_FE AND #		3297 #
	SC_FE#			3298 #	7672	7676	7689	7760	7764	8067
	SC_FE+#			3299 #	6382
	SC_FE+1			3300 #	9301	9303	9304
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-51
; 							Cross Reference Listing					

	SC_FE+S			3301 #
	SC_FE+SC		3302 #	5577	5580	7054	7686	8149	8151	8162	8164	8344	8832	9176
				9291	10468
	SC_FE-#			3303 #	8968
	SC_FE-1			3304 #	6561
	SC_FE-SC		3305 #	7790	8373
	SC_FE-SC-1		3306 #	8952	8954
	SC_P			3307 #	7981	8087	9401	10484
	SC_P AND #		3308 #	9359	9361
	SC_P AND SC		3309 #	10565	10757	10763
	SC_P+S			3311 #	7967
	SC_P+1			3310 #	8110
	SC_P-#			3312 #	7667	7671	7755	7758	7862	8076	8079	8093	8386
	SC_S			3313 #	8673
	SC_SC AND #		3314 #
	SEL AC4			3317 #	8608	8897
	SEL DSTP		3318 #	9254	9255	9447	11071	11080
	SEL DSTP2		3319 #	8606	9248	9249	9454
	SET ACC+CLR UCODE	3321 #
	SET ACCOUNT EN		3322 #	3924	4997	10268	10279	10867	10972	11016	11045
	SET AROV		3323 #	5615	5690	5707	5968	6377	7219
	SET CONS XCT		3324 #	4967
	SET DATAI		3325 #	10404	10464
	SET DATAO		3326 #	9647	10390	11044
	SET EBUS DEMAND		3327 #	9677
	SET FL NO DIV		3328 #	6169	6680	6935
	SET FLAGS_AR		3329 #	3822	5372	11015
	SET FLOV		3330 #	7076	7081	7282	7440
	SET FPD			3331 #	7674	7679	7682	7762	7772	7775	8067	8291	8324	8993	9658
				9659
	SET FXU			3332 #	7080	7286	7441
	SET HALTED		3333 #	4950
	SET IO PF		3334 #	11041
	SET MTR PA EN		3335 #
	SET NO DIVIDE		3336 #	5794
	SET PC+1 INH		3337 #	3933
	SET PI CYCLE		3338 #	3880	10278	10334
	SET PXCT		3339 #	5427
	SET SR1			3340 #	6190	6414
	SET SR2			3341 #	6006	6662	6688	6946
	SET SR3			3342 #	6026	6600	6628	7003
	SET SXCT		3343 #
	SET TRACKS EN		3344 #
	SET TRK+PA EN		3345 #
	SFLGS_AR		3347 #	8667	8853	8855	9119	9121	9152	9290	9494
	SFLGS			3346 #	8667	8693	8746	8853	8855	8883	9093	9109	9111	9119	9121
				9136	9152	9290	9471	9494	9519	9521	11094	11098
	SH DISP			3348 #	5344	7071	7699	7700	7804	7806	8110	9292	9309	10337	10356
	SIGNS DISP		3349 #	5811	6005	6007	7915	8704	8708	8735	8755	8800	8801	8815
				8848	8972	9029	9054	9092	9120	9147	10580	10617	10619
	SKIP FETCH		3350 #	4712
	SKP -EBUS GRANT		3352 #	9691
	SKP -EBUS XFER		3353 #	9681
	SKP -LOCAL AC ADDR	3355 #	4191	4198	5271	5335
	SKP -START		3357 #	4957
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-52
; 							Cross Reference Listing					

	SKP -VMA SEC0		3359 #	7678	7766	7883	7976	8081	8294
	SKP AC EQ 0		3361 #	7862
	SKP AC REF		3362 #
	SKP AC#0		3363 #	4094	4713
	SKP AC0+		3364 #	5891
	SKP AC0-		3365 #	5894
	SKP AD NE		3366 #	5516	5802	6552	6598	6626	6769	6868	7021	7214	7217	7273
				7423	7489	8413	10456	10669	10757
	SKP AD NZ		3367 #
	SKP AD0			3368 #	5416	5418	5704	5964	5992	6166	6559	6676	6771	6775	6781
				6818	6829	6831	6874	6878	6886	6914	6915	6932	6944	6950	6988
				6992	7280	7284	7419	7444	7459	8409	8662	8671	8920
	SKP ADX0		3369 #
	SKP AR EQ		3370 #	4968
	SKP AR EQ -1		3371 #
	SKP AR GT BR		3372 #	8771	10430
	SKP AR NE		3373 #	7461	8936	9477	9518
	SKP AR NE BR		3374 #	5611
	SKP AR NZ		3375 #	7894	7922
	SKP AR SIG		3376 #	5686
	SKP AR0			3377 #	4175	4349	4356	5410	5527	5786	5818	5821	6332	6645	6764
				6768	6942	7276	7307	7947	7981	8087	8165	8686	8736	9189	9244
				10841	10849	11091	11100
	SKP AR1			3378 #
	SKP AR18		3379 #	4335	4342	5514	5532	5534	5553	5556	5572	8510	9131	10112
				10782
	SKP AR2			3380 #	8867	10104
	SKP AR6			3381 #	10343	10345	10381	10406
	SKP ARX LE BRX		3382 #
	SKP ARX LT BRX		3383 #	7570	7620
	SKP ARX NE		3384 #	6412
	SKP ARX NZ		3385 #	7924
	SKP ARX+MQ NE		3386 #	6685	7000
	SKP ARX0		3387 #	7990	8041	8797	9107	9296	9314	9339
	SKP ARX2		3388 #	9048
	SKP BR EQ		3390 #	6177	8822
	SKP BR EQ -1		3391 #	7547
	SKP BR0			3392 #	6018	6168	6261	6679	6934	6951	8691	8813	8992
	SKP CRY0		3393 #	5023	5028	5073	5092	5408	5997	6015	6016	6264	6266	6439
				6710	7022	7471	8869	8933	8967	9025	9097	10777
	SKP EVEN PAR		3394 #
	SKP EXP NE		3395 #
	SKP FE NZ		3396 #
	SKP FE0			3397 #	6003
	SKP FETCH		3398 #
	SKP INTRPT		3399 #	3954	3956	3958	3960	3978	3980	4962	4979	4994	5273	7469
				7495	7568	7618	8153	8155	8188	8190	8217	8219	8298	8300	8302
				8304	8320	8322	8473	8474	8475	8476	8498	8499	8704	8753	8832
				9035	9176	9360	9364	9695	10627	10711	10893
	SKP IO LEGAL		3400 #	4820	4821	4827	4829	4987	9628	9634	9727	9734	9736	9741
				9745	9768	9771	9797	9816	9819	10098	10102	10130	10161	10164	10216
				10232	10235	10244	10247	10256	10948
	SKP KERNEL		3401 #	4817
	SKP MQ EQ -1		3402 #	9044
	SKP MQ NE		3403 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-53
; 							Cross Reference Listing					

	SKP PC SEC0		3407 #	4849	4850	5025	5076	5079	5114	5116	5139	5269	8626	9187
				9188	9242	9243	9284	9324	9356	9358	9390	9448	9490
	SKP PI CYCLE		3409 #	9640	9643	9656	10973
	SKP P NE		3404 #	10692	10787
	SKP P!S XCT		3405 #	7534	7559
	SKP RPW			3410 #	10543	10548
	SKP RUN			3411 #
	SKP SC NE		3415 #	5590	10116
	SKP SC NZ		3416 #
	SKP SC0			3417 #	6025	7684	7864	7865	7882	8971
	SKP SCAD NE		3418 #	5519	5522	8102	8105	8107	8109	8468	10644	10682	10706	10739
				10742	10769	10771	10813	10815
	SKP SCAD NZ		3419 #	8569
	SKP SCAD0		3420 #	5542	5545	5547	5561	5564	5577	5580	5592	5597	5614	6093
				6096	6098	6102	6349	6353	6366	6382	6572	6574	6610	6614	6616
				6837	7301	7687	7884	7886	7891	8024	8025	8027	8183	8185	8197
				8200	8337	8344	8364	8386	8707	9216	9295	9331	9343	9368	9371
				9392	9452	9468	10476	10483
	SKP USER		3421 #	4865	4882	4907	4908	4913	5359	5424	10726	11008
	SLEN			3423 #	8665	8677	8686	8691	8709	8733	8848	8865	8870	8876	8964
				9025	9027	9091	9146	9148	9149	11078	11089	11091	11100
	SLEN_AR			3424 #	8665	8733	8848	8964	9027	9091
	SR DISP			3425 #	6026	6027	6032	6417	6419	6421	6423	6450	6452	6453	6463
				6482	6502	6709	6732	7078	7087	7966	8339	8756	10572	10874	11055
				11067	11075	11096
	SRCP			3476 #	8698	8708	8798	8798	9160	9160	9182	9198	9199	9462	9462
				10897	11067
	SRCP2_AR		3479 #	9195
	SRCP2			3478 #	9187	9188	9195
	SRCP_AR			3477 #	8708	9182	9198	9199	11067
	SR_#			3426 #	6066	6067	6068	10905	10936	10944
	SR_0			3427 #	4108	4109	4113	4125	4920	4993	5352	6512	6516	6526	6727
				6742	7033	7092	7483	7498	7792	8051	8693	8713	8800	8895	9037
				9093	10897	10909	10928	10953	10958	11052
	SR_1			3428 #	6918	8045	10901	10913	10932
	SR_2			3429 #
	SR_BDD			3431 #	9050
	SR_BDF			3432 #	8977
	SR_BDT			3433 #	8927	8994	9053	10917
	SR_BLT(DST)		3434 #	7563	7564	7565
	SR_BLT(PXCT DST)	3436 #	7614	7615
	SR_BLT(PXCT SRC)	3437 #	7607	7625
	SR_BLT(SRC)		3439 #	7553	7575
	SR_DB			3440 #	8861
	SR_DST			3441 #	8745
	SR_DSTF			3442 #	8699
	SR_ED(+D)		3443 #	8809	9469	9520
	SR_ED(PAT)		3444 #	9274	9357
	SR_ED(S)		3445 #	8799	9313
	SR_MAP			3460 #	4984
	SR_SRC			3463 #	8670	8674	8682	8698	8700	8739	10940
	SR_SRC+DST		3464 #	8680	8737
	SR_WORD			3469 #
	SR_XBLT(DST)		3472 #	7467	7485	7494
	SR_XBLT(SRC)		3473 #	7457	7468	7497
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-54
; 							Cross Reference Listing					

	STACK UPDATE		3482 #	5073	5091	5407
	STORE			3484 #	4211	4393	4396	4399	4728	4743	4868	4884	4888	5118	5119
				5129	5277	5284	5340	5350	5367	5369	7468	7496	7618	7791	7842
				8076	8079	8083	8088	8089	8091	8100	8103	8111	8112	8119	8123
				8127	8131	8274	8281	8374	9427	9443	9446	9503	9504	9505	9506
				9509	9655	9749	9802	10229	10292	10294	10351	10382	10383	10414	10415
				10451	10452	10561	10694	10809	11004	11011	11013
	STORE VMA(EA)_ARX	3486 #	7568
	SV.ARX			3497 #	10533	10854	10856	10921
	SV.ARX_AR		3501 #	10533
	SV.AR_AR		3495 #	10532
	SV.AR			3491 #	10532	10850	11040
	SV.BR_AR		3504 #	10574
	SV.BR			3503 #	10574	10630	10647	10685	10693	10709	10720	10746	10783	10797	10842
	SV.IOP			3506 #	10335	11038
	SV.IOPF			3507 #	11039
	SV.IOPF_AR		3508 #	11039
	SV.PAR			3510 #	10563
	SV.PAR_AR		3511 #	10563
	SV.PFW			3512 #	10572	10573	10598	10814	10818	10948	10973
	SV.PFW_AR		3513 #	10572	10573	10598	10818
	SV.SC_AR		3515 #	10579
	SV.SC			3514 #	10579	10836
	SV.VMA			3516 #	10582	10719	10822	10866	10873
	SV.VMA_AR		3517 #	10582
	SWD			3519 #
	SWD_AR			3520 #
	SWEEP CACHE		3521 #	9729
	T0			3525 #	5985	6003	6793	6801	6831	6833	6836	6848	6887	7893	7966
				8040	8801	8810	8976	8979	11084
	T0_AR			3526 #	5985	7893	8040	8801	8976
	T1			3527 #	6001	6023	6794	6802	6835	6847	7914	7939	8952	8954	8991
	T1_AR			3528 #	6001	7914	8952	8954
	T2			3529 #	6765	6773	6780	6789	6805	6877	6926	6927	6943	6947	6987
				6990	7067	7322	7402	7415	7443	8963	8990
	T2_AR			3530 #	8963
	TAKE INTRPT		3531 #	4003	4959	5285	5300	5425	8158	8193	8327	8503
	TEST AR			3532 #
	TEST AR.AC0		3533 #	4663
	TEST AR.BR		3534 #	4938	9639	9641	10777
	TEST AR.MSK		3535 #	9097
	TEST ARX		3536 #	8743
	TEST ARX.AR*4		3537 #
	TEST BRL		3538 #	9643
	TEST CBR		3540 #	10669	10757
	TEST FETCH		3542 #	4655	4658	4663	8743	9639	9641	9643
	TRAP1			3543 #
	TRAP2			3544 #	5035	5040	5088	5089	5093
	TRAP3			3545 #
	UNCSH PHYS REF		3559 #
	UPT FETCH		3560 #
	UPT REF			3561 #	5338	5340	10311	10623	11004
	USER REF		3562 #
	VMA_#			3564 #	5274	5283	5339	10266	10301	10303	10305	10307	10968
	VMA_#+AR32-35		3565 #	8407	10616	10618
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-55
; 							Cross Reference Listing					

	VMA_40			3566 #
	VMA_40+PI*2		3567 #	10339	10340
	VMA_41			3568 #
	VMA_41+PI*2		3569 #	10354
	VMA_420+TRAP		3570 #	3926	3928
	VMA_430+MODE		3571 #	5337
	VMA_AC3			3572 #	9388	9414	9488
	VMA_AR			3573 #	4914	4918	5031	5082	7271	7312	7394	7408	9286	9359	9363
				9418	9504	9506	9658	9659	9729	10180	10364	10467	10645	10706
	VMA_AR AND ADMSK	3574 #	5353	10337
	VMA_AR+1		3575 #	10409	10474
	VMA_AR+BR		3576 #
	VMA_AR+CBR		3578 #	10680
	VMA_AR+E0		3580 #	9517
	VMA_AR+E0+1		3581 #	9402
	VMA_AR+E1		3582 #	9041	9105	9465
	VMA_AR+SBR		3584 #	10640	10655	10731
	VMA_AR+XR		3589 #
	VMA_AR-1		3590 #	10393
	VMA_ARX			3591 #	5081	5277	7468	7496	7618	9285
	VMA_ARX AND ADMSK	3592 #	10373	10397	10419
	VMA_ARX+1		3593 #	8508	8511	8513	8516
	VMA_ARX+BR		3594 #	7600	7616
	VMA_ARX+CBR		3596 #	10763
	VMA_ARX+XR		3598 #
	VMA_BR			3600 #	4861	5033	7484	10703
	VMA_E0+1		3601 #	9137	9400
	VMA_FM[]		3602 #	8973
	VMA_MQ			3603 #	8772	10114	10229
	VMA_MQ+1		3604 #	8773
	VMA_PC			3605 #	9174	9222
	VMA_PC+1		3606 #	8746	8867	8885	8932	9024	9312
	VMA_SV.VMA		3608 #	10873
	VMA_VMA HELD		3619 #	4008	10560
	VMA_VMA+1		3622 #	4374	4398	4888	4892	4902	4903	5120	5145	5278	5286	5343
				5367	5369	5934	5989	6542	6758	7275	7397	7979	7996	8085	8890
				8892	9031	9442	9509	9802	10294	10313	10432	11011	11013	11014
	VMA_VMA-1		3623 #	8090	9446	10292
	WR PT ENTRY		3625 #	10099	10824
	WR REFILL RAM		3626 #	9737
	WRITE (E)		3628 #	5095
	XR			3631 #	3925	4005	8159	8160	8194	8195	8209	8210	8328	8504
	[]_#[]			2304 #
	[]_ADA[]		2305 #
	[]_ADB[]		2306 #
	[]_FM[]			2307 #	6785	6789	6795	6800	6804	6831	6835	6836	6847	6848	6878
				6988	7060	7067	7253	7254	7266	7267	7268	8634
	[]_[]*FM[]		2302 #	6780	6926	6943	6994	6996	7197	7198	7204	7205	7434	7437
	[]_[]*[]		2301 #
	[]_[]-FM[]		2303 #	8636
	(AR+ARX+MQ)*.25		2309 #	6724	6899	7031
	(AR+ARX+MQ)*2		2310 #	5728	5729	5730	5732	5745	5746	5747	5749	6700	6898	6900
				7011
	(MQ)*.25		2311 #	6724	6899	7031
	(MQ)*2			2312 #	5728	5729	5730	5732	5745	5746	5747	5749	6700	6898	6900
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-56
; 							Cross Reference Listing					

				7011
(D) MACRO%
	AC			3670 #	4145	4146	4154	4155	4159	4160	4164	4165	4220	4222	4229
				4230	4234	4235	4239	4240	4244	4245	4249	4250	4254	4255	4259
				4260	4266	4267	4271	4272	4276	4277	4281	4282	4286	4287	4291
				4292	4296	4297	4301	4302	4406	4407	4416	4417	4426	4427	4436
				4438	4445	4446	4461	4462	4471	4472	4481	4482	4491	4492	4501
				4502	4512	4513	4522	4523	4532	4533	4542	4543	4552	4553	4975
				5181	5637	5638	5649	5650	5662	5663	7662	7663
	B			3673 #	4409	4419	4429	4443	4448	4464	4474	4484	4494	4504	4515
				4525	4535	4545	4555	5640	5652	5665
	BLKI			3706 #	9556	9603	9614
	BLKO			3707 #	9605	9616
	CONI			3710 #	9539	9550	9561	9585	9597	9608	9619
	CONO			3711 #	9538	9549	9560	9584	9596	9607	9618
	CONSO			3712 #	9541	9552	9563	9587	9599	9610	9621
	CONSZ			3713 #	9540	9551	9562	9586	9598	9609	9620
	DATAI			3708 #	9535	9557	9604	9615
	DATAO			3709 #	9537	9559	9582	9583	9606	9617
	DBL AC			3674 #	5694	5695	5767	5768	5772	5773
	DBL B			3675 #	5697	5770	5775
	EA			3649 #	4206	4222	4438	4805	5003	5005	5101	5102	5159	5160	5161
				5162	5164	5165	5166	5167	5169	5170	5171	5172	5174	5175	5176
				5177	5180	5181	5183	5184	5185	5186	5187	5188	5189	5190	5201
				5202	5203	5204	5205	5206	5210	5211	5212	5213	5214	5215	5216
				5217	5218	5219	5220	5221	5222	5223	5224	5225	5226	5227	5228
				5229	5230	5231	5232	5233	5234	5235	5236	5237	5238	5239	5240
				5241	5245	5246	5392	5505	6276	6277
	FL-AC			3676 #	6042	6047	6048	6052	6057	6058	6115	6120	6121	6144	6145
				6149	6150	6279	6327
	FL-BOTH			3678 #	6045	6050	6055	6060	6118	6123	6147	6152
	FL-MEM			3677 #	6044	6049	6054	6059	6117	6122	6146	6151
	I			3644 #	4150	4566	4567	4568	4569	4570	4571	4582	4583	4584	4585
				4586	4587	4588	4589	4600	4601	4602	4603	4604	4605	4606	4607
				4618	4619	4620	4621	4622	4623	4624	4625	4674	4675	4676	4677
				4678	4679	4680	4681	4749	4750	4751	4752	4753	4754	4755	4756
				4764	4765	4766	4767	4768	4769	4770	4771	4779	4780	4781	4782
				4783	4784	4785	4786	4794	4795	4806	4975	5006	5103	5104	5393
				5498	5499	5500	5501	5502	5503	5504	5663	5695	5768	5773	6048
				6058	6121	6150	6279	9536	9538	9539	9540	9541	9549	9550	9551
				9552	9558	9560	9561	9562	9563	9567	9568	9569	9570	9571	9572
				9573	9574	9584	9585	9586	9587	9594	9595	9596	9597	9598	9599
				9607	9609	9610	9618	9620	9621
	I-PF			3645 #	4146	4155	4160	4165	4230	4235	4240	4245	4250	4255	4260
				4267	4272	4277	4282	4287	4292	4297	4302	4406	4407	4417	4427
				4446	4456	4462	4472	4482	4492	4501	4502	4513	4523	4533	4543
				4552	4553	4564	4565	4573	4574	5638	5650
	IW			3663 #	4408	4409	4503	4504	4554	4555
	M			3671 #	4156	4161	4166	4226	4231	4236	4241	4246	4251	4256	4261
				4268	4273	4278	4283	4288	4293	4298	4303	4408	4418	4428	4442
				4447	4457	4458	4463	4473	4483	4493	4503	4514	4524	4534	4544
				4554	5639	5651	5664	5696	5769	5774	7750	7751	9545
	R			3652 #	4367	4368	4575	4576	4577	4578	4579	4580	4591	4592	4593
				4594	4595	4596	4597	4598	4609	4610	4611	4612	4613	4614	4615
				4616	4627	4628	4629	4630	4631	4632	4633	4634	4683	4684	4685
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-57
; 							Cross Reference Listing					

				4686	4687	4688	4689	4690	4701	4702	4703	4704	4705	4706	4707
				4708	4974	5004	5662	5694	5767	5772	5921	5922	5923	5924	6042
				6043	6047	6052	6053	6057	6115	6116	6120	6144	6145	6149	6323
				6324	6326	6327	6534	6535	6536	6537	6751	6752	6753	6754	7663
				7751	7858	9537	9547	9548	9559	9582	9583	9606	9617
	R-PF			3653 #	4145	4154	4159	4164	4220	4229	4234	4239	4244	4249	4254
				4259	4266	4271	4276	4281	4286	4291	4296	4301	4416	4426	4436
				4445	4455	4461	4471	4481	4491	4512	4522	4532	4542	5637	5649
	RPW			3656 #	4152	4157	4162	4167	4205	4226	4227	4231	4232	4237	4242
				4247	4252	4257	4262	4268	4269	4273	4274	4279	4284	4289	4294
				4299	4304	4418	4419	4428	4429	4442	4443	4447	4448	4463	4464
				4473	4474	4483	4484	4493	4494	4514	4515	4524	4525	4534	4535
				4544	4545	4717	4718	4719	4720	4721	4722	4723	4724	4732	4733
				4734	4735	4736	4737	4738	4739	5639	5640	5651	5652
	RW			3654 #	5664	5665	5696	5697	5769	5770	5774	5775	6044	6045	6049
				6050	6054	6055	6059	6060	6117	6118	6122	6123	6146	6147	6151
				6152	7662	7750	9556	9603	9605	9614	9616
	S			3672 #	4152	4157	4162	4167	4227	4232	4237	4242	4247	4252	4257
				4262	4269	4274	4279	4284	4289	4294	4299	4304
	SJC-			3697 #	4674	4683	4701	4717	4732	4749	4764	4779
	SJCA			3701 #	4678	4687	4705	4721	4736	4753	4768	4783
	SJCE			3699 #	4676	4685	4703	4719	4734	4751	4766	4781	5161
	SJCG			3704 #	4681	4690	4708	4724	4739	4756	4771	4786	5167
	SJCGE			3702 #	4679	4688	4706	4722	4737	4754	4769	4784	4794	5165
	SJCL			3698 #	4675	4684	4702	4718	4733	4750	4765	4780	4795	5160
	SJCLE			3700 #	4677	4686	4704	4720	4735	4752	4767	4782	5162
	SJCN			3703 #	4680	4689	4707	4723	4738	4755	4770	4785	5166
	TC-			3688 #	4600	4601	4609	4610
	TCA			3690 #	4604	4605	4613	4614
	TCE			3689 #	4602	4603	4611	4612
	TCN			3691 #	4606	4607	4615	4616
	TN-			3680 #
	TNA			3682 #	4568	4569	4577	4578
	TNE			3681 #	4566	4567	4575	4576
	TNN			3683 #	4570	4571	4579	4580	4806
	TO-			3692 #	4618	4619	4627	4628
	TOA			3694 #	4622	4623	4631	4632
	TOE			3693 #	4620	4621	4629	4630
	TON			3695 #	4624	4625	4633	4634
	TZ-			3684 #	4582	4583	4591	4592
	TZA			3686 #	4586	4587	4595	4596
	TZE			3685 #	4584	4585	4593	4594
	TZN			3687 #	4588	4589	4597	4598
	W			3651 #	4156	4161	4166	4236	4241	4246	4251	4256	4261	4278	4283
				4288	4293	4298	4303	4387	4388	4457	4458	9534	9535	9545	9546
				9557	9580	9581	9592	9593	9604	9608	9615	9619
(U) MAJVER			1931 #
(U) MARK			1925 #
(U) MBOX CTL			2175 #	10590	10592	10594
	CLR PT LINE		2180 #	10191
	CLR PT LINE(NK)		2178 #	10197
	NORMAL			2184 #	10193	10199
	PT DIR CLR		2183 #	10182
	PT DIR CLR(NK)		2179 #	10186
	PT DIR WR		2181 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-58
; 							Cross Reference Listing					

	PT WR			2182 #	10099	10824
	SET IO PF ERR		2177 #	11041
	SET PAGE FAIL		2176 #
(U) MEM				1756 #
	A RD			1760 #	3962	3963	3964	3965	3975	3976	8478	8479	8480	8481	8496
				8497
	AD FUNC			1769 #	10866
	ARL IND			1758 #	4193	4194	5140	5141	5356	5357	5513	5519	5524	5571	5576
				5579	5779	5784	6103	6104	6107	6316	6413	6462	6583	6586	6644
				6648	6668	6670	6921	6923	6951	7056	7087	7316	7323	7424	7534
				7891	7982	7989	8041	8068	8510	8707	8708	8734	8868	8918	8928
				8969	8971	9003	9005	9044	9048	9051	9091	9151	9174	9192	9222
				9284	9333	9362	9374	9375	9388	9417	9488	9493	9522	9767	9815
				10101	10147	10160	10173	10226	10243	10289	10483	10566	10577	10597	10632
				10633	10634	10722	10725	10726	10741	10823	11066
	B WRITE			1761 #	4172	4181	4313	4315	4318	4321	4325	4329	4338	4339	4345
				4346	4352	4353	4359	4360	4413	4423	4433	4452	4468	4478	4488
				4498	4508	4519	4529	4539	4549	4559	4990	5644	5657	5689	5706
				5797	5799	5804	6512	6516	9635	9826	10174
	EA CALC			1770 #	3953	3955	3957	3959	3977	3979	5023	5028	5069	5071	5095
				7558	7559	7566	7568	7672	7676	7681	7760	7764	7774	8148	8150
				8152	8154	8161	8163	8182	8184	8187	8189	8196	8199	8216	8218
				8293	8297	8299	8301	8303	8306	8307	8308	8309	8317	8318	8319
				8321	8473	8474	8475	8476	8498	8499	9414	9418	9442
	FETCH			1762 #	4655	4658	4663	4694	4712	4760	4775	4791	4799	4938	8743
				8803	9639	9641	9643
	IFET			1773 #	3825	4058	4091	4093	4099	4101	4191	4198	4381	4861	4890
				4914	4918	4996	5033	5038	5081	5082	5114	5120	5145	5353	5411
				5518	5529	5548	5567	5582	5605	5630	5849	5858	5978	6019	6385
				6386	6741	7090	7216	7218	7219	7274	7282	7286	7477	7483	7491
				7573	7623	7684	7704	7705	7706	7707	7708	7709	7710	7712	7718
				7719	7720	7721	7722	7723	7725	7731	7732	7733	7737	7738	7866
				7895	7970	7991	7999	8681	8894	8970	9032	9335	9650	10392
	LOAD AR			1764 #	4819	4833	4892	4902	4903	5278	5995	7271	7312	7394	7408
				7463	7475	7484	7493	7600	7616	7979	8085	8408	8561	8774	8973
				9041	9105	9137	9285	9286	9359	9363	9400	9402	9465	9517	9658
				10310	10311	10367	10368	10441	10442	10446	10447	10622	10623	10641	10646
				10661	10683	10708	10732	10767	11014
	LOAD ARX		1765 #	3932	4374	5031	5276	5286	5338	5934	6542	6758	7292	7397
				7996	10313	10358	10359	10361	10364
	MB WAIT			1759 #	3938	4002	4003	4036	4047	4069	4080	4092	4096	4118	4183
				4377	4378	4398	4904	4905	4908	4920	4987	5035	5040	5046	5046
				5074	5075	5077	5078	5093	5094	5339	5342	5352	5365	5943	5945
				6033	6035	7272	7300	7313	7396	7410	7467	7469	7489	7495	7563
				7564	7571	7574	7614	7621	7624	7670	7674	7678	7735	7758	7762
				7766	7772	7786	7786	7811	7812	7813	7814	7815	7816	7817	7819
				7825	7826	7827	7828	7829	7830	7832	7838	7839	7840	7844	7845
				8086	8115	8157	8158	8192	8193	8291	8294	8326	8327	8337	8339
				8362	8362	8365	8409	8502	8503	8650	8688	8775	8977	9046	9107
				9218	9291	9419	9434	9447	9467	9472	9508	9516	9634	9638	9640
				9647	9654	9730	10125	10207	10268	10293	10350	10381	10390	10391	10549
				10563	10627	10642	10662	10688	10703	10710	10733	10770	10968	11006	11016
	REG FUNC		1763 #	4011	4981	9729	9737	9794	9798	10114	10150	10168	10180	10562
	RESTORE VMA		1768 #	4008	8415	10560	10822
	RPW			1772 #	10342	10349
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-59
; 							Cross Reference Listing					

	RW			1771 #
	WRITE			1766 #	4211	4393	4396	4399	4728	4743	4868	4884	4888	5118	5119
				5129	5277	5284	5340	5350	5367	5369	7468	7496	7618	7791	7842
				8076	8079	8083	8088	8089	8091	8100	8103	8111	8112	8119	8123
				8127	8131	8274	8281	8374	9427	9443	9446	9503	9504	9505	9506
				9509	9655	9749	9802	10229	10292	10294	10351	10382	10383	10414	10415
				10451	10452	10561	10694	10809	11004	11011	11013
(U) MINVER			1932 #
(U) MQ				1662 #
	MQ SEL			1666 #	5728	5729	5730	5732	5745	5746	5747	5749	5931	6700	6732
				6898	6900	7011	7313	7410	7431
	MQ*.25			1665 #	5724	5734	5736	5738	5740	5752	5754	5756	5757	5762	5954
				5976	6693	6894	7004	7051
	MQ*2			1664 #	5839	5842	5847	5856	5872	5873	5874	5875	5876	5877	5878
				5879	5903	5904	5905	5906	5974	6014	6016	7927	8811
	MQM SEL			1667 #	6724	6899	6906	7031	7768	7967	8629
	SH			1663 #	4213	5358	5558	5565	5574	5576	5581	5619	5679	5701	5907
				5908	5909	5910	5947	5956	5962	6001	6024	6095	6098	6100	6135
				6383	6443	6576	6589	6591	6595	6613	6615	6630	6640	6647	6704
				6705	6711	6714	6716	6858	6882	6885	6890	6938	7016	7017	7023
				7026	7028	7403	7458	7461	7467	7492	7812	7813	7814	7815	7816
				7817	7820	7821	7822	7823	7826	7827	7828	7829	7830	7833	7834
				7835	7836	7864	7884	7966	8186	8362	8402	8466	8554	8665	8796
				8849	8991	9027	9214	9300	9302	9323	10111	10217	10265	10334
(U) MQ CTL			2046 #
	AD			2054 #	6906	7768	7967	8629
	MQ*.25			2052 #	6724	6899	7031
	MQ*2			2048 #	5728	5729	5730	5732	5745	5746	5747	5749	6700	6898	6900
				7011
	SH			2051 #
	0S			2050 #	5931	6732	7313	7410	7431
	1S			2053 #
(U) MREG FNC			2161 #
	LOAD CCA		2168 #	9729
	LOAD EBR		2173 #	10180
	LOAD UBR		2172 #	10114
	MAP			2174 #	4011	4981	10562
	READ EBR		2164 #	10168
	READ ERA		2165 #	9794
	READ UBR		2163 #	10150
	SBUS DIAG		2162 #	9798
	WR REFILL RAM		2166 #	9737
(U) MTR CTL			2185 #
	CLR E CNT		2188 #	10283
	CLR M CNT		2189 #	10284
	CLR PERF		2187 #	10282
	CLR TIME		2186 #	10281
	CONO MTR		2192 #	10250
	CONO TIM		2193 #	10106
	LD PA LH		2190 #	10239
	LD PA RH		2191 #
(U) NONSTD			1958 #
	OPTIONS			1962 #	9749
(U) PARITY			2244 #
(D) PARITY			2286 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-60
; 							Cross Reference Listing					

(U) PC FLAGS			2058 #
	AROV			2059 #	5615	5690	5707	5968	6377	7219
	DIV CHK			2065 #	5794
	FDV CHK			2066 #	6169	6680	6935
	FLOV			2060 #	7076	7081	7282	7440
	FPD			2061 #	7674	7679	7682	7762	7772	7775	8067	8291	8324	8993	9658
				9659
	FXU			2064 #	7080	7286	7441
	TRAP1			2063 #
	TRAP2			2062 #	5035	5040	5088	5089	5093
(U) PV				1965 #
	OPTIONS			1967 #	9749
(U) PXCT			1986 #	3927	3938	5428	8470
(U) SC				1712 #
	AR SHIFT		1715 #	5513	5532	5534	5553	5556	5572	6316	8051
	FE			1713 #	6836	6839	7768	7787	7789	7923	7925	8342	8369	10208	10822
	SCAD			1714 #	3881	3894	3896	4009	4096	4118	4380	4393	4396	4934	4936
				4950	5299	5342	5343	5516	5541	5544	5546	5561	5564	5577	5580
				5592	5596	5618	5620	5623	5632	5686	5706	5784	5797	5799	5803
				5938	5967	5985	5987	5991	6020	6021	6031	6082	6085	6093	6095
				6098	6102	6133	6136	6162	6261	6265	6267	6366	6380	6382	6438
				6444	6541	6561	6574	6592	6610	6613	6615	6631	6644	6650	6672
				6685	6699	6705	6782	6787	6837	6841	6855	6873	6903	6907	6928
				7000	7018	7052	7054	7058	7063	7070	7072	7074	7094	7314	7324
				7425	7429	7445	7667	7671	7672	7676	7683	7686	7689	7705	7706
				7707	7708	7709	7710	7713	7714	7715	7716	7719	7720	7721	7722
				7723	7726	7727	7728	7729	7755	7758	7760	7764	7790	7811	7812
				7813	7814	7815	7816	7817	7819	7820	7821	7822	7823	7825	7826
				7827	7828	7829	7830	7832	7833	7834	7835	7836	7838	7862	7864
				7882	7913	7967	7981	8020	8022	8023	8026	8029	8033	8035	8038
				8039	8043	8044	8067	8076	8079	8087	8093	8110	8113	8118	8122
				8126	8130	8149	8151	8162	8164	8337	8344	8371	8373	8380	8386
				8404	8467	8668	8673	8832	8858	8884	8891	8893	8918	8921	8937
				8940	8952	8954	8968	8998	9001	9027	9034	9107	9136	9174	9176
				9185	9203	9216	9222	9246	9273	9289	9291	9292	9301	9303	9304
				9311	9359	9361	9401	9470	9519	9521	9726	10104	10124	10126	10130
				10154	10165	10179	10218	10229	10271	10291	10341	10363	10425	10440	10445
				10450	10468	10475	10484	10562	10565	10591	10593	10596	10627	10724	10751
				10757	10758	10763	10768	10841	10849	10855	10990	10992	11077
(U) SCAD			1689 #
	A			1690 #	3881	3894	3896	4006	4008	4009	4047	4069	4096	4096	4118
				4118	4194	4380	4393	4396	4934	4936	4950	4950	4967	5342	5343
				5357	5516	5519	5522	5524	5572	5575	5623	5632	5680	5686	5702
				5706	5797	5799	5803	5810	5817	5820	5907	5908	5909	5910	5938
				5948	5963	5967	5987	6003	6018	6020	6031	6082	6085	6133	6135
				6165	6332	6380	6438	6444	6462	6502	6541	6553	6592	6640	6650
				6672	6677	6685	6699	6705	6732	6782	6785	6787	6789	6854	6855
				6873	6880	6885	6903	6907	6924	6928	6932	6991	7000	7018	7052
				7058	7063	7070	7072	7074	7094	7314	7321	7324	7395	7409	7425
				7427	7429	7445	7672	7676	7689	7699	7700	7705	7706	7707	7708
				7709	7710	7713	7714	7715	7716	7719	7720	7721	7722	7723	7726
				7727	7728	7729	7737	7738	7760	7764	7804	7806	7811	7812	7813
				7814	7815	7816	7817	7819	7820	7821	7822	7823	7825	7826	7827
				7828	7829	7830	7832	7833	7834	7835	7836	7838	7844	7845	7881
				7882	7913	7922	7925	7930	7931	7981	8019	8021	8023	8026	8029
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-61
; 							Cross Reference Listing					

				8033	8035	8038	8039	8041	8043	8044	8067	8077	8080	8087	8113
				8118	8122	8126	8130	8212	8275	8311	8312	8313	8314	8380	8404
				8505	8552	8560	8666	8668	8703	8796	8849	8851	8858	8884	8891
				8893	8921	8928	8937	8940	8940	8971	8998	8998	9001	9001	9034
				9107	9119	9136	9141	9160	9189	9192	9244	9248	9273	9292	9311
				9389	9401	9462	9470	9489	9493	9519	9521	9726	9796	10104	10124
				10126	10130	10154	10165	10179	10210	10218	10229	10271	10291	10341	10363
				10425	10440	10445	10450	10484	10543	10548	10562	10575	10627	10692	10724
				10787	10841	10841	10849	10893	10990	10992	11077
	A+1			1694 #	5724	5734	5736	5738	5740	5752	5754	5756	5757	5762	5784
				5954	5985	5991	5991	6103	6104	6107	6162	6451	6501	6648	6667
				6669	6697	6731	6891	6893	6920	6922	7008	7039	7969	8100	8103
				8110	8119	8123	8127	8131	8918	9027	9301	9303	9304	9334	10751
	A+B			1692 #	4958	5541	5544	5546	5561	5564	5577	5580	5590	5592	5596
				5614	5618	5620	5728	5729	5730	5732	5745	5746	5747	5749	5890
				5893	6094	6095	6102	6136	6138	6176	6179	6265	6267	6318	6366
				6382	6415	6449	6571	6574	6612	6613	6631	6644	6694	6696	6791
				6800	7005	7006	7054	7056	7291	7302	7309	7316	7674	7679	7681
				7686	7769	7787	7967	7982	8052	8088	8111	8112	8149	8151	8162
				8164	8291	8293	8344	8368	8379	8403	8411	8467	8468	8661	8673
				8832	8966	9004	9006	9176	9182	9198	9199	9257	9258	9261	9291
				9294	9345	9358	10116	10147	10360	10424	10468	10479	10576	10593	10595
				11066	11071
	A-1			1693 #	4961	5839	5842	5847	5856	5872	5873	5874	5875	5876	5877
				5878	5879	5903	5904	5905	5906	5997	6021	6418	6561	6700	7011
				7305	7927	8951	8953	10190	10196	10622	10623
	A-B			1695 #	5299	6093	6098	6261	6349	6353	6420	6422	6442	6446	6455
				6464	6610	6615	6701	6728	6837	6841	7013	7034	7300	7667	7671
				7683	7755	7758	7762	7772	7774	7789	7790	7862	7864	7884	7886
				7889	7891	7973	7975	7989	8024	8025	8027	8068	8076	8079	8083
				8085	8093	8101	8104	8106	8108	8280	8337	8370	8371	8373	8386
				8705	8707	8968	9174	9185	9203	9216	9222	9246	9331	9343	9367
				9370	9392	9452	9468	10475	10483	10753	10758	10768	10778	10779
	A-B-1			1691 #	7768	8040	8182	8185	8197	8200	8363	8844	8952	8954	10851
				10855
	AND			1697 #	7083	8568	9057	9121	9122	9126	9287	9289	9333	9359	9361
				9380	9395	10111	10343	10345	10356	10381	10406	10423	10463	10472	10565
				10591	10596	10628	10644	10671	10681	10688	10705	10718	10739	10741	10757
				10763	10769	10771	10812	10814
	OR			1696 #	8569	8852	9123	9124	9125	9127	9151	9374	9375	9522	10566
				10597	10710	10733	10781	10816	10821	10823
(U) SCADA			1698 #
	AR EXP			1701 #	6082	6085	6093	6133	6136	6162	6261	6318	6349	6353	6541
				6610	6644	7314	10841	10841	10849
	AR0-5			1700 #	5519	5522	7083	7667	7671	7755	7758	7862	7881	7882	7891
				7967	7969	7981	8068	8076	8079	8087	8093	8100	8101	8103	8104
				8106	8108	8110	8119	8123	8127	8131	8386	8707	9057	9119	9121
				9122	9123	9124	9125	9126	9127	9151	9174	9189	9222	9244	9287
				9333	9334	9345	9359	9361	9380	9395	9401	9468	9522	10111	10483
				10484	10565	10591	10692	10757	10763	10771	10787	10814	10823	11066	11071
	FE			1699 #	4961	5524	5577	5580	5724	5734	5736	5738	5740	5752	5754
				5756	5757	5762	5839	5842	5847	5856	5872	5873	5874	5875	5876
				5877	5878	5879	5903	5904	5905	5906	5954	6003	6094	6103	6104
				6107	6176	6179	6382	6415	6418	6420	6422	6442	6446	6449	6451
				6455	6464	6501	6502	6561	6612	6667	6669	6694	6696	6697	6700
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-62
; 							Cross Reference Listing					

				6701	6728	6731	6732	6891	6893	6920	6922	7005	7006	7008	7011
				7013	7034	7039	7054	7300	7302	7305	7309	7672	7676	7686	7689
				7760	7764	7790	7884	7886	7927	7973	7975	7989	8024	8025	8027
				8040	8041	8052	8067	8083	8085	8149	8151	8162	8164	8182	8185
				8197	8200	8280	8344	8363	8373	8568	8569	8666	8705	8832	8849
				8851	8852	8951	8952	8953	8954	8968	8971	9176	9185	9192	9248
				9291	9301	9303	9304	9331	9343	9358	9367	9370	9374	9375	9392
				9452	9493	10190	10196	10360	10424	10468	10479	10566	10575	10593	10595
				10597	10628	10671	10688	10710	10718	10733	10741	10753	10769	10779	10781
				10812	10816
	#			1702 #	3881	3894	3896	4006	4008	4009	4047	4069	4380	4393	4396
				4934	4936	5299	5342	5343	5516	5541	5544	5546	5561	5564	5572
				5575	5592	5596	5614	5618	5620	5623	5632	5680	5686	5702	5706
				5797	5799	5803	5810	5817	5820	5907	5908	5909	5910	5938	5948
				5963	5967	5987	6018	6031	6095	6098	6102	6135	6138	6165	6265
				6267	6332	6366	6380	6438	6444	6553	6571	6574	6592	6613	6615
				6631	6640	6650	6672	6677	6685	6699	6705	6782	6787	6837	6841
				6854	6855	6880	6885	6903	6907	6928	6932	7000	7018	7052	7058
				7063	7070	7072	7074	7094	7321	7324	7395	7409	7425	7429	7445
				7683	7699	7700	7706	7707	7708	7709	7710	7714	7715	7716	7720
				7721	7722	7723	7727	7728	7729	7737	7738	7769	7787	7789	7804
				7806	7812	7813	7814	7815	7816	7817	7820	7821	7822	7823	7826
				7827	7828	7829	7830	7833	7834	7835	7836	7844	7845	7889	7913
				7922	7925	7930	7931	8019	8021	8023	8026	8029	8033	8035	8038
				8039	8043	8044	8077	8080	8113	8118	8122	8126	8130	8212	8275
				8311	8312	8313	8314	8337	8370	8371	8380	8404	8467	8505	8552
				8560	8668	8703	8796	8858	8884	8891	8893	8921	8937	8940	8940
				8998	8998	9001	9001	9034	9107	9136	9141	9160	9203	9216	9246
				9273	9289	9292	9294	9311	9462	9470	9519	9521	9726	9796	10104
				10124	10126	10130	10154	10165	10179	10210	10218	10229	10271	10291	10341
				10343	10345	10356	10363	10381	10406	10423	10425	10440	10445	10450	10463
				10472	10475	10543	10548	10562	10596	10627	10644	10681	10705	10724	10739
				10778	10821	10893	10990	10992	11077
(U) SCADA EN			1703 #
	0S			1704 #	4096	4096	4118	4118	4194	4950	4950	4958	4967	5357	5590
				5728	5729	5730	5732	5745	5746	5747	5749	5784	5890	5893	5985
				5991	5991	5997	6020	6021	6462	6648	6785	6789	6791	6800	6873
				6924	6991	7056	7291	7316	7427	7674	7679	7681	7705	7713	7719
				7726	7762	7768	7772	7774	7811	7819	7825	7832	7838	7864	7982
				8088	8111	8112	8291	8293	8368	8379	8403	8411	8468	8661	8673
				8844	8918	8928	8966	9004	9006	9027	9182	9198	9199	9257	9258
				9261	9389	9489	10116	10147	10576	10622	10623	10751	10758	10768	10851
				10855
(U) SCADB			1706 #
	AR0-8			1709 #	4958	6366	6791	6800	7291	8568	8569	8661	8844	8966	9289
				9294	10596	10628	10644	10681	10705	10718	10739	10821
	AR6-11			1708 #	7674	7679	7681	7762	7772	7774	7864	7884	7886	7891	7967
				7989	8068	8083	8085	8280	8291	8293	8411	8673	8705	8707	9174
				9182	9185	9198	9199	9203	9222	9246	9257	9258	9261	9468	10343
				10345	10356	10381	10406	10423	10463	10472	10483	11066	11071
	SC			1707 #	5299	5541	5544	5546	5561	5564	5577	5580	5590	5592	5596
				5614	5618	5620	5728	5729	5730	5732	5745	5746	5747	5749	5890
				5893	6093	6094	6095	6098	6102	6136	6138	6261	6265	6267	6318
				6571	6574	6610	6612	6613	6615	6631	6644	6701	6728	6837	6841
				7013	7034	7054	7056	7316	7683	7686	7768	7769	7787	7789	7790
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-63
; 							Cross Reference Listing					

				7889	7973	7975	7982	8040	8052	8088	8111	8112	8149	8151	8162
				8164	8182	8185	8197	8200	8337	8344	8363	8368	8370	8371	8373
				8379	8403	8467	8468	8832	8952	8954	9004	9006	9151	9176	9216
				9291	9333	9345	9374	9375	9522	10116	10147	10468	10475	10565	10566
				10576	10593	10595	10597	10741	10753	10757	10758	10763	10768	10771	10778
				10814	10823	10851	10855
	#			1710 #	6176	6179	6349	6353	6382	6415	6420	6422	6442	6446	6449
				6455	6464	6694	6696	7005	7006	7083	7300	7302	7309	7667	7671
				7755	7758	7862	8024	8025	8027	8076	8079	8093	8101	8104	8106
				8108	8386	8852	8968	9057	9121	9122	9123	9124	9125	9126	9127
				9287	9331	9343	9358	9359	9361	9367	9370	9380	9392	9395	9452
				10111	10360	10424	10479	10591	10671	10688	10710	10733	10769	10779	10781
				10812	10816
(U) SH				1722 #	3953	3955	3957	3959	3962	3963	3964	3965	3976	3979	7672
				7676	7760	7764	8150	8154	8161	8163	8184	8189	8196	8199	8216
				8218	8297	8299	8301	8303	8306	8307	8308	8309	8318	8321	8473
				8474	8475	8476	8478	8479	8480	8481	8497	8499
	AR			1724 #	4211	4213	4213	4958	5358	5589	5591	5595	5609	5679	5701
				5796	5798	5809	5907	5908	5909	5910	5937	5956	5962	6024	6095
				6098	6101	6104	6135	6332	6367	6568	6570	6573	6630	6640	6647
				6676	6711	6714	6716	6836	6838	6840	6854	6882	6885	6890	6932
				6938	6992	7023	7026	7028	7057	7072	7074	7276	7314	7411	7425
				7432	7458	7461	7467	7492	7528	7667	7670	7673	7677	7686	7755
				7758	7761	7765	7881	7966	7981	8042	8044	8067	8086	8093	8343
				8362	8380	8402	8404	8411	8466	8662	8665	8665	8698	8736	8770
				8796	8796	8814	8816	8868	8927	8991	8993	9027	9035	9108	9108
				9182	9188	9199	9214	9240	9243	9258	9261	9290	9362	9388	9395
				9488	9823	10124	10126	10154	10217	10255	10265	10334	10363	10388	10405
				10407	10425	10465	10484	10629	10718	10751	10990	10992	11094
	AR SWAP			1726 #	4172	4318	4321	4324	4325	4328	4329	4345	4346	4352	4353
				4658	4661	4906	4909	4931	5129	5140	5141	5362	5406	6081	6132
				6161	7422	7534	7552	7553	7606	7607	7931	8412	9048	9110	9110
				9632	9748	9767	9773	9815	9821	10101	10160	10168	10174	10237	10243
				10258	10270	10577	10582	10582	10767	10812	10842	11054
	ARX			1725 #	3924	4005	4849	4867	5034	5039	5041	5146	5363	5368	5567
				5576	5582	5613	5617	5973	5985	5990	6001	6176	6412	6585	6588
				6596	6613	6615	6703	6796	6801	7015	7291	7322	7403	7476	7478
				7812	7813	7814	7815	7816	7817	7820	7821	7822	7823	7826	7827
				7828	7829	7830	7833	7834	7835	7836	7864	7884	7914	7975	7977
				7983	8090	8209	8328	8471	8504	8508	8511	8513	8516	8554	8744
				8800	8849	8859	8931	8952	8954	8978	9051	9152	9190	9195	9251
				9284	9285	9286	9300	9302	9311	9315	9323	9344	9363	9393	9441
				9449	9503	9504	9505	9506	9774	10103	10240	10257	10289	10423	10532
				10533	10548	10579	10598	10640	10671	10680	10758	10763	10822	10850	10853
				10855	11010	11060	11082	11086
	SHIFT AR!ARX		1723 #	4095	4125	4399	4935	5299	5299	5344	5349	5520	5522	5541
				5546	5558	5563	5565	5574	5579	5581	5594	5619	5625	5630	5631
				5689	5816	5819	5947	5972	5974	5975	5978	5986	5988	5992	6003
				6032	6100	6103	6106	6381	6383	6387	6443	6445	6576	6587	6589
				6591	6595	6597	6701	6704	6705	6728	6740	6842	6847	6849	6858
				7013	7016	7017	7034	7059	7071	7088	7216	7218	7427	7430	7688
				7699	7700	7788	7790	7790	7791	7804	7806	7921	7923	8045	8110
				8342	8350	8369	8372	8372	8374	8674	8861	8895	8920	9036	9119
				9274	9292	9292	9309	9728	10111	10139	10172	10294	10300	10302	10304
				10306	10337	10356	10426	10454	10632	10642	10643	10722	10726	10994	11078
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-64
; 							Cross Reference Listing					

(U) SKIP			1785 #
	AC REF			1826 #
	AC#0			1793 #	4094	4713	7862
	AD CRY0			1803 #	4968	5023	5028	5073	5092	5408	5997	6015	6016	6177	6264
				6266	6412	6439	6685	6710	7000	7022	7461	7471	7547	8771	8822
				8869	8933	8936	8967	9025	9044	9097	9477	9518	10430	10777
	AD#0			1805 #	5516	5611	5686	5802	6552	6598	6626	6769	6868	7021	7214
				7217	7273	7423	7489	7894	7922	7924	8413	10456	10669	10757
	AD0			1804 #	5416	5418	5704	5891	5894	5964	5992	6166	6559	6676	6771
				6775	6781	6818	6829	6831	6874	6878	6886	6914	6915	6932	6944
				6950	6988	6992	7280	7284	7419	7444	7459	8409	8662	8671	8867
				8920	10104
	ADX0			1802 #	7570	7620	9048
	AR0			1792 #	4175	4349	4356	5410	5527	5786	5818	5821	6332	6645	6764
				6768	6942	7276	7307	7947	7981	8087	8165	8686	8736	9189	9244
				10841	10849	11091	11100
	AR18			1791 #	4335	4342	5514	5532	5534	5553	5556	5572	8510	9131	10112
				10782
	ARX0			1790 #	7990	8041	8797	9107	9296	9314	9339
	BR0			1789 #	6018	6168	6261	6679	6934	6951	8691	8813	8992
	EVEN PAR		1788 #
	FETCH			1810 #
	INTRPT			1818 #	3954	3956	3958	3960	3978	3980	4962	4979	4994	5273	7469
				7495	7568	7618	8153	8155	8188	8190	8217	8219	8298	8300	8302
				8304	8320	8322	8473	8474	8475	8476	8498	8499	8704	8753	8832
				9035	9176	9360	9364	9695	10627	10711	10893
	IO LEGAL		1821 #	4820	4821	4827	4829	4987	9628	9634	9727	9734	9736	9741
				9745	9768	9771	9797	9816	9819	10098	10102	10130	10161	10164	10216
				10232	10235	10244	10247	10256	10948
	KERNEL			1811 #	4817
	P!S XCT			1822 #	7534	7559
	PC SEC0			1798 #	4849	4850	5025	5076	5079	5114	5116	5139	5269	8626	9187
				9188	9242	9243	9284	9324	9356	9358	9390	9448	9490
	PI CYCLE		1815 #	9640	9643	9656	10973
	PUBLIC			1813 #
	RPW REF			1814 #	10543	10548
	RUN			1820 #
	SC0			1794 #	6025	7684	7864	7865	7882	8971
	SCAD#0			1801 #	5519	5522	5590	8102	8105	8107	8109	8468	8569	10116	10343
				10345	10381	10406	10644	10682	10692	10706	10739	10742	10769	10771	10787
				10813	10815
	SCAD0			1800 #	5542	5545	5547	5561	5564	5577	5580	5592	5597	5614	6003
				6093	6096	6098	6102	6349	6353	6366	6382	6572	6574	6610	6614
				6616	6837	7301	7687	7884	7886	7891	8024	8025	8027	8183	8185
				8197	8200	8337	8344	8364	8386	8707	9216	9295	9331	9343	9368
				9371	9392	9452	9468	10476	10483
	USER			1812 #	4865	4882	4907	4908	4913	5359	5424	10726	11008
	-EBUS GRANT		1816 #	9691
	-EBUS XFER		1817 #	9681
	-LOCAL AC ADDR		1807 #	4191	4198	5271	5335
	-MTR REQ		1827 #	4003	4959	5285	5300	5425	8158	8193	8327	8503
	-START			1819 #	4957
	-VMA SEC0		1824 #	7678	7766	7883	7976	8081	8294
(U) SP MEM			2129 #
	CACHE INH		2136 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-65
; 							Cross Reference Listing					

	EPT			2150 #	10310	10367	10414	10622
	EPT CACHE		2151 #	8408
	EPT EN			2135 #
	EPT FETCH		2152 #	10358	10359	10364
	EXEC			2132 #
	FETCH			2130 #
	PT			2155 #	3932	5276
	PT FETCH		2156 #
	SEC 0			2133 #
	UNCSH+UNPAGE		2137 #
	UNPAGED			2149 #	10349	10351	10442	10447	10452	10641	10646	10661	10683	10703	10708
				10732	10767
	UNPAGED+CACHED		2138 #
	UPT			2153 #	5338	5340	10311	10623	11004
	UPT EN			2134 #
	UPT FETCH		2154 #
	USER			2131 #
(U) SPEC			1901 #
	AD LONG			1921 #	4379	4395	5841	5841	5844	5844	5889	5892	5943	5945	5955
				5996	5999	6030	6034	6262	6262	6388	6549	6549	6639	6685	6697
				6731	6743	6743	6766	6766	6884	6939	6939	7000	7008	7039	7053
				8406	8878	8879	8892	8936	8937	8937	8951	8994	9034	9056	9095
				9464	10228	10291
	ARL IND			1916 #	4199	4199	4378	4394	4867	4882	5074	5077	5146	5349	5359
				5524	5935	6082	6085	6093	6133	6136	6162	6162	6261	6318	6541
				6541	6610	6759	7009	7272	7396	7428	7917	7917	7920	7977	7983
				8085	8379	8403	8666	8851	9748	10240	10250	10575	10576	11008	11010
	CLR FPD			1906 #	4112	7686	7688	7704	7712	7718	7725	7731	7732	7733	7791
				7839	7840	7866	8115	9030
	FLAG CTL		1918 #	3822	4813	4854	4858	4904	4905	4908	4910	4911	4937	4940
				4948	5372	9651	10391	11015
	GEN CRY18		1909 #	4193	4799	5346	5356	6439	9643	9655	10135
	INH CRY18		1903 #	5409	9184	9193	9241	9245	9249	9332	9369
	LOAD PC			1907 #	4951
	MQ SHIFT		1904 #	5974	5976	6014	6016	6693	7004	7051	8811
	MTR CTL			1917 #	10233	10245	10257	10258	10259	10273	10274	10275	10276
	SAVE FLAGS		1919 #	4821	4832	5025	5114	5116	5365
	SCM ALT			1905 #	5513	5532	5534	5553	5556	5572	6316	6836	6839	7768	7787
				7789	7923	7925	8051	8342	8369	10208	10822
	SP MEM CYCLE		1920 #	3932	5276	5338	5340	8408	10310	10311	10349	10351	10358	10359
				10364	10367	10414	10442	10447	10452	10622	10623	10641	10646	10661	10683
				10703	10708	10732	10767	11004
	STACK UPDATE		1914 #	5023	5028	5073	5091	5407
	XCRY AR0		1908 #	4091	4124	5411	5594	5595	5606	5686	5690	5816	5819	5966
				6031	6100	6101	6333	6354	6381	6570	6573	6838	6840	7057	7404
				7411	7425	8042	8044	8894	9189	9244
(U) SPEC INSTR			2077 #	10656
	CONS XCT		2086 #	4967
	CONT			2087 #	4961
	HALTED			2085 #	4950
	INH PC+1		2080 #	3933
	INSTR ABORT		2084 #	10288	10994
	INTRPT INH		2083 #
	KERNEL CYCLE		2079 #
	PXCT			2082 #	5427
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-66
; 							Cross Reference Listing					

	SET PI CYCLE		2078 #	3880	10278	10334
	SXCT			2081 #
(U) SWITCH%
	ADJBP			1176
	BACK.BLT		1186	7538	7543	7546	7578	7588
	BIG.PT			1244	10115	10117	10119	10185	10187	10194	10200	10670	10673	10676	10687
				10689	10691	10740	10743	10745	10748	10750	10752	10754
	BLT.PXCT		10	1133	3084	3086	3088	3435	3438	3485	3487	3489	7530	7533
				7537	7597	7626
	CST.WRITE		1243	10780	10784	10786
	DBL.INT			1173	5915	5920	6036
	DDT.BUG			1245	9744	9751	9764
	DIAG.INST		1229	1231	1233	5379	5390	5395	5432	5442	5454
	EPT540			5	1226	10608	10614	10620
	EXTEND			1170	6356	6369	6371	8420	8585	8596	8640	8643	8760	8763	8833
				8836	8900	8903	9058	9061	9264	9267	9524
	EXTEXP			12	3883	3889	3891	3939	3942	3947	5182	5191	5200	5247	5250
				5443	5453	6748	6862	6865	6909	6912	6952	6955	7095	7098	7221
				7229	7246	7250	7252	7449
	FPLONG			9	1115	6063	6071	6077	6125	6130	6154	6159	6195	6253	6272
				6275	6278	6282	6311	6473	6475	6477	6483	6491	6507	6509	6511
				6513	6515	6517	6525	6527	6529
	GFTCNV			17	1246	6859	6861	7099	7188	7255	7260	7265	7327	7393
	IMULI.OPT		1149	5668	5671	5678	5682	5685
	INSTR.STAT		1210	1212	1215	1973	1975	1977	9780	9784	9792	9855	10087
	IPA20			16	1241	9645	9648	10346	10348	10352
	KLPAGE			8	1122	1139	1145	1147	1945	1947	1949	2389	2392	2425	2427
				2448	2450	2534	2536	2550	2556	2687	2689	2710	2712	2744	2749
				2804	2806	2841	2843	3181	3184	3198	3200	3237	3240	3261	3265
				3268	3270	3459	3461	3490	3492	3494	3496	3498	3500	3502	3505
				3509	3518	3539	3541	3577	3579	3583	3585	3595	3597	3607	3609
				3616	3618	3620	4983	4985	4991	4998	10092	10096	10100	10488	10878
				10888	10920	10922	10924	10945	10949	10965	10974	10988	11019	11026	11051
				11053
	LONG.PC			6	1224	1952	1954	1956	4818	4822	4826	4831	4834	4837	4864
				4927
	MODEL.B			7	1129	1140	1143	1152	1155	1164	1166	1679	1682	1684	1732
				1738	1767	1774	1781	1795	1797	1799	1806	1808	1823	1825	1855
				1857	1868	1870	1873	1897	1899	1910	1913	1915	1966	1968	1970
				1985	2003	2010	2013	2099	2127	2167	2169	2171	2253	2255	2314
				2317	2320	2327	2329	2331	2333	2335	2337	2507	2509	2770	2772
				2775	2779	2783	2786	2788	2790	2792	2795	2799	2803	2856	2858
				2860	2862	2864	2903	2909	2955	2958	2961	2971	2973	3051	3053
				3055	3069	3072	3074	3076	3078	3080	3082	3089	3091	3187	3191
				3201	3203	3228	3230	3316	3320	3354	3356	3358	3360	3406	3408
				3412	3414	3430	3446	3458	3462	3465	3468	3470	3475	3481	3483
				3613	3615	3621	3627	3629	3818	3821	3823	3981	3992	4013	4031
				4033	4035	4042	4044	4046	4053	4055	4057	4064	4066	4068	4075
				4077	4079	5015	5022	5029	5051	5067	5096	5327	5329	5331	5727
				5731	5744	5748	5782	5787	5792	5801	5806	6358	6361	6364	6551
				6554	6557	6575	6578	6581	6594	6601	6623	6633	6638	6661	6663
				6665	6686	6689	6692	6708	6717	6722	6735	6737	6739	6747	7450
				8432	8437	8442	8525	8532	8584	8825	8828	8831	8896	8898	9138
				9140	9142	9210	9213	9215	9225	9230	9237	9278	9282	9288	9316
				9319	9326	9406	9411	9457	9482	9484	9492	9498	9511	9514	9706
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-67
; 							Cross Reference Listing					

				9708	9710	9712	9714	9716	10132	10134	10137	10141	10146	10153	10369
				10372	10374	10375	10377	10394	10396	10398	10416	10418	10420	10438	10457
				10526	10528	10530	10832	10835	10837	10843	10845	10847	10879	10881	10883
				10955	10959	11032	11037	11046	11070	11072	11074	11079	11081
	MOS			9800	9801	9803	9812
	MULTI			13	1119	2139	2148	2157
	NOCST			14	1242	10668	10677	10679	10695	10702	10712	10756	10760	10762	10798
				10808	10825
	NONSTD			1235	1959	1961	1963
	OP.CNT			1097	1194	1196	3905	3908	9907	9951
	OP.TIME			1100	1198	1200	3909	3912	9953	9983
	OWGBP			15	1240	3884	3886	3888	3943	3946	8384	8416	8541	8548	8572
				8597	8639	8648	8654	8660	8769	8777	8788	8840	8845	8847	8915
				8922	8926	9270	9275	9277
	PAGCNT			1111	1217	1220	4952	4955	10107	10110	10534	10538	10542	10545	10547
				10552	10554
	PUSHM			1168	5444	5448	5452
	RPW			1179	3655	3657	3659
	SHIFT.MUUO		1124	5310	5312	5314	10967	10969	10971
	SMP			11	1236
	SNORM.OPT		3	1160	6424	6435	6456
	SO.CNT			1103	1202	1204	3913	3917	9987	10036
	SO2.CNT			1106	1206	1208	3918	3922	9986	10039	10086
	SXCT			1153	1156	3987	3989	3991	5431	5456	5493
	TRACKS			1093	1190	1192	3901	3904	9858	9860	9862	9867	9883	9889	9905
	TRXDEF			1222	2430	2433	2563	2572	2752	2754	3546	3557	3586	3588	3610
				3612
	WRTST			1183	3660	3662	3664	4147	4149	4151
	XADDR			4	2109	2111	2113	2875	2877	2880	3471	3474	3646	3648	3650
				3951	3972	3993	4000	4012	4189	4200	4221	4223	4225	4437	4439
				4441	4843	4847	4853	4871	4877	4886	4895	4901	4917	4919	4926
				5108	5113	5125	5132	5136	5143	5262	5268	5287	5304	5332	5376
				5401	5405	5413	6357	6365	6368	7247	7249	7455	7500	8238	8268
				8329	8427	8461	8519	8533	8540	8583	9177	9179	9181	9186	9204
				9224	9238	9263	9320	9322	9325	9347	9355	9372	9385	9387	9391
				9412	9416	9420	9425	9456	9485	9487	9491	9499	9501	9510	10950
				10954	10995	11003	11017	11058	11062
(U) TIME			1746 #
	2T			1749 #	4666	4667	4668	4962	7915	8000	8019	8021	8033	8039	8078
				8080	8083	8113	8118	8122	8126	8130	8275	8280	9094
	3T			1750 #	3927	4006	4037	4047	4059	4069	4080	4957	5046	5072	5075
				5078	5810	7786	7805	7807	8159	8160	8194	8195	8209	8210	8328
				8338	8363	8505	8774	9044	9096	9746	10191	10196	10593	10595	10719
	4T			1751 #
	5T			1752 #	3894	3896	4923	4989	5346	5348	9676	9679	9683	9696	9701
				9702	9735	9742	9769	9772	9773	9775	9817	9820	9822	9824	10103
				10135	10152	10162	10166	10170	10220	10221	10222	10223	10233	10236	10238
				10245	10248	10257	10258	10259	10264	10273	10274	10275	10276	10344	10404
				10549	10564
(U) U0				1562 #
(U) U21				1624 #
(U) U23				1630 #
(U) U42				1705 #
(U) U45				1711 #
(U) U48				1718 #
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-68
; 							Cross Reference Listing					

(U) U51				1739 #
(U) U73				1923 #
(U) VMA				1740 #	10866
	AD			1745 #	3879	4008	4861	4914	4918	5031	5033	5081	5082	5277	5353
				7271	7312	7394	7408	7463	7468	7475	7484	7493	7496	7600	7616
				7618	8508	8511	8513	8516	8772	8773	8973	9041	9105	9137	9285
				9286	9359	9363	9388	9400	9402	9414	9418	9465	9488	9504	9506
				9517	9658	9659	9729	10114	10180	10190	10196	10209	10229	10337	10364
				10373	10393	10397	10409	10419	10467	10474	10560	10640	10645	10655	10680
				10703	10706	10731	10763	10873
	LOAD			1743 #	3926	3928	3953	3955	3957	3959	3977	3979	5023	5028	5069
				5071	5095	5274	5283	5337	5339	7558	7559	7566	7568	7672	7676
				7760	7764	8148	8150	8152	8154	8161	8163	8182	8184	8187	8189
				8196	8199	8216	8218	8297	8299	8301	8303	8306	8307	8308	8309
				8317	8318	8319	8321	8407	8415	8473	8474	8475	8476	8497	8498
				8499	10266	10301	10303	10305	10307	10339	10340	10354	10616	10618	10822
				10968
	PC			1742 #	3825	4951	9174	9222	9470	10392
	PC+1			1744 #	3962	3963	3964	3965	3975	3976	4058	4091	4093	4099	4101
				4191	4198	4381	4655	4658	4663	4694	4712	4760	4775	4791	4799
				4890	4938	4996	5038	5411	5518	5548	5567	5582	5605	5630	5849
				5858	5978	6019	6385	6386	6741	7090	7216	7218	7219	7274	7282
				7286	7477	7483	7491	7573	7623	7684	7704	7705	7706	7707	7708
				7709	7710	7712	7718	7719	7720	7721	7722	7723	7725	7731	7732
				7733	7737	7738	7866	7895	7970	7991	7999	8681	8743	8746	8803
				8867	8885	8932	8970	9024	9312	9639	9641	9643	9650
	VMA			1741 #	4374	4398	4888	4892	4902	4903	5120	5145	5278	5286	5343
				5367	5369	5934	5989	6542	6758	7275	7397	7681	7774	7979	7996
				8085	8090	8293	8890	8892	9031	9442	9446	9509	9802	10292	10294
				10313	10432	11011	11013	11014
(U) VMAX			1733 #
	AD12-17			1737 #
	PC SEC			1735 #	5137	5142
	PREV SEC		1736 #	4866	4883	5360	10148	11009
	VMAX			1734 #
(U) #				1929 #	3881	3893	3894	3896	3924	3926	3928	3962	3963	3964	3965
				3975	3976	4006	4008	4009	4047	4069	4096	4108	4109	4113	4118
				4125	4380	4393	4396	4920	4934	4936	4984	4993	4997	5274	5283
				5299	5337	5339	5342	5343	5352	5373	5374	5375	5428	5516	5541
				5544	5546	5561	5564	5572	5575	5592	5596	5614	5618	5620	5623
				5632	5680	5686	5702	5706	5797	5799	5803	5810	5817	5820	5907
				5908	5909	5910	5938	5948	5963	5967	5987	6006	6018	6026	6031
				6066	6067	6068	6095	6098	6102	6135	6138	6165	6176	6179	6190
				6265	6267	6332	6349	6353	6366	6380	6382	6414	6415	6420	6422
				6438	6442	6444	6446	6449	6455	6464	6512	6516	6526	6553	6571
				6574	6592	6599	6600	6613	6615	6628	6631	6640	6650	6662	6672
				6677	6685	6688	6694	6696	6699	6705	6727	6742	6782	6787	6817
				6837	6841	6854	6855	6871	6880	6885	6903	6907	6918	6925	6928
				6933	6946	7000	7003	7005	7007	7018	7033	7052	7058	7061	7063
				7070	7072	7074	7083	7092	7094	7279	7281	7287	7301	7302	7309
				7320	7321	7324	7395	7401	7409	7414	7426	7429	7445	7457	7467
				7468	7483	7485	7494	7497	7498	7553	7563	7564	7565	7575	7607
				7614	7615	7625	7667	7671	7683	7699	7700	7706	7707	7708	7709
				7710	7714	7715	7716	7720	7721	7722	7723	7727	7728	7729	7737
				7738	7755	7758	7769	7787	7789	7792	7804	7806	7812	7813	7814
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page CRF-69
; 							Cross Reference Listing					

				7815	7816	7817	7820	7821	7822	7823	7826	7827	7828	7829	7830
				7833	7834	7835	7836	7844	7845	7862	7889	7913	7922	7925	7930
				7931	8019	8021	8023	8024	8025	8026	8027	8029	8033	8035	8038
				8039	8043	8044	8045	8051	8076	8077	8079	8080	8093	8101	8104
				8106	8108	8113	8118	8122	8126	8130	8212	8275	8311	8312	8313
				8314	8337	8370	8371	8380	8386	8404	8407	8467	8478	8479	8480
				8481	8496	8497	8505	8552	8560	8661	8668	8670	8674	8680	8682
				8693	8698	8699	8700	8703	8713	8737	8739	8745	8796	8799	8800
				8809	8844	8852	8858	8861	8884	8891	8893	8895	8921	8927	8937
				8940	8966	8968	8977	8994	8998	9001	9034	9037	9050	9053	9057
				9093	9107	9121	9122	9123	9124	9125	9126	9127	9136	9141	9160
				9203	9216	9246	9273	9274	9287	9289	9292	9294	9311	9313	9331
				9343	9357	9358	9359	9361	9367	9370	9380	9392	9395	9452	9462
				9469	9470	9519	9520	9521	9726	9796	10104	10111	10113	10124	10125
				10126	10130	10154	10165	10179	10207	10210	10218	10229	10266	10268	10271
				10279	10291	10301	10303	10305	10307	10339	10340	10341	10343	10345	10354
				10356	10360	10363	10381	10406	10423	10424	10425	10440	10445	10450	10463
				10472	10475	10479	10529	10531	10543	10548	10562	10591	10596	10616	10618
				10627	10644	10671	10681	10688	10705	10710	10724	10733	10739	10753	10769
				10778	10779	10781	10812	10816	10821	10867	10893	10897	10901	10905	10909
				10913	10917	10928	10932	10936	10940	10944	10953	10958	10968	10972	10990
				10992	11016	11045	11052	11077
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLD-1
; 							Location / Line Number Index
; Dcode Loc'n	0	1	2	3	4	5	6	7					

D 0000		5159	5160	5161	5162	5164	5165	5166	5167
D 0010		5169	5170	5171	5172	5174	5175	5176	5177
D 0020		5180	5181	5183	5184	5185	5186	5187	5188
D 0030		5189	5190	5201	5202	5203	5204	5205	5206
D 0040		5210	5211	5212	5213	5214	5215	5216	5217
D 0050		5218	5219	5220	5221	5222	5223	5224	5225
D 0060		5226	5227	5228	5229	5230	5231	5232	5233
D 0070		5234	5235	5236	5237	5238	5239	5240	5241

D 0100		5245	5246	6751	6752	5392	5393	6753	6754
D 0110		6534	6535	6536	6537	5921	5922	5923	5924
D 0120		4367	4368	6323	6324	4387	4388	6326	6327
D 0130		6276	6277	6279	7858	7662	7663	7750	7751
D 0140		6042	6043	6044	6045	6047	6048	6049	6050
D 0150		6052	6053	6054	6055	6057	6058	6059	6060
D 0160		6115	6116	6117	6118	6120	6121	6122	6123
D 0170		6144	6145	6146	6147	6149	6150	6151	6152

D 0200		4145	4146	4150	4152	4154	4155	4156	4157
D 0210		4159	4160	4161	4162	4164	4165	4166	4167
D 0220		5662	5663	5664	5665	5694	5695	5696	5697
D 0230		5767	5768	5769	5770	5772	5773	5774	5775
D 0240		5498	5499	5500	5501	5502	5503	5504	5505
D 0250		4205	4206	4794	4795	4805	4806	4974	4975
D 0260		5003	5004	5005	5006	5101	5102	5103	5104
D 0270		5637	5638	5639	5640	5649	5650	5651	5652

D 0300		4674	4675	4676	4677	4678	4679	4680	4681
D 0310		4683	4684	4685	4686	4687	4688	4689	4690
D 0320		4749	4750	4751	4752	4753	4754	4755	4756
D 0330		4701	4702	4703	4704	4705	4706	4707	4708
D 0340		4764	4765	4766	4767	4768	4769	4770	4771
D 0350		4717	4718	4719	4720	4721	4722	4723	4724
D 0360		4779	4780	4781	4782	4783	4784	4785	4786
D 0370		4732	4733	4734	4735	4736	4737	4738	4739

D 0400		4406	4407	4408	4409	4416	4417	4418	4419
D 0410		4426	4427	4428	4429	4436	4438	4442	4443
D 0420		4445	4446	4447	4448	4455	4456	4457	4458
D 0430		4461	4462	4463	4464	4471	4472	4473	4474
D 0440		4481	4482	4483	4484	4491	4492	4493	4494
D 0450		4501	4502	4503	4504	4512	4513	4514	4515
D 0460		4522	4523	4524	4525	4532	4533	4534	4535
D 0470		4542	4543	4544	4545	4552	4553	4554	4555

D 0500		4220	4222	4226	4227	4229	4230	4231	4232
D 0510		4234	4235	4236	4237	4239	4240	4241	4242
D 0520		4244	4245	4246	4247	4249	4250	4251	4252
D 0530		4254	4255	4256	4257	4259	4260	4261	4262
D 0540		4266	4267	4268	4269	4271	4272	4273	4274
D 0550		4276	4277	4278	4279	4281	4282	4283	4284
D 0560		4286	4287	4288	4289	4291	4292	4293	4294
D 0570		4296	4297	4298	4299	4301	4302	4303	4304

; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLD-2
; 							Location / Line Number Index
; Dcode Loc'n	0	1	2	3	4	5	6	7					

D 0600		4564	4565	4566	4567	4568	4569	4570	4571
D 0610		4573	4574	4575	4576	4577	4578	4579	4580
D 0620		4582	4583	4584	4585	4586	4587	4588	4589
D 0630		4591	4592	4593	4594	4595	4596	4597	4598
D 0640		4600	4601	4602	4603	4604	4605	4606	4607
D 0650		4609	4610	4611	4612	4613	4614	4615	4616
D 0660		4618	4619	4620	4621	4622	4623	4624	4625
D 0670		4627	4628	4629	4630	4631	4632	4633	4634

D 0700		9534	9535	9536	9537	9538	9539	9540	9541
D 0710		9545	9546	9547	9548	9549	9550	9551	9552
D 0720		9556	9557	9558	9559	9560	9561	9562	9563
D 0730		9567	9568	9569	9570	9571	9572	9573	9574
D 0740		9580	9581	9582	9583	9584	9585	9586	9587
D 0750		9592	9593	9594	9595	9596	9597	9598	9599
D 0760		9603	9604	9605	9606	9607	9608	9609	9610
D 0770		9614	9615	9616	9617	9618	9619	9620	9621
; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLU-1
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 0000		3822:	3825:	4172=	4037:	4048:	4059:	4070:	4081:
U 0010		4656=	4659=	4661=	4663=	4665=	4666=	4667=	4668=
U 0020		4112=	4113=	9638=	9639=	6478=	6482=	9640=	9641=
U 0030		3940 	6497=	7864=	7865=	4936=	6498=	4937=	7866=
U 0040		10893=	10897=	10901=	10905=	10909=	10913=	10917=	10921=
U 0050		10928=	10932=	10936=	10940=	10944=	10948=	10953=	10958=
U 0060		3932=	4091=	4092=	4094=	4095=	4096=	4099=	4101=
U 0070		4002=	4003=	4108=	4109=	3933=	3938=	4849=	4850=

U 0100		4181:	4183:	4191:	4211:	4214:	4313:	4315:	4199:
U 0110		4393=	4394=	4175=	4396=	4324=	4325=	6033=	6035=
U 0120		4377=	4378=	6137=	4380=	3941 	9658=	6138=	9659=
U 0130		5031=	4008 	5034=	5035=	6501=	6503=	4135:	4140:
U 0140		3880=	4009 	3882=	3890=	3893=	3894=	3896=	4011 
U 0150		9145=	6512=	3925=	3926=	9146=	6516=	3927=	3928=
U 0160		3954=	3956=	3958=	3960=	3962=	3963=	3964=	3965=
U 0170		3967=	3968=	3969=	3970=	3975=	3976=	3978=	3980=

U 0200		7668=	4178=	6081=	6083=	7671=	4318=	4321=	6085=
U 0210		5931=	5935=	4335=	5936=	7673=	7675=	7677=	7678=
U 0220		5276=	4118 	5277=	5279=	4193=	4194=	4967=	4968=
U 0230		5345=	5347=	4124 	5348=	7680=	7681=	7682=	7684=
U 0240		9647=	4125 	9650=	9651=	4381 	7704=	7705=	7706=
U 0250		7707=	7708=	7709=	7710=	7712=	7713=	7714=	7715=
U 0260		7716=	7718=	7719=	7720=	7721=	7722=	7723=	7725=
U 0270		7726=	7727=	7728=	7729=	7731=	7732=	7733=	7735=

U 0300		7756=	6543=	5681=	6544=	7759=	6759=	5686=	6760=
U 0310		6066=	6067=	6068=	6069=	7761=	7763=	7765=	7766=
U 0320		5728=	5729=	5730=	5732=	5734=	5736=	5738=	5740=
U 0330		5542=	5545=	5547=	5548=	4852=	4854=	7768=	7769=
U 0340		6678=	4398 	6679=	6680=	4399 	7811=	7812=	7813=
U 0350		7814=	7815=	7816=	7817=	7819=	7820=	7821=	7822=
U 0360		7823=	7825=	7826=	7827=	7828=	7829=	7830=	7832=
U 0370		7833=	7834=	7835=	7836=	7838=	7839=	7840=	7842=

U 0400		5703=	4342=	4328=	4329=	5704=	4349=	5706=	5707=
U 0410		5781=	5783=	5786=	5794=	4338=	4339=	5797=	5799=
U 0420		5745=	5746=	5747=	5749=	5752=	5754=	5756=	5757=
U 0430		6568=	4401 	5372=	5373=	6572=	6574=	5374=	5375=
U 0440		4857=	4858=	9654=	9655=	5271=	5273=	4402 	9656=
U 0450		6782=	6783=	6784=	6785=	5840=	5841=	5843=	5844=
U 0460		4006=	8917=	5848=	5849=	5274=	9272=	5851=	5852=
U 0470		5943=	8919=	5945=	6015=	5948=	9273=	4791 	6016=

U 0500		9628=	9629=	9632=	4356=	4345=	4346=	9634=	9635=
U 0510		9741=	9742=	9745=	9747=	4352=	4353=	4374=	9748=
U 0520		8866=	8867=	5857=	5858=	8869=	8870=	5860=	5861=
U 0530		5954=	6844=	4859 	6845=	5955=	6029=	5956=	6030=
U 0540		5872=	5873=	5874=	5875=	5876=	5877=	5878=	5879=
U 0550		6787=	4865 	6788=	6789=	4866=	4868=	6710=	6712=
U 0560		5610=	6850=	5611=	6851=	5889=	5891=	5892=	5894=
U 0570		6547=	6904=	6549=	6905=	6553=	6741=	6561=	6742=

; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLU-2
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 0600		4812:	4813:	4815:	4816:	4817:	4819:	4820:	4821:
U 0610		4827:	4828:	4829:	4830:	4832:	4833:	4838:	4839:
U 0620		6933=	4882 	6934=	6935=	4893 	4907:	4909:	4888:
U 0630		4861:	5979=	4862:	5980=	4890:	7080=	4902 	7081=
U 0640		6632=	4903 	4904 	7073=	6639=	4920 	6640=	7075=
U 0650		10113=	10114=	8752=	7076=	4922 	10116=	8753=	7078=
U 0660		8650=	4923 	8651=	7436=	4879=	4881=	4925 	7439=
U 0670		4883=	4885=	8652=	7440=	4910=	4911=	4931 	7441=

U 0700		4934:	4413=	4359=	4360=	9734=	9735=	9736=	9737=
U 0710		9782=	9794=	9797=	9798=	9768=	9769=	9771=	9772=
U 0720		5903=	5904=	5905=	5906=	5907=	5908=	5909=	5910=
U 0730		10482=	4938 	10483=	7699=	4913=	4914=	10484=	7700=
U 0740		5963=	4939 	7090=	7092=	5964=	4940 	5967=	5968=
U 0750		4916=	4918=	4951 	7805=	4958=	4959=	4984 	7807=
U 0760		7563=	7564=	7565=	7566=	4961=	4962=	4990 	7918=
U 0770		5046 	7614=	7615=	7616=	4988=	4989=	5073 	7920=

U 1000		5406:	4423=	5336:	5083 	5571:	5259:	5269:	4433=
U 1010		5290:	5292:	5294:	7226:	7228:	5092 	4947:	4948:
U 1020		4993=	4994=	5337:	5095 	6025=	5120 	6026=	6027=
U 1030		7978=	7980=	5139 	7981=	4996=	4997=	7968=	7970=
U 1040		6018=	6019=	5142 	6020=	5145 	6021=	6731=	6733=
U 1050		5146 	8020=	8022=	8023=	8024=	8025=	8026=	8027=
U 1060		6166=	5286 	6168=	6169=	5027=	5028=	6177=	6179=
U 1070		5338 	8100=	8102=	8103=	8105=	8107=	8109=	8110=

U 1100		10098=	10099=	10102=	10103=	7231:	7233:	7235:	7237:
U 1110		7239:	7241:	7243:	7245:	4979=	4981=	4452=	5339 
U 1120		6413=	6414=	6416=	6417=	6419=	6421=	6423=	6439=
U 1130		8157=	8158=	8159=	8160=	8111=	8112=	8114=	8115=
U 1140		8192=	8193=	8194=	8195=	6450=	6452=	6453=	6455=
U 1150		8616=	8617=	8618=	8619=	8149=	8151=	8153=	8155=
U 1160		5039=	5041=	6647=	6648=	5351 	7974=	6650=	7976=
U 1170		8626=	8627=	8628=	8629=	5076=	5079=	8162=	8165=

U 1200		9816=	4468=	4478=	9817=	9819=	4488=	4498=	9820=
U 1210		10131=	10136=	10138=	10139=	10161=	10162=	10165=	10166=
U 1220		6660=	6662=	5352 	6668=	6002=	6670=	6003=	6673=
U 1230		8698=	8699=	5353 	8700=	8183=	8186=	8188=	8190=
U 1240		6685=	6688=	6694=	6696=	6697=	6699=	6700=	6701=
U 1250		8744=	8745=	5358 	8746=	5081=	5082=	8198=	8201=
U 1260		5088=	5089=	5359 	8210=	5093=	5094=	5362 	8211=
U 1270		5118=	5119=	5363 	8212=	5123=	5124=	5365 	8213=

U 1300		10232=	10233=	10235=	10236=	10244=	10245=	10247=	10248=
U 1310		10253=	4508=	10256=	10257=	5023=	5025=	5069=	5071=
U 1320		6764=	6765=	6766=	6767=	6768=	6770=	6772=	6773=
U 1330		8940=	8942=	5367 	8943=	5140=	5141=	8217=	8219=
U 1340		8298=	8300=	8302=	8304=	8306=	8307=	8308=	8309=
U 1350		8311=	8312=	8313=	8314=	8317=	8318=	8320=	8322=
U 1360		8473=	8474=	8475=	8476=	8478=	8479=	8480=	8481=
U 1370		8483=	8484=	8485=	8486=	8496=	8497=	8498=	8499=

; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLU-3
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 1400		5114=	5116=	5129=	5137=	5514=	5516=	5533=	5534=
U 1410		5554=	5555=	6132=	6133=	6161=	6162=	6333=	6350=
U 1420		6883=	5369 	10404=	10405=	6884=	5408 	6886=	6887=
U 1430		8977=	8978=	5412 	8979=	5284=	5285=	8707=	8708=
U 1440		6894=	5556 	5564 	8921=	6898=	6899=	6900=	8927=
U 1450		9196=	5572 	9198=	9199=	5299=	5300=	8709=	8710=
U 1460		6917=	6918=	5580 	6921=	7398=	6923=	7399=	6924=
U 1470		5341=	5342=	5600 	8758=	5356=	5357=	5607 	8759=

U 1500		6354=	6367=	6317:	7862:	9727=	9728=	10216=	10218=
U 1510		4519=	4529=	4539=	4549=	4559=	4694=	4713=	4728=
U 1520		7001=	7003=	7005=	7007=	7008=	7010=	7012=	7013=
U 1530		8799=	8800=	8801=	5657 	5724 	8809=	8803=	8811=
U 1540		9214=	9217=	5762 	9218=	5360=	5361=	7039=	7052=
U 1550		9252=	5802 	9254=	9255=	5410=	5411=	8818=	8819=
U 1560		7928=	5811 	7930=	7932=	7938=	7939=	7947=	7966=
U 1570		9390=	9393=	5938 	9395=	7545=	8849=	7547=	8850=

U 1600		4743=	4760=	4775=	4790=	4799=	5644=	5656=	5960 
U 1610		9400=	9401=	5961 	9402=	5416=	5418=	8973=	8974=
U 1620		9490=	9493=	5972 	9494=	7990=	7991=	5973 	7996=
U 1630		9668=	9669=	5974 	9677=	7599=	9034=	7600=	9035=
U 1640		5424=	5425=	8067=	8068=	5427=	5428=	8069=	8070=
U 1650		10220=	10221=	10222=	10223=	8405=	9056=	8406=	9057=
U 1660		8963=	9120=	8964=	9121=	9463=	9122=	9464=	9123=
U 1670		9517=	9124=	9518=	9125=	10151=	9126=	10152=	9127=

U 1700		10301=	10303=	10305=	10307=	8076=	8078=	8079=	8081=
U 1710		5518=	5520=	5976 	9131=	5523=	5525=	5977 	9132=
U 1720		5528=	5529=	8274=	8276=	5559=	5561=	8277=	8278=
U 1730		5566=	5567=	5985 	9148=	5575=	5577=	5987 	9149=
U 1740		10431=	10432=	5989 	10433=	8292=	8293=	8294=	5991 
U 1750		5581=	5582=	5992 	9159=	5590=	5592=	6023 	9160=
U 1760		5594=	5597=	8509=	8510=	5605=	5606=	8512=	8514=
U 1770		10440=	6031 	10441=	10442=	5614=	5615=	6032 	10531:

U 2000		10169=	9295=	10171=	9296=	10314=	9297=	10315=	9298=
U 2010		10363=	9299=	10364=	9301=	10429=	9303=	10430=	9304=
U 2020		8599=	8600=	8601=	8602=	8603=	8604=	8605=	8606=
U 2030		10339=	10340=	10341=	10342=	10343=	10344=	10345=	10349=
U 2040		5618=	5620=	6086 	10358=	5624=	5625=	6092 	10359=
U 2050		5630=	5632=	6093 	10360=	5689=	5690=	6098 	10361=
U 2060		10445=	6107 	10446=	10447=	6110 	10367=	6135 	10368=
U 2070		10450=	6318 	10451=	10452=	6380 	10373=	6382 	10381=

U 2100		10388=	10389=	10390=	10391=	10392=	10393=	6387 	10397=
U 2110		10991=	10993=	6388 	10994=	5804=	5805=	6447 	10399=
U 2120		10407=	10408=	11044=	11045=	6526 	10409=	6588 	10410=
U 2130		5818=	5821=	5995=	5997=	5998=	5999=	6590 	10411=
U 2140		6005=	6007=	6094=	6096=	6591 	10414=	6593 	10415=
U 2150		6100=	6102=	6103=	6104=	6595 	10419=	6596 	10421=
U 2160		6190=	6191=	6261=	6262=	8668=	8669=	8670=	8671=
U 2170		6265=	6267=	6377=	6378=	6597 	10543=	10548=	10549=

; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLU-4
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 2200		6385=	6386=	6442=	6444=	6463=	6465=	6598 	10572=
U 2210		6577=	6583=	6586=	6587=	6599=	6600=	6610 	10573=
U 2220		8677=	8678=	6612=	6614=	8680=	8681=	8682=	6616 
U 2230		6625=	6626=	6627=	6628=	6715=	6716=	10583=	10591=
U 2240		8735=	8736=	8737=	6645 	6703 	9427=	8739=	9428=
U 2250		6727=	6728=	6775=	6776=	6704 	10617=	6706 	10619=
U 2260		6793=	6794=	6802=	6803=	6829=	6830=	8851=	8852=
U 2270		6832=	6833=	6838=	6839=	6709 	10622=	6724 	10623=

U 2300		6867=	6868=	6869=	6870=	6875=	6876=	8859=	8860=
U 2310		10629=	10630=	6743 	10632=	6781 	10633=	6790 	10634=
U 2320		6879=	6880=	8951=	8952=	6890=	6891=	8953=	8954=
U 2330		10719=	10720=	6792 	10724=	6795 	10725=	6796 	10727=
U 2340		6914=	6916=	6938=	6939=	6942=	6944=	8992=	8993=
U 2350		8999=	9002=	6797 	9004=	6799 	9006=	10192=	10193=
U 2360		6946=	6947=	6950=	6951=	9050=	9051=	9054=	6800 
U 2370		9092=	9093=	9094=	6801 	6995=	6997=	9095=	9096=

U 2400		7022=	7024=	7027=	7029=	9135=	9136=	9137=	9141=
U 2410		7033=	7034=	7197=	7198=	9183=	9185=	9187=	9188=
U 2420		7204=	7205=	7216=	7217=	9240=	9241=	9242=	9243=
U 2430		9312=	9313=	9314=	9315=	9324=	9435=	6804 	9436=
U 2440		7218=	7219=	7274=	7275=	6805 	9437=	6807 	9442=
U 2450		7277=	7278=	7281=	7282=	6816 	9453=	6817 	9454=
U 2460		9465=	6818 	9467=	9468=	9469=	9470=	9471=	9472=
U 2470		7285=	7286=	7304=	7305=	9503=	9504=	9505=	9506=

U 2500		7307=	7310=	7420=	7421=	9519=	9520=	6835 	9521=
U 2510		10179=	6836 	10180=	6837 	7424=	7426=	10182=	10186=
U 2520		7446=	7447=	7461=	7463=	7471=	7472=	10198=	10199=
U 2530		10273=	10274=	10275=	10276=	10277=	6840 	10779=	10782=
U 2540		10463=	10464=	6841 	10465=	10467=	10468=	6842 	10469=
U 2550		10473=	6846 	7475=	7477=	10474=	10476=	6847 	10477=
U 2560		7483=	7485=	7491=	7493=	7497=	7498=	7535=	7536=
U 2570		7558=	7559=	7570=	7571=	7573=	7575=	7620=	7621=

U 2600		7623=	7625=	7687=	7689=	7773=	7774=	7881=	7882=
U 2610		7883=	7884=	7887=	7889=	7892=	7893=	7895=	7913=
U 2620		7924=	7925=	7982=	7983=	7999=	8000=	8028=	8030=
U 2630		8034=	8035=	8038=	8039=	8043=	8044=	8084=	8085=
U 2640		8088=	8089=	8118=	8119=	8122=	8123=	8126=	8127=
U 2650		8130=	8131=	8281=	6848 	8326=	8327=	8338=	8339=
U 2660		8342=	8344=	8350=	8351=	8364=	8365=	8368=	8370=
U 2670		8386=	8387=	8389=	8390=	8411=	8412=	8414=	8415=

U 2700		8465=	8466=	8469=	8470=	8502=	8503=	8517=	8518=
U 2710		8570=	8571=	8663=	8664=	8688=	8689=	8692=	8693=
U 2720		8704=	8705=	8755=	8756=	8772=	8773=	8795=	8797=
U 2730		8815=	8816=	8830=	8832=	8842=	8843=	8875=	8876=
U 2740		8891=	8893=	8936=	8937=	8969=	8970=	9029=	9031=
U 2750		9036=	9037=	9041=	9042=	9046=	9047=	9109=	9111=
U 2760		9175=	9176=	9189=	9190=	9192=	9193=	9244=	9245=
U 2770		9248=	9249=	9285=	9286=	9308=	9309=	9333=	9334=

; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLU-5
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 3000		9344=	9345=	9711:	9717:	6849 	8552:	8556:	6852 
U 3010		8557:	8558:	8560:	7248:	7253:	6853 	9356=	9357=
U 3020		9360=	9361=	9368=	9371=	6854 	8553:	9374=	9375=
U 3030		9415=	9417=	8561:	6856 	6858 	8554:	9449=	9450=
U 3040		9680=	9681=	9691=	9692=	9694=	9695=	9720=	9721=
U 3050		9829=	9830=	10106=	10112=	10124=	10125=	10208=	10210=
U 3060		10227=	10228=	10266=	10267=	10290=	10292=	10382=	10383=
U 3070		10560=	10561=	10646=	10647=	10655=	10656=	6871 	8581:

U 3100		10672=	10680=	10684=	10685=	7254:	7256:	7257:	7258:
U 3110		7259:	7266:	7267:	7268:	10693=	10694=	10708=	10709=
U 3120		10742=	10746=	10751=	10753=	10759=	10763=	10772=	10778=
U 3130		10783=	10787=	10797=	10809=	10815=	10816=	10821=	10822=
U 3140		10842=	6872 	10850=	10851=	10854=	10856=	10968=	10972=
U 3150		11009=	11011=	11093=	11094=	6873 	6877 	6881 	6902 
U 3160		6906 	6908 	6913 	6925 	6926 	6927 	6929 	6987 
U 3170		6988 	6990 	6991 	6992 	7015 	7016 	7019 	8582:

U 3200		7021 	7031 	7036 	7037 	7038 	7054 	7055 	7056 
U 3210		7058 	7060 	7061 	7063 	7067 	7068 	7070 	7071 
U 3220		7084 	7086 	7087 	7094 	7214 	7220 	7271 	7272 
U 3230		7273 	7279 	7280 	7283 	7284 	7287 	7288 	7289 
U 3240		7290 	7292 	7301 	7312 	7313 	7314 	7315 	7316 
U 3250		7317 	7318 	7319 	7320 	7321 	7322 	7323 	7325 
U 3260		7395 	7396 	7400 	7401 	7403 	7404 	7405 	7409 
U 3270		7410 	7411 	7412 	7413 	7414 	7416 	7423 	7427 

U 3300		7428 	7429 	7430 	7431 	7432 	7443 	7457 	7459 
U 3310		7467 	7468 	7469 	7478 	7489 	7494 	7495 	7529 
U 3320		7534 	7552 	7553 	7554 	7568 	7606 	7608 	7618 
U 3330		7737 	7738 	7775 	7787 	7789 	7790 	7792 	7844 
U 3340		7845 	7894 	7915 	7922 	7984 	8040 	8041 	8045 
U 3350		8051 	8052 	8086 	8087 	8091 	8093 	8209 	8324 
U 3360		8328 	8371 	8373 	8374 	8379 	8380 	8401 	8402 
U 3370		8403 	8407 	8408 	8409 	8413 	8467 	8468 	8471 

U 3400		8505 	8568 	8569 	8607 	8608 	8609 	8611 	8620 
U 3410		8634 	8635 	8636 	8638 	8653 	8661 	8662 	8665 
U 3420		8666 	8667 	8673 	8674 	8675 	8686 	8691 	8712 
U 3430		8713 	8770 	8771 	8774 	8775 	8813 	8822 	8844 
U 3440		8848 	8853 	8855 	8856 	8857 	8858 	8861 	8862 
U 3450		8878 	8879 	8883 	8885 	8894 	8895 	8897 	8899 
U 3460		8929 	8930 	8931 	8933 	8956 	8965 	8966 	8968 
U 3470		8972 	8976 	8990 	8994 	9025 	9032 	9044 	9048 

U 3500		9097 	9105 	9107 	9147 	9151 	9152 	9203 	9223 
U 3510		9246 	9257 	9259 	9262 	9274 	9283 	9284 	9287 
U 3520		9290 	9291 	9292 	9331 	9335 	9339 	9358 	9362 
U 3530		9364 	9380 	9418 	9419 	9434 	9443 	9446 	9448 
U 3540		9455 	9477 	9508 	9509 	9516 	9522 	9643 	9684 
U 3550		9697 	9701 	9702 	9729 	9730 	9750 	9773 	9774 
U 3560		9775 	9802 	9822 	9824 	9825 	9826 	10104 	10126 
U 3570		10147 	10148 	10149 	10155 	10172 	10173 	10174 	10206 

; KL10 Microcode for TOPS-10 and TOPS-20		24 July 1985  V2(411)		MICRO %34(270)		Page LLU-6
; 							Location / Line Number Index
; Ucode Loc'n	0	1	2	3	4	5	6	7					

U 3600		10229 	10237 	10238 	10240 	10250 	10258 	10259 	10263 
U 3610		10264 	10268 	10271 	10278 	10279 	10281 	10282 	10283 
U 3620		10284 	10288 	10293 	10294 	10310 	10311 	10334 	10335 
U 3630		10337 	10350 	10351 	10354 	10356 	10406 	10423 	10424 
U 3640		10425 	10426 	10434 	10454 	10456 	10479 	10532 	10533 
U 3650		10539 	10550 	10562 	10563 	10564 	10565 	10566 	10574 
U 3660		10575 	10576 	10578 	10580 	10593 	10595 	10596 	10597 
U 3670		10598 	10627 	10640 	10641 	10642 	10643 	10645 	10661 

U 3700		10662 	10669 	10682 	10688 	10692 	10703 	10706 	10711 
U 3710		10731 	10732 	10733 	10739 	10757 	10768 	10770 	10813 
U 3720		10818 	10823 	10824 	10836 	10866 	10867 	10874 	10973 
U 3730		11004 	11005 	11007 	11008 	11013 	11014 	11015 	11016 
U 3740		11038 	11039 	11040 	11041 	11052 	11054 	11055 	11060 
U 3750		11061 	11066 	11067 	11069 	11071 	11075 	11077 	11078 
U 3760		11080 	11082 	11084 	11086 	11087 	11088 	11089 	11091 
U 3770		11095 	11096 	11098 	11100 				10529:

No errors detected
End of microcode assembly
337 pages of listing
Used 56.35 in 07:25.07
  Memory used: 129P
  Symbol table: 42P
  Text strings: 13P
  Loc'n assignment: 18P
  Cross reference: 49P
