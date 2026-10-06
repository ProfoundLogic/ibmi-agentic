     H*--- START of  Summary Run at 14:34:05 on 06/11/92 -----------------------
     H*  @ - Incoming parameters
     H*  # - Keylist
     H*  $ - Generic disk
     H* KB - Toggle indicator 91
     H* KC - Seton LR, QUIT
     H* KD - Create a REquisition
     H* KE - Toggle into VNMGR - lookup vendor #
     H* KF - Toggle into OHMGR
     H* KG - Toggle into
     H* KH - Toggle showing deleted records
     H* KI - Toggle to alternative SORT key indicator 92
     H* KJ - Toggle into VNMGR AND VPMGR - vendor and item find
     H* KK - Toggle indicator 90
     H* KL - Cancel job & return to prior menu.
     H* KM - Run Query
     H* KP - Reopen Req/ detach from po
     H* KQ - Cancel Requisiton
     H* KR - Toggle between "I" and "D" order types
     H* KS - Create PO
     H* LR -
     H* 15 - Deleted record (reverse image/blink on delete field)
     H* 20 - Work
     H* 21 - Work
     H* 24 - Existing OMT record retrieved for UPDATE
     H* 25 - Existing record retrieved for UPDATE
     H* 26 - Attempt to change key to existing record
     H* 27 - Validate key & get related INFO only(27) or get all(N27)
     H* 28 - UPDATE mode allowed(28) or non-display certain fields(N28)
     H* 29 - Instant DELETE allowed
     H* 30 - Read EOF
     H* 31 - Page UP
     H* 32 - Empty SFL, (N32) conditions SFLDSP; (32) colors MSG1
     H* 33 - Page DOWN
     H* 35 - Read EOF 1st time
     H* 36 - Read EOF 2nd time
     H* 38 - Execute SEARCH routine
     H* 39 - Execute 2nd SEARCH routine
     H* 41 - Prior page UP
     H* 43 - Prior page DOWN
     H* 47 - Allows approval of Requisitions with an 'E' status
     H* 48 - Allows creation of PO from requisition
     H* 49 - Allows approval of Requisitions with an 'A' status
     H* 50 - HOME key and moving cursor to ALTERNATE position
     H* 66 - Only allows New Record by Auto generating one
     H* 67 - Only allows PAKSYS to change NN CUST ID
     H* 76 - toggle on "chrg cust frt"
     H* 89 - First(N89) or subsequent(89) pass into UPDATE
     H* 90 - Inquiry/Detail(90) or UPDATE(N90)  toggle(KK)
     H* 91 - Inquiry(N91) or Detail/UPDATE(91)  toggle(KB)
     H* 92 - REQ# (N92) or SortKey(92)          toggle(KI)
     H* 93 - Show(N93) or suppress(93) deleted records
     H* 95 - Allow DUPCHK S/R to prime Hold fields from previous entry
     H* 96 - Error alarm for search or NO UPDATE
     H* 97 - Conditions PUTOVR on dspfmt FOOTER after exit to outside
     H* 98 - Warning highlight on Detail/UPDATE
     H* 99 - First cycle processing
     H*--- END   of  Summary Run at 14:34:05 on 06/11/92 -----------------------
     FOMMGR     CF   E             WORKSTN
     F                                     SFILE(SFL:SFLR#)
     F                                     INFDS(INFDS)
     FORMSRR    UF A E           K DISK
     F                                     RENAME(OMFMT:DTAFMT)
     F                                     INFSR(INF$R)
     F                                     INFDS(INFD$)
     F* Alternate index of primary file
     FORMSRL1   IF   E           K DISK
     F* Optional related files needed to display/validate INFO
     FORMTX     UF A E           K DISK
     FORITX     UF A E           K DISK
     FORHDR     UF A E           K DISK
     FORHTX     UF A E           K DISK
     FORITM     UF A E           K DISK
     F*RMSR     IF   E           K DISK    RENAME(OMFMT:OMFMT2) PREFIX(O2:2)
     FCOADDRES  UF   E           K DISK
     FCUSTSHPP  IF   E           K DISK
     F*ENDMSTR  IF   E           K DISK
     FVENDMST   IF   E           K DISK
     FVENDPACO  UF A E           K DISK
     FVENDPABLF UF A E           K DISK
     FVENDPART  UF   E           K DISK
     FMASTSPEP  UF   E           K DISK
     FVENDPATG  IF   E           K DISK
     FVENDSAZZ  IF   E           K DISK
     F*ENDSHPR  IF   E           K DISK
     FWIPSHIP   IF   E           K DISK
     FUSERFILE  IF   E           K DISK
     FUSERIDS   IF   E           K DISK
     FCURPOS    IT   F  132        DISK
     FLOCXREFLF IF   E           K DISK                                         L0FMT
     FPRDXREFLF IF   E           K DISK                                         P0FMT
     FRIGHTSZ   IF   E           K DISK                                         RSFMT
     DBUM              S             15A   DIM(26)
     DVZZDS          E DS                  EXTNAME(VENDSAZZ)
     D  VZZ                   21    410    DIM(26)
     D CP              S              1    DIM(132) FROMFILE(CURPOS)
     D                                     PERRCD(132)
      * display wo#
     D OMWONUM         DS
     D  WONN                   1      2
     D  WOJOB                  3      7  0
      * Purchase Order # data structure
     D PONUM           DS
     D  PONUMA                 1      2
     D  PONUMB                 3      4
     D  PONUMC                 5     10
      * NN/ID/LOC
     D NNIDLC          DS
     D  WSNNN                  1      2
     D  NNID                   3      6
     D  WSLC                   7      9
     D SRA             S              4    DIM(14) CTDATA PERRCD(14)
     D SRN             S              4  0 DIM(14) CTDATA PERRCD(14)
     D TRA             S              4    DIM(14) CTDATA PERRCD(14)
     D TRN             S              4  0 DIM(14) CTDATA PERRCD(14)
     D CPU             S              2    DIM(9) CTDATA PERRCD(9)
     D PLT             S              1    DIM(9) CTDATA PERRCD(9)
     D ERM             S             38    DIM(90) CTDATA PERRCD(1)
     D*-----------------------------------
     D DIGITS          C                   CONST('0123456789')
     DOIDS           E DS                  EXTNAME(ORITM) OCCURS(2) INZ
      * data structure for vendpart
     D VRDS          E DS                  EXTNAME(VENDPART) INZ
     D  MAJMIN               103    108
     D  UOM                  310    329
     D                                     DIM(5) ASCEND
     D  QTY                  372    391P 0
     D                                     DIM(5) ASCEND
     D  CST                  392    421P 2
     D                                     DIM(5) DESCEND
     D  VRF                  437    466P 2
     D                                     DIM(5) DESCEND
     D  VRA                  467    496P 2
     D                                     DIM(5) DESCEND
     DVODS           E DS                  EXTNAME(VENDPACO) INZ                Ovr_Org costs
     D  VOQ                   68     87P 0 DIM(5) ASCEND
     D  VOC                   88    117P 2 DIM(5)
     D  VOF                  133    162P 2 DIM(5)
     D  VOA                  163    192P 2 DIM(5)
     DVYDS           E DS                  OCCURS(2) EXTNAME(VENDPABY) INZ
     D  VYQ                  411    430P 0 DIM(5) ASCEND
     D  VYC                  431    460P 2 DIM(5)
     D  VYF                  476    505P 2 DIM(5)
     D  VYA                  506    535P 2 DIM(5)
     D BM            E DS                  EXTNAME(ORMSR)
     D  KEY                    2     18
     D* Be SURE length of DTASAV is same as BM to permit COMP
     D DTASAV          DS           468
     D  SVSTAT                 1      1
     D  SVKEY                  2     18
     D  WSNN                  12     13
     D  WSJOB                 14     18  0
     D  @OMJOBX               14     18
     D  SVVN#                 21     26  0
     D* REQDT                 94     97P 0
     D* DUESAV               206    209P 0
     D* PODT                 289    292P 0
     D  SVFRTC               461    461
     D  SVFRTA               462    468  2
     D BMOT          E DS                  EXTNAME(ORMTX)
     D  OMTKEY                 2     18
     D  OMTX                  19    168    DIM(3)
     D  OMTXT1C               19     19
     D  OMTXT1E               20     68
     D  OMTXT2C               69     69
     D  OMTXT2E               70    118
     D  OMTXT3C              119    119
     D  OMTXT3E              120    168
     D SVOT            DS           191
     D KEYNX           DS
     D  NXK01                  1      2
     D  NXK02                  3      4
     D  NXK03                  5     10  0
     D  NXK04                 11     17
     D LOKEY           DS
     D  LOK01                  1      2
     D  LOK02                  3      4
     D  LOK03                  5     10  0
     D  LOK04                 11     17
     D HIKEY           DS
     D  HIK01                  1      2
     D  HIK02                  3      4
     D  HIK03                  5     10  0
     D  HIK04                 11     17
     D PATTRN          DS
     D  STR                    1     20
     D                                     DIM(20)
     D XCKEY           DS
     D  EXPK1                  1      1
     D  EXPK2                  2      4  0
     D                 DS
     D  MSG1                   1     38
     D  MSG2                  39     76
     D  MSG3                  77    114
     D  MSG4                 115    152
     D  MSG                    1    152
     D                                     DIM(4)
     D                 DS
     D  ZIPD5                 19     23  0
     D  ZIP5                  19     23
     D  ZIPD4                 24     27  0
     D  ZIP4                  24     27
     DSAPID                                LIKE(UISAP)
     DREQINFO                              LIKE(OITXT1)
     DCHGQTY                               LIKE(OMQTY)
     DSVPOIT                               LIKE(OMPOIT)
     DSVPOYY                               LIKE(OMPOYY)
     DSVPOPP                               LIKE(OMPOPP)
     DSVPO#                                LIKE(OMPO#) INZ(0)
     DSVREQ                                LIKE(OMREQ)
     DSVLEAD                               LIKE(OMLEAD) INZ(0)
     DSVDC                                 LIKE(OMDC)
     DSVCODE                               LIKE(OMCODE)
     D@CSNN                                LIKE(CSNN)
     D@CSID                                LIKE(CSID)
     D@CSORAX                              LIKE(CSORAX)
     DNEXTYR           S                   LIKE(TODAY)
     DLASTYR           S                   LIKE(TODAY)
     DPOTOT            S             10P 2 INZ(0)
     DPOMAX            S              8P 2 INZ(0)
     DPOMIN            S              8P 2 INZ(10000.00)
     D@QTY             S              7P 0 INZ
     D@VYDEL           S                   LIKE(VYDEL) INZ(' ')
     DALT              S              1    INZ(' ')
     DALVEND           S                   LIKE(OHVEND)
     DATVEND           S                   LIKE(OHVEND)
     D@EXPD            S                   LIKE(VOEXPD)
     DCSNN             S                   LIKE(VRNN)
     DCSID             S                   LIKE(VRID)
     DCSORAX           S             15
     DLPP              S              4S 0
     DTYPE             S              1
     D DA80            S              8P 0
     DTODAY            S               D
     DWEEKLATER        S               D
     DISODATE          S               D
     DISOTIME          S               T
     D DATEISO         S               D
     DCMD$             S            500
     DWSDC             S                   LIKE(WSWH)
     DOMWSTIME         S                   LIKE(WSTIME)
     D INFDS           DS
     D  STATUS           *STATUS
     D  CURROW               370    370
     D INFD$           DS
     D  STATU$           *STATUS
     D                SDS
     D  CPF#                  40     46
     D PTTRN2          DS
     D  TTR                    1     20
     D                                     DIM(20)
     DOH#C             S              6
     DCSFRGHT          C                   CONST('CSFREIGHTCHARGES')
     DCSFRFSC          C                   CONST('CSFREIGHTFSC')
     DESFRGHT          C                   CONST('ESFREIGHT')
     DCSTARIF          C                   CONST('CSTARIFF')
     DESPROCS          C                   CONST('ESPROCESSINGFEE')
     DCSRUSH           C                   CONST('CSRUSHFEE')
     DCDSALES          C                   CONST('CSCDSALES')
     DPPSALES          C                   CONST('CSPPSALES')
     DOHMGR            C                   CONST('OHMGR')
     DWHMGR            C                   CONST('WHMGR')
     DVNDMGR           C                   CONST('VNDMGR')
     DQRYFILES         C                   CONST('QRYFILES')
     DVPMGR            C                   CONST('VPMGR')
     DVYMGR            C                   CONST('VYMGR')
     DALTVND           C                   CONST('PABYALTCL')
     D*---------------------------------------------------------------------
     I*GLAMAST   NS  01    1NC
     I*ASSET     NS  02
     C*---------------------------------------------------------------------
     C*::::: Initialize, defer write, and read screen formats
     C     *IN99         IFEQ      '1'
     C     *IN91         IFEQ      '0'
     C                   READ      SFLCTL                                 20
     C                   SETOFF                                       32
     C   50              GOTO      BYEBYE
     C                   Z-ADD     1             X
     C     CURROW        LOOKUP    CP(X)                                  20
     C   20X             IFGE      5
     C     X             ANDLE     21
     C                   SUB       4             X
     C     X             CHAIN     SFL                                20
     C                   MOVE      KEY           KEYNX
     C                   ENDIF
     C                   ELSE
     C                   READ      UPDATE                                 20
     C                   SETOFF                                       32
     C  N90*IN50         CABEQ     '1'           BYEBYE               50
     C   90*IN50         CABEQ     '1'           BYEBYE
     C                   ENDIF
     C*****************************************************************
     C* Reading FOOTER which has NO CF, or ROLL keywords assigned     *
     C* causes OS/400 to update *STATUS with the appropriate          *
     C* status codes which can be used in processing the display file.*
     C* NO STATUS CODES are returned from reading SFLCTL or UPDATE    *
     C* because they assign indicators to the ROLL keywords.          *
     C*****************************************************************
     C                   READ      FOOTER                                 20
     C                   ELSE
     C                   SETON                                          99
     C     @IN90         COMP      '1'                                    90
     C     @IN91         COMP      '1'                                    91
     C* Process same as first pass into UPDATE when update mode
     C* is called directly.
     C     *IN90         IFEQ      '0'
     C     #KEYNX        SETLL     DTAFMT                                 50
     C     *IN50         IFEQ      '1'
     C                   EXSR      BLANKS
     C                   Z-ADD     4             E
     C                   EXSR      ERRORS
     C                   ELSE
     C                   SETON                                        89
     C                   EXSR      GETREC
     C                   ENDIF
     C                   GOTO      UPDAT9
     C                   ENDIF
     C                   ENDIF
     C*::::: Process function keys
     C                   SETOFF                                       969798
     C                   CLEAR                   MSG
     C     *INKB         CASEQ     '1'           SRKB                           F2
     C     *INKK         CASEQ     '1'           SRKK                           F11
     C     *INKD         CASEQ     '1'           SRKD                           F4
     C     *INKE         CASEQ     '1'           SRKE                     97    F5
     C     *INKF         CASEQ     '1'           SRKF                     97    F6
     C     *INKG         CASEQ     '1'           SRKG                     97    F7
     C     *INKH         CASEQ     '1'           SRKH                           F8
     C     *INKI         CASEQ     '1'           SRKI                           F9
     C     *INKJ         CASEQ     '1'           SRKJ                     97    F9
     C     *INKM         CASEQ     '1'           SRKM                     97    F13
     C     *INKP         CASEQ     '1'           SRKP                     97    F15
     C     *INKC         CASEQ     '1'           SRKC                           F3
     C     *INKL         CASEQ     '1'           SRKC                           F12
     C     *INKW         CASEQ     '1'           SRKW                     97    F22
     C     *INKS         CASEQ     '1'           SRKS                     97    F18
     C     *INKX         CASEQ     '1'           SRKX                     97    F18
     C                   ENDCS
     C   LR              GOTO      BYEBYE
     C*================================================================
     C*     |----------------------------- Cmd 11 ---->-----|
     C*     |----- Cmd 2 --->----|                          |
     C*     |                    |-------- Cmd 11 ---->-----|
     C*  Inquiry               Detail                     UPDATE
     C*   90N91                 90 91                     N90 91
     C*     |                    |-------- Cmd 11 ----<-----|
     C*     |----- Cmd 2 ---<----|                          |
     C*     |----- Cmd 2 ---<-------------------------------|
     C*============== Full Screen Inquiry =============================
     C     *IN90         IFEQ      '1'
     C     *IN91         ANDEQ     '0'
     C     STATUS        IFLE      2
     C*::::: ENTER & Fkey processing
     C  N92#KEYNX        SETLL     DTAFMT
     C* N92*LOVAL        SETLL     DTAFMT
     C   92#KEYNZ        SETLL     OMFMT
     C*  92*LOVAL        SETLL     OMFMT
     C                   SETON                                        33
     C                   ELSE
     C*::::: PAGE key processing
     C     *IN92         IFEQ      '0'
     C   33
     CAN 41#HIKEY        SETGT     DTAFMT
     C   31
     CAN 43#LOKEY        SETLL     DTAFMT
     C   33
     CAN 43
     CAN 30*LOVAL        SETLL     DTAFMT
     C   31
     CAN 41
     CAN 30*HIVAL        SETLL     DTAFMT
     C                   ELSE
     C   33
     CAN 41#HIKEZ        SETGT     OMFMT
     C   31
     CAN 43#LOKEZ        SETLL     OMFMT
     C   33
     CAN 43
     CAN 30*LOVAL        SETLL     OMFMT
     C   31
     CAN 41
     CAN 30*HIVAL        SETLL     OMFMT
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      LOAD
     C                   ELSE
     C*============== Detail View =====================================
     C     *IN90         IFEQ      '1'
     C     *IN91         ANDEQ     '1'
     C   33
     CAN 41
     CAN 30*LOVAL        SETLL     DTAFMT
     C   31
     CAN 43
     CAN 30*HIVAL        SETLL     DTAFMT
     C                   EXSR      GETREC
     C                   ELSE
     C*============== Detail UPDATE ===================================
     C     *IN90         IFEQ      '0'
     C     STATUS        IFLE      2
     C     *INKK         IFEQ      '0'
     C*::::: Second+ passes thru UPDATE
     C     *IN89         IFEQ      '1'
     C*::::: Only ENTER,F14,F16 go thru update (If changes made)
     C     STATUS        IFNE      0
     C     *INKP         ANDEQ     '0'
     C     *INKQ         ANDEQ     '0'
     C     *INKS         ANDEQ     '0'
     C                   GOTO      NOUPDT
     C                   ENDIF
     C                   SETON                                        95
     C                   EXSR      EDIT
     C   96              GOTO      NOUPDT
     C  NKP
     CANNKQBM            IFEQ      DTASAV
     C     BMOT          ANDEQ     SVOT
     C*    TYPE          ANDEQ     *BLANKS
     C*    CHGQTY        ANDEQ     0
     C                   Z-ADD     21            E
     C                   GOTO      NOUPDT
     C                   ENDIF
     C     *IN25         IFEQ      '0'
     C*::::: Add NEW record
     C     *IN66         IFEQ      '1'
     C                   EXSR      SHOCHG
     C                   MOVEL     @USER         OMBY
     C                   MOVE      TODAY         OMRQDT
     C                   MOVE      ISOTIME       OMRQTM
     C  N15              WRITE     DTAFMT
     C     OMTXT1E       IFGT      *BLANKS
     C                   EXSR      OTUPD
     C                   ENDIF
     C                   SETOFF                                         66
     C                   ELSE
     C                   Z-ADD     30            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   ELSE
     C     *IN15         IFEQ      '1'
     C*::::: Update EXISTING record marked for deletion
     C                   EXSR      SHOCHG
     C*    *INKQ         IFEQ      '0'
     C*    *INKP         ANDEQ     '0'
     C                   EXCEPT    UPD
      * OMP interface so shows as cancelled
     C                   IF        OMBY='OMP'
      * ..OMPGEN @ID(xxxxx) @TYPE(PO)
     C                   EVAL      CMD$='OMPGEN @ID('+
     C                             %TRIM(OMCUPO) +
     C                             ') @TYPE(PO)'
     C                   Z-ADD     50            EXC#             15 5
     C                   CALL      'QCMDEXC'                            20
     C                   PARM                    CMD$
     C                   PARM                    EXC#
     C                   ENDIF
     C*                  ELSE
     C*                  DELETE    DTAFMT                               20
     C*    *IN20         IFEQ      '1'
     C*                  Z-ADD     18            E                    96
     C*                  GOTO      NOUPDT
     C*                  ENDIF
     C*                  ENDIF
     C                   ELSE
     C     *IN26         IFEQ      '0'
     C*::::: Update EXISTING record
     C     *IN96         IFEQ      '1'
     C                   GOTO      NOUPDT
     C                   ENDIF
     C                   EXSR      SHOCHG
     C                   UPDATE    DTAFMT
     C                   EXSR      OTUPD
     C                   ELSE
     C*::::: Change KEY on EXISTING record (even in DUPKEY files)
     C                   DELETE    DTAFMT                               20
     C     *IN20         IFEQ      '1'
     C                   Z-ADD     18            E                    96
     C                   GOTO      NOUPDT
     C                   ENDIF
     C                   EXSR      SHOCHG
     C                   WRITE     DTAFMT
     C*    #KEYNX        CHAIN     OMTFMT                             20
     C     #OTKEY        CHAIN     OMTFMT                             20
     C  N20              DELETE    OMTFMT
     C                   MOVE      KEY           OMTKEY
     C                   WRITE     OMTFMT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C  N15              Z-ADD     22            E                 2 0
     C*  15
     C*NNKQ              Z-ADD     7             E
     C*  15
     C*N KQ              Z-ADD     8             E
     C   KS              EXSR      NEXTREC
     C     NOUPDT        TAG
     C                   EXSR      ERRORS
     C     *IN96         CABEQ     '1'           UPDAT9                   98
     C     KEY           COMP      KEYNX                                  50
     C                   ENDIF
     C                   SETON                                        89
     C                   EXSR      GETREC
     C                   GOTO      UPDAT9
     C                   ENDIF
     C*::::: First pass into UPDATE
     C     #KEYNX        SETLL     DTAFMT                                 50
     C     *IN50         IFEQ      '1'
     C                   EXSR      BLANKS
     C                   Z-ADD     4             E
     C                   EXSR      ERRORS
     C                   ELSE
     C                   SETON                                        89
     C                   EXSR      GETREC
     C                   ENDIF
     C     UPDAT9        TAG
     C                   ELSE
     C*::::: PAGE key processing
     C                   SETON                                        5089
     C   33
     CAN 41
     CAN 30*LOVAL        SETLL     DTAFMT
     C   31
     CAN 43
     CAN 30*HIVAL        SETLL     DTAFMT
     C                   EXSR      GETREC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*============== Load commands ===================================
     C                   DO        4             X                 3 0
     C                   SELECT
     C*::::: 1st command in importance
     C     X             WHENEQ    1
     C     *IN90         IFEQ      '1'
     C                   Z-ADD     10            E
     C                   ELSE
     C                   Z-ADD     12            E
     C                   ENDIF
     C*::::: 2nd command in importance
     C     X             WHENEQ    2
     C     *IN90         IFEQ      '1'
     C     *IN91         IFEQ      '0'
     C                   Z-ADD     11            E
     C                   ELSE
     C                   Z-ADD     13            E
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     14            E
     C                   ENDIF
     C*::::: 3rd command in importance
     C     X             WHENEQ    3
     C                   Z-ADD     9             E
     C*::::: 4th command in importance (n/a in update)
     C     X             WHENEQ    4
     C   90*IN91         IFEQ      '0'
     C                   Z-ADD     15            E
     C                   ELSE
     C                   Z-ADD     16            E
     C                   ENDIF
     C                   ENDSL
     C                   EXSR      ERRORS
     C                   ENDDO
     C*============== End Detail Calculations =========================
     C*::::: Prior page direction memory
     C     *IN31         COMP      '1'                                    41
     C     *IN33         COMP      '1'                                    43
     C
     C     BYEBYE        TAG
     C*----------------------------------------------------------------
     C* Prime USER,YMD for update
     CSR   SHOCHG        BEGSR
     C                   TIME                    TIMEDATE
     C     *USA          MOVE      TIMEDATE      TODAY
     C                   MOVEL     TIMEDATE      ISOTIME
     C                   MOVE      'X'           OMX
     C                   MOVE      @USER         OMUSER
     C                   MOVE      TODAY         OMCDAT
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Full screen load.
     CSR   LOAD          BEGSR
     C                   SETON                                        91
     C                   WRITE     SFLCTL                                       clears subfile
     C                   SETOFF                                       91
     C                   EXSR      SRCH1
     C                   EXSR      SRCH1A
     C   33              Z-ADD     1             SFLR#             2 0
     C   31              Z-ADD     17            SFLR#
     C                   DO        17            Z                 2 0
     C     OMSTAT        DOUNE     'D'
     C     OMSTAT        ANDNE     'R'
     C     OMSTAT        ANDNE     'C'
     C     LOADS0        TAG
     C     *IN92         IFEQ      '0'
     C   33              READ(N)   DTAFMT                                 30
     C   31              READP(N)  DTAFMT                                 30
     C                   ELSE
     C   33              READ      OMFMT                                  30
     C   31              READP     OMFMT                                  30
     C                   ENDIF

     C*                  IF        (OMCODE <> @UPDAT AND NOT *IN30)
     C*                            OR (OMDC<> @O AND NOT *IN30)
     C*                  IF        OMDC<> @O AND NOT *IN30
     C*                            AND @O <> *BLANKS
     C*                  GOTO      LOADS0
     C*                  ENDIF

     C     OMDC          CHAIN(N)  COAFMT
     C     *IN30         IFEQ      '1'
     C                   Z-ADD     25            E
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   IF        Z=1
     C                   EVAL      SVCTST=STCTST
     C                   ENDIF
     C   93
     CANN30              ENDDO
     C*::::: Search processing
     C  N30
     CAN 38              DO
     C                   EXSR      SRCH2
     C                   CALL      'QCLSCAN'
     C                   PARM                    STRING
     C                   PARM                    STRLEN
     C                   PARM                    STRPOS
     C                   PARM                    PATTRN
     C                   PARM                    PATLEN
     C                   PARM                    CHAR1
     C                   PARM                    CHAR0
     C                   PARM                    WILD
     C                   PARM                    RESULT            3 0
     C*    RESULT        CABEQ     0             LOADS0
      * special logic for double-search to allow "or" logic if same search used in both
     C                   IF        RESULT=0 AND
     C                             SRCODE<>TRCODE
     C                   GOTO      LOADS0
     C                   ENDIF
     C                   ENDDO
     C*::::: Search processing 2
     C                   IF        NOT *IN30 AND *IN39
     C                   EXSR      SRCH2A
     C                   CALL      'QCLSCAN'
     C                   PARM                    STRNG2
     C                   PARM                    STRLN2
     C                   PARM                    STRPS2
     C                   PARM                    PTTRN2
     C                   PARM                    PATLN2
     C                   PARM                    CHAR1A
     C                   PARM                    CHAR0A
     C                   PARM                    WILD2
     C                   PARM                    RESLT2            3 0
     C*    RESLT2        CABEQ     0             LOADS0
      * special logic for double-search to allow "or" logic if same search used in both
     C                   IF        RESLT2=0 AND
     C                             (SRCODE<>TRCODE OR
     C                              RESULT=0 AND
     C                              SRCODE=TRCODE)
     C                   GOTO      LOADS0
     C                   ENDIF
     C                   ENDIF


     C*::::: End of search processing
     C     Z             IFEQ      1
     C   33
     CANN30              MOVE      KEY           LOKEY
     C   31
     CANN30              MOVE      KEY           HIKEY
     C*****************************************************************
     C*  Use of *HIVAL in LOKEY means KEYNX will also have *HIVAL.    *
     C*  Since  *HIVAL is hexFF it will appear as BLANK char to user  *
     C*  when he rolls to EOF and looks at next key. However, it is   *
     C*  hexFF not BLANK.                                             *
     C*****************************************************************
     C   30              SELECT
     C     *IN33         WHENEQ    '1'
     C                   MOVE      *HIVAL        LOKEY
     C                   MOVE      *HIVAL        LOK03
     C     *IN31         WHENEQ    '1'
     C                   MOVE      *LOVAL        HIKEY
     C                   MOVE      *LOVAL        HIK03
     C                   ENDSL
     C   96
     COR 30              SETON                                        32
     C                   ENDIF
     C   96
     COR 30              GOTO      LOAD01
     C                   IF        OMSTAT <> 'A'
     C                   EVAL      *IN78=*ON
     C                   ELSE
     C                   EVAL      *IN78=*OFF
     C                   ENDIF
     C                   WRITE     SFL
     C   33              ADD       1             SFLR#
     C   31              SUB       1             SFLR#
     C                   ENDDO
     C     LOAD01        TAG
     C   33
     CANN30              MOVE      KEY           HIKEY
     C   31
     CANN30              MOVE      KEY           LOKEY
     C   30              SELECT
     C     *IN33         WHENEQ    '1'
     C                   EVAL      HIKEY=*HIVAL
     C                   EVAL      HIK03=*HIVAL
     C     *IN31         WHENEQ    '1'
     C                   EVAL      LOKEY=*LOVAL
     C                   EVAL      LOK03=*LOVAL
     C                   ENDSL
     C                   MOVE      LOKEY         KEYNX
     C                   EXSR      LOADH
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Full screen load header
     CSR   LOADH         BEGSR
     C*::::: Clear header fields
     C*                  IF        YESRL>*ZEROS
     C*                  EVAL      *IN32=*OFF
     C*                  ENDIF
     C
     C   32              GOTO      LOADH9
     C*****************************************************************
     C*  Use HIDDEN fields on SFL record, if needed, to preserve      *
     C*  fields contained in records of BROWSED FILE to use as        *
     C*  (1) header info directly, or                                 *
     C*  (2) keyflds to chain to OTHER FILES for header info.         *
     C*  Do NOT attempt to chain BACK to BROWSED FILE, as this will   *
     C*  reset pointer and cause PAGE to start at wrong record.       *
     C*****************************************************************
     C   331             CHAIN     SFL                                20
     C   31SFLR#         ADD       1             X
     C   31X             CHAIN     SFL                                20
     CSR   LOADH9        ENDSR
     C*----------------------------------------------------------------
     C* Detail or UPDATE load.
     CSR   GETREC        BEGSR
     C     STATUS        IFLE      2
     C*::::: ENTER & Fkey resets pointer
     C  N92#KEYNX        SETLL     DTAFMT
     C   92#KEYNZ        SETLL     OMFMT
     C*    'X'           COMP      'X'                                    33
     C     'X'           COMP      'X'                                93  33
     C                   ENDIF
     C                   SETOFF                                       1525
     C                   SETOFF                                       3536
     C                   EXSR      SRCH1
     C                   EXSR      SRCH1A
     C*::::: Read next record
     C     OMSTAT        DOUNE     'D'
     C     OMSTAT        ANDNE     'R'
     C     OMSTAT        ANDNE     'C'
     C     GETRS0        TAG                                                    Search Tag
     C     *IN92         IFEQ      '0'
     C   33
     CAN 90              READ(N)   DTAFMT                                 30
     C   31
     CAN 90              READP(N)  DTAFMT                                 30
     C   33
     CANN90              READ      DTAFMT                                 30
     C   31
     CANN90              READP     DTAFMT                                 30
     C                   ELSE
     C   33              READ      OMFMT                                  30
     C   31              READP     OMFMT                                  30
     C                   ENDIF
     C     *IN30         IFEQ      '1'
     C*::::: 1st EOF retry
     C   35              SETON                                        36        Avoids loop with all
     C                   SETON                                        35
     C     *IN92         IFEQ      '0'
     C   33*LOVAL        SETLL     DTAFMT
     C   90
     CAN 33
     CANN36
     CANN39
     CANN38              READ(N)   DTAFMT                                 30
     C  N90
     CAN 33
     CANN36
     CANN39
     CANN38              READ      DTAFMT                                 30
     C   31*HIVAL        SETLL     DTAFMT
     C   90
     CAN 31
     CANN36
     CANN39
     CANN38              READP(N)  DTAFMT                                 30
     C  N90
     CAN 31
     CANN36
     CANN39
     CANN38              READP     DTAFMT                                 30
     C                   ELSE
     C   33*LOVAL        SETLL     OMFMT
     C   33
     CANN36
     CANN39
     CANN38              READ      OMFMT                                  30
     C   31*HIVAL        SETLL     OMFMT
     C   31
     CANN36
     CANN39
     CANN38              READP     OMFMT                                  30
     C                   ENDIF
     C     *IN30         IFEQ      '1'
     C*::::: 2nd EOF error
     C   39
     COR 38              Z-ADD     26            E
     C  N36
     CANN39
     CANN38              Z-ADD     27            E                    98
     C   36              Z-ADD     28            E                    98
     C                   EXSR      ERRORS
     C*::::: This way prevents records being added to a file with
     C*::::: all DELETED or otherwise inaccessible records.
     C*    N36N38          MOVE *BLANKS   KEY
     C*    N36N38          GOTO GETRE7
     C*::::: This way permits record additions to a file with
     C*::::: all DELETED or otherwise inaccessible records.
     C  N39
     CANN38              MOVE      *BLANKS       KEY
     C  N39
     CANN38              GOTO      GETRE7
     C                   EXSR      BLANKS
     C   33              MOVE      *HIVAL        KEYNX
     C   31              MOVE      *LOVAL        KEYNX
     C   33              MOVE      *HIVAL        NXK03
     C   31              MOVE      *LOVAL        NXK03
     C                   GOTO      GETRE9
     C                   ENDIF
     C                   ENDIF
     C   93
     CAN 90              ENDDO
     C*::::: Search processing
     C     *IN38         IFEQ      '1'
     C                   EXSR      SRCH2
     C                   CALL      'QCLSCAN'
     C                   PARM                    STRING
     C                   PARM                    STRLEN
     C                   PARM                    STRPOS
     C                   PARM                    PATTRN
     C                   PARM                    PATLEN
     C                   PARM                    CHAR1
     C                   PARM                    CHAR0
     C                   PARM                    WILD
     C                   PARM                    RESULT
     C*    RESULT        CABEQ     0             GETRS0
      * special logic for double-search to allow "or" logic if same search used in both
     C                   IF        RESULT=0 AND
     C                             SRCODE<>TRCODE
     C                   GOTO      GETRS0
     C                   ENDIF
     C                   ENDIF
     C*::::: Search processing 2
     C                   IF        *IN39
     C                   EXSR      SRCH2A
     C                   CALL      'QCLSCAN'
     C                   PARM                    STRNG2
     C                   PARM                    STRLN2
     C                   PARM                    STRPS2
     C                   PARM                    PTTRN2
     C                   PARM                    PATLN2
     C                   PARM                    CHAR1A
     C                   PARM                    CHAR0A
     C                   PARM                    WILD2
     C                   PARM                    RESLT2            3 0
     C*    RESLT2        CABEQ     0             GETRS0
      * special logic for double-search to allow "or" logic if same search used in both
     C                   IF        RESLT2=0 AND
     C                             (SRCODE<>TRCODE OR
     C                              RESULT=0 AND
     C                              SRCODE=TRCODE)
     C                   GOTO      GETRS0
     C                   ENDIF
     C                   ENDIF
     C*::::: Force equal KEYS on page or search
     C                   IF        (STATUS>2 AND (NOT *IN30)) OR *IN38
     C                   MOVE      KEY           KEYNX
     C                   ENDIF
     C     GETRE7        TAG
     C     KEYNX         IFEQ      KEY
     C                   SETOFF                                       27
     C*          #OTKEY    CHAINOMTFMT               20
     C   90#OTKEY        CHAIN(N)  OMTFMT                             20
     C  N90#OTKEY        CHAIN     OMTFMT                             20
     C   20              RESET                   BMOT
     C* N20              EVAL      OMTXT1C=%SUBST(OMTXT1:1:1)
     C* N20              EVAL      OMTXT1E=%SUBST(OMTXT1:2:49)
     C* N20              EVAL      OMTXT2C=%SUBST(OMTXT2:1:1)
     C* N20              EVAL      OMTXT2E=%SUBST(OMTXT2:2:49)
     C* N20              EVAL      OMTXT3C=%SUBST(OMTXT3:1:1)
     C* N20              EVAL      OMTXT3E=%SUBST(OMTXT3:2:49)
     C                   IF        OMTXT1C=' '
     C                   EVAL      OMTXT1C='E'
     C                   ENDIF
     C                   IF        OMTXT2C=' '
     C                   EVAL      OMTXT2C='E'
     C                   ENDIF
     C                   IF        OMTXT3C=' '
     C                   EVAL      OMTXT3C='E'
     C                   ENDIF
     C*::::: Requested record retrieved successfully
     C*::::: Convert special data before output to screen,
     C*::::: for example, conversion of BIT storage to alpha data.
     C     OMSTAT        COMP      'D'                                    15
     C     *IN90         IFEQ      '0'
     C                   MOVEL     BM            DTASAV
     C                   MOVEL     BMOT          SVOT
     C*                  MOVE      *BLANKS       TYPE
     C                   MOVE      *BLANKS       PO#
     C                   RESET                   SVLEAD
     C*                  Z-ADD     0             CHGQTY
     C                   SETON                                        25
     C  N15              Z-ADD     1             E
     C*  15              Z-ADD     2             E
     C   15              Z-ADD     24            E
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   EXSR      INFO
     C                   ELSE
     C*::::: Requested record missing (NEW RECORD assumed)
     C                   EXSR      BLANKS
     C     *IN92         IFEQ      '1'
     C                   SETON                                            90
     C                   GOTO      MISSED
     C                   ENDIF
     C     #KEYNX        SETLL     DTAFMT
     C                   MOVE      KEYNX         KEY
     C                   SETON                                        27
     C                   SETOFF                                       95
     C                   EXSR      INFO
     C  N96
     CANN90              Z-ADD     3             E
     C     MISSED        TAG
     C  N96
     CAN 90              Z-ADD     5             E
     C  N96              EXSR      ERRORS
     C                   ENDIF
     C                   MOVE      OMYY          OMTYY
     C                   MOVE      OMNN          OMTNN
     C                   Z-ADD     OMREQ         OMTREQ
     C                   MOVE      OMWO#         OMTWO#
     C     OMDC          CHAIN(N)  COAFMT
     C                   EVAL      SVCTST=STCTST
     C*  90#OTKEY        CHAIN(N)  OMTFMT                             20
     C* N90#OTKEY        CHAIN     OMTFMT                             20
     C*  20              RESET                   BMOT
     C                   IF        OMTXT1C=' '
     C                   EVAL      OMTXT1C='E'
     C                   ENDIF
     C                   IF        OMTXT2C=' '
     C                   EVAL      OMTXT2C='E'
     C                   ENDIF
     C                   IF        OMTXT3C=' '
     C                   EVAL      OMTXT3C='E'
     C                   ENDIF
     C*  90OMPART        CHAIN(N)  VRFMT                              20
     C* N90OMPART        CHAIN     VRFMT                              20
     C     OMPART        CHAIN(N)  VRFMT                              20
     C   20              RESET                   VRDS
     C     GETRE9        TAG
     C     SHODEL        COMP      *BLANK                                 93
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Prepare/validate screen information.
     CSR   INFO          BEGSR
     C   95              EXSR      DUPCHK
     C*::::: Validate key
     C*::::: Get related information, then exit or do full edit
     C     OMPART        CHAIN(N)  VRFMT                              20
     C  NKQ              IF        *IN20 OR VRDEL='D'
     C                   Z-ADD     39            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
      *
     C                   EVAL      OMDESC=VQDESC
     C                   IF        OMLEAD = *ZEROS
     C                   EVAL      OMLEAD=VRLEAD
     C                   ENDIF
     C* Retrieve Requisitioner's Name
     C     OMBY          CHAIN(N)  UIFMT                              20
     C                   EVAL      SAPID=*BLANKS
     C  N20              MOVEL     UINAME        REQBY            20
     C     *IN20         IFEQ      '1'
     C                   MOVEL     OMBY          REQBY
     C                   ENDIF
     C* Retrieve Buyer's Name
     C     OMPOBY        CHAIN(N)  UIFMT                              20
     C                   MOVEL     UINAME        BUYER            20
     C     *IN20         IFEQ      '1'
     C                   MOVEL     *BLANKS       BUYER
     C                   ENDIF

     C*                  EVAL      *IN76=*OFF

     C* F10 pressed to load VPMGR.  If a value is returned, prime VN#
     C                   IF        *IN37 AND VN#>0
     C                   IF        VN# <> *ZEROS
     C                   EVAL      OMVN#=VN#
     C                   ENDIF
     C                   IF        PART# <> *BLANKS
     C                   EVAL      OMPART=PART#
     C                   ENDIF
     C                   EVAL      CHGQTY=OMQTY
     C                   SETOFF                                       37
     C                   ENDIF
     C     *IN66         IFEQ      '0'
     C* Set up Vendor Name
     C                   SELECT
     C                   WHEN      *INKQ
      * bypass vendor check if F16
     C                   WHEN      OMVN# = 0 AND
     C                             (OMFRTC<>SVFRTC OR OMFRTA<>SVFRTA OR
     C                              OMCODE='F')
      * bypass vendor when setting up freight only
     C                   WHEN      OMVN#  = *ZEROS AND TYPE = 'C'
      * bypass vendor check if adding to existing po (type="C")
     C                   OTHER
     C     OMVN#         CHAIN(N)  VENDFMT                            20
     C                   IF        *IN20 OR VNCODE <> ' '
     C                   MOVEL     *BLANKS       VNNAME
     C                   MOVEL     *ZEROS        VN#
     C                   Z-ADD     33            E                    96
     C                   ENDIF
     C                   ENDSL
     C* Set up Customer Name
      * bypass customer check if inventory replace req
     C                   IF        OMCODE <> 'I'
     C                   EVAL      NNIDDSC='NN/Cuid/Loc'
     C                   EVAL      *IN55=*OFF
     C                   EVAL      WSNNN=OMWSNN
     C                   EVAL      WSLC=OMWSLC
     C                   MOVE      OMWSID        NNID
     C     #CSKEY        CHAIN(N)  CSFMT                              20
     C     *IN20         IFEQ      '1'
     C                   MOVEL     *BLANKS       CSNAME
     C                   Z-ADD     76            E
     C                   ENDIF
     C                   EVAL      CHRGMSG='Chrg Cust Frt:'
     C                   EVAL      *IN76=*OFF
     C                   ELSE
     C                   EVAL      *IN55=*ON
     C                   EVAL      CSNAME='INVENTORY REPLENISHMENT'
     C                   EVAL      CHRGMSG=*BLANKS
     C                   EVAL      *IN76=*ON
     C                   ENDIF
     C*  Validate date
     C  N90              IF        SVLEAD=*ZEROS
     C                   EVAL      SVLEAD=OMLEAD
     C                   ENDIF

     C  N90              IF        OMDDAT = *LOVAL
     C     TODAY         ADDDUR    OMLEAD:*D     OMDDAT
     C                   ELSE
     C                   IF        SVLEAD <> OMLEAD
     C                   SUBDUR    SVLEAD:*D     OMDDAT
     C                   ADDDUR    OMLEAD:*D     OMDDAT
     C                   EVAL      SVLEAD=*ZEROS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     OMDC          CHAIN(N)  COAFMT
     C                   EVAL      SVCTST=STCTST
     C                   IF        OMSTAT <> 'A'
     C                   EVAL      *IN78=*ON
     C                   ELSE
     C                   EVAL      *IN78=*OFF
     C                   ENDIF
      * d/c validation - cant be blank
     C  N90              IF        (OMDC = *BLANKS
     C                             OR (OMDC='DS' AND OMCODE<>'D')
     C                             OR (OMDC<>'DS' AND OMCODE='D'))
     C                             AND SVDC = *BLANKS
     C                   Z-ADD     62            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
      * validation - verify that Req & PO are same D/C & Type
     C  N90              IF        (OMDC <> OHDC OR
     C                             OMCODE <> OHCODE)
     C                             AND PO# > *ZEROS
     C                   Z-ADD     80            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C  N90              IF        PO# <> *BLANKS AND
     C                             OMVN# <> OHVEND
     C                             AND OMVN# <> *ZEROS
     C                   Z-ADD     68            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
      * req qty check -  cant be zero
     C  N90              IF        OMQTY <= *ZEROS
     C                             AND SVDC = *BLANKS
     C                   Z-ADD     63            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
      * if wo# entered, convert to drop ship
     C  N90              IF        OMWO# <> *BLANKS
     C                             AND OMCODE = 'D'
     C                   EVAL      OMWONUM=OMWO#
     C*    #WSKEY        CHAIN(N)  WSFMT                              20
     C*  20              Z-ADD     64            E                    96
     C*  20              EXSR      ERRORS
     C                   IF        OMCODE = 'D'
     C                   EVAL      WSDC = 'DS'
     C                   ELSE
     C                   EVAL      WSDC = OMDC
     C                   ENDIF
     C                   Z-ADD     OMMSD#        OMWSTIME                       zoned to packed
     C     #WSKEY        CHAIN(N)  WSFMT                              20
     C*    #WSKEY        SETLL     WSFMT
     C*                  DOW       'X'='X'
     C*    #WSKEY        READE(N)  WSFMT                                  20
     C*                  IF        *IN20 OR (WSDEL=*BLANKS AND
     C*                            WSPO=OMCUPO AND WSLOC=OMWSLC)                specific rls
     C*                  LEAVE
     C*                  ENDIF
     C*                  ENDDO
     C                   IF        (*IN20 OR WSDEL='D') AND
     C                             NOT(OMSTAT='D' AND *INKQ)
     C                   Z-ADD     64            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   ENDIF
      * check cost override first, otherwise use vendpart cost
     C                   IF        OMCODE='I'
     C     #VOKEYI       CHAIN(N)  VOFMT                              20        get Org_Xref costs
     C                   ELSE
     C     #VOKEYD       CHAIN(N)  VOFMT                              20        get SPECIFIC costs
     C                   ENDIF
      * if no price entered, use vendpart to determine best cost
     C*    OMEST$        IFEQ      0
      * check for vendor level cost first,  Inv type only
     C     #VYKEY        SETLL     VYFMT                                  22
     C                   DOW       'X'='X'
     C     #VYKEY        READE(N)  VYFMT                                  22
     C                   IF        *IN22 OR VYDEL<>'D'
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO

      * populate text fields
      * IF VYDESC <> VQDESC, move to first available po text line
     C                   IF        NOT *IN22
     C                   EVAL      T=1
     C                   IF        VYDESC <> *BLANKS
     C                             AND VYDESC <> VQDESC
     C                   DO        3             T
     C                   IF        %SUBST(OMTX(T):2:49)= %SUBST(VYDESC:1:49)
     C                   LEAVE
     C                   ELSE
     C                   IF        %SUBST(OMTX(T):2:49)=*BLANKS
     C                   EVAL      %SUBST(OMTX(T):2:49)=VYDESC
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C                   IF        VYDESC2 <> *BLANKS
     C                   EVAL      T=1
     C                   DO        3             T
     C                   IF        %SUBST(OMTX(T):2:49)= %SUBST(VYDESC2:1:49)
     C                   LEAVE
     C                   ELSE
     C                   IF        %SUBST(OMTX(T):2:49)=*BLANKS
     C                   EVAL      %SUBST(OMTX(T):2:49)=VYDESC2
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      * 4th text line used to pass internal info to Req
     C                   IF        VYXT04 <> *BLANKS
     C                   EVAL      T=1
     C                   DO        3             T
     C                   IF        %SUBST(OMTX(T):2:49)= %SUBST(VYXT04:1:49)
     C                   LEAVE
     C                   ELSE
     C                   IF        %SUBST(OMTX(T):2:49)=*BLANKS
     C                   EVAL      %SUBST(OMTX(T):2:49)=VYXT04
     C                   EVAL      %SUBST(OMTX(T):1:1)='I'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF

      * if no price entered, use vendpart to determine best cost
     C                   IF        OMEST$=0 OR (OMVN#<>SVVN# AND NOT *IN90)
     C                   IF        NOT *IN22
     C                   XFOOT     VYQ           TLQTY            11 0
     C                   XFOOT     VYC           TLCST            15 2
     C                   IF        TLQTY > *ZEROS
     C                             AND TLCST > *ZEROS
     C                   EVAL      VRCCOD=VYCCOD
     C                   EVAL      QTY=VYQ
     C                   EVAL      CST=VYC
     C                   EXSR      COSTS
     C                   ELSE
     C                   GOTO      COSTA
     C                   ENDIF
     C                   ELSE
     C     COSTA         TAG
      * check cost override first, otherwise use vendpart cost
     C                   IF        NOT *IN20 AND VODEL = ' '
     C                   EVAL      VRCCOD=VOCCOD
     C                   EVAL      QTY=VOQ
     C                   EVAL      CST=VOC
     C                   ENDIF
     C                   EXSR      COSTS
     C                   ENDIF
     C                   ENDIF
     C                   IF        OMUOM = *BLANKS
     C                   MOVEL     VRPER1        OMUOM
     C                   ENDIF
     C                   IF        OMUOM=*BLANKS
     C                   EVAL      OMUOM='EACH'
     C                   ENDIF


     C* Retrieve PO unit price
     C                   Z-ADD     *ZEROS        POUNIT
     C     OMCODE        IFNE      ' '
     C     OMCODE        ORNE      'F'
     C                   CLEAR                   PONUM
     C     #POKEY        SETLL     OIFMT                                  20
     C                   DOW       'X' = 'X'
     C     #POKEY        READE(N)  OIFMT                                  20
     C   20              LEAVE
     C                   IF        OIPART = OMPOIT
     C                   Z-ADD     OIUNIT        POUNIT           15 5
     C                   Z-ADD     OIRQTY        QTYRCV
     C                   EVAL      PONUM='PR'+OMYY+%EDITC(OI#:'2')
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ELSE
     C                   Z-ADD     *ZEROS        POUNIT
     C                   ENDIF

      * Warn if ReqQ exceeds 1 truckload, based on VR Units per TL
     C                   IF        VRUPTL > 0 AND VRUNIQ > 0 AND
     C                             OMQTY / VRUNIQ > VRUPTL
     C                   Z-ADD     81            E                    98
     C                   EXSR      ERRORS
     C                   ENDIF

     C   KQ
     COR 27              GOTO      INFO99
     C*::::: Perform full editing of all info in record
     C*      including data restrictions on use of KQ
     C                   TIME                    TIMEDATE
     C     *USA          MOVE      TIMEDATE      TODAY
     C                   MOVEL     TIMEDATE      ISOTIME
     C*
     C                   IF        OMPO#=*ZEROS
     C                   EVAL      *IN77=*ON
     C                   ELSE
     C                   EVAL      *IN77=*OFF
     C                   ENDIF
     C*                  EVAL      PONUMA=*BLANKS
     C*                  EVAL      PONUMB=*BLANKS
     C*                  EVAL      PONUMC=*BLANKS
     C                   IF        OMSTAT='C'
     C                   EVAL      PONUMA='PR'
     C                   EVAL      PONUMB=OMYY
     C*                  IF        OH# < 4556
     C*                  MOVE      '12'          PONUMB            2
     C*                  ENDIF
     C                   MOVE      OMPO#         PONUMC
     C                   ENDIF
     C*Only allows a 'A' or 'E' stat to be changed to ' ' 'D'
     C*Only allows a ' ' stat to be changed to 'A' 'D'
     C  N90OMSTAT        IFNE      SVSTAT
     C                   SELECT
     C     OMSTAT        WHENEQ    ' '
     C     OMSTAT        OREQ      'D'
     C     OMSTAT        OREQ      'A'
     C                   SELECT
     C     SVSTAT        WHENEQ    'A'                                          AUDIT
     C     SVSTAT        OREQ      ' '                                          APPROVED
     C*Only allows APPROVAL BY MGMT IF USER THE SAME
     C     @USER         IFEQ      OMBY
     C                   MOVEL(P)  'OMAMGR'      @PGM             10
     C     #USRKE        CHAIN     USRFMT                             20
     C                   IF        *IN20 OR USRDEL='D'
     C                   EVAL      @PGM=*BLANKS
     C     #USRKE        CHAIN     USRFMT                             20
     C                   ENDIF
     C     *IN20         IFEQ      '1'
     C     USRDEL        OREQ      'D'
     C                   Z-ADD     31            E                    96
     C                   MOVEL     SVSTAT        OMSTAT
     C                   MOVEL     *BLANKS       OMAPP
     C                   MOVEL     *BLANKS       APPROV
     C*                  Z-ADD     0             OMCYMD
     C                   MOVE      *LOVAL        OMCDAT
     C                   EXSR      ERRORS
     C                   ELSE
     C                   MOVE      @USER         APPROV           10
     C                   MOVE      ISOTIME       OMRQTM
     C                   ENDIF
     C                   ELSE
     C                   MOVE      @USER         APPROV           10
     C                   MOVE      ISOTIME       OMRQTM
     C                   ENDIF
     C* Retrieve Approver's Name
     C     APPROV        CHAIN(N)  UIFMT                              20
     C     *IN20         IFEQ      '0'
     C                   MOVEL     UINAME        OMAPP
     C                   EVAL      SAPID=UISAP
     C                   ENDIF
     C*Only allows APPROVAL of EDI If user has INT authority.
     C*Only allows update of EDI PO's
     C     SVSTAT        WHENEQ    'E'                                          EDI APPROVALS
     C   47OMBY          IFNE      'EDI'
     C     OMBY          ANDNE     'SNADS'
     C     OMBY          ANDNE     'REORDER'
     C                   Z-ADD     53            E                    96
     C                   ENDIF
     C                   MOVE      @USER         APPROV
     C                   MOVE      ISOTIME       OMRQTM
     C* Retrieve Approver's Name
     C     APPROV        CHAIN(N)  UIFMT                              20
     C     *IN20         IFEQ      '0'
     C                   MOVEL     UINAME        OMAPP
     C                   ENDIF
     C*Only allows status change of ORMSR If user has EXP authority.
     C                   OTHER
     C  N48              Z-ADD     32            E                    96
     C                   ENDSL
     C*Only allows status change of ORMSR If user has EXP authority.
     C                   OTHER
     C  N48              Z-ADD     32            E                    96
     C                   ENDSL
     C                   ENDIF

     C     @USER         CHAIN(N)  UIFMT                              20
     C     *IN20         IFEQ      '0'
     C                   EVAL      SAPID=UISAP
     C                   ENDIF

     C* populate approval in ormsr (requision file)
     C                   IF        TYPE<>' ' AND CHGQTY > *ZEROS
     C                   MOVE      @USER         APPROV           10
     C* Retrieve Approver's Name
     C     APPROV        CHAIN(N)  UIFMT                              20
     C     *IN20         IFEQ      '0'
     C                   MOVEL     UINAME        OMAPP
     C                   ENDIF
     C                   ENDIF                                                  TYPE<>' '
     C*Only allows SALES SERVICE to update 'C' code Req's
     C* N48@UPDAT        IFEQ      'C'
     C*    OMCODE        ANDNE     'C'
     C*                  Z-ADD     32            E                    96
     C*                  ENDIF
     C     OMPART        IFEQ      *BLANKS
     C                   Z-ADD     52            E                    96
     C                   ENDIF
     C*
     C                   IF        OMFRTA>0 AND OMFRTC<>'Y'
     C                             AND OMCODE = 'D'
     C                   Z-ADD     17            E                    96
     C                   ENDIF
     C*
      * due date must be with the year +/-
     C                   IF        OMSTAT='A'
     C                   IF        ((OMDDAT < LASTYR OR OMDDAT > NEXTYR)
     C                             AND OMDDAT <> *LOVAL)
     C                             OR OMDDAT = *LOVAL
     C                   Z-ADD     77            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   ENDIF
     C     *IN29         CABEQ     '1'           INFO99
     C* Perform these calcs only if restricted-update-user accessed.
     C     INFO99        TAG
     C                   EXSR      ERRORS
     C                   SETOFF                                       27
     CSR                 ENDSR
     C*----------------------------------------------------------------
     CSR   UPDVO         BEGSR
      * lock for update
     C                   IF        OMCODE='I'
     C     #VOKEYI       CHAIN     VOFMT                              20        get Org_Xref costs
     C                   ELSE
     C     #VOKEYD       CHAIN     VOFMT                              20        get SPECIFIC costs
     C                   ENDIF
      *... prime new record if missing
     C                   IF        *IN20
     C                   EVAL      VOPART =VRPART
     C                   EVAL      VOOO   =ORG
     C                   IF        OMCODE='I'
     C                   EVAL      VONN   =*BLANKS
     C                   EVAL      VOID   =0
     C                   EVAL      VOORAX =*BLANKS
     C                   EVAL      VOLOC  =*BLANKS
     C                   ELSE
     C                   EVAL      VONN   =CSNN
     C                   EVAL      VOID   =CSID
     C                   EVAL      VOORAX =CSORAX
     C                   EVAL      VOLOC  =CSLOC
     C                   ENDIF
     C                   EVAL      VOCCOD =VRCCOD
     C                   EVAL      VOQ    =QTY
     C                   EVAL      VOC    =CST
     C                   EVAL      VOA    =VRA
     C                   EVAL      VOF    =VRF
     C                   EVAL      VOOVHD=VROVHD
     C                   EVAL      VOSRCL=VRSRCL
     C                   EVAL      VOSRCU=VRSRCU
     C                   ENDIF
      * .. always update these values
     C                   EVAL      VODEL  =' '
     C     OMDDAT        ADDDUR    7:*D          WEEKLATER
     C     *CYMD         MOVE      WEEKLATER     VOEXPD
     C                   Z-ADD     1             Y
      *replace proper override cost with PO cost
     C                   SELECT
     C     VOCCOD        WHENEQ    1
     C     VOCCOD        OREQ      4
     C     VOCCOD        OREQ      5
     C                   DO        5             Y                 1 0
     C                   SELECT
     C                   WHEN      OMQTY=QTY(Y)
     C                   LEAVE
     C                   WHEN      QTY(Y) = *ZEROS OR
     C                             OMQTY<=QTY(Y)
     C                   IF        Y>1
     C                   SUB       1             Y
     C                   ENDIF
     C                   LEAVE
     C                   ENDSL
     C                   ENDDO
      * if didn't find qualifying cost, use last bucket
     C                   IF        Y=6
     C                   SUB       1             Y
     C                   ENDIF

     C                   IF        VOCCOD=1 OR VOCCOD=4
     C     OMEST$        MULT(H)   1000          VOC(Y)
     C                   ELSE
     C                   EVAL      VOC(Y)=OMEST$
     C                   ENDIF
     C     VOCCOD        WHENEQ    6
     C     OMEST$        MULT(H)   1000          VOC(Y)
     C     VOCCOD        WHENEQ    7
     C                   Z-ADD     OMEST$        VOC(Y)
     C     VOCCOD        WHENEQ    10
     C     OMEST$        ANDNE     0
     C                   Z-ADD     OMEST$        VOC(Y)
     C     VOCCOD        WHENEQ    10
     C     OMEST$        ANDEQ     0
     C                   Z-ADD     35            E                    96
     C                   ENDSL
     C                   Z-ADD     1             ACI               5 4
     C                   IF        VREFFD>TODAY AND VRPCT>0
     C                   EVAL      ACI=1+VRPCT
     C                   ENDIF
      * added new calcs for applied costs
     C*                  ... Translate costs per base unit to Applied costs
     C                   EVAL      VOA=0
      * added 7/31/2013 dont add frt cost into "DS" paco record
     C                   IF        OMDC='DS'
     C                   EVAL      VOF=*ZEROS
     C                   ENDIF
     C                   DO        5             X
     C                   IF        X>1 AND VOQ(X)=0
     C                   LEAVE
     C                   ENDIF
     C*                  ... Ovhd% + Advance Cost Increase + Unit based Ovhd
     C                   EVAL(H)   VOA(X)=VOC(X)*ACI*(VOOVHD+1)+VOSRCU
     C*                  ... Lot based Ovhd
     C                   IF        VOSRCL>0 OR VOF(X)>0
     C                   SELECT
     C                   WHEN      VOCCOD=10
     C                   EVAL      VOA(X)=VOA(X)+(VOSRCL+VOF(X))
     C                   WHEN      VOCCOD=1 OR VOCCOD=6
     C                   EVAL(HR)  VOA(X)=VOA(X)+(VOSRCL+VOF(X))/VOQ(X)*1000
     C                   OTHER
     C                   EVAL(H)   VOA(X)=VOA(X)+(VOSRCL+VOF(X))/VOQ(X)
     C                   ENDSL
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      VOX    ='X'
     C                   IF        *INKS AND OI# > *ZEROS
     C                   EVAL      VOUSER='PR' + OIYY +                         PO# NOT DETERMINED
     C                             %TRIM(%EDITW(OI#:'0      '))                 YET....USE REQ#
     C                   ELSE
     C                   EVAL      VOUSER='RQ' + OMYY +
     C                             %TRIM(%EDITW(OMREQ:'0      '))
     C                   ENDIF
     C     *CYMD         MOVE      TODAY         VOCYMD
     C  N20              UPDATE    VOFMT
     C                   MONITOR
     C   20              WRITE     VOFMT
     C                   ON-ERROR
     C                   Z-ADD     71            E                    96
     C                   EXSR      ERRORS
     C                   ENDMON
     CSR                 ENDSR
     C*----------------------------------------------------------------
     CSR   UPDVY         BEGSR
      * Determine if a different vendor is primary for this DC/Itm
     C     2             OCCUR     VYDS
     C                   RESET                   @VYDEL                         _
     C     #VYKEYV       SETLL     VYFMT
     C                   DOW       'X' = 'X'
     C     #VYKEYV       READE     VYFMT                                  20
     C                   IF        *IN20 OR
     C                             (VYDEL=' ' AND OMVN# <>VYVEND)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   IF        NOT *IN20
      * ..found, prompt to Allow user to mark new vendor as primary or alternate
     C                   CALL      ALTVND
     C                   PARM                    @VYDEL
     C                   PARM                    OMVN#
      * ..if setting new vendor as primary, mark old primary as alternate
     C                   IF        @VYDEL=' '
     C                   EVAL      VYDEL='A'
     C                   EVAL      VYUSER='RQ' + OMYY +
     C                             %TRIM(%EDITW(OMREQ:'0      '))
     C                   EVAL      VYCHGD=TODAY
     C                   UPDATE    VYFMT
     C                   ENDIF
     C                   ENDIF
     C     1             OCCUR     VYDS
      * .. skip all updates if answered with X
     C  N96@VYDEL        CABEQ     'X'           UPDVY9                   96
     C
      * write vendor/item data to vendpaby if not already there
     C     #VYKEY        CHAIN     VYFMT                              20
     C                   IF        *IN20 OR VYCCOD = *ZEROS
     C                   IF        *IN20
     C                   RESET                   VYFMT
     C                   EVAL      VYPART=OMPART
     C                   EVAL      VYVEND=OMVN#
     C                   EVAL      VYSKU=VRIT
     C                   EVAL      VYUOM=OMUOM
     C                   ENDIF
     C                   EVAL      VYDC=OMDC
     C                   IF        OMCODE='I'
     C     #VOKEYI       CHAIN(N)  VOFMT                              22        get Org_Xref costs
     C                   ELSE
     C     #VOKEYD       CHAIN(N)  VOFMT                              22        get SPECIFIC costs
     C                   ENDIF
     C                   IF        *IN22 OR VODEL='D'
      * prime paby cost from vendpart
     C     OMPART        CHAIN(N)  VRFMT
     C                   EVAL      VYQ   =QTY
     C                   EVAL      VYC   =CST
     C                   EVAL      VYF   =VRF
     C                   EVAL      VYA   =VRA
     C                   EVAL      VYCCOD=VRCCOD
     C                   EVAL      VYEXPD=VREXPD
     C                   EVAL      VYOVHD=VROVHD
     C                   EVAL      VYSRCL=VRSRCL
     C                   EVAL      VYSRCU=VRSRCU
     C                   ELSE
      * prime paby cost from vendpaco
     C                   EVAL      VYQ   =VOQ
     C                   EVAL      VYC   =VOC
     C                   EVAL      VYF   =VOF
     C                   EVAL      VYA   =VOA
     C                   EVAL      VYCCOD=VOCCOD
     C                   EVAL      VYEXPD=VOEXPD
     C                   EVAL      VYOVHD=VOOVHD
     C                   EVAL      VYSRCL=VOSRCL
     C                   EVAL      VYSRCU=VOSRCU
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     1             Y
      *replace vendor by part cost with PO cost
     C                   SELECT
     C     VYCCOD        WHENEQ    1
     C     VYCCOD        OREQ      4
     C     VYCCOD        OREQ      5
     C                   DO        5             Y                 1 0
     C                   SELECT
     C                   WHEN      OMQTY=VYQ(Y)
     C                   LEAVE
     C                   WHEN      VYQ(Y) = *ZEROS OR
     C                             OMQTY<VYQ(Y)
     C                   IF        Y>1
     C                   SUB       1             Y
     C                   ENDIF
     C                   LEAVE
     C                   ENDSL
     C                   ENDDO
      * if didn't find qualifying cost, use last bucket
     C                   IF        Y=6
     C                   SUB       1             Y
     C                   ENDIF
     C                   IF        VYCCOD=1 OR VYCCOD=4
     C     OMEST$        MULT(H)   1000          VYC(Y)
     C                   ELSE
     C                   Z-ADD     OMEST$        VYC(Y)
     C                   ENDIF
     C     VYCCOD        WHENEQ    6
     C     OMEST$        MULT(H)   1000          VYC(Y)
     C     VYCCOD        WHENEQ    7
     C                   Z-ADD     OMEST$        VYC(Y)
     C     VYCCOD        WHENEQ    10
     C     OMEST$        ANDNE     0
     C                   Z-ADD     OMEST$        VYC(Y)
     C     VYCCOD        WHENEQ    10
     C     OMEST$        ANDEQ     0
     C                   Z-ADD     35            E                    96
     C                   ENDSL
      * update vendpaby applied cost
     C                   Z-ADD     1             ACI               5 4
     C                   IF        VREFFD>TODAY AND VRPCT>0
     C                   EVAL      ACI=1+VRPCT
     C                   ENDIF
     C*                  ... Translate costs per base unit to Applied costs
     C                   EVAL      VYA=0
     C                   DO        5             X
     C                   IF        X>1 AND VYQ(X)=0
     C                   LEAVE
     C                   ENDIF
     C*                  ... Ovhd% + Advance Cost Increase + Unit based Ovhd
     C                   EVAL(H)   VYA(X)=VYC(X)*ACI*(VYOVHD+1)+VYSRCU
     C*                  ... Lot based Ovhd
     C                   IF        VYSRCL>0 OR VYF(X)>0
     C                   SELECT
     C                   WHEN      VYCCOD=10
     C                   EVAL      VYA(X)=VYA(X)+(VYSRCL+VYF(X))
     C                   WHEN      VYCCOD=1 OR VYCCOD=6
     C                   EVAL(HR)  VYA(X)=VYA(X)+(VYSRCL+VYF(X))/VYQ(X)*1000
     C                   OTHER
     C                   EVAL(H)   VYA(X)=VYA(X)+(VYSRCL+VYF(X))/VYQ(X)
     C                   ENDSL
     C                   ENDIF
     C                   ENDDO
     C                   EVAL      VYUSER='RQ' + OMYY +
     C                             %TRIM(%EDITW(OMREQ:'0      '))
     C                   EVAL      VYCHGD=TODAY
     C                   EVAL      VYDEL=@VYDEL
     C   20              WRITE     VYFMT
     C  N20              UPDATE    VYFMT
     CSR   UPDVY9        ENDSR
     C*----------------------------------------------------------------
     CSR   UPDVP         BEGSR
     C     OMPART        CHAIN     VRFMT                              20        get Org_Xref costs
     C                   Z-ADD     1             Y
      *replace proper vendpart cost with PO cost
     C                   SELECT
     C     VRCCOD        WHENEQ    1
     C     VRCCOD        OREQ      4
     C     VRCCOD        OREQ      5
     C                   DO        5             Y                 1 0
     C                   SELECT
     C                   WHEN      OMQTY=QTY(Y)
     C                   LEAVE
     C                   WHEN      QTY(Y) = *ZEROS OR
     C                             OMQTY<QTY(Y)
     C                   IF        Y>1
     C                   SUB       1             Y
     C                   ENDIF
     C                   LEAVE
     C                   ENDSL
     C                   ENDDO
      * if didn't find qualifying cost, use last bucket
     C                   IF        Y=6
     C                   SUB       1             Y
     C                   ENDIF

     C                   IF        VRCCOD=1 OR VRCCOD=4
     C     OMEST$        MULT(H)   1000          CST(Y)
     C                   ELSE
     C                   EVAL      CST(Y)=OMEST$
     C                   ENDIF
     C     VRCCOD        WHENEQ    6
     C     OMEST$        MULT(H)   1000          CST(Y)
     C     VRCCOD        WHENEQ    7
     C                   Z-ADD     OMEST$        CST(Y)
     C     VRCCOD        WHENEQ    10
     C     OMEST$        ANDNE     0
     C                   Z-ADD     OMEST$        CST(Y)
     C     VRCCOD        WHENEQ    10
     C     OMEST$        ANDEQ     0
     C                   Z-ADD     35            E                    96
     C                   ENDSL
     C                   Z-ADD     1             ACI               5 4
     C                   IF        VREFFD>TODAY AND VRPCT>0
     C                   EVAL      ACI=1+VRPCT
     C                   ENDIF
      * added new calcs for applied costs
     C*                  ... Translate costs per base unit to Applied costs
     C                   EVAL      VRA=0
     C                   DO        5             X
     C                   IF        X>1 AND QTY(X)=0
     C                   LEAVE
     C                   ENDIF
     C*                  ... Ovhd% + Advance Cost Increase + Unit based Ovhd
     C                   EVAL(H)   VRA(X)=CST(X)*ACI*(VROVHD+1)+VRSRCU
     C*                  ... Lot based Ovhd
     C                   IF        VRSRCL>0 OR VRF(X)>0
     C                   SELECT
     C                   WHEN      VRCCOD=10
     C                   EVAL      VRA(X)=VRA(X)+(VYSRCL+VRF(X))
     C                   WHEN      VRCCOD=1 OR VRCCOD=6
     C                   EVAL(HR)  VRA(X)=VRA(X)+(VRSRCL+VRF(X))/QTY(X)*1000
     C                   OTHER
     C                   EVAL(H)   VRA(X)=VRA(X)+(VRSRCL+VRF(X))/QTY(X)
     C                   ENDSL
     C                   ENDIF
      * update mastspec cost with first applied cost
     C                   IF        X=1
     C                   EXSR      MQUPD
     C                   ENDIF
     C                   ENDDO
     C*                  EVAL      VRX    ='X'
     C                   EVAL      VRUSER='RQ' + OMYY +
     C                             %TRIM(%EDITW(OMREQ:'0      '))
     C     *CYMD         MOVE      TODAY         VRCYMD
     C  N20              UPDATE    VRFMT

     CSR                 ENDSR
     C*----------------------------------------------------------------
     CSR   MQUPD         BEGSR
     C     #VRMQKEY      CHAIN     MQFMT                              22
     C*                  ..... qualify Part COST with mq-qualifiers
     C                   Z-ADD     VRA(1)        COST             15 5
     C     MQMLT         COMP      1                                  20
     C   20              DIV(H)    MQMLT         COST
     C     MQ#PCS        COMP      1                                  20
     C   20              MULT      MQ#PCS        COST
     C     MQ#OUT        COMP      1                                  20
     C   20              DIV(H)    MQ#OUT        COST
     C     MQUSEX        COMP      1                                  20
     C   20              MULT      MQUSEX        COST
     C                   EVAL(H)   MQCOST=COST*1
     C                   SELECT
     C                   WHEN      VRCCOD=1 OR VRCCOD=6
     C                   EVAL      MQCCOD=6
     C                   WHEN      VRCCOD=5 OR VRCCOD=7
     C                   IF        COST>=1
     C                   EVAL      MQCCOD=7
     C                   ELSE
     C                   EVAL      MQCOST=COST*1000
     C                   EVAL      MQCCOD=6
     C                   ENDIF
     C                   OTHER
     C                   EVAL      MQCCOD=VRCCOD
     C                   ENDSL
     C                   MOVE      'X'           MQX
     C                   EVAL      MQUSER='PR' + OHYY +
     C                             %TRIM(%EDITW(OH#:'0      '))
     C     *CYMD         MOVE      TODAY         MQSYMD
     C  N22              UPDATE    MQFMT
     CSR   MQUPD99       ENDSR
     C*----------------------------------------------------------------
      * find quantity based costing
     CSR   COSTS         BEGSR
     C                   SELECT
     C     VRCCOD        WHENEQ    1
     C     VRCCOD        OREQ      4
     C     VRCCOD        OREQ      5
     C                   Z-ADD     CST(1)        OMEST$
     C                   DO        5             Y                 1 0
     C     OMQTY         IFGE      QTY(Y)
     C     QTY(Y)        ANDNE     0
     C                   Z-ADD     CST(Y)        OMEST$
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   IF        VRCCOD=1 OR VRCCOD=4
     C     .001          MULT(H)   OMEST$        OMEST$
     C                   ENDIF
     C     VRCCOD        WHENEQ    6
     C     .001          MULT(H)   VRC1          OMEST$
     C     VRCCOD        WHENEQ    7
     C                   Z-ADD     VRC1          OMEST$
     C     VRCCOD        WHENEQ    10
     C     OMEST$        IFEQ      0
     C                   Z-ADD     35            E                    96
     C                   ENDIF
     C                   ENDSL
     C                   EXSR      ERRORS
     CSR                 ENDSR
     C*----------------------------------------------------
     CSR   GET_CST       BEGSR
      * check cost override first, otherwise use vendpart cost
     C                   IF        OHDC='DS'
     C     #VOKEYDP      CHAIN(N)  VOFMT                              20        get Org_Xref costs
     C                   ELSE
     C     #VOKEYIP      CHAIN(N)  VOFMT                              20        get SPECIFIC costs
     C                   ENDIF
      * check for vendor level cost first
     C     #VYKEYP       SETLL     VYFMT                                  22
     C                   DOW       'X'='X'
     C     #VYKEYP       READE     VYFMT                                  22
     C                   IF        *IN22 OR VYDEL=*BLANK
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   IF        NOT *IN22
     C                   XFOOT     VYQ           TLQTY            11 0
     C                   XFOOT     VYC           TLCST            15 2
     C                   IF        TLQTY > *ZEROS
     C                             AND TLCST > *ZEROS
     C                   EVAL      QTY=VYQ
     C                   EVAL      CST=VYC
     C                   EVAL      VRCCOD=VYCCOD
     C                   EXSR      COSTSP
     C                   ELSE
     C                   GOTO      COSTAP
     C                   ENDIF
     C                   ELSE
     C     COSTAP        TAG
     C                   IF        NOT *IN20 AND VODEL <> 'D'
     C                   EVAL      VRCCOD=VOCCOD
     C                   EVAL      QTY=VOQ
     C                   EVAL      CST=VOC
     C                   ENDIF
     C                   EXSR      COSTSP
     C                   ENDIF
     C     GET_CST9      ENDSR
     C*----------------------------------------------------------------
      * find quantity based costing for pallet
     CSR   COSTSP        BEGSR
     C                   SELECT
     C     VRCCOD        WHENEQ    1
     C     VRCCOD        OREQ      4
     C     VRCCOD        OREQ      5
     C                   Z-ADD     CST(1)        OIUNIT
     C                   DO        5             Y                 1 0
     C     OIQTY         IFGE      QTY(Y)
     C     QTY(Y)        ANDNE     0
     C                   Z-ADD     CST(Y)        OIUNIT
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   IF        VRCCOD=1 OR VRCCOD=4
     C     .001          MULT(H)   OIUNIT        OIUNIT
     C                   ENDIF
     C     VRCCOD        WHENEQ    6
     C     .001          MULT(H)   VRC1          OIUNIT
     C     VRCCOD        WHENEQ    7
     C                   Z-ADD     VRC1          OIUNIT
     C*    VRCCOD        WHENEQ    10
     C*    OIUNIT        IFEQ      0
     C*                  Z-ADD     35            E                    96
     C*                  ENDIF
     C                   ENDSL
     C                   EXSR      ERRORS
     CSR                 ENDSR
     C*----------------------------------------------------
     CSR   DUPCHK        BEGSR
     C                   MOVE      OMWO#         FLD1              1
     C                   EXSR      CKFLD
     C   10              MOVE      HDWO#         OMWO#
     C                   MOVE      OMWO#         HDWO#             7
     C*                  MOVE      OMACCT        FLD1
     C*                  EXSR      CKFLD
     C*  10              MOVE      HDACCT        OMACCT
     C*                  MOVE      OMACCT        HDACCT           10
     C                   MOVE      OMPR          FLD1
     C                   EXSR      CKFLD
     C   10              MOVE      HDPR          OMPR
     C                   MOVE      OMPR          HDPR              2
     C                   MOVE      OMCONT        FLD1
     C                   EXSR      CKFLD
     C   10              MOVE      HDCONT        OMCONT
     C                   MOVE      OMCONT        HDCONT           30
     C*                  MOVE      OMMFG         FLD1
     C*                  EXSR      CKFLD
     C*  10              MOVE      HDMFG         OMMFG
     C*                  MOVE      OMMFG         HDMFG            30
     CSR                 ENDSR
     C*----------------------------------------------------
     C*C K F L D -- CHECKS HEX VALUE OF FLD1 FOR DUP VALUE
     CSR   CKFLD         BEGSR
     C                   TESTB     '345'         FLD1                     10
     C   10              TESTB     '01267'       FLD1                 10
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Edit screen information.
     CSR   EDIT          BEGSR
     C                   SETOFF                                       26
      * F16 - cancel requisition
     C                   IF        *INKQ
     C                   IF        OMSTAT = 'C' AND OMPO# <> *ZEROS
     C                   Z-ADD     79            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      EDIT9
     C                   ENDIF

     C                   SELECT
     C*                  WHEN      USRLVL <> 'INT'
     C                   WHEN      USRLVL <> 'NOV'
     C                   IF        OMSTAT = 'A' OR OMSTAT='E'
     C                   EVAL      OMSTAT = 'D'
     C                   ELSE
     C                   EVAL      OMSTAT = 'A'
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF
     C*  KN
     C*R KQ              MOVE      'D'           OMSTAT
     C     OMSTAT        COMP      'D'                                    15
     C                   EXSR      INFO
     C   96              GOTO      EDIT9
     C*::::: No errors detected in INFO subroutine
      * no need for cost override if vendpart set up for this org
     C                   IF        *INKS
     C                   IF        OMVN# > *ZEROS
     C                   IF        NOT(OIPART = CSFRGHT OR OIPART=ESFRGHT OR
     C                                 OIPART = CDSALES OR OIPART = PPSALES OR
     C                                 OIPART = CSTARIF OR OIPART = CSFRFSC OR
     C                                 %SUBST(OIPART:1:2)='ES' OR
     C                                 OIPART = ESPROCS OR OIPART=CSRUSH)
     C                   EXSR      UPDVY
     C   96              GOTO      EDIT9
     C                   ENDIF
     C                   ENDIF
     C                   IF        OMCODE='D'
     C     #VOKEYD       CHAIN(N)  VOFMT                              20        get Org_Xref costs
     C                   ELSE
     C     #VOKEYI       CHAIN(N)  VOFMT                              20        get SPECIFIC costs
     C                   ENDIF
     C                   IF        VRORIG<>OMDC
     C                   IF        *IN20 OR VODEL = ' '
     C                   IF        NOT(OIPART = CSFRGHT OR OIPART=ESFRGHT OR
     C                                 OIPART = CDSALES OR OIPART = PPSALES OR
     C                                 OIPART = CSTARIF OR OIPART = CSFRFSC OR
     C                                 %SUBST(OIPART:1:2)='ES' OR
     C                                 OIPART = ESPROCS OR OIPART=CSRUSH)
     C                   EXSR      UPDVO
     C   96              GOTO      EDIT9
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
      * update vendpart and master spec
     C                   EXSR      UPDVP
     C   96              GOTO      EDIT9
     C                   ENDIF
     C                   ENDIF
     C                   MOVEA     *BLANKS       MSG
     C   25KEY           COMP      SVKEY                              2626
     C*::::: Convert special data before output to disk,
     C*::::: for example, conversion of alpha data to BITS for storage.
     C                   EXSR      CHANGE
     CSR   EDIT9         ENDSR
     C*----------------------------------------------------------------
     C* Prepare data before final output to disk
     CSR   CHANGE        BEGSR
     C                   SETOFF                                       2021
     C                   SELECT
      * type "O" means creating new po from requisition
     C     TYPE          WHENEQ    'O'                                          Order an item
     C                   EXSR      CKOFF
     C   96              GOTO      CHANG9
     C                   RESET                   OHFMT
     C                   RESET                   OIFMT
     C                   RESET                   OITFMT
0104 C*                  RESET                   OAFMT
0104 C*                  EXSR      CHKIV
     C   96              GOTO      CHANG9
     C                   EVAL      OILIN#=001
     C                   EXSR      GETPO
     C   96              GOTO      CHANG9
     C                   Z-ADD     CHGQTY        OIQTY                  20
     C   20              Z-ADD     0             OIQTY
     C                   Z-ADD     CHGQTY        OMPOQ                  20
     C   20              Z-ADD     0             OMPOQ
     C*                  ADD       CHGQTY        IVONOR                 20
     C*  20              Z-ADD     0             IVONOR
0104 C*                  EXSR      UPDIV
      * type "C" - means adding to existing po
     C     TYPE          WHENEQ    'C'                                          Create an IVMGR item
     C     OMPART        IFEQ      *BLANKS
     C                   Z-ADD     39            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      CHANG9
     C                   ENDIF
     C*                  EVAL      OILIN#=001
     C                   EXSR      GETPO
     C                   EXSR      CKOFF
     C   96              GOTO      CHANG9
     C                   Z-ADD     CHGQTY        OIQTY                  20
     C   20              Z-ADD     0             OIQTY
     C                   Z-ADD     CHGQTY        OMPOQ                  20
     C   20              Z-ADD     0             OMPOQ
     C*                  RESET                   IVMFMT
     C*                  RESET                   IVPFMT
     C*                  RESET                   IVOFMT
     C*                  RESET                   IVTFMT
0104 C*`                 EXSR      WRTIV
     C                   OTHER
     C                   ENDSL
      * reset type code after each req convert to PO
     C                   EVAL      TYPE=' '
     CSR   CHANG9        ENDSR
     C*----------------------------------------------------------------
     CSR   NEXTREC       BEGSR
      *position to new open requistion
     C                   EVAL      SVPOYY=OMPOYY
     C                   EVAL      SVPOPP=OMPOPP
     C                   EVAL      SVPO#=OMPO#
     C                   EVAL      SVREQ=OMREQ
     C                   EVAL      SVDC=OMDC
     C                   EVAL      SVCODE=OMCODE
     C*    #OTKEY        CHAIN(N)  OMFMT2
     C     #KEYNX        SETGT     DTAFMT
     C     NXTREC        TAG
     C                   READ(N)   DTAFMT                                 20
     C     *IN20         IFEQ      '0'
     C*  93OMDC          CABNE     O2DC          NXTREC
     C*  93OMSTAT        CABNE     'A'           NXTREC
     C   93              IF        OMDC <> SVDC AND OMCODE <> SVCODE
     C                             AND OMSTAT <> 'A'
     C                   GOTO      NXTREC
     C                   ENDIF

     C                   MOVE      OMYY          NXK01
     C                   MOVE      OMREQ         NXK03
     C*                  Z-ADD     34            E
     C*                  EXSR      ERRORS
     C                   ELSE
     C                   SETON                                        90
     C                   EVAL      NXK03=*HIVAL
     C                   Z-ADD     25            E
     C                   EXSR      ERRORS
     C                   ENDIF
     CSR   NEXTREC9      ENDSR
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     CSR   CKOFF         BEGSR
     C*    OMNN          IFNE      @O
     C*    OMCODE        IFEQ      'M'
     C*    OMCODE        OREQ      'P'
     C*    OMCODE        OREQ      'O'
     C*    OMCODE        OREQ      'T'
     C*                  Z-ADD     51            E                    96
     C*                  EXSR      ERRORS
     C*                  ENDIF
     C*                  ENDIF
     CSR   NOOFF         ENDSR
     C*----------------------------------------------------------------
     CSR   GETPO         BEGSR
     C*::::: Perform validation on UOM.
     C                   MOVEL     OMUOM         VZZVAL           15
     C     VZZVAL        LOOKUP    BUM                                    20
     C  N20              Z-ADD     54            E                    96
     C  N20              EXSR      ERRORS
     C  N20              GOTO      GETPO9
     C*    @P            CABNE     '11'          GETPO9
     C*    OMVN#         IFEQ      0
     C                   IF        OMVN#=0 AND TYPE='O'
     C                   Z-ADD     49            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      GETPO9
     C                   ENDIF
     C*                  MOVE      00            OHYY
     C                   MOVE      OMYY          OHYY
     C*                  MOVE      00            OHPP
     C*                  MOVE      @O            OHDC
     C                   MOVE      OMDC          OHDC
     C*                  MOVE      '94'          OHPP
     C*                  MOVE      VRNN          OHPP
     C                   MOVE      OMPP          OHPP
     C
      *no po # enter - create new
     C                   SELECT
     C     PO#           WHENEQ    *BLANKS                                      New PO requested
     C     OMPO#         IFNE      0
     C                   Z-ADD     41            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      GETPO9
     C                   ENDIF
     C*    CHGQTY        IFLE      0
     C*                  Z-ADD     46            E                    96
     C*                  EXSR      ERRORS
     C*                  GOTO      GETPO9
     C*                  ENDIF
     C*                  EXSR      PRIMOH
     C*                  EXSR      PRIMOI

      *
      * determine truckload units, create PO's based on truckloads
      *
     C                   EXSR      TLOOP
      * if user doesnt want truckload po's created set loop=1
     C                   IF        TLOAD = 'N'
     C*                            OR TLOAD = 'Y'
     C                   EVAL      LOOP=1
     C                   ENDIF
     C                   DO        LOOP          LP                3 0
     C                   EXSR      PRIMOH
     C                   EXSR      PRIMOI
     C   96              GOTO      GETPO9

      * if total $ PO exceeds user $ max,  write po, display warning
     C*                  IF        POTOT < POMIN
     C*                  ELSE
     C                   IF        POTOT > POMAX
     C                   EVAL      %SUBST(ERM(66):6:11)=%EDITC(POTOT:'4')
     C                   EVAL      %SUBST(ERM(66):26:10)=%EDITC(POMAX:'4')
     C                   EVAL      OHSTAT = 'H'
     C                   Z-ADD     66            E                    98
     C                   EXSR      ERRORS
     C                   ENDIF
     C*                  ENDIF

     C                   WRITE     OHFMT
     C                   WRITE     OIFMT
     C                   WRITE     OITFMT
     C*                  WRITE     OAFMT

     C                   EXSR      RIGHTSIZE

     C                   ENDDO
      * end of truckload process

      * if loop=1, display single po msg otherwise show multi po msg
     C                   IF        LOOP=1
     C                   Z-ADD     42            E
     C                   ELSE
     C                   MOVEL     LOOP          ERM(73)
     C                   MOVE      PONUM         ERM(73)
     C                   Z-ADD     73            E
     C                   ENDIF

      * otherwise - add to existing po

     C                   OTHER                                                  Use existing PO
     C     DIGITS        CHECK     PO#                                    20
     C     *IN20         IFEQ      '1'
     C                   Z-ADD     47            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      GETPO9
     C                   ENDIF
     C                   MOVE      PO#           OH#

     C*                  IF        OH# < 4556
     C*                  MOVE      '12'          OMLR              2
     C*                  EVAL      PONUMB=OMLR
     C*    #OHMLR        CHAIN     OHFMT                              20
     C*                  ELSE
     C*    #OHKEY        CHAIN     OHFMT                              20
     C*                  ENDIF
     C     #OHKEY        CHAIN     OHFMT                              20
     C     *IN20         IFEQ      '1'
     C                   Z-ADD     45            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      GETPO9
     C                   ENDIF
     C                   EVAL      OMVN#=OHVEND

     C     GETPOX        TAG
     C     #ITMKY        SETLL     OIFMT                                  40
     C   40              ADD       1             OILIN#
     C   40              GOTO      GETPOX

     C   90#OIKEY        CHAIN(N)  OIFMT                              20
     C  N90#OIKEY        CHAIN     OIFMT                              20
     C     *IN20         IFEQ      '1'
     C*    CHGQTY        IFLE      0
     C*                  Z-ADD     46            E                    96
     C*                  EXSR      ERRORS
     C*                  GOTO      GETPO9
     C*                  ENDIF
     C                   MOVEL     *BLANKS       OITSUB
     C     #OITKY        CHAIN(N)  OITFMT                             20
     C* if adding to existing po - bypass vendor match check
     C                   IF        TYPE = 'O'
     C     OMVN#         IFNE      OHVEND
     C                   Z-ADD     48            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      GETPO9
     C                   ENDIF
     C                   ENDIF
      *
mike  * if adding to existing drop ship,  must be same nn id loc
     C                   IF        OMCODE='D'
     C*                  IF        WSNN <> OHNN OR WSID <> OHCUID
     C*                            OR WSLOC <> OHLOC
     C                   IF        NOT(WSNN = OHNN AND WSID = OHCUID
     C                             AND WSLOC = OHLOC)
     C                   Z-ADD     69            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      GETPO9
     C                   ENDIF
     C                   ENDIF
      *
     C                   RESET                   POTOT                          0
     C                   EXSR      PRIMOI
     C   96              GOTO      GETPO9
      * if total $ PO exceeds user $ max,  add to po but display warning
     C*                  IF        POTOT < POMIN
     C*                  ELSE
     C                   IF        POTOT > POMAX
     C                   EVAL      %SUBST(ERM(66):6:11)=%EDITC(POTOT:'4')
     C                   EVAL      %SUBST(ERM(66):26:10)=%EDITC(POMAX:'4')
     C                   EVAL      OHSTAT = 'H'
     C                   Z-ADD     66            E                    98
     C                   EXSR      ERRORS
     C                   ENDIF
     C*                  ENDIF

     C                   WRITE     OIFMT
     C                   WRITE     OITFMT
     C*                  WRITE     OAFMT
     C                   Z-ADD     43            E
     C                   ELSE
     C                   ADD       CHGQTY        OIQTY                  20
     C     *IN20         IFEQ      '1'
     C                   Z-ADD     50            E                    96
     C                   EXSR      ERRORS
     C                   UNLOCK    ORITM
     C                   GOTO      GETPO9
     C                   ENDIF
     C                   MOVE      OIYY          OMPOYY
     C                   MOVE      OIPP          OMPOPP
     C                   Z-ADD     OI#           OMPO#
     C                   MOVE      @USER         OMPOBY
     C                   MOVEL     OIPART        OMPOIT
     C                   MOVEL     OIUOM         OMPOUM
     C                   UPDATE    OIFMT
     C                   Z-ADD     44            E
     C                   ENDIF
     C                   MOVEL     'X'           OHX
     C                   UPDATE    OHFMT
     C                   ENDSL
     C                   MOVEL     'C'           OMSTAT
     C*    OHYY          CAT(P)    OHPP          PO               10
     C*                  MOVE      OH#           PO
     C*                  IF        OH# < 4556
     C*                  EVAL      PONUMB=OMLR
     C*                  ENDIF
     C                   MOVE      PONUM         ERM(E)
     C                   EXSR      ERRORS
     C     GETPO9        TAG
     C   96              UNLOCK    ORHDR                                20
     CSR                 ENDSR
     C*----------------------------------------------------------
     CSR   PRIMOH        BEGSR
     C                   RESET                   POTOT                          0
     C*:::::prime values into OH fields.
      * verify that D/C has valid cost center, error out if bad
     C     OMDC          CHAIN(N)  COAFMT                             20
     C                   MOVEL     PAYOTQ        COSTNO            6 0
     C                   IF        PAYOTQ = *BLANKS
     C*                            OR COSTNO < 700000 OR COSTNO >799999
     C                             OR COSTNO < 700000 OR COSTNO >810999
     C                   Z-ADD     61            E                    98                     Date
     C                   EXSR      ERRORS
     C                   ENDIF
     C     RETRY         TAG
     C*    '11'          CHAIN     COAFMT                             20
     C     '94'          CHAIN     COAFMT                             20
     C                   ADD       1             COAPO#
     C                   IF        COAPO#>999999
     C                   EVAL      COAPO#=100
     C                   ENDIF
     C                   UPDATE    COAFMT
     C                   Z-ADD     COAPO#        OH#
     C                   EVAL      PONUMA='PR'
     C                   EVAL      PONUMB=OMYY
     C*                  IF        OH# < 4556
     C*                  MOVE      '12'          PONUMB            2
     C*                  ENDIF
      * verify that D/C has valid cost center, error out if bad
     C*    OMDC          CHAIN(N)  COAFMT                             20
     C*                  MOVEL     PAYOTQ        COSTNO            6 0
     C*                  IF        PAYOTQ = *BLANKS
     C*                            OR COSTNO < 700000 OR COSTNO >799999
     C*                  Z-ADD     61            E                    98                     Date
     C*                  EXSR      ERRORS
     C*                  ENDIF
     C                   MOVE      OH#           PONUMC
     C     #OHKEY        SETLL     OHFMT                                  20
     C   20              GOTO      RETRY
      * verify that D/C has valid cost center, error out if bad
     C     OMDC          CHAIN(N)  COAFMT                             20
     C                   MOVEL     PAYOTQ        COSTNO            6 0
     C                   IF        PAYOTQ = *BLANKS
     C*                            OR COSTNO < 700000 OR COSTNO >799999
     C                             OR COSTNO < 700000 OR COSTNO >810999
     C                   Z-ADD     61            E                    98                     Date
     C                   EXSR      ERRORS
     C                   ENDIF
     C     OMCODE        IFEQ      'G'                                          GRAPHICS
     C     OMCODE        OREQ      'E'                                          PERSONNEL
     C     OMCODE        OREQ      'L'                                          LOGISTICS
     C     OMCODE        OREQ      'R'                                          FACTORY OUT RETAIL
     C     OMCODE        OREQ      'Z'                                          PRODUCTION
     C     OMCODE        OREQ      'Q'                                          TEST LAB
     C     OMCODE        OREQ      'H'                                          HEALTH CTR
     C     OMCODE        OREQ      'S'                                          NORTH FIELD
     C     OMCODE        OREQ      'C'                                          SALES SVC
     C                   MOVE      'C'           OHCODE                         NO IV OR VQ
     C                   ELSE
     C     OMCODE        IFEQ      'F'                                          FOLDING CARTON
     C                   MOVE      ' '           OHCODE
     C                   ELSE
     C                   MOVE      OMCODE        OHCODE
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      OMVN#         OHVEND
     C                   IF        OMCODE<>'I'
     C                   MOVE      OMWSNN        OHNN
     C                   Z-ADD     OMWSID        OHCUID
     C                   MOVE      OMWSLC        OHLOC
     C                   ENDIF
     C*                  MOVEL     REQBY         OHSATN
     C*                  MOVE      @O            OHDC
     C                   MOVE      OMDC          OHDC
     C                   MOVE      @USER         OHREQ
     C                   MOVE      TODAY         OHRDTE
     C                   MOVEL     'X'           OHX
     C                   MOVE      'N'           OHMETH
      * if processing  type I(inventory replnshmt, get shipto from vendshpr
     C*                  IF        OMCODE = 'I'
     C*    #VSKEY        CHAIN     VSFMT                              20
     C*                  IF        NOT *IN20
     C*                  EVAL      OHSHIP=VSNAME
     C*                  EVAL      OHSATN=VSATTN
     C*                  EVAL      OHSADR=VSADDR
     C*                  EVAL      OHSCTY=VSCITY
     C*                  EVAL      OHSST=VSST
     C*                  EVAL      OHSZP5=VSZIP5
     C*                  EVAL      OHSZP4=VSZIP4
     C*                  IF        NOT *IN20 AND
     C*                            (COADEL = 'W' OR COADEL=' ')
     C* ADDED TO GET PUR GROUP, COST CENTER FROM COADDRES FILE
     C*                  EVAL      OIPURG=PACKNO
     C*                  EVAL      OICSTC=PAYOTQ
     C*                  ENDIF
     C*                  ENDIF
     C
     C*                  ELSE
     C                   IF        OMCODE = 'I'
     C     OHDC          CHAIN(N)  COAFMT                             20
     C                   IF        NOT *IN20 AND
     C                             (COADEL = 'W' OR COADEL=' ')
     C                   MOVEL(P)  COANAM        OHSHIP
     C                   MOVEL(P)  STREET        OHSADR
     C* ADDED TO GET PUR GROUP, COST CENTER FROM COADDRES FILE
     C                   EVAL      OIPURG=PACKNO
     C                   EVAL      OICSTC=PAYOTQ
      * replace leading "prefix" based upon office, per DKR
     C                   SELECT
     C                   WHEN      OHPP='93'
     C                   MOVEL     '798'         OICSTC
     C                   WHEN      OHPP='94'
     C                   MOVEL     '799'         OICSTC
     C                   WHEN      OHPP='95'
     C                   MOVEL     '805'         OICSTC
     C                   WHEN      OHPP='96'
     C                   MOVEL     '810'         OICSTC
     C                   WHEN      OHPP='97'
     C*                  MOVEL     '768'         OICSTC                         old PDG signage
     C*                  MOVEL     '750'         OICSTC                         new PDG mfg
     C                   MOVEL     '751'         OICSTC                         pratt.com
     C                   WHEN      OHPP='98' AND                                new conv paper
     C                             (OHDC = 'MS' OR OHDC = 'DS')
     C                   MOVEL     '750751'      OICSTC                         head ofc per Casey
     C                   WHEN      OHPP='98'                                    new conv paper
     C                   MOVEL     '750'         OICSTC
     C                   ENDSL
     C                   Z-ADD     0             W
     C                   Z-ADD     0             T                 2 0
     C                   Z-ADD     0             ZIPD5
     C                   Z-ADD     0             ZIPD4
     C     ','           SCAN      STCTST        W                 2 0
     C                   SUB       1             W
     C     W             SUBST(P)  STCTST:1      OHSCTY
     C                   ADD       3             W
     C     2             SUBST     STCTST:W      OHSST
     C     '-'           SCAN      STCTST        W
     C     W             SUB       5             T
     C     5             SUBST     STCTST:T      ZIP5
     C                   ADD       1             W
     C     4             SUBST     STCTST:W      ZIP4
     C                   Z-ADD     ZIPD5         OHSZP5
     C                   Z-ADD     ZIPD4         OHSZP4
     C                   MOVE      @USER         OHCHBY
     C                   MOVE      TODAY         OHCHDT
     C                   MOVE      ISOTIME       OHCHTM
     C                   ENDIF
     C                   ELSE
     C     OHNN          IFNE      '  '
     C     OHCUID        ANDNE     0
     C     CSKEY         CHAIN(N)  CSFMT                              20
     C                   IF        NOT *IN20
     C                   MOVEL     CSNAME        OHSHIP
     C                   MOVEL     CSADDR        OHSADR
     C                   MOVEL     CSCITY        OHSCTY
      * usa address
     C                   IF        CSCTRY = *BLANKS
     C                   MOVEL     CSST          OHSST
     C                   Z-ADD     CSZIP5        OHSZP5
     C                   Z-ADD     CSZIP4        OHSZP4
     C                   EVAL      OHSTCTRY=*BLANKS
     C                   EVAL      OHSTPCD=*BLANKS
     C                   ELSE
      * foreign address
     C                   EVAL      OHSTCTRY=CSCTRY
     C                   EVAL      OHSTPCD=CSPOCD
     C                   MOVEL     CSST          OHSST
     C                   Z-ADD     *ZEROS        OHSZP5
     C                   Z-ADD     *ZEROS        OHSZP4
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     CSR                 ENDSR
     C*----------------------------------------------------------------
     CSR   PRIMOI        BEGSR
     C* Placing a ' ' into OISTAT ensures that POUPD will add the OIQTY
     C*:::::prime values into OI fields.
     C                   Z-ADD     OH#           OI#
     C                   MOVE      OHYY          OIYY
     C                   MOVE      OHPP          OIPP
     C                   MOVE      ' '           OISTAT
     C                   IF        LP=1
     C                   EVAL      CHGQTY=FULL
     C                   ENDIF
     C                   IF        LOOP=LP
     C                   EVAL      LPP=LOOP-1
     C                   EVAL      CHGQTY=OMQTY-(FULL*LPP)
     C                   ENDIF
     C                   Z-ADD     CHGQTY        OIQTY
     C*                  IF        OMWO#=*BLANKS
     C                   IF        OMDDAT = *LOVAL
     C     TODAY         ADDDUR    OMLEAD:*D     OIDDAT
     C                   ELSE
     C                   EVAL      OIDDAT=OMDDAT
     C                   ENDIF
      * calculate po due date so its not on holiday or weekend
     C                   MOVE      OIDDAT        DA80
     C                   CALL      'DAOFWKCL'
     C                   PARM                    DA80
     C                   PARM                    DURA              5 0
     C                   PARM      'B'           DIRE              1
     C                   PARM                    WSAT              1
     C                   PARM                    WSUN              1
     C                   PARM                    WHOL              1
     C                   PARM                    DAX               1 0
     C                   PARM                    DAXA              2
     C                   PARM                    HOLIDAY           1
     C                   PARM                    FRIDAY            8 0
     C                   PARM                    FCCYY             4
     C                   PARM                    FMM               2
     C                   PARM                    WORKDA            8 0
     C                   PARM                    DAW               1 0
     C                   PARM                    DAWA              2
     C                   PARM                    LFRIDAY           8 0
     C                   PARM                    LFCCYY            4
     C                   PARM                    LFMM              2
     C                   MOVE      WORKDA        DATEISO
     C                   MOVE      DATEISO       OIDDAT

     C                   MOVEL     OMPART        OIPART
     C                   MOVE      OMDESC        OIDESC
     C                   EVAL      OIRQTY=*ZEROS
     C                   EVAL      OIBQTY=*ZEROS
      * prime po tag line
     C                   EVAL      PONUMA='PR'
     C                   EVAL      PONUMB=OMYY
     C*                  IF        OH# < 4556
     C*                  MOVE      '12'          PONUMB            2
     C*                  ENDIF
     C                   MOVE      OH#           PONUMC

      * added to determine next line # for selected po
     C                   EVAL      OILIN#=001
     C     PRIMKY        TAG
     C     #ITMKY        SETLL     OIFMT                                  40
     C   40              ADD       1             OILIN#
     C   40              GOTO      PRIMKY

     C     2             OCCUR     OIDS
     C     #ITPOK        SETLL     OIFMT                                  40
     C                   DOW       'X' = 'X'
     C     #ITPOK        READE     OIFMT                                  40
     C   40              LEAVE
      * determine $ total for PO (before adding new item)
     C                   IF        OISTAT <> 'X'
     C                   MONITOR
     C                   EVAL(H)   POTOT=POTOT+(OIQTY*OIUNIT)
     C                   ON-ERROR
     C                   EVAL      POTOT=*HIVAL
     C                   ENDMON
     C                   ENDIF
     C                   ENDDO
     C     1             OCCUR     OIDS

     C*                  MOVEL     OMUOM         OIUOM

     C                   Z-ADD     OH#           OI#
      * default for all to calculate cost
     C     OMPART        CHAIN(N)  VRFMT                              20
     C     *IN20         IFEQ      '0'
     C*                  EXSR      COSTS
     C                   IF        OMVN# > *ZEROS
     C                   EXSR      UPDVY
     C   96              GOTO      PRIMOI9
     C                   ENDIF
     C                   IF        OMCODE='D'
     C     #VOKEYD       CHAIN(N)  VOFMT                              20        get Org_Xref costs
     C                   ELSE
     C     #VOKEYI       CHAIN(N)  VOFMT                              20        get SPECIFIC costs
     C                   ENDIF
     C                   IF        VRORIG<>OMDC
     C                   IF        *IN20 OR VODEL = ' '
     C                   EXSR      UPDVO
     C                   ENDIF
     C                   ELSE
      * update vendpart and master spec
     C                   EXSR      UPDVP
     C                   ENDIF

     C* PULL SAP DATA FROM VENDPATG FILE BASED ON MAJMIN CODE IN VENDPART
     C                   MOVEL(P)  MAJMIN        QQKEY            20
     C     QQKEY         CHAIN     QVQFMT                             20
     C                   EVAL      OIMTLG=QVQMTLG
     C                   EVAL      OIGLACT=QVQGLA
     C                   EVAL      OIMTLN=QVQMTL
     C                   EVAL      OIUOM=VRPER1
     C                   Z-ADD     OMEST$        OIUNIT
     C                   MOVE      VRIT          OISKU
     C                   MOVE      VRPER1        OMUOM
     C                   IF        OMUOM=*BLANKS
     C                   EVAL      OMUOM='EACH'
     C                   ENDIF
     C                   ENDIF

      * accumulate PO $ tl
     C                   MONITOR
     C*                  EVAL      POTOT=POTOT+(OMQTY*OMEST$)
     C                   EVAL      POTOT=POTOT+(OIQTY*OIUNIT)
     C                   ON-ERROR
     C                   EVAL      POTOT=*HIVAL
     C                   Z-ADD     63            E                    96
     C                   EXSR      ERRORS
     C                   ENDMON

     C*:::::prime values into OITX fields.
     C     OHDC          CHAIN(N)  COAFMT                             20
     C  N20              EVAL      OIPURG=PACKNO
     C  N20              EVAL      OICSTC=PAYOTQ
      * replace leading "prefix" based upon office, per DKR
     C                   SELECT
     C                   WHEN      OHPP='93'
     C                   MOVEL     '798'         OICSTC
     C                   WHEN      OHPP='94'
     C                   MOVEL     '799'         OICSTC
     C                   WHEN      OHPP='95'
     C                   MOVEL     '805'         OICSTC
     C                   WHEN      OHPP='96'
     C                   MOVEL     '810'         OICSTC
     C                   WHEN      OHPP='97'
     C*                  MOVEL     '768'         OICSTC                         old PDG signage
     C*                  MOVEL     '750'         OICSTC                         new PDG mfg
     C                   MOVEL     '751'         OICSTC                         pratt.com
     C                   WHEN      OHPP='98' AND                                new conv paper
     C                             (OHDC = 'MS' OR OHDC = 'DS')
     C                   MOVEL     '750751'      OICSTC                         head ofc per Casey
     C                   WHEN      OHPP='98'                                    new conv paper
     C                   MOVEL     '750'         OICSTC
     C                   ENDSL
     C* DEFAULT ALL TO 94 FOR PLANT CODE OIPP
     C*                  EVAL      OIPP='94'
     C                   MOVE      OIYY          OITYY
     C                   MOVE      OIPP          OITPP
     C                   Z-ADD     OI#           OIT#
     C                   EVAL      OITLN#=OILIN#
     C                   MOVEL     OIPART        OITPRT
     C                   MOVEL     *BLANKS       OITSUB
     C                   EVAL      OITXTU1='E'
     C                   EVAL      OITXTU2='E'
     C                   EVAL      OITXTU3='E'
     C                   EVAL      OITXTU4='E'
     C                   EVAL      OITXTU5='E'
     C                   EVAL      OITXTU6='E'
     C                   EVAL      OITXTU7='E'
     C                   EVAL      OITXTU8='E'
     C                   IF        (%SUBST(OMTXT1:2:49)<> *BLANKS)
     C                   EVAL      OITXT1 = %SUBST(OMTXT1:2:49)
     C                   ENDIF
     C                   IF        (%SUBST(OMTXT1:1:1)= 'I')
     C                   EVAL      OITXTU1 = 'I'
     C                   ELSE
     C                   EVAL      OITXTU1 = 'E'
     C                   ENDIF

     C                   IF        (%SUBST(OMTXT2:2:49)<> *BLANKS)
     C                   EVAL      OITXT2 = %SUBST(OMTXT2:2:49)
     C                   ENDIF
     C                   IF        (%SUBST(OMTXT2:1:1)= 'I')
     C                   EVAL      OITXTU2 = 'I'
     C                   ELSE
     C                   EVAL      OITXTU2 = 'E'
     C                   ENDIF

     C                   IF        (%SUBST(OMTXT3:2:49)<> *BLANKS)
     C                   EVAL      OITXT3 = %SUBST(OMTXT3:2:49)
     C                   ENDIF
     C                   IF        (%SUBST(OMTXT3:1:1)= 'I')
     C                   EVAL      OITXTU3 = 'I'
     C                   ELSE
     C                   EVAL      OITXTU3 = 'E'
     C                   ENDIF

     C*                  MOVE      REQBY         OITXT1
     C*                  MOVEL     OMTXT1        OITXT3
     C*                  MOVEL     OMTXT2        OITXT4
     C*                  MOVEL     OMTXT3        OITXT5
     C                   EVAL      OIREQ=OMREQ
     C                   IF        @OMJOBX <> *BLANKS
     C                   EVAL      OIWSNN=WSNN
     C                   EVAL      OIWSJOB=WSJOB
     C                   EVAL      OIWSLOC=OMWSLC
     C                   EVAL      OIWSYMD=OMPOQR
     C                   EVAL      OIWSTIME=OMMSD#
     C                   ENDIF
     C* JOB HERE
     C*                  EVAL      REQINFO='Requisition# '+%TRIM(%CHAR(OMREQ))
     C*                  EVAL      REQINFO='Requisition# '+%CHAR(OMREQ)
     C*                  EVAL      REQINFO='Requisition# '+
     C*                                    %TRIM(%EDITW(OMREQ:'0      '))
     C*                  EVAL      OITXT8=REQINFO
     C*:::::prime values into OA fields.
     C*                  Z-ADD     OH#           OA#
     C*                  MOVE      OHYY          OAYY
     C*                  MOVE      OHPP          OAPP
     C*                  MOVEL     OIPART        OAPART
     C*                  Z-ADD     1.00          OAPCT
     C* Update Requisition to reflect PO Items Created
     C                   EVAL      OMVN#=OHVEND
     C                   MOVEL     OIPART        OMPOIT
     C                   MOVEL     OIUOM         OMPOUM
     C* Update Requisition to reflect PO Created
     C                   MOVE      OIYY          OMPOYY
     C                   MOVE      OIPP          OMPOPP
     C                   Z-ADD     OI#           OMPO#
     C                   MOVE      @USER         OMPOBY
     C                   IF        OMPODT = *LOVAL
     C                   EVAL      OMPODT=TODAY
     C                   ENDIF
     C                   EVAL      OMPODD=OIDDAT

     C*                  IF        OMPODD = *LOVAL
     C*    TODAY         ADDDUR    OMLEAD:*D     OMPODD
     C*                  ENDIF
     C*                  EVAL      OITXT1=VYDESC
     C                   MOVE      @USER         OICHBY
     C                   MOVE      TODAY         OICHDT
     C                   MOVE      ISOTIME       OICHTM
     CSR   PRIMOI9       ENDSR
     C*----------------------------------------------------------------
     CSR   RIGHTSIZE     BEGSR

      * Determine if VENDOR in right-size list of suppliers
     C                   EVAL      L0LAKT='RSV'
     C                   EVAL      L0LAK=%TRIM(%EDITW(OHVEND:'0      '))
     C     #L0KEY        CHAIN     L0FMT                              20
     C                   IF        *IN20 OR L0DEL='D'
     C                   GOTO      RIGHTSIZE9
     C                   ENDIF

      * Determine if WH in right-size list of warehouses
     C                   EVAL      L0LAKT='RSZ'
     C                   EVAL      L0LAK=OHDC
     C     #L0KEY        CHAIN     L0FMT                              20
     C                   IF        *IN20 OR L0DEL='D'
     C                   GOTO      RIGHTSIZE9
     C                   ENDIF

      * Determine linked pallet
      * ..check specific site override first
     C                   EVAL      RSWH=OHDC
     C     #RSKEY        CHAIN     RSFMT                              20
     C                   IF        *IN20 OR RSDEL='D'
      * ..not found, check generic entry
     C                   EVAL      RSWH=*BLANKS
     C     #RSKEY        CHAIN     RSFMT                              20
     C                   ENDIF
     C                   IF        *IN20 OR RSDEL='D' OR RSPARTP=*BLANKS
     C                   GOTO      RIGHTSIZE9
     C                   ENDIF

     C                   EXSR      ADDPALLET

     CSR   RIGHTSIZE9    ENDSR
     C*---------------------------------------------------------------------
     CSR   ADDPALLET     BEGSR

      * get pallet info
     C     #VRMQKEY      CHAIN(N)  MQFMT                              20        MASTER ITEM
     C  N20RSPARTP       CHAIN(N)  VRFMT                              20
     C                   IF        *IN20 OR VRDEL='D' OR MQDEL='D'
     C                   GOTO      ADDPALLET9
     C                   ENDIF

      * set qty as number of units
     C                   MONITOR
     C                   EVAL      @QTY =%DIV(OIQTY:MQUNIQ)
     C                   ON-ERROR
      * ..branch without adding if cannot calc
     C                   GOTO      ADDPALLET9
     C                   ENDMON
     C                   IF        %REM(OIQTY:MQUNIQ)<>0
     C                   EVAL      @QTY=@QTY+1
     C                   ENDIF
      * ..for pallets, factor in triple-stacked items (1 per pallet position)
     C*                  EVAL      P0LAK=RSPART
     C*                  EVAL      P0LAKT='TPL'
     C*    #P0KEY        CHAIN     P0FMT                              20
     C*                  IF        NOT *IN20 AND P0DEL<>'D'
     C*                  EVAL      @QTY=%DIV(@QTY:3)
     C*                  IF        %REM(@QTY:3)<>0
     C*                  EVAL      @QTY=@QTY+1
     C*                  ENDIF
     C*                  ELSE
      * ..for pallets, half it for double-stacked items (1 per pallet position)
     C*                  EVAL      P0LAKT='DBL'
     C*    #P0KEY        CHAIN     P0FMT                              20
     C*                  IF        NOT *IN20 AND P0DEL<>'D'
     C*                  EVAL(H)   @QTY=@QTY/2
     C*                  ENDIF
     C*                  ENDIF
     C                   EVAL      OIQTY=@QTY

      * ..costing info
     C                   EXSR      GET_CST

      * set up remaining line info for pallet
     C     PRIMKY2       TAG
     C     #ITMKY        SETLL     OIFMT                                  40
     C   40              ADD       1             OILIN#
     C   40              GOTO      PRIMKY2
     C                   EVAL      OIPART=RSPARTP
     C                   EVAL      OIDESC=VQDESC
      *.. pull sap data from vendpatg file based on majmin code in vendpart
     C                   MOVEL(P)  MAJMIN        QQKEY            20
     C     QQKEY         CHAIN     QVQFMT                             20
     C                   EVAL      OIMTLG=QVQMTLG
     C                   EVAL      OIGLACT=QVQGLA
     C                   EVAL      OIMTLN=QVQMTL
     C                   EVAL      OIUOM=VRPER1
     C                   MOVE      VRIT          OISKU
     C                   EVAL      OIWSNN=*BLANKS
     C                   EVAL      OIWSJOB=*ZEROS
     C                   EVAL      OIWSLOC=*BLANKS
     C                   EVAL      OIWSYMD=*ZEROS
     C                   EVAL      OIWSTIME=*ZEROS

      * accumulate PO $ tl
     C                   MONITOR
     C                   EVAL      POTOT=POTOT+(OIQTY*OIUNIT)
     C                   ON-ERROR
     C                   EVAL      POTOT=*HIVAL
     C                   Z-ADD     63            E                    96
     C                   EXSR      ERRORS
     C                   ENDMON

      * set up comment
     C                   EVAL      OITLN#=OILIN#
     C                   MOVEL     OIPART        OITPRT
     C                   MOVEL     *BLANKS       OITSUB
     C                   EVAL      OITXTU1='E'
     C                   EVAL      OITXTU2='E'
     C                   EVAL      OITXTU3='E'
     C                   EVAL      OITXTU4='E'
     C                   EVAL      OITXTU5='E'
     C                   EVAL      OITXTU6='E'
     C                   EVAL      OITXTU7='E'
     C                   EVAL      OITXTU8='E'
     C                   EVAL      OITXT1='Right-size ' + OIPART

      * write OI,OIT
     C                   WRITE     OIFMT
     C                   WRITE     OITFMT

     CSR   ADDPALLET9    ENDSR
     C*-------------------------------------------------------------------
     C* Disk file error routine.
     CSR   INF$R         BEGSR
     C     *IN90         IFEQ      '0'
     C     *IN89         ANDEQ     '1'
     C     STATU$        IFEQ      1021
     C*::::: Note error if NEW KEY already exists in UNIQUE file
     C                   Z-ADD     19            E                    96
     C   25
     CAN 26*IN20         IFEQ      '0'
     C*::::: Restore OLD record if NEW KEY cannot be added
     C                   SETON                                        20
     C                   MOVE      SVKEY         KEY
     C                   WRITE     DTAFMT
     C                   ELSE
     C                   Z-ADD     20            E                    96
     C                   ENDIF
     C                   ELSE
     C                   MOVE      STATU$        ERM(23)
     C                   Z-ADD     23            E                    96
     C                   ENDIF
     C                   GOTO      NOUPDT
     C                   ENDIF
     CSR                 ENDSR
     C*-----------------------------------------------------------------
     CSR   SRCH1         BEGSR
     C                   SETOFF                                       38
     C     SRCODE        CABEQ     *BLANKS       SRCH19
     C*::::: define STRING as long enough to contain LONGEST FIELD
     C*::::: define as DS if >256 characters
     C                   MOVE      *BLANKS       STRING           30
     C                   Z-ADD     1             SR#               2 0
     C     SRCODE        LOOKUP    SRA(SR#)                               20
     C  N20              MOVE      '????'        SRCODE
     C  N20              SETON                                        98
     C   20PATTRN        COMP      *BLANKS                            38
     C   20
     CAN 38              Z-ADD     SRN(SR#)      STRLEN            3 0
     C                   Z-ADD     20            S                 3 0  20
     C   38S             DOWGT     0
     C     STR(S)        IFNE      ' '
     C     STR(S)        IFEQ      ''''
     C     S             SUB       1             PATLEN            3 020
     C                   ELSE
     C                   Z-ADD     S             PATLEN               20
     C                   ENDIF
     C                   ENDIF
     C                   SUB       1             S
     C  N20              ENDDO
     C     PATLEN        IFGT      STRLEN
     C                   Z-ADD     29            E                    98  38
     C                   EXSR      ERRORS
     C                   ENDIF
     CSR   SRCH19        ENDSR
     C*----------------------------------------------------------------
     C* Prime the fields to be searched.
     CSR   SRCH2         BEGSR
     C                   SELECT
     C     SR#           WHENEQ    1
     C                   MOVEL     OMSTAT        STRING
     C     SR#           WHENEQ    2
     C                   MOVEL     OMWO#         STRING
     C     SR#           WHENEQ    3
     C                   MOVEL     OMPART        STRING
     C     SR#           WHENEQ    4
     C                   MOVEL     OMCODE        STRING
     C                   WHEN      SR#=5 AND OMRQDT>*LOVAL
     C                   MOVE      OMRQDT        ISODATE
     C                   MOVEL     ISODATE       STRING
     C     SR#           WHENEQ    6
     C                   MOVEL     OMBY          STRING
     C     SR#           WHENEQ    7
     C                   MOVEL     OMCONT        STRING
     C     SR#           WHENEQ    8
     C                   MOVEL     OMAPP         STRING
     C     SR#           WHENEQ    9
     C                   MOVEL     OMPOIT        STRING
     C*    SR#           WHENEQ    10
     C*                  MOVEL     OMEXPC        STRING
     C     SR#           WHENEQ    11
     C                   MOVEL     OMVN#         STRING
     C     SR#           WHENEQ    12
     C                   MOVEL     OMDESC        STRING
     C     SR#           WHENEQ    13
     C                   MOVEL     OMDC          STRING
     C     SR#           WHENEQ    14
     C                   MOVEL     OMCUPO        STRING
     C                   ENDSL
     CSR                 ENDSR
     C*----------------------------------------------------------------
     CSR   SRCH1A        BEGSR
     C                   SETOFF                                       39
     C     TRCODE        CABEQ     *BLANKS       SRCH99
     C*::::: define STRING as long enough to contain LONGEST FIELD
     C*::::: define as DS if >256 characters
     C                   MOVE      *BLANKS       STRNG2           30
     C                   Z-ADD     1             TR#               2 0
     C     TRCODE        LOOKUP    TRA(TR#)                               20
     C  N20              MOVE      '????'        TRCODE
     C  N20              SETON                                        98
     C   20PTTRN2        COMP      *BLANKS                            39
     C   20
     CAN 39              Z-ADD     TRN(TR#)      STRLN2            3 0
     C                   Z-ADD     20            T                      20
     C   39T             DOWGT     0
     C     TTR(T)        IFNE      ' '
     C     TTR(T)        IFEQ      ''''
     C     T             SUB       1             PATLN2            3 020
     C                   ELSE
     C                   Z-ADD     T             PATLN2               20
     C                   ENDIF
     C                   ENDIF
     C                   SUB       1             T
     C  N20              ENDDO
     C     PATLN2        IFGT      STRLN2
     C                   Z-ADD     29            E                    98  39
     C                   EXSR      ERRORS
     C                   ENDIF
     CSR   SRCH99        ENDSR
     C*---------------------------------------------------------------
     C* Prime the fields to be searched.
     CSR   SRCH2A        BEGSR
     C                   SELECT
     C     TR#           WHENEQ    1
     C                   MOVEL     OMSTAT        STRNG2
     C     TR#           WHENEQ    2
     C                   MOVEL     OMWO#         STRNG2
     C     TR#           WHENEQ    3
     C                   MOVEL     OMPART        STRNG2
     C     TR#           WHENEQ    4
     C                   MOVEL     OMCODE        STRNG2
     C                   WHEN      TR#=5 AND OMRQDT>*LOVAL
     C                   MOVE      OMRQDT        ISODATE
     C                   MOVEL     ISODATE       STRNG2
     C     TR#           WHENEQ    6
     C                   MOVEL     OMBY          STRNG2
     C     TR#           WHENEQ    7
     C                   MOVEL     OMCONT        STRNG2
     C     TR#           WHENEQ    8
     C                   MOVEL     OMAPP         STRNG2
     C     TR#           WHENEQ    9
     C                   MOVEL     OMPOIT        STRNG2
     C     TR#           WHENEQ    11
     C                   MOVEL     OMVN#         STRNG2
     C     TR#           WHENEQ    12
     C                   MOVEL     OMDESC        STRNG2
     C     TR#           WHENEQ    13
     C                   MOVEL     OMDC          STRNG2
     C     TR#           WHENEQ    14
     C                   MOVEL     OMCUPO        STRNG2
     C                   ENDSl
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Blank fields on the UPDATE screen.
     CSR   BLANKS        BEGSR
     C                   RESET                   DTAFMT
     C*::::: set BITS in special fields
     C                   MOVEL     BM            DTASAV
     C                   EVAL      OMDC=@O
     C                   IF        SVCONT <> *BLANKS
     C                   EVAL      OMCONT=SVCONT
     C                   ENDIF
     C*::::: Blank & z-add 0 to related info as required
     C                   RESET                   OMTFMT
     C                   MOVEL     BMOT          SVOT
     C                   CLEAR                   CSNAME
     C                   CLEAR                   VNNAME
     C                   CLEAR                   REQBY
     C                   CLEAR                   BUYER
     C                   CLEAR                   APPROV
     C                   CLEAR                   PONUM
     C                   CLEAR                   POUNIT
     C                   CLEAR                   STCTST
     C
     C                   MOVE      *BLANKS       TYPE
     C                   MOVE      *BLANKS       PO#
     C                   Z-ADD     0             CHGQTY
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Determine truckload loops
     CSR   TLOOP         BEGSR
      *
     C                   Z-ADD     *ZEROS        FCT1              9 5
     C                   Z-ADD     *ZEROS        LOOP              4 0
     C                   IF        VRUNIQ = 0 OR VRUPTL = 0
     C                   EVAL      FCT1 = 1
     C                   ELSE
     C                   EVAL(h)   FCT1=OMQTY/(VRUNIQ*VRUPTL)
     C                   ENDIF
     C                   IF        FCT1 < 1.00
     C                   EVAL      LOOP=1
     C                   EVAL      FCT1=1.00
     C                   ELSE
     C                   IF        VRUNIQ = 0 OR VRUPTL = 0
     C                   EVAL      LOOP = 1
     C                   ELSE
     C                   EVAL(H)   LOOP=OMQTY/(VRUNIQ*VRUPTL)
     C                   ENDIF
     C                   ENDIF
     C     OMQTY         DIV       FCT1          FULL              8 0
     C                   IF        FCT1 > LOOP
     C                   EVAL      LOOP=LOOP+1
     C                   ENDIF
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Fill the MSG array.
     CSR   ERRORS        BEGSR
     C     E             IFGT      0
     C   20              BITON     '0'           IND20             1
     C  N20              BITOFF    '0'           IND20
     C                   Z-ADD     1             M@                1 0
     C     *BLANKS       LOOKUP    MSG(M@)                                20
     C   20              MOVE      ERM(E)        MSG(M@)
     C                   Z-ADD     0             E                 2 0
     C                   TESTB     '0'           IND20                    20
     C                   ENDIF
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Toggle : Inquiry (N91) v. Detail/UPDATE (91)
     CSR   SRKB          BEGSR
     C     'X'           COMP      'X'                                899290
     C  N91              Z-ADD     0             TOGGLE            1 0
     C   91              Z-ADD     1             TOGGLE
     C     TOGGLE        COMP      0                                      91
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Toggle : ShoDel (N93) V. NoShoDel (93)
     CSR   SRKH          BEGSR
     C  N93              MOVE      *BLANK        SHODEL            1
     C   93              MOVE      'Y'           SHODEL
     C     SHODEL        COMP      *BLANK                                 93
     C*                  Z-ADD     1             YESRL             1 0
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Toggle : MSR# (N92) V. SortKey (92)
     CSR   SRKI          BEGSR
     C  N92              Z-ADD     0             TOGGLE
     C   92              Z-ADD     1             TOGGLE
     C     TOGGLE        COMP      0                                      92
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Toggle : UPDATE (N90) V. Inquiry/Detail (90)
     CSR   SRKK          BEGSR
     C                   IF        USRLVL <> 'NOV'
     C     'X'           COMP      'X'                                899291
     C  N90              Z-ADD     0             TOGGLE
     C   90              Z-ADD     1             TOGGLE
     C     TOGGLE        COMP      0                                      90
     C                   MOVE      *BLANKS       TYPE
     C                   MOVE      *BLANKS       PO#
     C                   ENDIF
     C*                  EVAL      *IN45=*ON
     C*                  Z-ADD     0             CHGQTY
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Create a Requisition
     CSR   SRKD          BEGSR
      * setoff 48, to position in qty if creating new PO
      * regardless of user level default above "NOV"
     C                   IF        USRLVL = 'NOV'
     C                   Z-ADD     67            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      SRKDEND
     C                   ENDIF

     C                   CALL      'RTVOFCCL'
     C                   PARM                    @O
     C                   PARM      'NO'          @OWNER            2
     C                   IF        @O= *BLANKS
     C                   EXSR      SRKC
     C                   ENDIF

     C                   CALL      'RTVUNN'
     C                   PARM                    @P
     C                   EVAL      OMDC=@O

     C                   EVAL      *IN48=*OFF
     C                   EVAL      *IN45=*ON
     C                   Z-ADD     16            E
     C                   EXSR      ERRORS
     C                   EXSR      BLANKS
     C                   MOVEL     *BLANKS       SRCODE
     C                   MOVEL(P)  *BLANKS       PATTRN
     C*                  EVAL      OMYY=%SUBST(@FY:3:2)
     C                   MOVE      UYEAR         OMYY
     C                   MOVE      @P            OMNN
     C                   MOVE      *BLANKS       OMWO#
     C     RETRI         TAG
     C     '94'          CHAIN     COAFMT                             20
     C                   ADD       1             COREQ#
     C                   UPDATE    COAFMT
     C                   Z-ADD     COREQ#        OMREQ
     C     #OMKEY        SETLL     DTAFMT                                 20
     C   20              GOTO      RETRI
     C                   MOVE      OMYY          NXK01
     C                   MOVE      OMNN          NXK02
     C                   Z-ADD     OMREQ         NXK03
     C                   MOVE      OMWO#         NXK04
     C                   IF        OMWO# <> *BLANKS
     C                   EVAL      OMCODE = 'D'
     C                   ENDIF
     C     @USER         CHAIN     UIFMT                              20
     C     *IN20         IFEQ      '0'
     C                   MOVEL(P)  UINAME        SVCONT           30
     C                   ENDIF
     C                   SETON                                          9166
     C                   SETOFF                                       909289
      * auto calls to select vendor and vendpart
     C                   EXSR      SRKJ
     CSR   SRKDEND       ENDSR
     C*----------------------------------------------------------------
     C*Call vnmgr - lookup vendor number only
     CSR   SRKE          BEGSR
     C*                  UNLOCK    ORHDR
     C*                  UNLOCK    ORITM
     C                   CALL      VNDMGR                               20
     C                   PARM                    @UPDAT            1
     C                   PARM                    @USER            10
     C                   PARM                    @EOJ              1
     C                   PARM      '1'           @IN90             1
     C                   PARM      '0'           @IN91             1
     C     OMVN#         PARM      OMVN#         VN#               6 0
     C     OMPART        PARM      OMPART        PART#            30
     C*                  PARM      '1'           @IN39             1
     C                   SETON                                        37
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C*Toggle into Purchase Order Manager @ ICT only
     CSR   SRKF          BEGSR
     C                   IF        SVPO#>*ZEROS
     C                   EVAL      OMPOYY=SVPOYY
     C                   EVAL      OMPOPP=SVPOPP
     C                   EVAL      OMPO#=SVPO#
     C                   ENDIF
     C                   IF        (OMPO# <> SVPO#
     C                             AND SVPO# <> *ZEROS)
     C                             OR SVPO# = *ZEROS
     C                             AND OMPO# = *ZEROS
     C                   Z-ADD     70            E                    98
     C                   EXSR      ERRORS
     C                   GOTO      SRKF9
     C                   ENDIF
     C                   UNLOCK    ORMSRR                               20
     C                   SETON                                        90
     C                   CALL      'RTVGENIE'
     C                   PARM                    @ISGENIE          1
     C                   IF        @ISGENIE = '1'
     C                   EVAL      OH#C = %CHAR(OMPO#)
     C                   CALL      'PODTLUI'
     C                   PARM                    @EOJ              1
     C                   PARM      '00001'       @RET              5
     C                   PARM      '00001'       @SIZ              5
     C                   PARM      OMPOYY        @OHYY             2
     C                   PARM      OH#C          @OH#C             6
     C                   ELSE
     C                   CALL      OHMGR
     C                   PARM                    @P                2
     C                   PARM      OMDC          @O                2
     C                   PARM                    @USER            10
     C                   PARM                    @FY               4
     C                   PARM      ' '           @UPD              1
     C                   PARM      '1'           @IN90             1
     C                   PARM      '1'           @IN91             1
     C                   PARM                    @EOJ              1
     C                   PARM      OMPOYY        @OHYY             2
     C                   PARM      OMPOPP        @OHPP             2
     C                   PARM      OMPO#         @OH#              6 0
     C                   PARM      'C'           @CORH             1
     C                   ENDIF
     C     SRKF9         TAG
     C     @EOJ          COMP      'Y'                                    LR
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Toggle to WHMGR
     CSR   SRKG          BEGSR
     C                   UNLOCK    ORHDR
     C                   UNLOCK    ORITM
     C                   CALL      WHMGR                                20
     C                   PARM      '11'          @WP               2            Position for all WHs
     C                   PARM                    @USER
     C                   PARM      'X'           @UPDAT
     C                   PARM      '1'           @IN90
     C                   PARM      '0'           @IN91
     C                   PARM                    @EOJ
     C                   PARM      VRNN          @WHK01            2
     C                   PARM      VRID          @WHK02            4 0
     C                   PARM      VRIT          @WHK03           30
     C                   PARM      'S'           @SORP             1
     C                   PARM      '11000101'    @SWS              8
     C     @EOJ          COMP      'Y'                                    LR
     CSR   NOKG          ENDSR
     C*----------------------------------------------------------------
     C* Toggle to VNMGR & select vendor number
     CSR   SRKJ          BEGSR
     C*                    UNLCKOHDR
     C                   IF        *IN66 = *ON
     C                   UNLOCK    ORHDR
     C                   UNLOCK    ORITM
     C                   CALL      VNDMGR                               20
     C                   PARM                    @UPDAT            1
     C                   PARM                    @USER            10
     C                   PARM                    @EOJ              1
     C                   PARM      '1'           @IN90             1
     C                   PARM      '0'           @IN91             1
     C     OMVN#         PARM      OMVN#         VN#               6 0
     C*                  PARM      '1'           @IN39             1
     C                   SETON                                        37
     C                   CALL      VPMGR
     C                   PARM                    @P                2
     C                   PARM                    @USER            10
     C                   PARM      'X'           @UPDAT            1
     C                   PARM      '1'           @IN90
     C                   PARM      '0'           @IN91
     C                   PARM                    @EOJ
     C                   PARM      OMVN#         OMVN#
     C     OMPART        PARM      OMPART        PART#            30
     C     @EOJ          COMP      'Y'                                    LR
     C                   ELSE
     C                   Z-ADD     40            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Toggle to VPMGR & select vendor part
     CSR   SRKM          BEGSR
     C                   CALL      VPMGR
     C                   PARM                    @P                2
     C                   PARM                    @USER            10
     C                   PARM      'X'           @UPDAT            1
     C                   PARM      '1'           @IN90
     C                   PARM      '0'           @IN91
     C                   PARM                    @EOJ
     C                   PARM      OMVN#         VN#               6 0
     C     OMPART        PARM      OMPART        OMPART
     C     @EOJ          COMP      'Y'                                    LR
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Detact Requisition from  P/O     e
     CSR   SRKP          BEGSR
     C                   SELECT
     C*                  WHEN      OMSTAT = 'C' AND USRLVL <> 'INT'
     C                   WHEN      OMSTAT = 'C' AND USRLVL <> 'NOV'
      * if created muliple PO's - cannot reopen
     C                   EXSR      TLOOP
     C                   EVAL      LOOP=1
     C*                  IF        LOOP > 1
     C*                  Z-ADD     72            E                    96
     C*                  EXSR      ERRORS
     C*                  GOTO      SRKP9
     C*                  ENDIF
      *goto po lines and inactive item
     C     #ITKEY        SETLL     OIFMT                                  20
     C                   IF        NOT *IN20
     C                   Z-ADD     82            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      SRKP9
     C                   ENDIF
     C     'X'           DOWEQ     'X'
     C     #ITKEY        READE     OIFMT                                  20
     C   20              LEAVE
     C     OIPART        IFEQ      OMPART
      * if qtys dont match,  dont allow reopen
     C                   IF        OIQTY <> OMQTY
     C                   Z-ADD     72            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      SRKP9
     C                   ENDIF
      * not allowed if line complete or received against
     C                   IF        OISTAT='C' OR OIRQTY > *ZEROS
     C                   Z-ADD     59            E                    96
     C                   EXSR      ERRORS
     C                   LEAVE
     C                   ELSE
     C     #ITKEY        CHAIN     OHFMT
     C                   IF        OHMETH='Y'
     C                   Z-ADD     60            E                    98
     C*                  IF        OH# < 4556
     C*                  MOVE      '12'          PONUMB            2
     C*                  ENDIF
     C                   MOVE      PONUM         ERM(E)
     C                   EXSR      ERRORS
     C                   ENDIF
     C
     C                   ENDIF

     C                   EVAL      OISTAT='X'
     C                   MOVE      @USER         OICHBY
     C                   MOVE      TODAY         OICHDT
     C                   MOVE      ISOTIME       OICHTM
     C                   EVAL      OIREQ=*ZEROS
     C                   EVAL      OIWSNN=*BLANKS
     C                   EVAL      OIWSJOB=*ZEROS
     C                   UPDATE    OIFMT
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO

      * remove po data from req, reset back to open
     C                   IF        NOT *IN96
      * req reopen,  check if PO $ value drops below max $ value
     C                   EVAL      POTOT=POTOT-(OMQTY*OMEST$)
     C                   IF        POTOT < POMAX
     C     #ITKEY        CHAIN     OHFMT                              22
     C  N22              EVAL      OHSTAT = ' '
     C  N22              UPDATE    OHFMT
     C                   ENDIF
     C   96              UNLOCK    ORHDR                                20

     C     #KEYNX        CHAIN     DTAFMT                             20
     C                   IF        NOT *IN20
     C*                  EVAL      POTOT=POTOT-(OMQTY*OMEST$)
     C                   EVAL      OMSTAT='A'
     C                   EVAL      OMAPP=*BLANKS
     C                   EVAL      OMPOYY=*BLANKS
     C                   EVAL      OMPOPP=*BLANKS
     C                   EVAL      OMPO#=*ZEROS
     C                   EVAL      OMPOIT=*BLANKS
     C                   EVAL      OMPODD=*LOVAL
     C                   EVAL      OMPOQ=*ZEROS
     C                   EVAL      OMPOUM=*BLANKS
     C                   EVAL      OMPOBY=*BLANKS
     C                   EVAL      OMPODT=*LOVAL
     C                   UPDATE    DTAFMT
     C                   IF        NOT *IN96
     C*                  IF        OH# < 4556
     C*                  MOVE      '12'          PONUMB            2
     C*                  ENDIF
     C                   Z-ADD     58            E                    98
     C                   MOVE      PONUM         ERM(E)
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   EXSR      SRKK
     C                   ENDIF
     C                   ENDIF

      * for shipments from inventory, allow toggle to status R instead of C.
      * C is linked to PO, R is not, but still treated as hidden like staus C.
      * R is placeholder for "freight sale" amt to be billed instead of normal frt markup
     C                   WHEN      OMSTAT='A' AND OMCODE='F'
     C*     #KEYNX        CHAIN     DTAFMT                             20
     C*                   IF        NOT *IN20
     C                   EVAL      OMSTAT='R'
     C*                  UPDATE    DTAFMT
     C*                   ENDIF

     C                   ENDSL
     C     SRKP9         ENDSR
     C*----------------------------------------------------------------
     C* Toggle type between "I" and "D"
     CSR   SRKW          BEGSR
     C     OMSTAT        CABNE     'A'           SRKW9
     C                   SELECT
      * drop ship
     C                   WHEN      OMCODE='D'
     C                   EVAL      OMCODE='I'
     C*                  IF        OMWSNN <> *BLANKS
     C*                  EVAL      OMDC=OMWSNN
     C*                  ENDIF
     C                   IF        @O=*BLANKS
     C                   CALL      'RTVOFCCL'
     C                   PARM                    @O
     C                   PARM      'NO'          @OWNER            2
     C                   ENDIF
     C                   EVAL      OMDC=@O
     C                   IF        OMDC='DS' OR @O=*BLANKS
     C                   Z-ADD     74            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
      * freight
     C                   WHEN      OMCODE='F'
     C                   EVAL      OMCODE='I'
      * inventory
     C                   OTHER
     C                   IF        OMWO#<> *BLANKS
     C                             AND OMWSNN <> *BLANKS
     C                             AND OMWSID <> *ZEROS
     C                   EVAL      OMCODE = 'D'
     C                   EVAL      OMDC='DS'
     C                   ELSE
     C                   Z-ADD     65            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   ENDSL

     C                   IF        OMCODE='D'
     C                             AND OMDC<>'DS'
     C                   Z-ADD     75            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C  N96              UPDATE    DTAFMT
     CSR   SRKW9         ENDSR
     C*----------------------------------------------------------------
     C* Create PO from requisition
     C     SRKS          BEGSR
      * 48=user can create po from req, omstat=A(open req)
     C                   IF        (*IN48 AND (OMSTAT='A' OR OMSTAT='E')
     C                             AND SAPID <> *BLANKS AND OMCODE ='I')
     C                             OR (OMWO# <> *BLANKS AND OMCODE ='D'
     C                             AND SAPID <> *BLANKS AND OMSTAT = 'A')
     C                   IF        OMCODE = 'I'
     C                             OR (OMWO# <> *BLANKS AND OMCODE ='D')
     C                   EVAL      TYPE='O'
     C                   EVAL      CHGQTY=OMQTY
     C                   IF        PO#<> *BLANKS
     C                   MOVE      PO#           OH#
     C     #OHMKY        CHAIN(N)  OHFMT                              20
     C                   ENDIF

      * validate date field - fatal error if less than today
     C                   IF        (OMDDAT < TODAY AND OMDDAT <> *LOVAL)
     C                   Z-ADD     78            E                    98
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   IF        PO#<> *BLANKS
     C                   IF        OMVN# <> OHVEND
     C                             AND OMVN# <> *ZEROS
     C                   Z-ADD     68            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      SRKS9
     C                   ENDIF
     C                   EVAL      TYPE='C'
     C                   ENDIF
     C                   SETON                                        37
     C                   SELECT
     C                   WHEN      OMCODE='D'
     C                   IF        NOT(WSNN = OHNN AND WSID = OHCUID
     C                             AND WSLOC = OHLOC)
     C                             AND PO#<> *BLANKS
     C                   Z-ADD     69            E                    96
     C                   EXSR      ERRORS
     C                   GOTO      SRKS9
     C                   ENDIF
     C                   SETON                                        37
     C                   ENDSL

     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     55            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C     SRKS9         ENDSR
     C*----------------------------------------------------------------
     C* Toggle req code from "D" to "I" if still active
     CSR   SRKX          BEGSR
     C                   IF        OMSTAT='A'
     C                   IF        OMCODE='D'
     C                   EVAL      OMCODE='I'
     C                   ELSE
     C                   IF        OMWO#<> *BLANKS
     C                             AND OMWSNN <> *BLANKS
     C                             AND OMWSID <> *ZEROS
     C                   EVAL      OMCODE = 'D'
     C                   ELSE
     C                   Z-ADD     65            E                    96
     C                   EXSR      ERRORS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Program termination has been requested.
     CSR   SRKC          BEGSR
     C                   SETON                                        LR
     C   KC              MOVE      'Y'           @EOJ
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Add/Update to ORMTX
     CSR   OTUPD         BEGSR
     C                   MOVE      OMYY          OMTYY
     C                   MOVE      OMNN          OMTNN
     C                   Z-ADD     OMREQ         OMTREQ
     C                   MOVE      OMWO#         OMTWO#
     C                   MOVE      TODAY         OMTDAT
     C                   MOVE      'X'           OMTCX
     C                   MOVE      OMDC          OMTXP
     C                   MOVE      @USER         OMTUSE
     C     #OTKEY        SETLL     OMTFMT                                 20
     C   20#OTKEY        DELETE    OMTFMT                             20
     C*                  EVAL      OMTXT1=OMTXT1C+OMTXT1E
     C*                  EVAL      OMTXT2=OMTXT2C+OMTXT2E
     C*                  EVAL      OMTXT3=OMTXT3C+OMTXT3E
     C                   WRITE     OMTFMT
     CSR                 ENDSR
     C*----------------------------------------------------------------
     C* Security check.
     C*R         SECCHK    BEGSR
     C*R                   ENDSR
     C*----------------------------------------------------------------
     C* First cycle subroutine
     CSR   *INZSR        BEGSR
     C     *ENTRY        PLIST
     C                   PARM                    @P                2
     C                   PARM                    @O                2
     C                   PARM                    @USER            10
     C                   PARM                    @FY               4
     C                   PARM                    @UPDAT            1
     C                   PARM      *IN90         @IN90             1
     C                   PARM      *IN91         @IN91             1
     C                   PARM                    @EOJ              1
     C     NXK01         PARM      NXK01         @NXK01            2
     C     NXK02         PARM      NXK02         @NXK02            2
     C     NXK03         PARM      NXK03         @NXK03            6 0
     C     NXK04         PARM      NXK04         @NXK04            7
     C*::::: Initialize values
     C*                  EVAL      NXK03=*LOVAL
     C*    @NXK04        COMP      *BLANKS                            92
     C                   TIME                    TIMEDATE         14 0
     C     *USA          MOVE      TIMEDATE      TODAY
     C*    TODAY         ADDDUR    7:*D          WEEKLATER
     C                   MOVEL     TIMEDATE      ISOTIME
     C     *CYMD         MOVE      TODAY         @EXPD
     C     TODAY         ADDDUR    1:*Y          NEXTYR
     C     TODAY         SUBDUR    1:*Y          LASTYR
     C                   Z-ADD     1             STATUS
     C                   MOVE      '1'           CHAR1             1
     C                   MOVE      '0'           CHAR0             1
     C                   Z-ADD     1             STRPOS            3 0
     C                   MOVE      '_'           WILD              1
     C* Load UofM translation tables
     C                   EVAL      VZZKEY = 'BASEUOFM'
     C     VZZKEY        CHAIN     VZZFMT                             20
     C                   MOVEA     VZZ           BUM
     C*::::: Establish default values
      * if called from WSMGR, update allowed.   Limit view to CustPO of req passed in key.
     C                   IF        @UPDAT='W'
     C                   EVAL      @UPDAT='U'
     C     #KEYNX        CHAIN(N)  DTAFMT                             20
     C                   IF        NOT *IN20
     C                   MOVEL     'CUPO'        SRCODE
     C                   MOVEL(P)  OMCUPO        PATTRN
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     1             X
     C     OMDC          LOOKUP    CPU(X)                                 20
     C   20              MOVEL     PLT(X)        EXPK1
     C*****************************************************************
     C* When using Display DDS to value check screen, if no default   *
     C* value is specified here and the value from CLEAR DTAFMT will  *
     C* fail the DDS value check, you must specify DSPATR(MDT)        *
     C* conditioned by N90N50 for that field in display file. If not, *
     C* the value check will not occur and invalid data may result.   *
     C*****************************************************************
     C                   CLEAR                   DTAFMT
     C                   CLEAR                   OMTFMT
     C                   CLEAR                   OHFMT
     C                   CLEAR                   OIFMT
     C                   CLEAR                   OITFMT
     C                   CLEAR                   VRDS
     C                   CLEAR                   APPROV
     C                   CLEAR                   REQBY
     C                   CLEAR                   BUYER
     C                   EVAL      OMTXT1C='E'
     C                   EVAL      OMTXT2C='E'
     C                   EVAL      OMTXT3C='E'
     C                   EVAL      PONUMA='PR'
     C                   EVAL      ALTCD='AAA'
     C*                  MOVE      '94'          OMPP
     C                   MOVE      '  '          OMNN
     C                   MOVE      @O            OMDC
     C                   MOVE      @USER         OMTUSE
     C                   MOVE      'X'           OMTCX
     C                   MOVE      @P            OMTXP
     C                   MOVE      TODAY         OMTDAT
     C                   MOVE      'Y'           OMFRTC
     C*                  MOVE      'Y'           SHODEL
     C                   CLEAR                   DTASAV
     C                   EXSR      VALUSR
     C                   MOVE      UINN          OMPP
     C*                  IF        SRCODE<>'CUPO'
     C   93@NXK04        COMP      *BLANKS                                93
     C*                  ENDIF
     C  N93              MOVE      'Y'           SHODEL
      * is this next block to activate search necessary any longer?
     C     *IN49         IFEQ      '1'
     C                   MOVEL     'STAT'        SRCODE
     C                   MOVEL(P)  'A'           PATTRN
     C                   ENDIF
     C                   MOVE      '1'           CHAR1A            1
     C                   MOVE      '0'           CHAR0A            1
     C                   Z-ADD     1             STRPS2            3 0
     C                   MOVE      '_'           WILD2             1
     C     @UPDAT        IFNE      'U'                                          UPDATE 'U'
     C     @UPDAT        ORNE      ' '                                          RESALE ' '
     C                   EVAL      OMCODE='I'                                   OMCODES
     C                   MOVEL     'A'           OMSTAT                         M P O T G E L F
     C*    @O            IFNE      '41'                                         Z Q H S C
     C*                  MOVEL     '11'          OMWSNN
     C*                  Z-ADD     4680          OMWSID
     C*                  ELSE
     C*                  MOVEL     '41'          OMWSNN
     C*                  Z-ADD     5440          OMWSID
     C*                  ENDIF
     C                   ENDIF
     C*    @O            CHAIN     COAFMT
     C*                  EVAL      SVCTST=STCTST
     C*::::: Set up composite keys if necessary (corresponding DS)
     C     #KEYNA        KLIST
     C                   KFLD                    NXK01
     C                   KFLD                    NXK02
     C     #KEYNX        KLIST
     C                   KFLD                    NXK01
     C                   KFLD                    NXK03
     C*                  KFLD                    NXK02
     C*                  KFLD                    NXK04
     C     #LOKEY        KLIST
     C                   KFLD                    LOK01
     C                   KFLD                    LOK03
     C*                  KFLD                    LOK02
     C*                  KFLD                    LOK04
     C     #HIKEY        KLIST
     C                   KFLD                    HIK01
     C                   KFLD                    HIK03
     C*                  KFLD                    HIK02
     C*                  KFLD                    HIK04
     C*      Key for Alternate sort (OMFMT)
     C     #KEYNZ        KLIST
     C                   KFLD                    NXK04
     C     #LOKEZ        KLIST
     C                   KFLD                    LOK04
     C     #HIKEZ        KLIST
     C                   KFLD                    HIK04
     C     #OMKEY        KLIST
     C                   KFLD                    OMYY
     C*                  KFLD                    OMNN
     C*                  KFLD                    OMDC
     C                   KFLD                    OMREQ
     C     #OTKEY        KLIST
     C                   KFLD                    OMYY
     C                   KFLD                    OMNN
     C                   KFLD                    OMREQ
     C*                  KFLD                    OMWO#
     C     CSKEY         KLIST
     C                   KFLD                    OHNN
     C                   KFLD                    OHCUID
     C                   KFLD                    OHLOC
     C     #CSKEY        KLIST
     C                   KFLD                    OMWSNN
     C                   KFLD                    OMWSID
     C                   KFLD                    OMWSLC
     C     #VQKEY        KLIST
     C                   KFLD                    OMVN#
     C                   KFLD                    OMPART
     C     #VYKEYV       KLIST
     C                   KFLD                    OMDC
     C                   KFLD                    VRIT
     C     #VYKEYVV      KLIST
     C                   KFLD                    OMDC
     C                   KFLD                    VRIT
     C                   KFLD                    ATVEND
     C     #VYKEY        KLIST
     C                   KFLD                    OMDC
     C                   KFLD                    VRIT
     C                   KFLD                    OMVN#
     C     #VYKEYP       KLIST
     C                   KFLD                    OMDC
     C                   KFLD                    VRIT
     C                   KFLD                    OMVN#
     C*
     C     #VRMQKEY      KLIST
     C                   KFLD                    VRNN
     C                   KFLD                    VRID
     C                   KFLD                    VRIT
     C     #IVKEY        KLIST
     C                   KFLD                    OMPART
     C     #VSKEY        KLIST
     C                   KFLD                    OMVN#
     C                   KFLD                    ALTCD             3
     C     #WSKEY        KLIST
     C                   KFLD                    WONN
     C                   KFLD                    WOJOB
     C                   KFLD                    WSDC
     C                   KFLD                    OMPOQR
     C                   KFLD                    OMWSLC
     C                   KFLD                    OMWSTIME
     C     #OHMKY        KLIST
     C                   KFLD                    OMYY
     C*                  KFLD                    OXPP              2
     C                   KFLD                    OMPP
     C                   KFLD                    OH#
     C*                  EVAL      OXPP='94'
     C*    #OHMLR        KLIST
     C*                  KFLD                    OMLR
     C*                  KFLD                    OXPP              2
     C*                  KFLD                    OH#
     C     #OHKEY        KLIST
     C                   KFLD                    OHYY
     C                   KFLD                    OHPP
     C                   KFLD                    OH#
     C     #ITKEY        KLIST
     C                   KFLD                    OMPOYY
     C                   KFLD                    OMPOPP
     C                   KFLD                    OMPO#
     C     #OIKEY        KLIST
     C                   KFLD                    OHYY
     C                   KFLD                    OHPP
     C                   KFLD                    OH#
     C                   KFLD                    OILIN#
     C                   KFLD                    OMPART
     C     #ITPOK        KLIST
     C                   KFLD                    OHYY
     C                   KFLD                    OHPP
     C                   KFLD                    OH#
     C     #ITMKY        KLIST
     C                   KFLD                    OHYY
     C                   KFLD                    OHPP
     C                   KFLD                    OH#
     C                   KFLD                    OILIN#
     C     #POKEY        KLIST
     C                   KFLD                    OMPOYY
     C                   KFLD                    OMPOPP
     C                   KFLD                    OMPO#
     C*                  KFLD                    OILIN#
     C*                  KFLD                    OMPOIT
     C     #OITKY        KLIST
     C                   KFLD                    OIYY
     C                   KFLD                    OIPP
     C                   KFLD                    OI#
     C                   KFLD                    OILIN#
     C                   KFLD                    OIPART
     C                   KFLD                    OITSUB
     C     SRTKEY        KLIST
     C                   KFLD                    EXPK1
     C                   KFLD                    EXPK2
     C     #VOKEYD       KLIST
     C                   KFLD                    OMPART
     C                   KFLD                    ORG
     C                   KFLD                    CSNN
     C                   KFLD                    CSID
     C                   KFLD                    CSORAX
     C     #VOKEYI       KLIST
     C                   KFLD                    OMPART
     C                   KFLD                    ORG
     C                   KFLD                    @CSNN
     C                   KFLD                    @CSID
     C                   KFLD                    @CSORAX
     C     #L0KEY        KLIST
     C                   KFLD                    L0LAKT
     C                   KFLD                    L0LAK
     C     #P0KEY        KLIST
     C                   KFLD                    P0LAKT
     C                   KFLD                    P0LAK
     C     #VOKEYDP      KLIST
     C                   KFLD                    VRPART
     C                   KFLD                    ORG
     C                   KFLD                    CSNN
     C                   KFLD                    CSID
     C                   KFLD                    CSORAX
     C     #VOKEYIP      KLIST
     C                   KFLD                    VRPART
     C                   KFLD                    ORG
     C                   KFLD                    @CSNN
     C                   KFLD                    @CSID
     C                   KFLD                    @CSORAX
     C                   EVAL      @CSNN=*BLANKS
     C                   EVAL      @CSID=0
     C                   EVAL      @CSORAX=*BLANKS
     C     #RSKEY        KLIST
     C                   KFLD                    OIPART
     C                   KFLD                    RSWH
     CSR                 ENDSR
     C*------------------------------------------------------------
     CSR   VALUSR        BEGSR
     C*::::: User security/feature access
     C     #USRKE        KLIST
     C                   KFLD                    @USER
     C                   KFLD                    @PGM
     C                   MOVEL(P)  'OHMGR'       @PGM             10
     C     #USRKE        CHAIN     USRFMT                             20
     C                   IF        *IN20 OR USRDEL='D'
     C                   EVAL      @PGM=*BLANKS
     C     #USRKE        CHAIN     USRFMT                             20
     C                   ENDIF

     C                   SELECT
     C*......User in authority file
     C     USRLVL        WHENEQ    'PGMR'                                       Programmer
     C                   SETON                                        282948
     C                   SETON                                        9367
     C                   IF        @USER='KMCCLURE'
     C                             or @USER='DYINGLIN'
     C                             or @USER='AKIRSCHE'
     C                             or @USER='EKDAVIS '
     C                             or @USER='JHISE   '
     C                             or @USER='JMILLER '
     C                             or @USER='BNABER  '
     C                             or @USER='KMILLICA'
     C                             or @USER='RHILLHOU'
     C                             or @USER='SHENRY  '
     C                             or @USER='SSUCHKE '
     C                   EVAL      POMAX=50000.00
     C                   ELSE
     C                   EVAL      POMAX=200000.00
     C                   ENDIF
     C     USRLVL        WHENEQ    'EXP'                                        Expert
     C                   SETON                                        282948
     C                   SETON                                        9367
     C                   EVAL      POMAX=50000.00
     C     USRLVL        WHENEQ    'INT'                                        Intermediate
     C*                  SETON                                        284947
     C                   SETON                                        282948
     C                   SETON                                        9367
     C                   EVAL      POMAX=15000.00
     C     USRLVL        WHENEQ    'NOV'                                        Novice
     C                   EVAL      POMAX=*ZEROS
     C                   SETON                                        284793
     C                   SETON                                        49
     C                   ENDSL
     C     @UPDAT        COMP      'P'                                    67
     CSR                 ENDSR
     C*------------------------------------------------------------
     ODTAFMT    E    15      UPD
     O                       OMSTAT
     O                       OMX
     O                       OMUSER
     O                       OMCDAT
     OFOOTER    D    99NLR
     O                       *ALL
     ONOSFL     D    90N91NLR
     O         AND   38 32
     O         OR    90N91NLR
     O         AND   39 32
     O                       *ALL
     OSFLCTL    D    90N91NLR
     O                       *ALL
     OUPDATE    D    91NLR
     O                       *ALL
** SRA
STATJOB PARTCODEDATEBY  CONTAPP POITEXPCVENDDESCDC  CUPO
** SRN
00010007003000010010001000300030003000030006003000020020
** TRA
STATJOB PARTCODEDATEBY  CONTAPP POITEXPCVENDDESCDC  CUPO
** TRN
00010007003000010010001000300030003000030006003000020020
** CPU
111213141516172041
** PLT
12335A6A4
** ERM
Update ACTIVE  record.                - 1   Use 1-29 for STANDARD msgs
Update DELETED record.                - 2
Enter  NEW     record.                - 3
ENTER to access record in UPDATE.     - 4
Requested record not on file.         - 5
6                                     - 6
DeActivation of record completed.     - 7
Removal of record completed.          - 8
F3=F12=ext F5=Vndr F9=Key F18=PO Ad/Up- 9
Enter/page=position    Home=cursor    -10
F2=detail F4=NEW REQ F8=ShoD   F11=UPD-11
Page=BYPASS UPD F6=PO HDR   F22=ChgTyp-12
F2=INQ F4=NEW REQ F11=UPD F22=Chg Typ -13
F2=inq F11=detl F15=Reopen F16=Cancel -14
F8=shoDRC/noDRC F9=REQ#vsJob#         -15
F6=PO Hdr F10=NewReq Vndr/Itm         -16
Freight$ requires Chrg Cust Frt='Y'   -17
Old key NOT deleted. No change made.  -18
Proposed NEW KEY is already in use.   -19
Old key deleted. NEW KEY NOT ADDED!!!!-20
No changes = No UPDATE.               -21
Successful ADD, UPDATE, or KEY CHANGE.-22
Update file error STATUS=======> 00000-23
Req Inactive - F16 to reactivate      -24
BOF/EOF reached. Inquiry will wrap.   -25
>>> Search reached end of file. <<<   -26
!!! File being scanned is empty !!!   -27
All records scanned are Deleted.      -28
<<< Scan PATTERN longer than FIELD >>>-29
Invalid REQ/MUST AUTO GENERATE REQ#   -30
Requistioner can not approve own REQ. -31
Not authorized to change this Req.    -32
Invalid Vendor#         F18=PO Add/Upd-33
DESCRIPTION CANNOT BE BLANK           -34
CC=10 Must supply a cost > 0          -35
Account# & EXPC cannot be blank       -36
Invalid G/L account number            -37
Invalid expense center                -38
Invalid Inventory Item                -39
Invalid - Not entering new Requisition-40
Item already on existing PO           -41
Item ordered on new PO#====>yyppxxxxxx-42
Item added on existing PO#=>yyppxxxxxx-43
Qty chgd for item on PO#===>yyppxxxxxx-44
PO# not on file.                      -45
ChgQty<=0 not allowed for new PO Item.-46
PO# MUST be 6 digits long.            -47
Vndr1 must match existing Po's Vendor.-48
Order cannot be placed without Vendor.-49
ChgQty>ItemQty on PO.                 -50
You cannot order or create WRONG OFFICE51
Part cannot be blank                  -52
WRONG UPDATE to change this Req.      -53
Invalid UOM fix before ordering on PO -54
No P/O -Req closed or invalid authrity-55
Date entered is not in current year.  -56
Enter CYyMmDd:  C<=1999=0, C>=2000=1  -57
Req Reopen Removed fromPO#=>yyppxxxxxx-58
Denied-POLine closed/partal received  -59
Rq Reopen PrtFlg=Y for PO#=>yyppxxxxxx-60
Invalid or missing Cost Center for DC -61
Invalid or missing D/C -Please enter  -62
Qty Error - Please enter valid Qty    -63
Invalid or missing Job # entered      -64
Chg denied - Toggle REQ type I to D   -65
PO $ yyyyyyyyyyy > $ max xxxxxxxxxx   -66
View only  - invalid level for create -67
Mismatch vendor - add to PO denied    -68
Mismatch nn/id/loc add to PO denied   -69
Requisition not converted to PO       -70
Cost CostOvr exists for non-DRP org???-71
Reopen denied - Req qty <> PO line Qty-72
xxx Truckload POs - Last PO yyppxxxxxx-73
D/C Invalid for "I" type Reqs         -74
D/C for DropShip must be "DS"         -75
Invalid Customer Number- Please Retry -76
Invalid Due Date for Active Req       -77
Warning Due Date less than today      -78
Cancel denied - Attached to PO line   -79
Mismatch Type/DC - Req not attached   -80
ReqQ exceeds 1 TL.  Create TL POs?    -81
Cannot detach, Req not attached to PO -82
                                      -83
                                      -84
                                      -85
                                      -86
                                      -87
                                      -88
                                      -89
                                      -90
