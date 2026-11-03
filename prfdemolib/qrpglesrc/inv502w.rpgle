      // @MBRTEXT: Item Inquiry - Variety Wholesalers web (EJS) version
      *********************************************************************
      * Application: Inventory Inquiries
      *        Task: Item Inquiry
      *   Called by: MENDRV (MMS menu)
      *
      * Page up/down keys are used to search for previous/next SKU instead
      * of paging through warehouse list, for consistency with previous
      * application INQINV.  If the number of warehouses exceeds 6,
      * changes will be needed.
      *
vw110 * 03/06/25  B. Andrews  Created
      *
0001  * Modified: 04/17/2025   Stacy Jarvis - RTC
0001  *           Added ability to pass a SKU number
0001  *           Added F2=Buyers to display all buyers
      *
ba.01 * 05/06/25  B. Andrews  Fixed merchandise hierarchy display
      *
44658 * 01/16/26  Angela Kearsley - RTC RTC7744658
44658 *           Include new Set types 5 & 6 for Set Inq
      *
PRF2  * 11/03/26  Demo build: calls to programs that are not in the
PRF2  *           PRFDEMOLIB restore are commented out (PRF2 c***),
PRF2  *           so F3 on the prompt and F2-F23 just redisplay.
      *
VW01  * 11/03/26  Web clone INV502W (menu option 11). Same logic as INV502;
VW01  *           display file INV502WD is an EJS (agentic) Rich Display
VW01  *           driven by the Profound UI Open Access handler. Changes,
VW01  *           all tagged VW01:
VW01  *           - INV502WD + HANDLER('PROFOUNDUI(HANDLER)')
VW01  *           - ejsOut()/ejsIn() around each EXFMT: publish indicators,
VW01  *             messages and IVNDP# to the template; map the button
VW01  *             ACTION posted by the browser onto KbdKey (INFDS AID)
VW01  *             so every existing "when KbdKey = Fnn" test is unchanged
VW01  *           - SndMsgW(): SndMsg() plus the message text for the screen
VW01  *             (EJS screens have no message subfile)
VW01  *           - MSGCTL/DUMMY/SCREEN2B writes removed: their fields are
VW01  *             part of SCREEN1/SCREEN2C in INV502WD
      *
      *********************************************************************
     h Option(*NoDebugIO:*SrcStmt)
     h DftActGrp(*No)
     h BndDir('RTCBNDDIR')

      * Display file
VW01 fINV502WD  cf   e             Workstn InfDS(KbdInfDS) IndDS(DspIndDS)
     f                                     SFile(Screen2S:RRN)
VW01 f                                     handler('PROFOUNDUI(HANDLER)')
      * Warehouses & stores by warehouse/store type
     fTBLSTRZ1  if   e           k disk    rename(RTBLSTR:RcdFmt1)
      * SKUs
     fINVMST    if   e           k disk
      * Chain inventory balances
     fINVCBL    if   e           k disk
      * Inventory balances by SKU/location
     fINVBAL    if   e           k disk
      * Inventory balances by location/SKU
     fINVBALZ1  if   e           k disk    rename(RINVBAL:RINVBALZ1)
      * Merchandise hierarchy
     fINVDPT    if   e           k disk    prefix(Dpt_)
      * Vendors
     fAPSUPP    if   e           k disk
      * Current regular price snapshot
     fITMRTL    if   e           k disk
      * Seasons
     fTBLSEA    if   e           k disk
      * Transfer details by SKU / from location
     fTRFDTLL2  if   e           k disk
ba.01f                                     prefix(TRF:3)
      * Order book by SKU
     fORDERBK1  if   e           k disk
      * PO header
     fPOMHDR    if   e           k disk
      * SKU first & last sold dates
     fINVFSD    if   e           k disk
      * Buyers
     fTBLBYR    if   e           k disk
      * Zero shipped transfers by to location / transfer# / SKU
     fTRFZSTL3  if   e           k disk    prefix(ZST_)
      * SKU pack & hold quantity clusterf**k files
      *  Whse 9001
     fPHPF      if   e           k disk
      *  Whse 9002
     fPHPFW2    if   e           k disk    rename(RPHPF:RPHPFW2)
      *  Whse 9151
     fPHPFB1    if   e           k disk    rename(RPHPF:RPHPFB1)
      *  Whse 9152
     fPHPFB2    if   e           k disk    rename(RPHPF:RPHPFB2)
      * MMS store-level configuration options
     fOPTSTR    if   e           k disk
0001 fDPTDIV    if   e           k disk    prefix(div_)

      *********************************************************************
      *--------------------------------------------------------------------
      * Display indicator data structure
      *--------------------------------------------------------------------
     d DspIndDS        ds
     d  MsgSflInz             31     31n
     d  S1SKU                 41     41n
     d  S2SKU                 42     42n
     d  SflEnd                33     33n
     d  SflDspCtl             36     36n
     d  SflDsp                37     37n
     d  SflClr                38     38n
     d  SflOption             41     41n
     d  Error                 99     99n

      *--------------------------------------------------------------------
      * Global variables
      *--------------------------------------------------------------------
      * Program status data structure
      /COPY MMCPY,PGMSTSDS_D
      * Keyboard information data structure
      /COPY MMCPY,KBDINFDS_D
      * Message subfile handling
      /COPY MMCPY,MESSAGE_D

     d SysParm       e ds           256    extName(SYS001ED)

     d SflPag          s                   like(RRN) inz(6)
VW01  * Browser button actions -> 5250 AID bytes (F1-F24)
VW01 d EjsAids         c                   const(x'3132333435363738393A3B3C-
VW01 d                                     B1B2B3B4B5B6B7B8B9BABBBC')
     d Exit            s               n

0001 d InSKU           s              9p 0

     d Stores          s                   like(STRNUM) dim(9999)
     d StrWhses        s                   like(STRWHS) dim(%elem(Stores))
     d #Stores         s              5p 0

     d LocTypStr       s                   like(STRHDO) inz('S')
     d LocTypWhs       s                   like(STRHDO) inz('W')
     d POStsRlsd       s                   like(POSTAT) inz('3')
     d TrfStsAlcd      s                   like(TRFSTS) inz('A')
     d TrfStsShpd      s                   like(TRFSTS) inz('S')

      *--------------------------------------------------------------------
      * Procedure prototypes
      *--------------------------------------------------------------------

      *--------------------------------------------------------------------
      * Global key lists
      *--------------------------------------------------------------------
     c     Key2          klist
     c                   kfld                    LocTypWhs

     c     RcdKey2       klist
     c                   kfld                    LocTypWhs
     c                   kfld                    STRNUM

      *--------------------------------------------------------------------
      * Global parameter lists
      *--------------------------------------------------------------------
0001 c     *Entry        plist
0001 c                   parm                    InSKU

      *--------------------------------------------------------------------
      * Message subfile setup
      *--------------------------------------------------------------------
     c                   eval      MsgF        = 'JDAMSGF'
     c                   eval      MsgPre      = 'INV'
     c                   eval      MsgPgm      = $ProcPgm
     c                   eval      MsgModule   = $ProcMod
     c                   eval      MsgProc     = $ProcName
     c                   callp     ClrMsg()
     c                   eval      MsgProc     = 'MAIN'

      *--------------------------------------------------------------------
      * Select & display first screen
      *--------------------------------------------------------------------
     c                   callp     init()

0001 c* If SKU passed go to screen 2
0001 c     $PARMS        ifeq      0
     c                   callp     s1Init()
VW01 c                   callp     ejsOut()
     c                   exfmt     Screen1
VW01 c                   callp     ejsIn()
0001 c                   else
0001 c                   eval      Inumbr = InSku
0001 c     INUMBR        chain(n)  INVMST
0001 c                   callp     s2Init()
0001 c                   endif

     c                   callp     main()

      *********************************************************************
      * Initialization
      *********************************************************************
     p init            b

     c                   eval      *InLR       = *on

     c                   call      'SYSGET'
     c                   parm                    SysParm

      * Build array of stores
     c     LocTypStr     setll     TBLSTRZ1
     c     LocTypStr     reade(n)  TBLSTRZ1
     c                   dow       not %eof(TBLSTRZ1)
     c                   eval      #Stores    += 1
     c                   eval      Stores(#Stores) = STRNUM
     c                   eval      StrWhses(#Stores) = STRWHS
     c     LocTypStr     reade(n)  TBLSTRZ1
     c                   enddo

     p init            e

      *********************************************************************
      * Main routine
      *********************************************************************
     p main            b

     c                   dow       not Exit

      * Clear message subfile
     c                   callp     ClrMsg()
VW01 c                   clear                   msg

      * Reset display indicators
     c                   callp     resetInd()

      * Edit input
     c                   select
     c                   when      Screen      = 1
     c                   callp     s1Edit()
     c                   when      Screen      = 2
     c                   callp     s2Edit()
     c                   endsl

      * Write messages to screen
     c                   eval      MsgSflInz   = *on
VW01 c***                write     MsgCtl

      * Display screen
VW01 c***                write     Dummy
     c                   eval      SflDsp      = RRN > *zero
     c                   eval      SflDspCtl   = *on
     c                   select
     c                   when      Exit
     c                   when      Screen      = 1
VW01 c                   callp     ejsOut()
     c                   exfmt     Screen1
VW01 c                   callp     ejsIn()
     c                   when      Screen      = 2
VW01 c***                write     Screen2B
VW01 c                   callp     ejsOut()
     c                   exfmt     Screen2C
VW01 c                   callp     ejsIn()
     c                   endsl

     c                   enddo

     p main            e

      *********************************************************************
      * Initialize screen 1 - select SKU
      *********************************************************************
     p s1Init          b

     c                   eval      Screen      = 1

     c                   clear                   INUMBR

     p s1Init          e

      *********************************************************************
      * Edit input for screen 1 - select SKU
      *********************************************************************
     p s1Edit          b

      *------------------
      * Handle function keys
      *------------------
     c                   select
      * F1 = Return to menu
     c                   when      KbdKey      = F01
     c                   eval      Exit        = *on
     c                   return
      * F3 = Search
     c                   when      KbdKey      = F03
PRF2 c***                call      'INV002'
PRF2 c***                parm                    INUMBR
PRF2 c***                parm                    SysParm
     c                   if        INUMBR      = 0
     c                   return
     c                   endif
     c                   endsl

      *------------------
      * Validate data entered
      *------------------
      * SKU
     c     INUMBR        chain(n)  INVMST
     c                   if        not %found(INVMST)
     c                   eval      Error       = *on
     c                   eval      S1SKU       = *on
VW01 c                   callp     SndMsgW('0297')
     c                   return
     c                   endif

      *------------------
      * Edits passed - proceed to item details
      *------------------
     c                   callp     s2Init()

     p s1Edit          e

      *********************************************************************
      * Initialize screen 2 - item information
      *********************************************************************
     p s2Init          b

     c                   eval      Screen      = 2

      * Subfile control fields
     c***                   clear                   Search1
     c                   eval      NewSKU      = INUMBR
     c                   callp     getSKUInfo()

      * Fill subfile
     c                   clear                   SflEnd
     c                   callp     setLLBeg()
     c                   callp     sflPageDn()

     p s2Init          e

      *********************************************************************
      * Edit input for screen 2 - item information
      *********************************************************************
     p s2Edit          b

     d skuA            s              9a
     d vendorA         s              6a
     d pogType         s              1a   inz('P')

     c                   eval      skuA        = %editc(INUMBR:'X')
     c                   eval      vendorA     = %editc(ASNUM:'X')

      * Process subfile options first
     c                   if        RRN         > 0
     c***                   callp     prcSflOpt()
     c                   endif

      *------------------
      * Handle function keys
      *------------------
     c                   select
      * Page down
     c                   when      KbdKey      = PageDn
     c***                   callp     sflPageDn()
     c                   callp     getNextSKU()
      * Page up
     c                   when      KbdKey      = PageUp
     c***                   callp     sflPageUp()
     c                   callp     getPrevSKU()

      * F1 = Return to menu
     c                   when      KbdKey      = F01
     c                   eval      Exit        = *on
     c                   return

0001  * F2 = Buyers
0001 c                   when      KbdKey      = F02
PRF2 c***                call      'INQDPTBYR'
PRF2 c***                parm                    Idept
0001 c                   return
0001
      * F3 = Distributions
     c                   when      KbdKey      = F03
PRF2 c***                call      'CLINQDST'
PRF2 c***                parm                    skuA
     c                   return

      * F4 = Order book
     c                   when      KbdKey      = F04
PRF2 c***                call      'INQOBK'
PRF2 c***                parm                    skuA
     c                   return

      * F5 = Open POs
     c                   when      KbdKey      = F05
PRF2 c***                call      'INQOPO'
PRF2 c***                parm                    skuA
     c                   return

      * F7 = Sets
     c                   when      KbdKey      = F07
     c                   if        ISET        = '1'
44658c                             OR ISET     = '5'
PRF2 c***                call      'INQSET1'
PRF2 c***                parm                    skuA
     c                   elseif    ISET        = '2'
44658c                             OR ISET     = '6'
PRF2 c***                call      'INQSET2'
PRF2 c***                parm                    skuA
     c                   endif
     c                   return

      * F8 = UPCs
     c                   when      KbdKey      = F08
PRF2 c***                call      'INQUPC'
PRF2 c***                parm                    skuA
     c                   return

      * F9 = Transfers
     c                   when      KbdKey      = F09
PRF2 c***                call      'INQ_TRF'
PRF2 c***                parm                    skuA
     c                   return

      * F10 = Ship points
     c                   when      KbdKey      = F10
PRF2 c***                call      'INQVSP'
PRF2 c***                parm                    skuA
     c                   return

      * F11 = Count bulletin
     c                   when      KbdKey      = F11
PRF2 c***                call      'INQCNT'
PRF2 c***                parm                    skuA
     c                   return

      * F12 = Receiving history
     c                   when      KbdKey      = F12
PRF2 c***                call      'INQ_RCV'
PRF2 c***                parm                    INUMBR
     c                   return

      * F13 = Vendor inquiry
     c                   when      KbdKey      = F13
PRF2 c***                call      'INQVND'
PRF2 c***                parm                    vendorA
     c                   return

      * F14 = Pack & hold
     c                   when      KbdKey      = F14
PRF2 c***                call      'INQPAH'
PRF2 c***                parm                    skuA
     c                   return

      * F16 = Warehouse inventory adjustments
     c                   when      KbdKey      = F16
PRF2 c***                call      'INQ_AUD'
PRF2 c***                parm                    skuA
     c                   return

      * F17 = New pack & hold
     c                   when      KbdKey      = F17
PRF2 c***                call      'PAHINQ'
PRF2 c***                parm                    skuA
     c                   return

      * F18 = SKU audit log
     c                   when      KbdKey      = F18
PRF2 c***                call      'INQSKU'
PRF2 c***                parm                    skuA
     c                   return

      * F19 = Price events
     c                   when      KbdKey      = F19
PRF2 c***                call      'INQEVT'
PRF2 c***                parm                    skuA
     c                   return

      * F20 = Inventory balances
     c                   when      KbdKey      = F20
PRF2 c***                call      'INQ_BAL'
PRF2 c***                parm                    skuA
     c                   return

      * F21 = Planograms
     c                   when      KbdKey      = F21
PRF2 c***                call      'POG605'
PRF2 c***                parm                    skuA
PRF2 c***                parm                    pogType
     c                   return

      * F22 = Item attributes
     c                   when      KbdKey      = F22
PRF2 c***                call      'INQITA'
PRF2 c***                parm                    skuA
     c                   return

      * F23 = JDA distributions
     c                   when      KbdKey      = F23
PRF2 c***                call      'INQDSQ'
PRF2 c***                parm                    skuA
     c                   return

      * Process subfile control option
     c***                   when      CtlOption  <> *blank
     c***                   callp     prcCtlOpt()

      * Process search field
     c***                   when      Search1    <> 0
     c***                   callp     search()

      * New SKU entered
     c                   when         NewSKU     <> INUMBR
     c                   callp        getNewSKU()

      * Process subfile options
     c***                   when      RRN         > 0
     c***                   callp     prcSflOpt()
     c                   endsl

      * Refresh screen
     c                   select
     c                   when      Error
     c                   return
     c                   when      RRN         > 0
     c                   eval      RRN         = 1
     c                   callp     chainSfl()
     c                   callp     setLLRcd()
     c                   other
     c                   callp     setLLBeg()
     c                   endsl

     c***                   clear                   Search1
     c                   clear                   SflEnd
     c                   callp     sflPageDn()

     p s2Edit          e

      *********************************************************************
      * Read next record from data file
      *********************************************************************
     p readRcd         b
     d                 pi              n

     c                   select
     c                   when      Screen      = 2
     c***                   read(n)   RcdFmt1
     c     Key2          reade(n)  RcdFmt1
     c                   return    not %eof(TBLSTRZ1)
     c                   endsl
     c                   return    *off

     p readRcd         e

      *********************************************************************
      * Read prior record from data file
      *********************************************************************
     p readPRcd        b
     d                 pi              n

     c                   select
     c                   when      Screen      = 2
     c***                   readp(n)  RcdFmt1
     c     Key2          readpe(n) RcdFmt1
     c                   return    not %eof(TBLSTRZ1)
     c                   endsl
     c                   return    *off

     p readPRcd        e

      *********************************************************************
      * Position file pointer after end of file
      *********************************************************************
     p setGTEnd        b

     c                   select
     c                   when      Screen      = 2
     c***     *hival        setgt     RcdFmt1
     c     Key2          setgt     RcdFmt1
     c                   endsl

     p SetGTEnd        e

      *********************************************************************
      * Set file pointer at record
      *********************************************************************
     p setLLRcd        b
     d                 pi              n

     c                   select
     c                   when      Screen      = 2
     c     RcdKey2       setll     RcdFmt1
     c                   return    %equal(TBLSTRZ1)
     c                   endsl
     c                   return    *off

     p setLLRcd        e

      *********************************************************************
      * Set file pointer at beginning of file
      *********************************************************************
     p setLLBeg        b

     c                   select
     c                   when      Screen      = 2
     c***     *loval        setll     RcdFmt1
     c     Key2          setll     RcdFmt1
     c                   endsl

     p setLLBeg        e

      *********************************************************************
      * Read changed record from subfile
      *********************************************************************
     p readCSfl        b
     d                 pi              n

     c                   select
     c                   when      Screen      = 2
     c                   readc     Screen2S
     c                   endsl
VW01 c                   return    not %eof(INV502WD)

     p readCSfl        e

      *********************************************************************
      * Update subfile record
      *********************************************************************
     p updateSfl       b

     c                   select
     c                   when      Screen      = 2
     c                   update    Screen2S
     c                   endsl

     p updateSfl       e

      *********************************************************************
      * Chain to subfile record
      *********************************************************************
     p chainSfl        b

     c                   select
     c                   when      Screen      = 2
     c     RRN           chain     Screen2S
     c                   endsl

     p chainSfl        e

      *********************************************************************
      * Write subfile control record
      *********************************************************************
     p writeCtl        b

     c                   select
     c                   when      Screen      = 2
VW01 c                   eval      ejsclr      = SflClr
     c                   write     Screen2C
VW01 c                   eval      ejsclr      = '0'
     c                   endsl

     p writeCtl        e

      *********************************************************************
      * Write subfile record
      *********************************************************************
     p writeSfl        b

     c                   select
     c                   when      Screen      = 2
     c                   write     Screen2S
     c                   endsl

     p writeSfl        e

      *********************************************************************
      * Process subfile control option
      *********************************************************************
     p prcCtlOpt       b

     d msgPreTemp      s                   like(MsgPre)

     c                   select

      * 1 = Add
     c***                   when      CtlOption   = '1'
     c***                   callp     create()

      * Invalid
     c***                   when      CtlOption  <> *blank
     c***                   eval      SflCtlOpt   = *on
     c***                   eval      Error       = *on
     c***                   eval      msgPreTemp  = MsgPre
     c***                   eval      MsgPre      = 'RTC'
     c***                   callp     SndMsg('0003')
     c***                   eval      MsgPre      = msgPreTemp
     c                   endsl

     p prcCtlOpt       e

      *********************************************************************
      * Process subfile options
      *********************************************************************
     p prcSflOpt       b

     d msgPreTemp      s                   like(MsgPre)

      * Read through changed subfile options
     c                   dow       readCSfl()
     c                   clear                   SflOption

     c                   select

      * 1 = Select
     c***                   when      Option      = '1'

      * Invalid
     c***                   when      Option     <> *blank
     c***                   eval      SflOption   = *on
     c***                   eval      Error       = *on
     c***                   eval      msgPreTemp  = MsgPre
     c***                   eval      MsgPre      = 'RTC'
     c***                   callp     SndMsg('0003')
     c***                   eval      MsgPre      = msgPreTemp
     c                   endsl

      * Clear option & read next
     c                   if        not SflOption
     c***                   clear                   Option
     c                   endif
     c                   callp     updateSfl()
     c                   enddo

     p prcSflOpt       e

      *********************************************************************
      * Find previous SKU
      *********************************************************************
     p getPrevSKU      b

     d msgPreTemp      s                   like(MsgPre)

     c     INUMBR        setll     INVMST
     c                   readp(n)  INVMST
     c                   if        %eof(INVMST)
     c                   eval      Error       = *on
     c                   eval      msgPreTemp  = MsgPre
     c                   eval      MsgPre      = 'RTC'
VW01 c                   callp     SndMsgW('0002')
     c                   eval      MsgPre      = msgPreTemp
     c                   return
     c                   endif

     c                   eval      NewSKU      = INUMBR
     c                   callp     getSKUInfo()

     p getPrevSKU      e

      *********************************************************************
      * Find next SKU
      *********************************************************************
     p getNextSKU      b

     d msgPreTemp      s                   like(MsgPre)

     c     INUMBR        setgt     INVMST
     c                   read(n)   INVMST
     c                   if        %eof(INVMST)
     c                   eval      Error       = *on
     c                   eval      msgPreTemp  = MsgPre
     c                   eval      MsgPre      = 'RTC'
VW01 c                   callp     SndMsgW('0001')
     c                   eval      MsgPre      = msgPreTemp
     c                   return
     c                   endif

     c                   eval      NewSKU      = INUMBR
     c                   callp     getSKUInfo()

     p getNextSKU      e

      *********************************************************************
      * Validate SKU entered
      *********************************************************************
     p getNewSKU       b

     c     NewSKU        chain(n)  INVMST
     c                   if        not %found(INVMST)
     c                   eval      Error       = *on
     c                   eval      S2SKU       = *on
VW01 c                   callp     SndMsgW('0297')
     c                   return
     c                   endif

     c                   callp     getSKUInfo()

     p getNewSKU       e

      *********************************************************************
      * Get SKU information
      *********************************************************************
     p getSKUInfo      b

     d tableName       s              6a
     d tableValue      s              6a
     d tableDesc       s             35a
     d tableGood       s              1a

     c     tablePList    plist
     c                   parm                    tableName
     c                   parm                    tableValue
     c                   parm                    tableDesc
     c                   parm                    tableGood

     c     deptKey       klist
     c                   kfld                    IDEPT
     c                   kfld                    Dpt_ISDEPT
     c                   kfld                    Dpt_ICLAS
     c                   kfld                    Dpt_ISCLAS

      * Vendor
     c                   clear                   ASNAME
     c     ASNUM         chain(n)  APSUPP
     c                   eval      VndDesc     = ASNAME

      * Department
     c                   clear     *all          RINVDPT
     c     deptKey       chain(n)  INVDPT
     c                   eval      DptDesc     = Dpt_DPTNAM

      * Subdepartment
     c                   eval      Dpt_ISDEPT  = ISDEPT
     c                   clear                   Dpt_DPTNAM
     c     deptKey       chain(n)  INVDPT
     c                   eval      SdpDesc     = Dpt_DPTNAM

      * Class
     c                   eval      Dpt_ICLAS   = ICLAS
     c                   clear                   Dpt_DPTNAM
     c     deptKey       chain(n)  INVDPT
     c                   eval      ClsDesc     = Dpt_DPTNAM

      * Status
     c                   eval      tableName   = 'IDSCCD'
     c                   eval      tableValue  = IDSCCD
     c                   call      'TBLF04'      tablePList
     c                   eval      StsDesc     = tableDesc

      * Buyer
     c                   clear                   BYRNAM
     c     BYRNUM        chain(n)  TBLBYR
0001 c***                eval      ByrDesc     = BYRNAM

      * Season
     c                   clear                   SEADSC
     c     ISEASN        chain(n)  TBLSEA
     c                   eval      SeaDesc     = SEADSC

      * Price label
     c                   eval      tableName   = 'ITKTTR'
     c                   eval      tableValue  = ITKTTR
     c                   call      'TBLF04'      tablePList
     c                   eval      LblDesc     = tableDesc

      * Order book
     c                   clear     *all          ORDBK
     c     INUMBR        chain(n)  ORDERBK1

      * Set code
     c                   if        ISET        = '1'
     c                   eval      SetDesc     = 'Parent'
     c                   elseif    ISET        = '2'
     c                   eval      SetDesc     = 'Child'
44658c                   elseif    ISET        = '5'
44658c                   eval      SetDesc     = 'Bundle'
44658c                   elseif    ISET        = '6'
44658c                   eval      SetDesc     = 'Bundle Component'
     c                   else
     c                   clear                   SetDesc
     c                   endif

      * Hazmat code
     c                   eval      tableName   = 'IHAZCD'
     c                   eval      tableValue  = IHAZCD
     c                   call      'TBLF04'      tablePList
     c                   eval      HazDesc     = tableDesc

      * Replenishment code
     c                   eval      tableName   = 'IRPLCD'
     c                   eval      tableValue  = IRPLCD
     c                   call      'TBLF04'      tablePList
     c                   eval      RplDesc     = tableDesc

      * Closeout attribute
     c                   eval      tableName   = 'IATRB1'
     c                   eval      tableValue  = IATRB1
     c                   call      'TBLF04'      tablePList
     c                   eval      At1Desc     = tableDesc

      * Store order attribute
     c                   eval      tableName   = 'IATRB2'
     c                   eval      tableValue  = IATRB2
     c                   call      'TBLF04'      tablePList
     c                   eval      At2Desc     = tableDesc

      * Hardline/softline attribute
     c                   eval      tableName   = 'IATRB3'
     c                   eval      tableValue  = IATRB3
     c                   call      'TBLF04'      tablePList
     c                   eval      At3Desc     = tableDesc

      * Carton label attribute
     c                   eval      tableName   = 'IATRB4'
     c                   eval      tableValue  = IATRB4
     c                   call      'TBLF04'      tablePList
     c                   eval      At4Desc     = tableDesc

      * Import attribute
     c                   eval      tableName   = 'IATRB5'
     c                   eval      tableValue  = IATRB5
     c                   call      'TBLF04'      tablePList
     c                   eval      At5Desc     = tableDesc

      * Average cost
     c                   clear                   CBAVCS
     c     INUMBR        chain(n)  INVCBL

      * Prices
     c                   clear     *all          ITMRTLR
     c     INUMBR        chain(n)  ITMRTL

      * First & last sale dates
     c                   clear     *all          RINVFSD
     c     INUMBR        chain(n)  INVFSD
     c                   eval      FrstSalMDY  = FSMTH * 10000 +
     c                                           FSDAY * 100 + FSYR
     c                   eval      LastSalMDY  = LSMTH * 10000 +
     c                                           LSDAY * 100 + LSYR

      * Fill subfile of warehouses
     c                   clear                   SflEnd
     c                   callp     setLLBeg()
     c                   callp     sflPageDn()

     p getSKUInfo      e

      *********************************************************************
      * Get information for subfile record
      *********************************************************************
     p sflRcdInfo      b

     d strIdx          s                   like(#Stores)
     d rpkWhs          s                   like(ISTORE)

     c     storeSKUKey   klist
     c                   kfld                    STRNUM
     c                   kfld                    INUMBR

     c     skuStoreKey   klist
     c                   kfld                    INUMBR
     c                   kfld                    STRNUM

     c     rpkBalKey     klist
     c                   kfld                    rpkWhs
     c                   kfld                    INUMBR

     c     poKey         klist
     c                   kfld                    PONUMB
     c                   kfld                    POBO#

     c     locOptKey     klist
     c                   kfld                    STRNUM
     c                   kfld                    CTLQS#

      * On hand, on order
     c     *noKey        clear     *all          RINVBAL
     c     storeSKUKey   chain(n)  INVBALZ1
     c                   eval      OnHand      = IBHAND
     c                   eval      OnOrder     = IBPOOQ

      * Last order date
     c                   monitor
     c                   eval      LastOrdMDY  = %dec(%date(IBALD1:*YMD):*MDY)
     c                   on-error
     c                   eval      LastOrdMDY  = 0
     c                   endmon

      * Last receipt date
     c                   monitor
     c                   eval      LastRcvMDY  = %dec(%date(IBALD2:*YMD):*MDY)
     c                   on-error
     c                   eval      LastRcvMDY  = 0
     c                   endmon

      * Out of stock date
     c                   monitor
     c                   eval      OOSMDY      = %dec(%date(IBALD5:*YMD):*MDY)
     c                   on-error
     c                   eval      OOSMDY      = 0
     c                   endmon

      * Repack on order
     c                   clear                   IBPOOQ
     c                   eval      CTLQS#      = 8037
     c     locOptKey     chain(n)  OPTSTR
     c                   if        %found(OPTSTR)
     c                   eval      rpkWhs      = CTLQNV
     c     rpkBalKey     chain(n)  INVBALZ1
     c                   endif
     c                   eval      RpkOnOrder  = IBPOOQ

      * Allocated
     c                   clear                   Allocated
     c     skuStoreKey   setll     TRFDTLL2
     c     skuStoreKey   reade(n)  TRFDTLL2
     c                   dow       not %eof(TRFDTLL2)
     c                   if        TRFSTS      = TrfStsAlcd or
     c                             TRFSTS      = TrfStsShpd and
     c                             TRFSHP      < TRFALC
     c                   eval      Allocated  += adjZeroShip(TRFALC) - TRFSHP
     c                   endif
     c     skuStoreKey   reade(n)  TRFDTLL2
     c                   enddo

      * In transit
     c                   clear                   InTransit
     c     INUMBR        setll     INVBAL
     c     INUMBR        reade(n)  INVBAL
     c                   dow       not %eof(INVBAL)
     c                   eval      strIdx      = %lookup(ISTORE:
     c                                           Stores:1:#Stores)
     c                   if        strIdx      > 0 and
     c                             StrWhses(strIdx) = STRNUM
     c                   eval      InTransit  += IBINTQ
     c                   endif
     c     INUMBR        reade(n)  INVBAL
     c                   enddo

      * Pack & hold
     c                   clear     *all          RPHPF
     c                   if        STRNUM      = 9001
     c     INUMBR        chain(n)  PHPF
     c                   elseif    STRNUM      = 9002
     c     INUMBR        chain(n)  PHPFW2
     c                   elseif    STRNUM      = 9151
     c     INUMBR        chain(n)  PHPFB1
     c                   elseif    STRNUM      = 9152
     c     INUMBR        chain(n)  PHPFB2
     c                   endif
      *  Quantity
     c                   eval      PackNHold   = PHQTY  + PHQTY2 + PHQTY3 +
     c                                           PHQTY4 + PHQTY5
      *  Next out date
     c                   monitor
     c                   eval      NextOutMDY  = %dec(%date(PHOUT:*YMD):*MDY)
     c                   on-error
     c                   eval      NextOutMDY  = 0
     c                   endmon

      * Case pack & ship pack
     c                   call      'GETSPK'
     c                   parm                    INUMBR
     c                   parm                    STRNUM
     c                   parm                    ShipPack
     c                   parm                    CasePack

     p sflRcdInfo      e

      *********************************************************************
      * Adjust transfer allocation for zero shipments
      *********************************************************************
     p adjZeroShip     b
     d                 pi                  like(TRFALC)
     d  origAlloc                          like(TRFALC) value

     c     locTrfSKUKey  klist
     c                   kfld                    STRNUM
     c                   kfld                    TRFBCH
     c                   kfld                    INUMBR

     c     locTrfSKUKey  chain(n)  TRFZSTL3
     c                   if        %found(TRFZSTL3) and
     c                             ZST_TRZSRS  = ' '
     c                   return    origAlloc - ZST_TRFALC
     c                   else
     c                   return    origAlloc
     c                   endif

     p adjZeroShip     e

      *********************************************************************
      * Position to search field value
      *********************************************************************
     p search          b

     d found           s               n

     c***     searchKey2    klist
     c***                   kfld                    Search1

     c                   select
     c                   when      Screen      = 2
     c***     searchKey2    setll     RcdFmt1
     c***                   eval      found       = %found(TBLSTRZ1)
     c                   endsl
     c                   if        not found
     c                   callp     setGTEnd()
     c                   callp     readPRcd()
     c                   callp     readPRcd()
     c                   endif

     c                   clear                   SflEnd
     c                   callp     sflPageDn()

     p search          e

      *********************************************************************
      * Determine whether data file record passes filter
      *********************************************************************
     p passFilter      b
     d                 pi              n

     c     STRNUM        setll     INVBALZ1
     c                   if        not %equal(INVBALZ1)
     c                   return    *off
     c                   endif

     c                   return    STRWHS      = STRNUM

     p passFilter      e

      *********************************************************************
      * Reset display indicators
      *********************************************************************
     p resetInd        b

     d SflEndTemp      s                   like(SflEnd)

     c                   eval      SflEndTemp  = SflEnd
     c                   clear                   DspIndDS
     c                   eval      SflEnd      = SflEndTemp

     p resetInd        e

      *********************************************************************
      * Never modify anything below this point!
      *********************************************************************

      *********************************************************************
      * Fill subfile
      *********************************************************************
     p sflPageDn       b

     d sflRcdCnt       s                   like(RRN)
     d msgPreTemp      s                   like(MsgPre)

      * Bottom of file
     c                   if        SflEnd
     c                   eval      Error       = *on
     c                   eval      msgPreTemp  = MsgPre
     c                   eval      MsgPre      = 'RTC'
VW01 c                   callp     SndMsgW('0001')
     c                   eval      MsgPre      = msgPreTemp
     c                   return
     c                   endif

      * Clear subfile
     c                   clear                   SflEnd
     c                   clear                   RRN
     c                   eval      SflClr      = *on
     c                   callp     writeCtl()
     c                   eval      SflClr      = *off

      * Loop over subfile
     c                   for       sflRcdCnt   = *zero to SflPag

      * Get next record from data file
     c                   if        readRcd()

      * Read one extra record from data file to determine 'More...'/'Bottom'
     c                   if        sflRcdCnt   < SflPag

      * Only write record to subfile if passes filter
     c                   if        passFilter()

      * Get additional info
     c                   callp     sflRcdInfo()

      * Write subfile record
     c                   eval      RRN         = RRN + 1
     c                   callp     writeSfl()

      * Fails filter
     c                   else
     c                   eval      sflRcdCnt   = sflRcdCnt - 1
     c                   endif

      * End of subfile
     c                   else
     c                   callp     readPRcd()
     c                   endif

      * End of data file
     c                   else
     c                   eval      SflEnd      = *on
     c                   callp     setGTEnd()
     c                   callp     readPRcd()
     c                   eval      sflRcdCnt   = SflPag
     c                   endif

     c                   endfor

     p sflPageDn       e

      *********************************************************************
      * Page up
      *********************************************************************
     p sflPageUp       b

     d sflRcdCnt       s                   like(RRN)
     d rollBack        s                   like(RRN)
     d msgPreTemp      s                   like(MsgPre)

      * Loop back through visible part of subfile
     c                   clear                   SflEnd
     c                   eval      rollBack    = SflPag + RRN
     c                   for       sflRcdCnt   = 1 to rollBack

      * Get previous record from data file
     c                   if        not readPRcd()
      * Top of data file
     c                   callp     setLLBeg()
     c                   if        sflRcdCnt  <= RRN
     c                   eval      Error       = *on
     c                   eval      msgPreTemp  = MsgPre
     c                   eval      MsgPre      = 'RTC'
VW01 c                   callp     SndMsgW('0002')
     c                   eval      MsgPre      = msgPreTemp
     c                   endif
     c                   eval      sflRcdCnt   = rollBack
     c                   else

      * Don't count records that don't pass filter
     c                   if        not passFilter()
     c                   eval      sflRcdCnt   = sflRcdCnt - 1
     c                   endif

     c                   endif
     c                   endfor

     c                   callp     sflPageDn()

     p sflPageUp       e

VW01  *********************************************************************
VW01  * EJS (web) display support - added for INV502W
VW01  *********************************************************************
VW01  * Before EXFMT: give the template what a 5250 screen shows through
VW01  * indicators, the message subfile and fields it cannot name.
VW01 p ejsOut          b
VW01 c                   eval      inds        = DspIndDS
VW01 c                   eval      ivndpn      = IVNDP#
VW01 c                   eval      ejsclr      = '0'
VW01 c                   clear                   action
VW01 p ejsOut          e

VW01  * After EXFMT: a browser button posts ACTION (F01..F24, PAGEUP,
VW01  * PAGEDOWN, ENTER). Turn it into the AID byte the program already
VW01  * tests. A real keyboard key leaves ACTION blank and the handler has
VW01  * already put its AID in the INFDS.
VW01 p ejsIn           b
VW01 d n               s              3s 0
VW01 c                   select
VW01 c                   when      action      = 'PAGEUP'
VW01 c                   eval      KbdKey      = PageUp
VW01 c                   when      action      = 'PAGEDOWN'
VW01 c                   eval      KbdKey      = PageDn
VW01 c                   when      action      = 'ENTER'
VW01 c                   eval      KbdKey      = Enter
VW01 c                   when      %subst(action:1:1) = 'F'
VW01 c                             and %check('0123456789':
VW01 c                                 %trimr(%subst(action:2))) = 0
VW01 c                             and %len(%trimr(action)) > 1
VW01 c                   eval      n           = %int(%trimr(%subst(action:2)))
VW01 c                   if        n >= 1 and n <= 24
VW01 c                   eval      KbdKey      = %subst(EjsAids:n:1)
VW01 c                   endif
VW01 c                   endsl
VW01 p ejsIn           e

VW01  * SndMsg() plus the first-level text, for the screen's msg field
VW01 p SndMsgW         b
VW01 d SndMsgW         pi
VW01 d  msg#                          4    value
VW01 d QMHRTVM         pr                  extpgm('QMHRTVM')
VW01 d  rcv                        1024    options(*varsize)
VW01 d  rcvLen                       10i 0 const
VW01 d  fmt                           8    const
VW01 d  msgId                         7    const
VW01 d  qMsgF                        20    const
VW01 d  rplDta                       10    const
VW01 d  rplLen                       10i 0 const
VW01 d  rplSub                       10    const
VW01 d  rtnCtl                       10    const
VW01 d  err                          16
VW01 d rtv             ds          1024
VW01 d  rtvLen                 9     12i 0
VW01 d  rtvTxt                25   1024
VW01 d apiErr          ds            16
VW01 d  apiPrv                 1      4i 0 inz(16)
VW01 c                   callp     SndMsg(msg#)
VW01 c                   callp     QMHRTVM(rtv:%size(rtv):'RTVM0100':
VW01 c                                     MsgPre + msg#:MsgF + '*LIBL':
VW01 c                                     ' ':0:'*NO':'*NO':apiErr)
VW01 c                   if        rtvLen > 0
VW01 c                   eval      msg         = %subst(rtvTxt:1:rtvLen)
VW01 c                   else
VW01 c                   eval      msg         = MsgPre + msg#
VW01 c                   endif
VW01 p SndMsgW         e

