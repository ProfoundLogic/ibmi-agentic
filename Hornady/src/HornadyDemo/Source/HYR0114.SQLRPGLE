     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 1999')
     H BNDDIR('HYR0114')
      **************************************************************************
      * TITLE: Order Ship (Scanner) Packing List Print
      *    BY: William Earl (MCA)
      *  DATE: November 1999
      *
      *  REV DATE  BY   CHANGE
      *  --------  ---  -----------------------------------------------------
      *  01/18/00  MCA  For packing list print only, add 2 decimal places to
      *                 weight fields.
      *  03/21/00  MCA  Fix packing list total line overflow.
      *  04/10/01  MCA  Retrieve freight class from item warehouse record instead
      *                 of forcing "ALL".
      *  05/31/01  MCA  Fix bug - order header comments not printing.
      *                 (Actually, no order comments were printing.)
      *  11/15/01 MCA  Applied Hornady custom modifications to HD4.
      *                Changes made due to changes to OEPKLH, specifically the
      *                removal of SDDSHC and the addition of SDDSHP.
      *  09/17/02 MCA  Don't print returns.
      *  12/03/02 MCA  Modifications due to HD4.1 upgrade.
      *  03/02/04 MCA  For Gander Mountain shipments, print each item's UPC.
      *  06/20/06 MCA  Print indication of whether address is a drop-ship.
      *  08/09/06 MCA  Fix bug caused when applying PTF OE-514 on 10/08/2003.  Use
      *                customer item number from order detail instead of retrieving
      *                it from the customer item master file.
      *  09/12/07 MCA  Shipping day cutoff time.
      *  01/23/08 MCA  Print backorder quantities, if requested.
      *  05/14/08 MCA  If a non-shipped backordered line is included on the packing
      *                list, don't update order line with the packing list number.
      *  07/01/08 MCA  Don't print call tag on packing list.
      *  02/05/09 MCA  Print tracking numbers on packing list.
      *  09/23/09 MCA  Don't print MAILTAG item.
      *  11/18/09 MCA  Print extra copy of packing list for imaging.  This is
      *                because the packing list will now use an overlay and thus
      *                has to be *AFPDS, and RJS cannot image *AFPDS.
      *  01/10/11 MCA  Fix bug - when extra copy for imaging added, the environment
      *                overrides print file needed to be padded with blanks on the
      *                main copy environment overrided retrieval.
      *  05/09/11 MCA  Print DOT spec sheets for canned air.
      *  11/04/11 MCA  Print bulk bullet lot numbers, if available.
      *  01/18/12 MCA  Add email call-tag function.
      *  06/27/12 MCA  Expand bulk bullet/case lot array size from 200 to 2000.
      *                Also, send message to I.T. users if max index reached.
      *  04/30/13 MH   Changed to look at shipping location and put the appropriate
      *                address on the packing list.
      *  05/07/13 MCA  Maintain turnaround number separation.
      *  07/03/13 MCA  Populate turnaround number in packing list records.
      *  07/06/13 MCA  Populate packing list number in HYPTDTA.
      *  10/11/13 MCA  Fix bug - packing list comments attached to a xWRNTY line
      *                never printed because the xWRNTY lines never print on a
      *                packing list.  Changed the program to print the comments,
      *                even though the xWRNTY line still will not print.  T#190
      *  03/28/14 MCA  Fix bug from change made 10/11/13.  When xWRNTY line
      *                comments were printed, the order number and p.o. number
      *                fields had not yet been populated for the current packing
      *                list.
      *  09/23/15 MCA  For customers set to print bulk lot numbers, print
      *                bulk case lot numbers in addition to bulk bullets.
      *  06/18/16 MCA  SnapSafe changes.  Allow multiple-packing-list (or
      *                single-packing-list) .pdf files to be created instead
      *                of the normal (printable) spooled file.
      *  12/08/17 MCA  Allow a customer's item number to be printed instead
      *                of Hornady's item number.  This change was made
      *                specifically for Cabela's Herter's ammo.
      *  02/10/18 MCA  HD2 product class conversion.
      *  02/18/19 MCA  Check HYPSTRK for trackin number.
      *  05/22/19 MH   Allow for custom packing lists.
      *  01/22/21 MH   Change for custom packing list parameters sent.
      *  04/04/22 MCA  Print lithium battery spec sheet for Case Prep Duo.
      *  08/01/22 MCA  For Varsity shipments, retrieve tracking numbers from
      *                Varsity.  This will allow for printing multiple tracking
      *                numbers.
      *  02/10/23 MCA  Export document text source changed from HDCCMT to HYPEXTX.
      *                With this change, a new TTB comment for FAET taxable items
      *                might be printed (if any applicable items are shipped).
      *  03/06/23 MCA  Fix bug.  Domestic customer drop shipments were printing
      *                the ITAR and TTB.
      *  03/08/23 MCA  For ITAR and TTB, dont print if packaging (PCLS=ATAC) is
      *                the only product shipped.
      *  11/01/23 MCA  Print lithium battery spec sheet for HIT Target.
      *  10/12/24 MCA  Vendor-direct shipments.
      *  04/08/25 MCA  Convert ounce weights to pounds for packing list totals.
      *  06/27/25 MCA  Change spec-sheet print check to use new SHIPSPECS table.
      *  07/09/25 MCA  Print mil-spec lot numbers when found.
      *  07/25/25 MCA  Print mil-spec lot number purchase quantity and UOM.
      *  08/18/25 MCA  For imaged spooled file (HOEPKPPF2), move lot number
      *                to the right 32 characters so that the WebDocs watermark
      *                does not overlay it.  Ticket #67295
      *  03/13/26 MCA  If an order line is for a mil-spec item, and its pricing
      *                UOM (e.g., CT) is different than the item's stocking UOM
      *                (e.g., BX), convert quantities to pricing UOM, and print
      *                the pricing UOM and quantity shipped on the packing list
      *                in those columns, and print the stocking UOM and
      *                quantity-shipped below the item description.
      **************************************************************************
      *
     FOEORHD    IF   E           K DISK
     FOEORDT    UF   E           K DISK
     FOEORHP    UF   E           K DISK
     FOEORDP01  UF   E           K DISK
     FHYPTDTA   UF   E           K DISK
     FOEOCMT    IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FHDDSHP03  IF   E           K DISK
     FHDCCMT    IF   E           K DISK
     FHDVCIT    IF   E           K DISK
     FHDVCIC    IF   E           K DISK
     FHDIMST    IF   E           K DISK
     FHDIWHS    IF   E           K DISK
     FHDIMXD    IF   E           K DISK
     FHDSHQT    IF   E           K DISK
     FHYLSSCC2  IF   E           K DISK    USROPN
     FHYLSCWK2  IF   E           K DISK    USROPN
     FHDCTRY    IF   E           K DISK
     FHDWHSM    IF   E           K DISK
     FOEPKLH    UF A E           K DISK
     FOEPKLD    UF A E           K DISK
     FHDSHPV    IF   E           K DISK
     FHYPSTRK   IF   E           K DISK
     FMFD1MD    IF   E           K DISK
     FBARCUSTL1 IF   E           K DISK
     FHDCALD    IF   E           K DISK    USROPN
     FGUPTDAT   IF   E           K DISK
     FIVCTRL    IF   E             DISK
     FOECTRL    UF   E             DISK
     FHYPPSCT   IF   E             DISK    USROPN
     FHYPSLOC   IF   E           K DISK
     FHYPCSPK   IF   E           K DISK
     FHYPEXTX   IF   E           K DISK    USROPN
     FHYLEXTX1  IF   E           K DISK    RENAME(HYREXTX:HYREXTX1) USROPN
     FHOEPKPPF  O    E             PRINTER OFLIND(*IN97)
     F                                     USROPN
     FHOEPKPPF2 O    E             PRINTER OFLIND(*IN96)
     F                                     USROPN
     F                                     EXTDESC('HOEPKPPF')
     F                                     RENAME(PKLH1:XPKLH1)
     F                                     RENAME(PKLB1:XPKLB1)
     F                                     RENAME(PKLL1:XPKLL1)
     F                                     RENAME(PKLL2:XPKLL2)
     F                                     RENAME(PKLB2:XPKLB2)
     F                                     RENAME(PKLB3:XPKLB3)
     F                                     RENAME(PKLB4:XPKLB4)
     F                                     RENAME(PKLB5:XPKLB5)
     F                                     RENAME(PKLC1:XPKLC1)
     F                                     RENAME(PKLT1:XPKLT1)

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D HYR6900         PR                  EXTPGM('HYR6900')
     D  PklReprint                    1N   CONST

     D HYR6900B        PR                  EXTPGM('HYR6900B')                   Case Prep Duo specs
     D  PklReprint                    1N   CONST

     D HYR6900C        PR                  EXTPGM('HYR6900C')                   HIT Target specs
     D  PklReprint                    1N   CONST

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')                    Exec system command
     D  out_Command                 512A   CONST
     D  out_CmdLen                   15P 5 CONST

     D GUCSNDUM        PR                  EXTPGM('GUCSNDUM')
     D  MsgFile                      10A   CONST
     D  MsgID                         7A   CONST
     D  MsgData                     512A   CONST
     D  User                         10A   CONST

     D********************************************************************
     D* OVRPRTF DATA STRUCTURE *******************************************
     D                 DS
     D  OVRS                   1    500
     D                                     DIM(2)                               OVRPRTF COMMAND
     D  JOVR1                  1    250
     D  JOVR2                251    500
     D* Error Message Data Structure *************************************
     D                 DS
     D  MSG                    1     75
     D  MSGDS                  1     67
     D  MSGDS#                69     75
     D* System Data Structure ********************************************
     D                SDS
     D  PGMNAME                1     10
     D  WS                   244    253
     D  USER                 254    263
     D* ARRAY Data Structure *********************************************
     D                 DS
     D  LOA                    1    220
     D                                     DIM(5)                               Location Address
     D  LOA1                   1     44
     D  LOA2                  45     88
     D  LOA3                  89    132
     D  LOA4                 133    176
     D  LOA5                 177    220
     D                 DS
     D  STA                    1    264
     D                                     DIM(6)                               Ship-To Address
     D  STA1                   1     44
     D  STA2                  45     88
     D  STA3                  89    132
     D  STA4                 133    176
     D  STA5                 177    220
     D  STA6                 221    264
     D                 DS
     D  BTA                    1    264
     D                                     DIM(6)                               Bill-To Address
     D  BTA1                   1     44
     D  BTA2                  45     88
     D  BTA3                  89    132
     D  BTA4                 133    176
     D  BTA5                 177    220
     D  BTA6                 221    264
      *****************************************************************
      * Time & Date Stamp Data Structure
      *****************************************************************
     D/COPY QCPYILE,GUITDSDS

     D                 DS
     D ds_CYMD                 1      7  0
     D ds_MDY                  4      9  0
     D ds_YY1                  2      3  0
     D ds_YY2                  8      9  0

     D* HSYUSN External PLIST ********************************************
     D USNDS         E DS                  EXTNAME(HSYUSNPL) INZ

     D ary_Lots        S             55A   DIM(2000) ASCEND                     26 + 15 + 14
     D ary_Lots2       S             15A   DIM(2000)
     D idx1            S              5P 0
     D maxidx1         S                   LIKE(idx1) INZ(%elem(ary_Lots))
     D idx2            S              5P 0
     D idx3            S              5P 0
     D idx4            S              5P 0
     D wk_PrtOvrWS     S             10A
     D wk_QSTC         S                   LIKE(IDQSTC)
     D wk_LotQty       S                   LIKE(PLQTYS)
     D wk_TotLotQty    S                   LIKE(PLQTYS)
     D wk_Lot#         S                   LIKE(SQLOTN)
     D wk_Lot#LJ       S                   LIKE(SQLOTN)
     D wk_ShipDay      S              1A
     D wk_DayCutoff4   S              4P 0 DTAARA(SHIPCUTOFF)
     D wk_DayCutoff    S              6P 0
     D wk_CalendarNbr  S                   LIKE(CDCALN) INZ(1)
     D wk_PklCYMD      S              7P 0
     D wk_PklMDY       S              6P 0
     D wk_PklTime      S              6P 0
     D wk_QtyBO        S                   LIKE(ODQORD)
     D wk_QtyBO2       S              9P 0
     D wk_CannedAir    S              1N   INZ(*OFF)
     D wk_CPDuo        S              1N   INZ(*OFF)
     D wk_HITTarget    S              1N   INZ(*OFF)
     D wk_MsgData      S            512A
     D wk_ShipLoc      S              4A
     D wk_SLocFound    S               N   INZ(*OFF)
     D wk_CSZ          S             38A
     D wk_Foreign      S               N   INZ(*OFF)
     D wk_ITARTTB      S               N   INZ(*OFF)
     D wk_PDFClose     S               N   INZ(*OFF)
     D wk_Pos          S              3P 0
     D wk_OvrCmd       S            512A
     D wk_CustomPL     S               N   INZ(*OFF)
     D wk_Splf         S             10A
     D wk_OutQ         S             10A
     D wk_UserData     S             10A
     D wk_Form         S              4A
     D wk_Save         S              4A
     D wk_Hold         S              4A
     D wk_Trk#Line1    S              1N   INZ(*OFF)
     D wk_BlnkCmtPrtd  S              1N
     D wk_PurUOM       S              5A
     D wk_PurUOMS      S              7A
     D wk_PurQty       S              7P 0
     D wk_PurQty2      S              9P 2
     D wk_PrcUOMQty    S             13P 4
     D wk_MilSpec      S              1N
     D wk_ODPRUM       S              2A
     D wk_ODSTUM       S              2A
     D wk_ODORUF       S              9P 4
     D wk_OMMSLT       S             20A
     D wk_VDVEND       S              7S 0
     D wk_VDCARR       S              2A
     D wk_VDCDSC       S             15A
     D wk_VCBOLR       S              1A
     D wk_VMVNA1       S             26A
     D wk_VMVNA2       S             26A
     D wk_VMVNA3       S             26A
     D wk_VMVNA4       S             19A
     D wk_VMVCTY       S             26A
     D wk_VMST         S              2A
     D wk_VMZIP        S             13A
     D wk_VMCTRY       S              3A
     D wk_VMCDES       S             30A
     D wk_CMCTRY       S              3A
     D wk_DOCTITAR     S              3A
     D wk_DOCTTTB      S              3A
     D wk_FrgnTxtCust  S              1N
     D wk_FrgnTxtCtry  S              1N
     D wk_NeedITAR     S              1N
     D wk_NeedTTB      S              1N
     D wk_RcdCnt       S              7P 0
     D p1              S              5S 0
     D p2              S              5S 0
     D sv_LINE         S                   LIKE(LINE)
     D sv_BLN#         S                   LIKE(BLN#)
     D kf_MDCMNO       S                   LIKE(MDCMNO) INZ('01')
     D kf_MDOTYP       S                   LIKE(MDOTYP) INZ(*Blank)
     D kf_MDORDR       S                   LIKE(MDORDR)
     D kf_CTRY         S                   LIKE(ETCTRY)
     D kf_CUST         S                   LIKE(ETCUST)
     D kf_PCLS         S                   LIKE(ETPCLS)
     D kf_DOCT         S                   LIKE(ETDOCT)
     D kf_HT           S                   LIKE(ETHT)
     D PLWGTXX         S              9P 3

     D Yes             C                   CONST('Y')
     D No              C                   CONST('N')
     D @BLKPC          C                   CONST('BULKPRDCLS')
     D Alda            C                   CONST('ALDA')
     D GI              C                   CONST('GI  ')
     C* Mainline *********************************************************
     C*
     C                   SELECT

     C                   WHEN      work_PDFName = '*CLOSE'
     C                   CLOSE     HOEPKPPF

     C                   OTHER
     C                   CLEAR                   ListNumber
     C                   MOVEL     No            StartedList       1
     C     work_ORD#     CHAIN     HYPCSPK
     C                   IF        %found(HYPCSPK)
     C                   EVAL      wk_CustomPL = *ON
     C                   ELSE
     C                   EVAL      wk_CustomPL = *OFF
     C                   ENDIF                                                  %found()
     C                   EVAL      wk_PrtOvrWS = WS
      /Free
       wk_VDVEND = *Zero;
       wk_VDCARR = *Blanks;
       wk_VDCDSC = *Blanks;
       EXEC SQL select VDVEND, VDCARR, VDCDSC
                 into :wk_VDVEND, :wk_VDCARR, :wk_VDCDSC
                from HYPVDS
                where VDORD#=:work_ORD# and VDTURN=:work_Turn
                fetch first row only;
       wk_VCBOLR = *Blank;
       IF wk_VDCARR <> *Blanks;
         EXEC SQL select VCBOLR
                   into :wk_VCBOLR
                  from HYPVDSC
                  where VCCARR=:wk_VDCARR;
         wk_PrtOvrWS = 'VNDRDIR';
       ENDIF;                                                                // wk_VDCARR<>*Blanks
      /End-Free
     C     work_ORD#     CHAIN     OEORHD                             95
     C     *IN95         IFEQ      *OFF
     C                   EXSR      $RtvWhsAndPkl
     C     work_ORD#     SETLL     OEORDT
     C     work_ORD#     READE     OEORDT                                 95
     C     *IN95         DOWEQ     *OFF
     C     ODBLKT        IFNE      'Y'
     C***  ODQSTC        IFNE      *ZERO
     C                   IF        %subst(ODITEM:2:6) <> 'WRNTY '
     C                               and  ODITEM <> 'CALLTAG'
     C                               and  ODITEM <> 'MAILTAG'
     C                               and  ODITEM <> 'EMAILTAG'
     C                               and  ODITEM <> 'CALLTAGA'
     C                               and  ODITEM <> 'MAILTAGA'
     C                               and  ODITEM <> 'EMAILTAGA'
     C                   EVAL      wk_QSTC = *Zero
     C     IDKEY         SETLL     OEORDP01
     C                   DOU       %eof(OEORDP01)
     C     IDKEY         READE(N)  OEORDP01
     C                   IF        %eof(OEORDP01)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP01)
     C                   EVAL      wk_QSTC = wk_QSTC + IDQSTC
     C                   ENDDO                                                  %eof(OEORDP01)
     C***  ODQSTC        IFGT      *ZERO
     C***                IF        ODQSTC > *Zero  or
     C                   IF        wk_QSTC > *Zero  or
     C                               (work_PrtBO = Yes  and  ODORST <> 'C'  and
     C                                ODQORD > *Zero  and  ODQORD > ODQSTD)
     C     StartedList   IFEQ      No
     C                   EXSR      $StartList
     C                   MOVEL     Yes           StartedList
     C                   ENDIF                                                  StartedList=No
     C                   EXSR      $AddToList
     C***                IF        ODQSTC > *Zero
     C                   IF        wk_QSTC > *Zero
     C                   MOVE      ListNumber    ODPKL#
     C                   UPDATE    OEORDTR
     C     IDKEY         SETLL     OEORDP01
     C                   DOU       %eof(OEORDP01)
     C     IDKEY         READE     OEORDP01
     C                   IF        %eof(OEORDP01)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP01)
     C                   EVAL      IDPKL# = ListNumber
     C                   UPDATE    OEORDPR
     C                   ENDDO                                                  %eof(OEORDP01)
     C                   ELSE
     C                   UNLOCK    OEORDT
     C                   ENDIF                                                  wk_QSTC>*Zero
     C                   ELSE
     C                   UNLOCK    OEORDT
     C                   ENDIF                                                  wk_QSTC=*ZERO
     C                   ELSE
     C                   UNLOCK    OEORDT
     C                   IF        %subst(ODITEM:2:6) = 'WRNTY '
     C                   EVAL      LINE = ODORL#
     C                   EVAL      BLN# = ODBLN#
     C                   EXSR      $PrintComment3
     C                   ENDIF                                                  ODITEM='WRNTY '
     C                   ENDIF                                                  ODITEM='WRNTY '
     C                   ENDIF                                                  ODBLKT='Y'
     C     work_ORD#     READE     OEORDT                                 95
     C                   ENDDO                                                  *IN95=*OFF
     C     StartedList   IFEQ      Yes
     C                   EXSR      $FinishList
     C                   ENDIF                                                  StartedList=Yes

     C                   IF        wk_CustomPL
     C                   EXSR      $PrtCustomPL
     C                   ENDIF

     C                   ENDIF                                                  *IN95=*OFF

     C                   ENDSL

     C                   RETURN
     C/EJECT
     C********************************************************************
     C* Retrieve ship-from warehouse and current packing list number.    *
     C********************************************************************
     C     $RtvWhsAndPkl BEGSR
     C*
     C                   MOVEL     No            MultipleWhse      1
     C                   MOVEL     No            MultipleLists     1
     C                   CLEAR                   save_ODWH
     C***                CLEAR                   save_ODPKL#
     C                   CLEAR                   save_IDPKL#
     C     work_ORD#     SETLL     OEORDT
     C     work_ORD#     READE     OEORDT                                 95
     C     *IN95         DOWEQ     *OFF
     C                   EVAL      wk_QSTC = *Zero
     C     IDKEY         SETLL     OEORDP01
     C                   DOU       %eof(OEORDP01)
     C     IDKEY         READE(N)  OEORDP01
     C                   IF        %eof(OEORDP01)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEORDP01)
     C                   IF        IDQSTC > *Zero
     C                   EVAL      wk_QSTC = wk_QSTC + IDQSTC
     C                   IF        IDPKL# <> save_IDPKL#
     C                   IF        save_IDPKL# <> *Zero  and  IDPKL# <> *Zero
     C                   EVAL      MultipleLists = Yes
     C                   ELSE
     C                   EVAL      save_IDPKL# = IDPKL#
     C                   ENDIF                                                  save_IDPKL#=*Zero
     C                   ENDIF                                                  IDPKL#=save_IDPKL#
     C                   ENDIF                                                  IDQSTC>*Zero
     C                   ENDDO                                                  %eof(OEORDP01)
     C***  ODQSTC        IFNE      *ZERO
     C     wk_QSTC       IFNE      *ZERO
     C     ODWH          IFNE      save_ODWH
     C     save_ODWH     IFNE      *ZERO
     C                   MOVEL     Yes           MultipleWhse
     C                   ELSE
     C                   MOVE      ODWH          save_ODWH
     C                   ENDIF                                                  save_ODWH=*ZERO
     C                   ENDIF                                                  ODWH=save_ODWH
     C***  ODPKL#        IFNE      save_ODPKL#
     C***  save_ODPKL#   IFNE      *ZERO
     C***  ODPKL#        ANDNE     *ZERO
     C***                MOVEL     Yes           MultipleLists
     C***                ELSE
     C***                MOVE      ODPKL#        save_ODPKL#
     C***                ENDIF                                                  save_ODPKL#=*ZERO
     C***                ENDIF                                                  ODPKL#=save_ODPKL#
     C                   ENDIF                                                  wk_QSTC=*ZERO
     C     work_ORD#     READE     OEORDT                                 95
     C                   ENDDO                                                  *IN95=*OFF
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Start packing list.                                              *
     C********************************************************************
     C     $StartList    BEGSR
     C*
     C* Retrieve and format current system date & time.
     C*
     C                   EXSR      $RtvSysDatTim
     C*
     C* Retrieve current or next packing list number.
     C*
     C                   EXSR      $RtvPackListNo
     C*
     C* Populate Packing List Header Master fields.
     C*
     C                   EXSR      $FillHeader
     C*
     C* Open packing list print file.
     C*
     C                   IF        not wk_CustomPL
     C                   EXSR      $OpenList
     C                   ENDIF
     C*
     C* Reset first-detail flag.
     C*
     C                   MOVEL     Yes           FirstDetail       1
     C                   EVAL      wk_CannedAir = *OFF
     C                   EVAL      wk_CPDuo = *OFF
     C                   EVAL      wk_HITTarget = *OFF
     C*
     C* Clear total line fields.
     C*
     C                   CLEAR                   TOTWGT
     C                   CLEAR                   TOTUOM
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Add to packing list.                                             *
     C********************************************************************
     C     $AddToList    BEGSR
     C*
     C* Write a record to the Packing List Detail Master file.
     C*
     C                   EXSR      $WriteToDetail
     C*
     C* Write packing list header lines and reset line counter.
     C*
     C     FirstDetail   IFEQ      Yes
     C                   EXSR      $FirstDetail
     C                   MOVEL     No            FirstDetail
     C                   ENDIF                                                  FirstDetail=Yes
     C*
     C* Write packing list detail lines.
     C*
     C                   EXSR      $WriteDtlLines
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* First detail - print headings and stuff.                         *
     C********************************************************************
     C     $FirstDetail  BEGSR

     C                   EXSR      $WriteHdrLines

     C     OESVSV        CHAIN     HDSHPV
     C                   IF        not %found(HDSHPV)
     C                   EVAL      SVBOLR = No
     C                   ENDIF                                                  not %found(HDSHPV)
     C                   IF        wk_VDCDSC <> *Blanks
     C                   EVAL      SVBOLR = wk_VCBOLR
     C                   ENDIF                                                  wk_VDCDSC<>*Blanks
     C     work_Turn     CHAIN(N)  HYPTDTA
     C                   IF        %found(HYPTDTA)  and  TDVORD <> *Blanks
     C                   EVAL      kf_MDORDR = TDVORD
     C     key_MD        SETLL     MFD1MD
     C                   ENDIF                                                  %found(HYPTDTA) and
     C     key_TR        CHAIN     HYPSTRK
     C                   IF        %found(HYPTDTA)  and  TDVORD <> *Blanks  and
     C                               %equal(MFD1MD)
     C                   EVAL      wk_Trk#Line1 = *ON
     C                   DOU       %eof(MFD1MD)
     C     key_MD        READE     MFD1MD
     C                   IF        %eof(MFD1MD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(MFD1MD)
     C                   IF        wk_Trk#Line1
     C                   IF        MDFTRK <> *Blanks
     C                   EVAL      CMT = 'TRACKING# ' + MDFTRK
     C                   ELSE
     C                   EVAL      CMT = 'TRACKING# ' + MDFBRC
     C                   ENDIF                                                  MDFTRK<>*Blanks
     C                   EVAL      wk_Trk#Line1 = *OFF
     C                   ELSE
     C                   IF        MDFTRK <> *Blanks
     C                   EVAL      CMT = '          ' + MDFTRK
     C                   ELSE
     C                   EVAL      CMT = '          ' + MDFBRC
     C                   ENDIF                                                  MDFTRK<>*Blanks
     C                   ENDIF                                                  wk_Trk#Line1
     C                   IF        not %eof(MFD1MD)
     C     key_MD        READE     MFD1MD
     C                   IF        not %eof(MFD1MD)
     C                   IF        MDFTRK <> *Blanks
     C                   EVAL      CMT = %trimr(CMT) + ', ' + MDFTRK
     C                   ELSE
     C                   EVAL      CMT = %trimr(CMT) + ', ' + MDFBRC
     C                   ENDIF                                                  MDFTRK<>*Blanks
     C                   ENDIF                                                  not %eof(MFD1MD)
     C                   ENDIF                                                  not %eof(MFD1MD)
     C                   IF        not wk_CustomPL
     C                   EXSR      SRLOOP
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      LINCNT = LINCNT + 1
     C                   ENDIF                                                  not wk_CustomPL
     C                   ENDDO                                                  %eof(MFD1MD)
     C                   IF        not wk_CustomPL
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      LINCNT = LINCNT + 1
     C                   ENDIF                                                  not wk_CustomPL
     C                   ELSE
     C                   SELECT
     C                   WHEN      %found(HYPSTRK)  and  TRTRK# <> *Blanks
     C                   EVAL      CMT = 'TRACKING# ' + TRTRK#
     C                   WHEN      OEUDF3 = *Blanks
     C                   EVAL      CMT = *Blanks
     C                   WHEN      OESVSV = 'UP'  or  OESVSV = 'UN'  or         UPS
     C                               OESVSV = 'U2'  or  OESVSV = 'U3'  or
     C                               OESVSV = 'UC'  or  OESVSV = 'CU'  or
     C                               OESVSV = 'RG'  or  OESVSV = 'RN'  or
     C                               OESVSV = 'R2'  or  OESVSV = 'R3'
     C                   EVAL      CMT = 'TRACKING# 1Z6' + OEUDF3
     C                   WHEN      SVBOLR = 'Y'                                 Truck
     C                   EVAL      CMT = 'PRO# ' + OEUDF3
     C                   OTHER                                                  Fedex, Airborne, etc
     C                   EVAL      CMT = 'TRACKING# ' + OEUDF3
     C                   ENDSL
     C                   IF        not wk_CustomPL
     C                   IF        CMT <> *Blanks
     C                   EXSR      SRLOOP
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      LINCNT = LINCNT + 2
     C                   ENDIF                                                  CMT<>*Blanks
     C                   ENDIF                                                  not wk_CustomPL
     C                   ENDIF                                                  %found(HYPTDTA) and

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Finish packing list.                                             *
     C********************************************************************
     C     $FinishList   BEGSR
     C*
     C* Write a record to the Packing List Header Master file.
     C*
     C                   EXSR      $WriteToHeader
     C*
     C* Write packing list trailer lines.
     C*
     C                   EXSR      $WriteTlrLines
     C*
     C* Close packing list print file.
     C*
     C                   IF        (work_PDFName = *Blanks  or                  Normal spooled file
     C                               work_PDFName = '*NONE')  or
     C                               wk_PDFClose                                .pdf file
     C                   CLOSE     HOEPKPPF
     C                   EVAL      wk_PDFClose = *OFF
     C                   ENDIF                                                  work_PDFName=*Blanks
     C                   CLOSE     HOEPKPPF2
     C*
     C* Clear packing list file buffers.
     C*
     C                   FEOD      OEPKLH
     C                   FEOD      OEPKLD
     C*
     C* Update in-process header record with packing list number.
     C*
     C     work_Turn     CHAIN     OEORHP
     C                   IF        %found(OEORHP)
     C                   EVAL      IHPKL# = ListNumber
     C                   UPDATE    OEORHPR
     C                   ENDIF                                                  %found(OEORHP)
     C*
     C* Update turnaround data record with packing list number.
     C*
     C     work_Turn     CHAIN     HYPTDTA
     C                   IF        %found(HYPTDTA)
     C                   EVAL      TDPKL# = ListNumber
     C                   UPDATE    HYRTDTA
     C                   ENDIF                                                  %found(HYPTDTA)
     C*
     C* If canned air was shipped, print DOT spec sheet.
     C*
     C                   IF        wk_CannedAir
     C                   CALLP(E)  HYR6900 (*OFF)
     C                   ENDIF                                                  wk_CanncedAir
     C*
     C* If Case Prep Duo was shipped, print lithium ion shipping spec sheet.
     C*
     C                   IF        wk_CPDuo
     C                   CALLP(E)  HYR6900B (*OFF)
     C                   ENDIF                                                  wk_CPDuo
     C*
     C* If HIT Target was shipped, print lithium ion shipping spec sheet.
     C*
     C                   IF        wk_HITTarget
     C                   CALLP(E)  HYR6900C (*OFF)
     C                   ENDIF                                                  wk_HITTarget
     C*
     C                   ENDSR
     C********************************************************************
     C* Retrieve and format current system date & time.                  *
     C********************************************************************
     C     $RtvSysDatTim BEGSR
     C*
     C* Retrieve current system date & time.
     C*
     C                   TIME                    T@TMDT
     C*
     C* Format current system date to CYYMMDD and move to work field.
     C*
     C                   MOVE      *ZERO         DHLD
     C                   MOVE      T@DSTP        DHLD
     C                   CALLB     'CSYDTC'
     C                   PARM                    DHLD              7
     C                   PARM      '*SYSVAL'     DFFFMT           10
     C                   PARM      '*CYMD'       DFTFMT           10
     C                   MOVEL     DHLD          date_CYYMMDD
     C*
     C* Move current system time to work field.
     C*
     C                   MOVE      T@TSTP        time_HHMMSS
     C*
     C* Determine whether today is a ship day, i.e., not Saturday, Sunday, or holiday.
     C*
     C                   IF        not %open(HDCALD)
     C                   OPEN      HDCALD
     C                   ENDIF                                                  not %open(HDCALD)
     C     CDKEY         CHAIN     HDCALD
     C                   IF        %found(HDCALD)
     C                   EVAL      wk_ShipDay = CDWRKD
     C                   ELSE
     C                   EVAL      wk_ShipDay = Yes
     C                   ENDIF                                                  %found(HDCALD)
     C*
     C                   IF        time_HHMMSS > wk_DayCutoff
     C                               or  wk_ShipDay <> Yes
     C                   IF        not %open(HDCALD)
     C                   OPEN      HDCALD
     C                   ENDIF                                                  not %open(HDCALD)
     C     CDKEY         SETGT     HDCALD
     C                   DOU       %eof(HDCALD)  or  CDWRKD = Yes
     C                   READ      HDCALD
     C                   ENDDO                                                  %eof(HDCALD) or
     C                   IF        not %eof(HDCALD)
     C                   EVAL      wk_PklCYMD = CDCYMD
     C                   EVAL      wk_PklTime = 000100
     C                   ELSE
     C                   EVAL      wk_PklCYMD = date_CYYMMDD
     C                   EVAL      wk_PklTime = time_HHMMSS
     C                   ENDIF                                                  not %eof(HDCALD)
     C                   ELSE
     C                   EVAL      wk_PklCYMD = date_CYYMMDD
     C                   EVAL      wk_PklTime = time_HHMMSS
     C                   ENDIF                                                  time_HHMMSS>wk_DayCu
     C                   EVAL      ds_CYMD = wk_PklCYMD
     C                   EVAL      ds_YY2 = ds_YY1
     C                   EVAL      wk_PklMDY = ds_MDY
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Retrieve current or next packing list number.                    *
     C********************************************************************
     C     $RtvPackListNoBEGSR
     C*
     C* If a current packing list exists according to the order detail,
     C* verify its existence.
     C*
     C     MultipleLists IFEQ      No
     C     save_IDPKL#   ANDNE     *ZERO
     C     save_IDPKL#   SETLL     OEPKLH                                 95
     C     *IN95         IFEQ      *OFF
     C                   CLEAR                   save_IDPKL#
     C                   ENDIF                                                  *IN95=*OFF
     C                   ENDIF                                                  MultipleLists=Yes
     C*
     C* If a current packing list exists, delete the existing Packing
     C* List Header Master records, and use that number.  Otherwise,
     C* retrieve the next available packing list number.
     C*
     C     MultipleLists IFEQ      No
     C     save_IDPKL#   ANDNE     *ZERO
     C     save_IDPKL#   CHAIN     OEPKLH                             95
     C     *IN95         IFEQ      *OFF
     C                   DELETE    OEPKLHR
     C                   ENDIF                                                  *IN95=*OFF
     C     save_IDPKL#   SETLL     OEPKLD
     C     save_IDPKL#   READE     OEPKLD                                 95
     C     *IN95         DOWEQ     *OFF
     C                   DELETE    OEPKLDR
     C     save_IDPKL#   READE     OEPKLD                                 95
     C                   ENDDO                                                  *IN95=*OFF
     C                   Z-ADD     save_IDPKL#   ListNumber
     C                   ELSE
     C     ListNumber    DOWEQ     *ZERO
     C     *IN98         DOUEQ     *OFF
     C     1             CHAIN     OECTRLR                            9598
     C     *IN98         IFEQ      *ON                                          Test for
     C                   MOVEL     'OECTRL'      RLFILE                         record lock
     C                   CALLB     'CSYRLO'      CSYRLO
     C                   ENDIF                                                  *IN98=*ON
     C                   ENDDO                                                  *IN98=*OFF
     C                   CLEAR                   RLCNT
     C     *IN95         IFEQ      *OFF
     C                   ADD       1             CELPL#
     C                   UPDATE    OECTRLR
     C     CELPL#        CHAIN(N)  OEPKLH                             95
     C     *IN95         IFEQ      *ON
     C                   MOVE      CELPL#        ListNumber
     C                   ENDIF                                                  *IN95=*ON
     C                   ENDIF                                                  *IN95=*OFF
     C                   ENDDO                                                  ListNumber=*ZERO
     C                   ENDIF                                                  MultipleLists=No
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Populate Packing List Header Master fields.                      *
     C********************************************************************
     C     $FillHeader   BEGSR
     C*
     C                   CLEAR                   OEPKLHR
     C                   MOVE      ListNumber    SDPKL#
     C     MultipleWhse  IFEQ      No
     C                   MOVE      save_ODWH     SDSHFM
     C                   ELSE
     C                   CLEAR                   SDSHFM
     C                   ENDIF
     C                   MOVE      OESHTO        SDSHTO
     C***  OEDSHP        IFEQ      *ZERO
     C***                MOVEL     No            SDDSHC
     C***                ELSE
     C***                MOVEL     Yes           SDDSHC
     C***                ENDIF                                                  OEDSHP=*ZERO
     C                   MOVE      OEDSHP        SDDSHP
     C                   MOVE      OEBLTO        SDBLTO
     C                   IF        wk_VDCDSC = *Blanks
     C                   MOVEL     OESVSV        SDSHPV
     C                   MOVEL     OESVDS        SDSVDS
     C                   ELSE
     C                   EVAL      SDSHPV = wk_VDCARR
     C                   EVAL      SDSVDS = wk_VDCDSC
     C                   ENDIF                                                  wk_VDCDSC<>*Blanks
     C***                MOVE      date_CYYMMDD  SDSHDT
     C***                MOVE      time_HHMMSS   SDSTIM
     C                   EVAL      SDSHDT = wk_PklCYMD
     C                   EVAL      SDSTIM = wk_PklTime
     C                   MOVEL     'CT'          SDSTZN
     C                   MOVEL     Yes           SDPKLP
     C                   MOVEL     No            SDBOLR
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Open packing list print file.                                    *
     C********************************************************************
     C     $OpenList     BEGSR
     C                   IF        work_PDFName <> *Blanks  and                 .pdf file
     C                               work_PDFName <> '*NONE'
     C                   EXSR      $ChkCtlChar
     C                   ENDIF                                                  work_PDFName<>*Blank
     C                   IF        (work_PDFName = *Blanks  or                  Normal spooled file
     C                               work_PDFName = '*NONE')  or
     C                             (work_PDFName <> *Blanks  and                .pdf file
     C                               work_PDFName <> '*NONE' and
     C                               not %open(HOEPKPPF))
     C*
      /Free
       IF work_PDFName <> *Blanks and work_PDFName <> '*NONE';
         IF not %open(HYPPSCT);
           OPEN HYPPSCT;
           CHAIN 1 HYPPSCT;
         ENDIF;                                                              // %open(HYPPSCT)
         wk_OvrCmd = 'OVRPRTF FILE(HOEPKPPF) DEVTYPE(*AFPDS) FRONTMGN(+
           .25 1.5) PAGRTT(0) PAGESIZE(66 132) OVRFLW(60) LPI(6) +
           CPI(12) FRONTOVL(' +
           %trim(CSPLOVL) + ' ' + %triml(%editc(CSPLOVLD:'3')) + ' ' +
           %triml(%editc(CSPLOVLA:'3')) + ') TOSTMF(''' + %trim(CSFLR1) +
           %trim(work_PDFName) + ''') WSCST(*PDF) OVRSCOPE(*CALLLVL)';
         CALLP QCMDEXC (wk_OvrCmd:%len(%trimr(wk_OvrCmd)));
      /End-Free
     C                   ELSE
     C*
     C                   MOVE      APID          JAPID             2
     C                   MOVE      USER          JUSER            10
     C                   MOVE      wk_PrtOvrWS   JWS              10
     C                   MOVEL     'HOEPKP'      JPGID            10
     C                   MOVEL(P)  'HOEPKPPF'    JPRTF            10
     C                   MOVE      *BLANKS       JPRTFT           10
     C                   CALLB     'HSYSBM'      HSYSBM
     C                   CALL      'QCMDEXC'     PLSTOV                 99
     C                   ENDIF                                                  work_PDFName<>*Blank
     C                   OPEN      HOEPKPPF
     C                   ENDIF                                                  work_PDFName=*Blanks
     C*
     C                   MOVE      APID          JAPID
     C                   MOVE      USER          JUSER
     C                   MOVE      wk_PrtOvrWS   JWS
     C                   MOVEL     'HOEPKP'      JPGID
     C                   MOVEL     'HOEPKPPF2'   JPRTF
     C                   CALLB     'HSYSBM'      HSYSBM
     C                   CALL      'QCMDEXC'     PLSTOV                 99
     C                   OPEN      HOEPKPPF2
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Check for control characters in .pdf file name.                  *
     C********************************************************************
     C     $ChkCtlChar   BEGSR

      /Free
         wk_PDFClose = *OFF;
         EVAL wk_Pos = %scan (' /C ':work_PDFName);
         IF wk_Pos > *Zero;
           work_PDFName = %replace('':work_PDFName:wk_Pos:4);
           wk_PDFClose = *ON;
         ELSE;
           IF %subst(work_PDFName:%len(work_PDFName)-2:3) = ' /C';
             work_PDFName = %subst(work_PDFName:1:%len(work_PDFName)-2);
             wk_PDFClose = *ON;
           ENDIF;                                                            // %subst(work_PDFName:
         ENDIF;                                                              // wk_Pos>*Zero
      /End-Free

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Write packing list header lines.                                 *
     C********************************************************************
     C     $WriteHdrLinesBEGSR
     C*
     C                   CLEAR                   LOA
     C                   CLEAR                   STA
     C                   CLEAR                   BTA
     C                   CLEAR                   WHCSZ
     C***                MOVE      *ALL'0'       DHLD
     C***                MOVE      UDATE         DHLD
     C***                CALLB     'CSYDTC'      PFMDY
     C***                MOVEL     DHLD          RUDATE
     C***                MOVE      time_HHMMSS   RUTIME
     C                   EVAL      RUDATE = wk_PklMDY
     C                   EVAL      RUTIME = wk_PklTime
     C                   EXSR      $ShipLoc
     C     SDSHFM        CHAIN     HDWHSMR                            99
     C                   MOVEL     WHWHCT        WHCSZ            44
     C     WHCSZ         CAT       WHWHST:1      WHCSZ
     C     WHCSZ         CAT       WHHWZP:2      WHCSZ
     C                   CLEAR                   CNYWH
     C     WHCTRY        IFNE      HDCTCD
     C     WHCTRY        CHAIN     HDCTRYR                            99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CNCDES        CNYWH            30
     C                   ENDIF
     C                   ENDIF

      *
      *  If not found in Shipping Location file (HYPSLOC)
      *
     C                   IF        wk_SLocFound = *OFF
     C     WHWHNM        IFNE      *BLANKS
     C                   DO        5             W                 2 0
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     WHWHNM        LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     WHWHAD        IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     WHWHAD        LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     WHWHA2        IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     WHWHA2        LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     WHCSZ         IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     WHCSZ         LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     CNYWH         IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     CNYWH         LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF                                                  wk_SLocFound=*OFF

      *
      *  If found in Shipping Location file (HYPSLOC)
      *
     C                   IF        wk_SLocFound = *ON
     C     SLNAME        IFNE      *BLANKS
     C                   DO        5             W                 2 0
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     SLNAME        LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     SLADDR1       IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     SLADDR1       LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     SLADDR2       IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     SLADDR2       LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     SLCITY        IFNE      *BLANKS
     C                   EVAL      wk_CSZ = %trim(SLCITY) + ', ' +
     C                               SLST + '  ' + %trim(SLZIP)
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     wk_CSZ        LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C     CNYWH         IFNE      *BLANKS
     C                   DO        5             W
     C     LOA(W)        IFEQ      *BLANKS
     C                   MOVEL     CNYWH         LOA(W)
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF                                                  wk_SLocFound=*ON

     C*
     C                   CLEAR                   CMCSZC
     C     SDBLTO        CHAIN     HDCUSTR                            99
     C                   MOVEL     CMCCTY        CMCSZC           44
     C     CMCSZC        CAT       CMST:1        CMCSZC
     C     CMCSZC        CAT       CMZIP:2       CMCSZC
     C                   CLEAR                   CNYCM
     C     CMCTRY        IFNE      HDCTCD
     C     CMCTRY        CHAIN     HDCTRYR                            99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CNCDES        CNYCM            30
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     1             W                 2 0
     C     CMCNA1        IFNE      *BLANKS
     C                   MOVEL     CMCNA1        BTA(W)
     C     SDDSHP        IFNE      *Zero
     C                   EVAL      %subst(BTA(W):43:2) = 'DS'
     C                   ENDIF                                                  SDDSHP=*Zero
     C                   ADD       1             W
     C                   ENDIF
     C     CMCNA2        IFNE      *BLANKS
     C                   MOVEL     CMCNA2        BTA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCNA3        IFNE      *BLANKS
     C                   MOVEL     CMCNA3        BTA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCNA4        IFNE      *BLANKS
     C                   MOVEL     CMCNA4        BTA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCSZC        IFNE      *BLANKS
     C                   MOVEL     CMCSZC        BTA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CNYCM         IFNE      *BLANKS
     C                   MOVEL     CNYCM         BTA(W)
     C                   ENDIF
     C*
     C                   CLEAR                   CMCSZC
     C***  SDDSHC        IFEQ      'Y'
     C***  SDSHTO        CHAIN     HDDSHPR                            99
     C     SDDSHP        IFNE      *Zero
     C     SDDSHP        CHAIN     HDDSHPR                            99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DSCITY        CMCSZC           44
     C     CMCSZC        CAT       DSST:1        CMCSZC
     C     CMCSZC        CAT       DSZIP:2       CMCSZC
     C                   CLEAR                   CNYCM
     C     DSCTRY        IFNE      HDCTCD
     C     DSCTRY        CHAIN     HDCTRYR                            99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CNCDES        CNYCM            30
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     1             W
     C     DSNAME        IFNE      *BLANKS
     C                   MOVEL     DSNAME        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     DSADR1        IFNE      *BLANKS
     C                   MOVEL     DSADR1        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     DSADR2        IFNE      *BLANKS
     C                   MOVEL     DSADR2        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     DSADR3        IFNE      *BLANKS
     C                   MOVEL     DSADR3        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCSZC        IFNE      *BLANKS
     C                   MOVEL     CMCSZC        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CNYCM         IFNE      *BLANKS
     C                   MOVEL     CNYCM         STA(W)
     C                   ENDIF
     C                   ENDIF                                                  *IN99=*OFF
     C                   ELSE
     C     SDSHTO        CHAIN     HDCUSTR                            99
     C                   MOVEL     CMCCTY        CMCSZC           44
     C     CMCSZC        CAT       CMST:1        CMCSZC
     C     CMCSZC        CAT       CMZIP:2       CMCSZC
     C                   CLEAR                   CNYCM
     C     CMCTRY        IFNE      HDCTCD
     C     CMCTRY        CHAIN     HDCTRYR                            99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CNCDES        CNYCM            30
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     1             W
     C     CMCNA1        IFNE      *BLANKS
     C                   MOVEL     CMCNA1        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCNA2        IFNE      *BLANKS
     C                   MOVEL     CMCNA2        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCNA3        IFNE      *BLANKS
     C                   MOVEL     CMCNA3        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCNA4        IFNE      *BLANKS
     C                   MOVEL     CMCNA4        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CMCSZC        IFNE      *BLANKS
     C                   MOVEL     CMCSZC        STA(W)
     C                   ADD       1             W
     C                   ENDIF
     C     CNYCM         IFNE      *BLANKS
     C                   MOVEL     CNYCM         STA(W)
     C                   ENDIF
     C***                ENDIF                                                  SDDSHC='Y'
     C                   ENDIF                                                  SDDSHP=*Zero
     C*
     C                   IF        not wk_CustomPL
     C                   WRITE     PKLH1
     C                   WRITE     XPKLH1
     C                   ENDIF                                                  not wk_CustomPL
     C                   CLEAR                   LINCNT
     C*
     C                   IF        wk_VDVEND <> *Zero
     C                   EXSR      $PrintVDSInfo
     C                   ENDIF                                                  wk_VDVEND<>*Zero
     C*
     C***  SDDSHC        IFNE      'Y'                                          Not Drop Ship
     C                   IF        SDDSHP = *Zero                               Not Drop Ship
     C                   CLEAR                   LINE
     C                   CLEAR                   BLN#                           Print Header
     C                   IF        not wk_CustomPL
     C                   EXSR      $PrintComment2                               Comments
     C                   ENDIF                                                  not wk_CustomPL
     C                   ELSE
     C                   IF        wk_ITARTTB
     C                   CLEAR                   LINE
     C                   CLEAR                   BLN#                           Print Header
     C                   EXSR      $PrtExportCmt
     C                   ENDIF                                                  wk_ITARTTB
     C                   ENDIF
     C*
     C                   CLEAR                   LINE
     C                   CLEAR                   BLN#                           Print Header
     C                   IF        not wk_CustomPL
     C                   EXSR      $PrintComment1                               Comments
     C                   ENDIF                                                  not wk_CustomPL
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Get Shipping Location Address Info.                              *
     C********************************************************************
     C     $ShipLoc      BEGSR

      *
      *  Initially default to GI ship loc
      *
     C                   EVAL      wk_ShipLoc = GI
     C                   EVAL      wk_SLocFound = *OFF
      *
      *  Ship Loc is Alda if this is an Alda workstation
      *
     C                   EVAL      @TABL = 'ALDAWKSTN'
     C                   EVAL      @KEY1 = WS
     C     TDKEY         CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   EVAL      wk_ShipLoc = Alda
     C                   ENDIF

      *
      *  Determine other ship locations here (future)
      *

      *
      *  Check Foreign Ship.  If it is, then always ship from GI.
      *
     C                   EXSR      $CheckForeign

     C                   IF        wk_Foreign = *ON
     C                   EVAL      wk_ShipLoc = GI
     C                   ENDIF
      *
      *  Get the shipping location address info to print on the label
      *
     C     SLKEY         CHAIN     HYPSLOC
     C                   IF        %found(HYPSLOC)
     C                   EVAL      wk_SLocFound = *ON
     C                   ENDIF
     C

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Check for Foreign shipment.                                      *
     C********************************************************************
     C     $CheckForeign BEGSR

     C                   EVAL      wk_Foreign = *OFF
     C                   EVAL      wk_ITARTTB = *OFF

     C     SDBLTO        CHAIN     HDCUSTR                            99

     C                   IF        *IN99 = *OFF
     C                   EVAL      @TABL = 'CUSTCLASS'
     C                   EVAL      @KEY1 = CMCCLS
     C     TDKEY         CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)  and  TDFF02 = Yes
     C                   EVAL      wk_Foreign = *ON
     C                   ENDIF                                                  %found and TDFF02=Y
     C                   IF        %found(GUPTDAT)  and  TDFF05 = Yes
     C                   EVAL      wk_ITARTTB = *ON
     C                   ENDIF                                                  %found and TDFF05=Y
     C                   ENDIF                                                  *IN99=*OFF

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Write packing list detail lines.                                 *
     C********************************************************************
     C     $WriteDtlLinesBEGSR

     C                   IF        wk_CustomPL
     C                   LEAVESR
     C                   ENDIF

      *
      * Check if item requires a spec sheet to print also.
      *
     C                   IF        PLQTYS > *Zero
     C                   EVAL      @TABL = 'SHIPSPECS'
     C                   EVAL      @KEY1 = PLITEM
     C     TDKEY         CHAIN     GUPTDAT
     C                   IF        %found(GUPTDAT)
     C                   SELECT
     C                   WHEN      TDCF02 = 'HYR6900'
     C                   EVAL      wk_CannedAir = *ON
     C                   WHEN      TDCF02 = 'HYR6900B'
     C                   EVAL      wk_CPDuo = *ON
     C                   WHEN      TDCF02 = 'HYR6900C'
     C                   EVAL      wk_HITTarget = *ON
     C                   ENDSL
     C                   ENDIF                                                  %found(GUPTDAT)
     C                   ENDIF                                                  PLQTYS>*Zero

      *
      * If shipped item is canned air, set flag to print DOT spec sheets.
      *
     C****               IF        PLITEM = '99900'  and  PLQTYS > *Zero
     C****               EVAL      wk_CannedAir = *ON
     C****               ENDIF                                                  PLITEM='99900' and

      *
      * If shipped item is Case Prep Duo, set flag to print lithium ion
      * battery shipping spec sheet.
      *
     C****               IF        PLITEM = '050180'  and  PLQTYS > *Zero
     C****               EVAL      wk_CPDuo = *ON
     C****               ENDIF                                                  PLITEM='050180' and

      *
      * If shipped item is HIT Target, set flag to print lithium ion
      * battery shipping spec sheet.
      *
     C****               IF        (PLITEM = '97300'  or  PLITEM = '97301'  or
     C****                           PLITEM = '97300B')  and  PLQTYS > *Zero
     C****               EVAL      wk_HITTarget = *ON
     C****               ENDIF                                                  PLITEM='97300' or

     C     CBKEY         CHAIN     BARCUSTL1
     C                   IF        %found(BARCUSTL1)  and  CBIPL = 'C'
     C                   EVAL      PLITEMX = CBCITEM
     C                   ELSE
     C                   EVAL      PLITEMX = PLITEM
     C                   ENDIF                                                  %found(BARCUSTL1)

      *
      * If the packing list is for a mil-spec order, and the order's pricing
      * UOM is different than the item's stocking UOM, change the packing
      * list UOM to the pricing UOM, and convert the packing list quantity-
      * shipped to the pricing UOM quantity-shipped, and print the stocking
      * UOM and quantity-shipped below the item description.
      *
      /Free
       wk_ODPRUM = *Blanks;
       wk_ODSTUM = *Blanks;
       wk_ODORUF = *Zero;
       EXEC SQL select ODPRUM, ODSTUM, ODORUF
                 into :wk_ODPRUM, :wk_ODSTUM, :wk_ODORUF
                from OEORDT
                where ODORD#=:PLORD# and ODORL#=:PLORL# and
                      ODBLN#=:PLBLN#;
       IF wk_ODORUF = *Zero;
         wk_ODORUF = 1;
       ENDIF;                                                                // wk_ODORUF=*Zero
       wk_MilSpec = *OFF;
       IF wk_ODPRUM = wk_ODSTUM;
         EVAL wk_PrcUOMQty = PLQTYS;
       ELSE;
         wk_RcdCnt = *Zero;
         EXEC SQL select count(*) into :wk_RcdCnt                            // Determine if the
                  from HDSHQT a                                              // shipped item is
                  where SQTURN=:work_Turn and SQORD=:PLORD# and              // mil-spec.
                   SQORL#=:PLORL# and SQBLN#=:PLBLN# and
                   exists (select * from LDPMORD b where
                   b.OMLOT=trim(a.SQLOTN) and b.OMMSLT<>' ');
         IF wk_RcdCnt = *Zero;
           EVAL wk_PrcUOMQty = PLQTYS;
         ELSE;
           EVAL wk_PrcUOMQty = PLQTYS / wk_ODORUF;
           PLSTUM = wk_ODPRUM;
           wk_MilSpec = *ON;
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ENDIF;                                                                // wk_ODPRUM<>wk_ODSTUM
      /End-Free

     C*
     C****               Z-ADD     PLQTYS        PLQTYSX
     C                   EVAL      PLQTYSX = wk_PrcUOMQty
     C                   EXSR      SRLOOP
     C                   WRITE     PKLB1
     C                   WRITE     XPKLB1
     C                   ADD       1             LINCNT            3 0
     C***  KVCIT         CHAIN     HDVCITR                            73
     C***  *IN73         IFEQ      *OFF
     C***  VCVCIT        ANDNE     *BLANKS
     C                   IF        ODORCS <> *Blanks
     C                   EXSR      SRLOOP
     C                   WRITE     PKLB3
     C                   WRITE     XPKLB3
     C                   ADD       1             LINCNT
     C                   ENDIF
      *
      * If mil-spec order line (and the pricing UOM is different than the
      * stocking UOM), print the stocking UOM and quantity-shipped below
      * the item description.
      *
     C                   IF        wk_MilSpec
     C                   EVAL      PLQTYSX = PLQTYS
     C                   EVAL      CMT = '  ' + %triml(%editc(PLQTYSX:'3'))
     C                   SELECT
     C                   WHEN      wk_ODSTUM = 'BX'
     C                   EVAL      CMT = %trimr(CMT) + ' BOXES'
     C                   OTHER
     C                   EVAL      CMT = %trimr(CMT) + ' ' + wk_ODSTUM
     C                   ENDSL
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ENDIF                                                  wk_MilSpec

      *
      * For Gander Mountain shipments, print each item's UPC.
      *
     C                   IF        SDBLTO = 21419  and IMUPC <> *Zero
     C                   EVAL      UPC = IMUPC
     C                   WRITE     PKLB4
     C                   WRITE     XPKLB4
     C                   ADD       1             LINCNT
     C                   ENDIF

     C*
     C     HDIVRL        IFNE      *ZEROS                                       I/V Installed
     C     CILTRL        ANDNE     'A'                                          Not Auto Relief
     C                   EXSR      $WriteLotLines                               Check For Lots
     C                   ENDIF

     C                   IF        %subst(IMPCLS:1:2) = 'BB'
     C                               or  IMPCLS = 'CAAA'                        Primed rifle cases
     C                               or  IMPCLS = 'CAAC'                        Unprimed rifle cases
     C                               or  IMPCLS = 'CBAA'                        Primed pistol cases
     C                               or  IMPCLS = 'CBAC'                        Unprimed pistol case
     C                               or  IMPCLS = 'CCAA'                        Primed shotgun cases
     C                               or  IMPCLS = 'CCAC'                        Unprimed shotgn case
     C                               or  %subst(IMPCLS:1:2) = 'CH'              OEM cases
     C                   IF        OEORTY <> 'V'
     C                   EVAL      @TABL = 'BULKLOTCUS'
     C                   EVAL      @KEY1 = %editc(OEBLTO:'X')
     C     TDKEY         CHAIN     GUPTDAT
     C                   ENDIF                                                  OEORTY<>'V'
     C                   IF        (OEORTY = 'V'  or  %found(GUPTDAT))
     C                   EXSR      $WriteLotLine2                               Check for bulk Lots
     C                   ENDIF                                                  OEORTY='V' or
     C                   ENDIF                                                  %subst(IMPCLS:1:2)=

      *
      * If backorder quantities requested to print, print them.
      *
     C                   IF        work_PrtBO = Yes
     C                   EVAL      wk_QtyBO = ODQORD - ODQSTD - ODQSTC
     C                   IF        wk_QtyBO > *Zero
     C                   EVAL      wk_QtyBO2 = wk_QtyBO
     C                   EVAL      PLIMDS = '    BACKORDER QTY' +
     C                               %editc(wk_QtyBO2:'3')
     C                   WRITE     PKLB5
     C                   WRITE     XPKLB5
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_QtyBO>*Zero
     C                   ENDIF                                                  work_PrtBO=Yes

     C                   Z-ADD     PLORL#        LINE
     C                   Z-ADD     PLBLN#        BLN#                           Print Detail
     C                   EXSR      $PrintComment1                               Comments
     C*
     C* Summarize total line fields.
     C*
     C                   SELECT
     C                   WHEN      PLWTUM = 'OZ'
     C                   EVAL(H)   PLWGTX = PLWGTXX / 16
     C                   ENDSL
     C                   ADD       PLWGTX        TOTWGT
     C****               MOVEL     PLWTUM        TOTUOM
     C                   MOVEL     'LB'          TOTUOM
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Write packing list detail lot lines.                             *
     C********************************************************************
     C     $WriteLotLinesBEGSR
     C*
     C     KSHQT         SETLL     HDSHQTR
     C     1             DOUEQ     2
     C     KSHQT         READE     HDSHQTR                                99
     C     *IN99         IFEQ      *ON
     C     SQLOTN        OREQ      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C     SQLQTY        IFLE      *ZEROS
     C                   ITER
     C                   ENDIF
     C                   EVAL      wk_Lot#LJ = %triml(SQLOTN)
      /Free
       wk_OMMSLT = *Blanks;
       EXEC SQL select OMMSLT into :wk_OMMSLT
                from LDPMORD
                where OMLOT=:wk_Lot#LJ;
      /End-Free
     C                   IF        wk_OMMSLT = *Blanks
     C                   EXSR      SRLOOP
     C                   EVAL      SQLOTN2 = %triml(SQLOTN)
     C                   WRITE     PKLL1
     C****               WRITE     XPKLL1
     C                   WRITE     XPKLL2
     C                   ADD       1             LINCNT
     C                   ELSE
     C                   EXSR      SRLOOP2
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = '  Sub Lot:'
     C                   EVAL      %subst(CMT:13:15) = wk_Lot#LJ
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = '  Mil Lot:'
     C                   EVAL      %subst(CMT:13:15) = wk_OMMSLT
     C                   IF        ODORUF > 1
     C                   SELECT
     C                   WHEN      ODPRUM = 'CT'
     C                   EVAL      wk_PurUOM = 'CRATE'
     C                   EVAL      wk_PurUOMS = 'CRATES'
     C                   WHEN      ODPRUM = 'CA'
     C                   EVAL      wk_PurUOM = 'CASE'
     C                   EVAL      wk_PurUOMS = 'CASES'
     C                   OTHER
     C                   EVAL      wk_PurUOM = 'UNIT'
     C                   EVAL      wk_PurUOMS = 'UNITS'
     C                   ENDSL
     C                   IF        %rem(%dec(SQLQTY:9:0):%dec(ODORUF:5:0)) =
     C                                  *Zero
     C                   EVAL      wk_PurQty = SQLQTY / ODORUF
     C                   IF        wk_PurQty = 1
     C                   EVAL      %subst(CMT:44:15) =
     C                               %trim(%editc(wk_PurQty:'J')) + ' ' +
     C                             wk_PurUOM
     C                   ELSE
     C                   EVAL      %subst(CMT:44:15) =
     C                               %trim(%editc(wk_PurQty:'J')) + ' ' +
     C                             wk_PurUOMS
     C                   ENDIF                                                  wk_PurQty=1
     C                   ELSE
     C                   EVAL(H)   wk_PurQty2 = SQLQTY / ODORUF
     C                   EVAL      %subst(CMT:44:15) =
     C                               %trim(%editc(wk_PurQty2:'J')) + ' ' +
     C                             wk_PurUOMS
     C                   ENDIF                                                  %rem(SQLQTY:ODORUF)=
     C                   ENDIF                                                  ODORUF>1
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       4             LINCNT
     C                   ENDIF                                                  wk_OMMSLT=*Blanks
     C                   ENDDO
     C*
     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Write packing list detail bulk bullet lot lines.                 *
     C********************************************************************
     C     $WriteLotLine2BEGSR

     C                   IF        not %open(HYLSSCC2)
     C                   OPEN      HYLSSCC2
     C                   ENDIF                                                  not %open(HYLSSCC2)
     C                   IF        not %open(HYLSCWK2)
     C                   OPEN      HYLSCWK2
     C                   ENDIF                                                  not %open(HYLSCWK2)

     C                   EVAL      ary_Lots = *Blanks
     C                   EVAL      idx1 = *Zero

     C     KSSCC         SETLL     HYLSSCC2
     C                   DOU       %eof(HYLSSCC2)
     C     KSSCC         READE     HYLSSCC2
     C                   IF        %eof(HYLSSCC2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSSCC2)
     C                   IF        SCLOT# = *Blanks
     C                               or  SCTURN <> work_Turn
     C                   ITER
     C                   ENDIF                                                  SCLOT#=*Blanks
     C                   EVAL      idx1 = idx1 + 1
     C                   IF        idx1 > maxidx1
     C                   ITER
     C                   ENDIF                                                  idx1>maxidx1
     C                   EVAL      ary_Lots(idx1) = %char(SCTSTP) + SCLOT# +
     C                               %triml(%editw(SCUOMQ:'0         .    '))
     C                   ENDDO                                                  %eof(HYLSSCC2)

     C                   IF        idx1 < maxidx1
     C     KSCWK         SETLL     HYLSCWK2
     C                   DOU       %eof(HYLSCWK2)
     C     KSCWK         READE     HYLSCWK2
     C                   IF        %eof(HYLSCWK2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLSCWK2)
     C                   IF        S1LOT# = *Blanks
     C                               or  S1TURN <> work_Turn
     C                   ITER
     C                   ENDIF                                                  S1LOT#=*Blanks
     C                   EVAL      idx1 = idx1 + 1
     C                   IF        idx1 > maxidx1
     C                   ITER
     C                   ENDIF                                                  idx1>maxidx1
     C                   EVAL      ary_Lots(idx1) = %char(S1TSTP) + S1LOT# +
     C                               %triml(%editw(S1UOMQ:'0         .    '))
     C                   ENDDO                                                  %eof(HYLSCWK2)
     C                   ENDIF                                                  idx1<maxidx1

     C                   IF        idx1 = *Zero
     C                   LEAVESR
     C                   ENDIF                                                  idx1=*Zero

     C                   IF        idx1 <= maxidx1
     C                   SORTA     %subarr(ary_Lots:1:idx1)
     C                   ELSE
     C                   EVAL      SQLOTN =
     C                               %triml(%editc(idx1:'Z')) + ' CTNS SHIPPED'
     C                   EVAL      SQLOTN2 = %triml(SQLOTN)
     C                   WRITE     PKLL1
     C****               WRITE     XPKLL1
     C                   WRITE     XPKLL2
     C                   EVAL      LINCNT = LINCNT + 1
     C                   EVAL      wk_MsgData = %triml(%editc(work_ORD#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:15) = PLITEM
     C                   EVAL      %subst(wk_MsgData:24:5) =
     C                               %triml(%editc(maxidx1:'Z'))
     C                   EVAL      %subst(wk_MsgData:29:8) =
     C                               %triml(%editc(SDPKL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:37:10) = PGMNAME
     C                   CALLP(E)  GUCSNDUM ('GUPMSGF':'ERR0969':wk_MsgData:
     C                               'RSEKORA')
     C                   CALLP(E)  GUCSNDUM ('GUPMSGF':'ERR0969':wk_MsgData:
     C                               'JMESSING')
     C                   CALLP(E)  GUCSNDUM ('GUPMSGF':'ERR0969':wk_MsgData:
     C                               'JELLIS')
     C                   CALLP(E)  GUCSNDUM ('GUPMSGF':'ERR0969':wk_MsgData:
     C                               'BEARL')
     C                   LEAVESR
     C                   ENDIF                                                  idx1<=maxidx1

     C                   EVAL      wk_TotLotQty = *Zero
     C                   FOR       idx2 = idx1 downto 1
     C                   EVAL      wk_LotQty =
     C                               %dec(%subst(ary_Lots(idx2):42:14):13:4)
     C                   EVAL      wk_TotLotQty = wk_TotLotQty + wk_LotQty
     C                   IF        wk_TotLotQty >= PLQTYS
     C                   LEAVE
     C                   ENDIF                                                  wk_TotLotQty>=PLQTYS
     C                   ENDFOR                                                 idx2=idx1 downto 1
     C                   IF        idx2 = *Zero
     C                   EVAL      idx2 = 1
     C                   ENDIF                                                  idx2=*Zero

     C                   EVAL      idx4 = *Zero
     C                   EVAL      ary_Lots2 = *Blanks
     C                   FOR       idx3 = idx2 to idx1
     C                   EVALR     wk_Lot# =
     C                               %trimr(%subst(ary_Lots(idx3):27:15))
     C                   IF        %lookup(wk_Lot#:ary_Lots2:1:idx4) > *Zero
     C                   ITER
     C                   ENDIF                                                  %lookup(wk_Lot#:ary_
     C                   EVAL      idx4 = idx4 + 1
     C                   EVAL      ary_Lots2(idx4) = wk_Lot#
     C                   EXSR      SRLOOP
     C                   EVALR     SQLOTN = wk_Lot#
     C                   EVAL      SQLOTN2 = %triml(SQLOTN)
     C                   WRITE     PKLL1
     C****               WRITE     XPKLL1
     C                   WRITE     XPKLL2
     C                   EVAL      LINCNT = LINCNT + 1
     C                   ENDFOR                                                 idx3=idx2 to idx1

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Write packing list trailer lines.                                *
     C********************************************************************
     C     $WriteTlrLinesBEGSR
     C*
     C                   IF        wk_CustomPL
     C                   LEAVESR
     C                   ENDIF

     C                   Z-ADD     999           LINE
     C                   CLEAR                   BLN#                           Print Trailer
     C                   EXSR      $PrintComment1                               Comments
     C*
     C***  SDDSHC        IFNE      'Y'                                          Not Drop Ship
     C                   IF        SDDSHP = *Zero                               Not Drop Ship
     C                   Z-ADD     999           LINE
     C                   CLEAR                   BLN#                           Print Trailer
     C                   EXSR      $PrintComment2                               Comments
     C                   ELSE
     C                   IF        wk_ITARTTB
     C                   Z-ADD     999           LINE
     C                   CLEAR                   BLN#                           Print Trailer
     C                   EXSR      $PrtExportCmt
     C                   ENDIF                                                  wk_ITARTTB
     C                   ENDIF
     C*
     C* Write total line.
     C*
     C                   ADD       3             LINCNT
     C                   EXSR      SRLOOP
     C                   WRITE     PKLT1
     C                   WRITE     XPKLT1
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* PRINT COMMENTS
     C********************************************************************
     C     $PrintComment1BEGSR
     C*
     C                   CLEAR                   HITCMT
     C                   CLEAR                   HT
     C     LINE          IFEQ      *ZEROS
     C                   MOVE      'H'           HT
     C                   ELSE
     C     LINE          IFEQ      999
     C                   MOVE      'T'           HT
     C                   ENDIF
     C                   ENDIF
     C*
     C     KOCMT         SETLL     OEOCMTR
     C     *IN99         DOUEQ     *ON
     C     KOCMT         READE     OEOCMTR                                99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   MOVE      'Y'           HITCMT            1
     C                   EXSR      SRLOOP
     C                   MOVEL     OCCMNT        CMT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO
     C*
     C     HITCMT        CABEQ     'Y'           SRCMTE
     C*
     C     HT            IFEQ      *BLANKS
     C     BLN#          IFEQ      *ZEROS
     C     KVCIT         CHAIN     HDVCITR                            73
     C     *IN73         IFEQ      *OFF
     C     KVCIC         SETLL     HDVCICR
     C     *IN99         DOUEQ     *ON
     C     KVCIC         READE     HDVCICR                                99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   MOVE      'Y'           HITCMT
     C                   EXSR      SRLOOP
     C                   MOVEL     ICCMNT        CMT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO
     C                   ENDIF
     C*
     C     HITCMT        IFEQ      *BLANKS
     C     KIMXD         SETLL     HDIMXDR
     C     *IN99         DOUEQ     *ON
     C     KIMXD         READE     HDIMXDR                                99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      SRLOOP
     C                   MOVEL     IXDESC        CMT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     SRCMTE        ENDSR
     C********************************************************************
     C* PRINT COMMENTS
     C********************************************************************
     C     $PrintComment2BEGSR
     C*
     C                   IF        wk_ITARTTB  and
     C                              (LINE = *Zero  or  LINE = 999)
     C                   EXSR      $PrtExportCmt
     C                   ENDIF                                                  wk_ITARTTB and
     C*
     C                   CLEAR                   HT
     C     LINE          IFEQ      *ZEROS
     C                   MOVE      'H'           HT
     C                   ELSE
     C     LINE          IFEQ      999
     C                   MOVE      'T'           HT
     C                   ENDIF
     C                   ENDIF
     C     KCCMT         SETLL     HDCCMTR
     C     *IN99         DOUEQ     *ON
     C     KCCMT         READE     HDCCMTR                                99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   EXSR      SRLOOP
     C                   MOVEL     CXCMNT        CMT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO
     C*
     C     SRCHTE        ENDSR
     C/EJECT
     C********************************************************************
     C* PRINT COMMENTS - xWRNTY LINES
     C********************************************************************
     C     $PrintComment3BEGSR
     C*
     C     KOCMT         SETLL     OEOCMTR
     C     *IN99         DOUEQ     *ON
     C     KOCMT         READE     OEOCMTR                                99
     C     *IN99         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C     StartedList   IFEQ      No
     C                   EVAL      sv_LINE = LINE
     C                   EVAL      sv_BLN# = BLN#
     C                   EVAL      PLORD# = work_ORD#
     C                   EVAL      PLORRF = OEORRF
     C                   EXSR      $StartList
     C                   EXSR      $FirstDetail
     C                   EVAL      *IN99 = *OFF
     C                   EVAL      LINE = sv_LINE
     C                   EVAL      BLN# = sv_BLN#
     C     KOCMT         SETLL     OEOCMTR
     C                   MOVEL     No            FirstDetail
     C                   MOVEL     Yes           StartedList
     C                   ITER
     C                   ENDIF                                                  StartedList=No
     C                   EXSR      SRLOOP
     C                   MOVEL     OCCMNT        CMT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Print foreign shipment comments.
     C********************************************************************
     C     $PrtExportCmt BEGSR
     C*
      /Free
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEORDP a join OEORDT b on a.IDORD#=b.ODORD# and
                 a.IDORL#=b.ODORL# and a.IDBLN#=b.ODBLN#
                 join HDIMST c on b.ODITEM=c.IMITEM
                 where a.IDTURN=:work_Turn and a.IDQSTC>0 and
                  ((substr(c.IMPCLS,1,1)='A' and c.IMPCLS<>'ATAC') or
                   substr(c.IMPCLS,1,1)='B' or
                   substr(c.IMPCLS,1,1)='C' or
                   (substr(c.IMPCLS,1,1)='P' and
                    substr(c.IMPCLS,1,2) not in ('PX','PY')));
       IF wk_RcdCnt = *Zero;
         wk_NeedITAR = *OFF;
       ELSE;
         wk_NeedITAR = *ON;
       ENDIF;                                                                // wk_RcdCnt=*Zero

       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from OEORDP a join OEORDT b on a.IDORD#=b.ODORD# and
                 a.IDORL#=b.ODORL# and a.IDBLN#=b.ODBLN#
                 join HDIMST c on b.ODITEM=c.IMITEM
                 join GUPTDAT d on d.TDTABL='PRDCLSGRP' and
                 d.TDKEY1='AMMO' and c.IMPCLS=d.TDKEY2 and
                 d.TDFF04='Y'
                 where a.IDTURN=:work_Turn and a.IDQSTC>0;
       IF wk_RcdCnt = *Zero;
         wk_NeedTTB = *OFF;
       ELSE;
         wk_NeedTTB = *ON;
       ENDIF;                                                                // wk_RcdCnt=*Zero
      /End-Free

     C                   IF        not wk_NeedITAR  and  not  wk_NeedTTB
     C                   LEAVESR
     C                   ENDIF                                                 wk_NeedITAR and

      /Free
       wk_FrgnTxtCust = *OFF;
       wk_FrgnTxtCtry = *OFF;
       wk_RcdCnt = *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPEXTX where ETCUST=:OESHTO;
       IF wk_RcdCnt <> *Zero;
         wk_FrgnTxtCust = *ON;
       ELSE;
         wk_CMCTRY = *Blanks;
         EXEC SQL select CMCTRY into :wk_CMCTRY
                  from HDCUST
                  where CMCUST=:OESHTO;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX where ETCTRY=:wk_CMCTRY;
         IF wk_RcdCnt <> *Zero;
           wk_FrgnTxtCtry = *ON;
         ENDIF;                                                              // wk_RcdCnt<>*Zero
       ENDIF;                                                                // wk_RcdCnt<>*Zero

       wk_RcdCnt = *Zero;
       SELECT;
       WHEN wk_FrgnTxtCust;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX
                  where ETCUST=:OESHTO and ETPCLS=' ' and ETDOCT='PKL';
       WHEN wk_FrgnTxtCtry;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX
                  where ETCTRY=:wk_CMCTRY and ETPCLS=' ' and ETDOCT='PKL';
       OTHER;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX
                  where ETCTRY='DFT' and ETPCLS=' ' and ETDOCT='PKL';
       ENDSL;
       IF wk_RcdCnt<> *Zero;
         wk_DOCTITAR = 'PKL';
       ELSE;
         wk_DOCTITAR = *Blanks;
       ENDIF;                                                                // wk_RcdCnt<>*Zero

       wk_RcdCnt = *Zero;
       SELECT;
       WHEN wk_FrgnTxtCust;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX
                  where ETCUST=:OESHTO and ETPCLS='*FET' and ETDOCT='PKL';
       WHEN wk_FrgnTxtCtry;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX
                  where ETCTRY=:wk_CMCTRY and ETPCLS='*FET' and ETDOCT='PKL';
       OTHER;
         EXEC SQL select count(*) into :wk_RcdCnt
                  from HYPEXTX
                  where ETCTRY='DFT' and ETPCLS='*FET' and ETDOCT='PKL';
       ENDSL;
       IF wk_RcdCnt<> *Zero;
         wk_DOCTTTB = 'PKL';
       ELSE;
         wk_DOCTTTB = *Blanks;
       ENDIF;                                                                // wk_RcdCnt<>*Zero
      /End-Free

     C                   SELECT
     C                   WHEN      wk_FrgnTxtCust
     C                   EVAL      kf_CTRY = *Blanks
     C                   EVAL      kf_CUST = OESHTO
     C                   WHEN      wk_FrgnTxtCtry
     C                   EVAL      kf_CTRY = wk_CMCTRY
     C                   EVAL      kf_CUST = *Zero
     C                   OTHER
     C                   EVAL      kf_CTRY = 'DFT'
     C                   EVAL      kf_CUST = *Zero
     C                   ENDSL

     C                   SELECT
     C                   WHEN      LINE = *Zero
     C                   EVAL      kf_HT = 'H'
     C                   WHEN      LINE = 999
     C                   EVAL      kf_HT = 'T'
     C                   OTHER
     C                   EVAL      kf_HT = *Blank
     C                   ENDSL

      /Free
       IF wk_NeedITAR;
         wk_RcdCnt = *Zero;
         SELECT;
         WHEN wk_FrgnTxtCust;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPEXTX where ETCUST=:OESHTO and ETHT=:kf_HT;
         WHEN wk_FrgnTxtCtry;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPEXTX where ETCTRY=:wk_CMCTRY and ETHT=:kf_HT;
         OTHER;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPEXTX where ETCTRY='DFT' and ETHT=:kf_HT;
         ENDSL;
         IF wk_RcdCnt = *Zero;
           EVAL wk_NeedITAR = *OFF;
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ENDIF;                                                                // wk_NeedITAR

       IF wk_NeedTTB;
         wk_RcdCnt = *Zero;
         SELECT;
         WHEN wk_FrgnTxtCust;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPEXTX
                    where ETCUST=:OESHTO and ETPCLS='*FET' and ETHT=:kf_HT;

         WHEN wk_FrgnTxtCtry;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPEXTX
                    where ETCTRY=:wk_CMCTRY and ETPCLS='*FET' and ETHT=:kf_HT;
         OTHER;
           EXEC SQL select count(*) into :wk_RcdCnt
                    from HYPEXTX
                    where ETCTRY='DFT' and ETPCLS='*FET' and ETHT=:kf_HT;
         ENDSL;
         IF wk_RcdCnt = *Zero;
           EVAL wk_NeedTTB = *OFF;
         ENDIF;                                                              // wk_RcdCnt=*Zero
       ENDIF;                                                                // wk_NeedTTB
      /End-Free

     C                   IF        wk_NeedITAR
     C                   EVAL      wk_BlnkCmtPrtd = *OFF
     C                   EVAL      kf_PCLS = *Blanks
     C                   EVAL      kf_DOCT = wk_DOCTITAR
     C                   IF        not wk_FrgnTxtCust
     C                   IF        not %open(HYPEXTX)
     C                   OPEN      HYPEXTX
     C                   ENDIF                                                  not %open(HYPEXTX)
     C     key_ET1       SETLL     HYPEXTX
     C                   DOU       %eof(HYPEXTX)
     C     key_ET1       READE     HYPEXTX
     C                   IF        %eof(HYPEXTX)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPEXTX)
     C                   IF        not wk_BlnkCmtPrtd
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   EVAL      wk_BlnkCmtPrtd = *ON
     C                   ENDIF                                                  not wk_BlnkCmtPrtd
     C                   EXSR      SRLOOP
     C                   EVAL      CMT = ETTEXT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO                                                  %eof(HYPEXTX)
     C                   ELSE
     C                   IF        not %open(HYLEXTX1)
     C                   OPEN      HYLEXTX1
     C                   ENDIF                                                  not %open(HYLEXTX1)
     C     key_ET2       SETLL     HYLEXTX1
     C                   DOU       %eof(HYLEXTX1)
     C     key_ET2       READE     HYLEXTX1
     C                   IF        %eof(HYLEXTX1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLEXTX1)
     C                   IF        not wk_BlnkCmtPrtd
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   EVAL      wk_BlnkCmtPrtd = *ON
     C                   ENDIF                                                  not wk_BlnkCmtPrtd
     C                   EXSR      SRLOOP
     C                   EVAL      CMT = ETTEXT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO                                                  %eof(HYLEXTX1)
     C                   ENDIF                                                  not wk_FrgnTxtCust
     C                   ENDIF                                                  wk_NeedITAR

     C                   IF        wk_NeedTTB
     C                   EVAL      wk_BlnkCmtPrtd = *OFF
     C                   EVAL      kf_PCLS = '*FET'
     C                   EVAL      kf_DOCT = wk_DOCTTTB
     C                   IF        not wk_FrgnTxtCust
     C                   IF        not %open(HYPEXTX)
     C                   OPEN      HYPEXTX
     C                   ENDIF                                                  not %open(HYPEXTX)
     C     key_ET1       SETLL     HYPEXTX
     C                   DOU       %eof(HYPEXTX)
     C     key_ET1       READE     HYPEXTX
     C                   IF        %eof(HYPEXTX)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPEXTX)
     C                   IF        not wk_BlnkCmtPrtd
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   EVAL      wk_BlnkCmtPrtd = *ON
     C                   ENDIF                                                  not wk_BlnkCmtPrtd
     C                   EXSR      SRLOOP
     C                   EVAL      CMT = ETTEXT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO                                                  %eof(HYPEXTX)
     C                   ELSE
     C                   IF        not %open(HYLEXTX1)
     C                   OPEN      HYLEXTX1
     C                   ENDIF                                                  not %open(HYLEXTX1)
     C     key_ET2       SETLL     HYLEXTX1
     C                   DOU       %eof(HYLEXTX1)
     C     key_ET2       READE     HYLEXTX1
     C                   IF        %eof(HYLEXTX1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLEXTX1)
     C                   IF        not wk_BlnkCmtPrtd
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   EVAL      wk_BlnkCmtPrtd = *ON
     C                   ENDIF                                                  not wk_BlnkCmtPrtd
     C                   EXSR      SRLOOP
     C                   EVAL      CMT = ETTEXT
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDDO                                                  %eof(HYLEXTX1)
     C                   ENDIF                                                  not wk_FrgnTxtCust
     C                   ENDIF                                                  wk_NeedTTB

     C                   IF        wk_NeedITAR  or  wk_NeedTTB
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_NeedITAR or
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Print vendor-direct shipment ship-from information.
     C********************************************************************
     C     $PrintVDSInfo BEGSR

      /Free
       wk_VMVNA1 = *Blanks;
       wk_VMVNA2 = *Blanks;
       wk_VMVNA3 = *Blanks;
       wk_VMVNA4 = *Blanks;
       wk_VMVCTY = *Blanks;
       wk_VMST = *Blanks;
       wk_VMZIP = *Blanks;
       wk_VMCTRY = *Blanks;
       EXEC SQL select VMVNA1, VMVNA2, VMVNA3, VMVNA4,
                       VMVCTY, VMST, VMZIP, VMCTRY
                 into :wk_VMVNA1, :wk_VMVNA2, :wk_VMVNA3, :wk_VMVNA4,
                      :wk_VMVCTY, :wk_VMST, :wk_VMZIP, :wk_VMCTRY
                from HDVEND
                where VMVEND=:wk_VDVEND;
       wk_VMCDES = *Blanks;
       IF wk_VMCTRY <> 'US'  and  wk_VMCTRY <> *Blanks;
         EXEC SQL select CNCDES
                   into: wk_VMCDES
                  from HDCTRY
                  where CNCTCD=:wk_VMCTRY;
         IF wk_VMCDES = *Blanks;
           wk_VMCDES = wk_VMCTRY;
         ENDIF;                                                              // wk_VMCDES=*Blanks
       ENDIF;                                                                // wk_VMCTRY<>'US' and
      /End-Free

     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = 'VENDOR-DIRECT SHIPMENT SHIPPED FROM:'
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   EVAL      CMT = '  ' + wk_VMVNA1 + '  #' +
     C                               %triml(%editc(wk_VDVEND:'Z'))
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       3             LINCNT
     C                   IF        wk_VMVNA2 <> *Blanks
     C                   EVAL      CMT = '  ' + wk_VMVNA2
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_VMVNA2<>*Blanks
     C                   IF        wk_VMVNA3 <> *Blanks
     C                   EVAL      CMT = '  ' + wk_VMVNA3
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_VMVNA3<>*Blanks
     C                   IF        wk_VMVNA4 <> *Blanks
     C                   EVAL      CMT = '  ' + wk_VMVNA4
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_VMVNA4<>*Blanks
     C                   IF        wk_VMVCTY <> *Blanks  or
     C                               wk_VMST <> *Blanks  or
     C                               wk_VMZIP <> *Blanks
     C                   EVAL      CMT = '  ' + wk_VMVCTY
     C                   IF        wk_VMST <> *Blanks
     C                   EVAL      CMT = %trimr(CMT) + ',' + wk_VMST
     C                   ENDIF                                                  wk_VMST<>*Blanks
     C                   EVAL      CMT = %trimr(CMT) + '  ' + wk_VMZIP
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_VMVCTY<>*Blanks
     C                   IF        wk_VMCDES <> *Blanks
     C                   EVAL      CMT = '  ' + wk_VMCDES
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C                   ENDIF                                                  wk_VMCDES<>*Blanks
     C                   EVAL      CMT = *Blanks
     C                   WRITE     PKLB2
     C                   WRITE     XPKLB2
     C                   ADD       1             LINCNT
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* PRINT CONTINUE MESSAGE
     C********************************************************************
     C     SRLOOP        BEGSR
     C     LINCNT        IFGE      38
     C                   WRITE     PKLC1
     C                   WRITE     PKLH1
     C                   WRITE     XPKLC1
     C                   WRITE     XPKLH1
     C                   CLEAR                   LINCNT
     C                   ENDIF
     C                   ENDSR
     C/SPACE 2
     C********************************************************************
     C* PRINT CONTINUE MESSAGE - Mil Spec lot number print.
     C********************************************************************
     C     SRLOOP2       BEGSR
     C     LINCNT        IFGE      34
     C                   WRITE     PKLC1
     C                   WRITE     PKLH1
     C                   WRITE     XPKLC1
     C                   WRITE     XPKLH1
     C                   CLEAR                   LINCNT
     C                   ENDIF
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Write a record to the Packing List Header Master file.           *
     C********************************************************************
     C     $WriteToHeaderBEGSR
     C*
     C                   WRITE     OEPKLHR
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Write a record to the Packing List Detail Master file.           *
     C********************************************************************
     C     $WriteToDetailBEGSR
     C*
     C                   CLEAR                   OEPKLDR
     C                   MOVE      ListNumber    PLPKL#
     C                   MOVE      ODORD#        PLORD#
     C                   MOVE      work_Turn     PLTURN
     C                   MOVE      ODORL#        PLORL#
     C                   MOVE      ODBLN#        PLBLN#
     C                   MOVE      ODWH          PLWHS
     C                   MOVEL     ODITEM        PLITEM
     C                   MOVEL     ODIMDS        PLIMDS
     C                   MOVEL     ODSTUM        PLSTUM
     C***                Z-ADD     ODQSTC        PLQTYS
     C                   Z-ADD     wk_QSTC       PLQTYS
     C                   MOVEL     ODBOAL        PLBOA
     C                   MOVEL     OEORRF        PLORRF
      *
      * Retrieve item master record to populate some of the detail
      * record's fields.
      * If bulk product, use IMUDN4 to calculate weight.  Otherwise,
      * use IMIMWG.
      *
     C     ODITEM        CHAIN     HDIMST                             95
     C     *IN95         IFEQ      *OFF
     C                   MOVEL     @BLKPC        @TABL
     C                   MOVEL(P)  IMPCLS        @KEY1
     C     TDKEY         CHAIN     GUPTDAT                            95
     C     *IN95         IFEQ      *OFF
     C***  IMUDN4        MULT(H)   ODQSTC        PLWGT
     C***  IMUDN4        MULT(H)   ODQSTC        PLWGTX
     C     IMUDN4        MULT(H)   wk_QSTC       PLWGT
     C     IMUDN4        MULT(H)   wk_QSTC       PLWGTX
     C     IMUDN4        MULT(H)   wk_QSTC       PLWGTXX
     C                   ELSE
     C***  IMIMWG        MULT(H)   ODQSTC        PLWGT
     C***  IMIMWG        MULT(H)   ODQSTC        PLWGTX
     C     IMIMWG        MULT(H)   wk_QSTC       PLWGT
     C     IMIMWG        MULT(H)   wk_QSTC       PLWGTX
     C     IMIMWG        MULT(H)   wk_QSTC       PLWGTXX
     C                   ENDIF                                                  *IN95=*OFF
     C                   MOVEL     IMUOMW        PLWTUM
     C                   MOVEL     IMHAZM        PLHAZM
     C                   ELSE
     C                   CLEAR                   PLWGT
     C                   CLEAR                   PLWGTX
     C                   CLEAR                   PLWGTXX
     C                   CLEAR                   PLWTUM
     C                   CLEAR                   PLHAZM
     C                   CLEAR                   IMUPC
     C                   EVAL      IMPCLS = ODPCLS
     C                   ENDIF                                                  *IN95=*OFF
      *
      * Retrieve item warehouse record to populate some of the detail
      * record's fields.
      *
     C     IWKEY         CHAIN     HDIWHS                             95
     C                   IF        *IN95 = *OFF
     C                   EVAL      PLFTCL = IWFTCL
     C                   ELSE
     C                   EVAL      PLFTCL = *Blanks
     C                   ENDIF
     C*
     C                   WRITE     OEPKLDR
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Print Custom Pack List.                                          *
     C********************************************************************
     C     $PrtCustomPL  BEGSR

      *
      *  Get info for custom packing list override.
      *
     C                   MOVE      APID          JAPID
     C                   MOVE      USER          JUSER
     C                   MOVE      wk_PrtOvrWS   JWS
     C                   MOVEL     'HOEPKP'      JPGID
     C**                 MOVEL     'HOEPKPPF2'   JPRTF
     C                   MOVEL     *Blanks       JPRTF
     C                   CALLB     'HSYSBM'      HSYSBM

     C                   EVAL      wk_Splf = 'HOEPKPPC  '
     C                   EVAL      wk_OutQ = JOUTQ

      *
      *  Retrieve form type from the override command.
      *
     C                   EVAL      p1 = %scan('FORMTYPE(':JOVR1)
     C                   IF        p1 > 0
     C                   EVAL      p2 = %scan(')':JOVR1:p1)
     C                   EVAL      wk_Form = %subst(JOVR1:p1+9:p2-(p1+9))
     C                   ELSE
     C                   EVAL      wk_Form = 'SSTD'
     C                   ENDIF

      *
      *  Retrieve save from the override command.
      *
     C                   EVAL      p1 = %scan('SAVE(':JOVR1)
     C                   IF        p1 > 0
     C                   EVAL      p2 = %scan(')':JOVR1:p1)
     C                   EVAL      wk_Save = %subst(JOVR1:p1+5:p2-(p1+5))
     C                   ELSE
     C                   EVAL      wk_Save = '*YES'
     C                   ENDIF

      *
      *  Retrieve hold from the override command.
      *
     C                   EVAL      p1 = %scan('HOLD(':JOVR1)
     C                   IF        p1 > 0
     C                   EVAL      p2 = %scan(')':JOVR1:p1)
     C                   EVAL      wk_Hold = %subst(JOVR1:p1+5:p2-(p1+5))
     C                   ELSE
     C                   EVAL      wk_Hold = '*NO '
     C                   ENDIF

      *
      *  Call the custom pack list print program.
      *
     C                   CALL      'HYC0114P'
     C                   PARM                    CPPKNM
     C                   PARM                    ListNumber
     C                   PARM                    wk_Splf
     C                   PARM                    wk_OutQ
     C                   PARM                    wk_Form
     C                   PARM                    wk_UserData
     C                   PARM                    wk_Save
     C                   PARM                    wk_Hold

     C                   ENDSR
     C********************************************************************
     C* Initialization.                                                  *
     C********************************************************************
     C     *INZSR        BEGSR
     C* Entry Section ****************************************************
     C     *ENTRY        PLIST
     C                   PARM                    APID              2
     C     work_ORD#     PARM                    entparm_ORD#
     C     work_Turn     PARM                    entparm_Turn
     C     work_PrtBO    PARM                    entparm_PrtBO     1
     C     work_PDFName  PARM                    entparm_PDFNam   60
     C                   PARM      ListNumber    entparm_Pkl#      8 0
     C* PLIST Section ****************************************************
     C     CSYRLO        PLIST
     C                   PARM                    RLFILE           10
     C                   PARM                    RLCNT             1 0
     C     HSYSCC        PLIST                                                  Security Check
     C                   PARM                    APID              2
     C                   PARM                    CHECK            10
     C     HSYAPP        PLIST
     C                   PARM                    APLID             2
     C                   PARM                    RELVER            5 1
     C                   PARM                    LIBLEV            3 0
     C     HSYUSN        PLIST
     C                   PARM                    USNDS                          External PLIST
     C     HSYSBM        PLIST
     C                   PARM                    JAPID             2
     C                   PARM                    JUSER            10
     C                   PARM                    JWS              10
     C                   PARM                    JPGID            10
     C                   PARM                    JPRTF            10
     C                   PARM                    JPRTFT           10
     C                   PARM                    JJNAM            10
     C                   PARM                    JJOBD            10
     C                   PARM                    JJOBQ            10
     C                   PARM                    JOUTQ            10
     C                   PARM                    JOVR1
     C                   PARM                    JOVR2
     C     PLSTOV        PLIST
     C                   PARM                    OVRS
     C                   PARM      500           LEN              15 5
     C***  PFMDY         PLIST                                                  FROM YMD
     C***                PARM                    DHLD              7
     C***                PARM      '*MDY'        DFFFMT           10
     C***                PARM      '*SYSVAL'     DFTFMT           10
     C* KLIST Section ****************************************************
     C     KOCMT         KLIST
     C                   KFLD                    work_ORD#
     C                   KFLD                    LINE
     C                   KFLD                    BLN#
     C                   KFLD                    DOCT
     C     KCCMT         KLIST
     C                   KFLD                    VCF               1
     C                   KFLD                    SDSHTO
     C                   KFLD                    DOCT
     C                   KFLD                    HT
     C     KVCIT         KLIST
     C                   KFLD                    VCF
     C                   KFLD                    SDSHTO
     C                   KFLD                    PLITEM
     C     KVCIC         KLIST
     C                   KFLD                    VCID
     C                   KFLD                    DOCT
     C     KIMXD         KLIST
     C                   KFLD                    PLITEM
     C                   KFLD                    DOCT
     C     KPKLD         KLIST
     C                   KFLD                    PLPKL#
     C                   KFLD                    PLORD#
     C                   KFLD                    PLORL#
     C                   KFLD                    PLBLN#
     C     KSHQT         KLIST
     C                   KFLD                    work_Turn
     C                   KFLD                    PLORD#
     C                   KFLD                    PLORL#
     C                   KFLD                    PLBLN#
     C                   KFLD                    PLITEM
     C     KSSCC         KLIST                                                  HYLSSCC2
     C                   KFLD                    PLORD#
     C                   KFLD                    PLORL#
     C                   KFLD                    PLBLN#
     C     KSCWK         KLIST                                                  HYLSCWK2
     C                   KFLD                    PLORD#
     C                   KFLD                    PLORL#
     C                   KFLD                    PLBLN#
     C     IWKEY         KLIST                                                  HDIWHS
     C                   KFLD                    ODITEM
     C                   KFLD                    ODWH
     C     CDKEY         KLIST                                                  HDCALD
     C                   KFLD                    wk_CalendarNbr
     C                   KFLD                    date_CYYMMDD
     C     CBKEY         KLIST                                                  BARCUSTL1
     C                   KFLD                    SDBLTO
     C                   KFLD                    PLITEM
     C     TDKEY         KLIST                                                  GUPTDAT
     C                   KFLD                    @TABL
     C                   KFLD                    @KEY1
     C     SLKEY         KLIST                                                  GUPTDAT
     C                   KFLD                    wk_ShipLoc
     C     IDKEY         KLIST                                                  OEORDP
     C                   KFLD                    work_Turn
     C                   KFLD                    ODORD#
     C                   KFLD                    ODORL#
     C                   KFLD                    ODBLN#
     C     key_TR        KLIST                                                  HYPSTRK
     C                   KFLD                    work_ORD#
     C                   KFLD                    work_Turn
     C     key_MD        KLIST                                                  MFD1MD
     C                   KFLD                    kf_MDCMNO
     C                   KFLD                    kf_MDOTYP
     C                   KFLD                    kf_MDORDR
     C     key_ET1       KLIST                                                  HYPEXTX
     C                   KFLD                    kf_CTRY
     C                   KFLD                    kf_CUST
     C                   KFLD                    kf_PCLS
     C                   KFLD                    kf_DOCT
     C                   KFLD                    kf_HT
     C     key_ET2       KLIST                                                  HYLEXTX1
     C                   KFLD                    kf_CUST
     C                   KFLD                    kf_PCLS
     C                   KFLD                    kf_DOCT
     C                   KFLD                    kf_HT
     C* Field Definition Section *****************************************
     C     *LIKE         DEFINE    OEORD#        entparm_ORD#
     C     *LIKE         DEFINE    SQTURN        entparm_Turn
     C     *LIKE         DEFINE    OEORD#        work_ORD#
     C     *LIKE         DEFINE    SQTURN        work_Turn
     C     *LIKE         DEFINE    entparm_PrtBO work_PrtBO
     C     *LIKE         DEFINE    entparm_PDFNamwork_PDFName
     C     *LIKE         DEFINE    SDPKL#        ListNumber
     C     *LIKE         DEFINE    SDSHDT        date_CYYMMDD
     C     *LIKE         DEFINE    SDSTIM        time_HHMMSS
     C     *LIKE         DEFINE    ODWH          save_ODWH
     C***  *LIKE         DEFINE    ODPKL#        save_ODPKL#
     C     *LIKE         DEFINE    IDPKL#        save_IDPKL#
     C     *LIKE         DEFINE    RELVER        HDIVRL
     C     *LIKE         DEFINE    ODORL#        LINE
     C     *LIKE         DEFINE    ODORL#        BLN#
     C     *LIKE         DEFINE    OCDOCT        DOCT
     C     *LIKE         DEFINE    CXHT          HT
     C     *LIKE         DEFINE    TDTABL        @TABL
     C     *LIKE         DEFINE    TDKEY1        @KEY1
     C* Initialization ***************************************************
     C                   CALLB     'HSYSCC'      HSYSCC
     C     CHECK         IFNE      'HDSPAST'
     C                   RETURN
     C                   ENDIF
     C                   MOVE      'PKL'         DOCT                           Packing List
     C                   MOVE      'C'           VCF                            Customer Item
     C*
     C                   IN        wk_DayCutoff4
     C                   EVAL      wk_DayCutoff = (wk_DayCutoff4 * 100) - 1
     C*
     C     1             CHAIN     IVCTRLR                            99         Get IV Control
     C     1             CHAIN(N)  OECTRLR                            9998       Get OE Control
     C                   TIME                    HMSMDY           12 0
     C                   CALLB     'HSYUSN'      HSYUSN                         Rtv Flds SysCtl
     C                   MOVE      'IV'          APLID
     C                   CALLB     'HSYAPP'      HSYAPP                         Rtv Rls Ver
     C                   Z-ADD     RELVER        HDIVRL                         I/V RELEASE VERSION
     C*
     C                   ENDSR
     C/EJECT
