     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 2001')
     H BNDDIR('HYR0142')
      **************************************************************************
      * TITLE: Bill of Lading Print
      *    BY: William Earl (MCA)
      *  DATE: June 2001
      *
      *  REV DATE  BY   CHANGE
      *  --------  ---  -----------------------------------------------------
      *  11/01/01  MCA  Skip addresses with "AKA", "%", and "PO DRAWER" on
      *                 bill of lading.
      *  04/19/04  MCA  Print ship-to customer comments if available, instead
      *                 of bill-to customer comments.
      *  12/20/04  RJS  Add Defined Freight code DF so do not mark BOL Freight
      *                 Charged Terms on printout
      *  06/27/06  MCA  Create second (duplicate) B.O.L., with *SCS attributes
      *                 (and without the overlay), for RJS imaging software.
      *  08/07/06  MCA  Add pallet weight to total weight.
      *  08/06/07  MCA  Print extra carrier data.
      *  10/19/07  MCA  Fix bug - multiple page BOL's did not show tare weight
      *                 on the printed BOL (but did on the BOL generated for
      *                 imaging).
      *  01/11/11  MCA  Skip addresses with "C/O" on bill of lading.
      *  08/19/11  MCA  Master BOL print.
      *  12/13/11  MCA  BOL form redesign.
      *  03/27/12  MCA  Print carton quantity on BOL in 2nd and 4th customer
      *                 order number columns.
      *  01/24/13  MCA  Canadian shipment equivalency certificate verbiage.
      *  05/02/13  MH   Changed to look at shipping location and put the appropriate
      *                 address on the Bill of Lading.
      *  06/06/13  MH   Fixed report from showing Page 1 of 3 when there is only
      *                 2 pages.
      *  06/12/13  MCA  Maintain turnaround number separation.
      *  09/17/13  MCA  Add hazardous material mark field.
      *  01/27/14  MCA  Extended commodity description (added for ticket #937, for
      *                 (but not restricted to) foreign non-Canada).
      *  06/15/15  MCA  Print emergency contact information if required.
      *  06/18/16  MCA  SnapSafe changes.  Allow multiple-BOL (or single-BOL)
      *                 .pdf files to be created instead of the normal
      *                 (printable) spooled file.
      *  07/27/16  MCA  For drop ships, print phone number on BOL if available.
      *  08/25/16  MCA  For residential shipments (non drop ships), print phone
      *                 number on BOL if available.
      *  09/28/16  MCA  For PDF creation, change to always pull "ZPDF" HDS
      *                 environment override (which does not specify overlay
      *                 parameters).  This is because, as of last Friday, 9/23,
      *                 Josh recreated the BOL overlay, and the newly created
      *                 overlay required overlay offset values.  So, all of the
      *                 environment overlays for HOEBOLPF were populated with
      *                 overlay offset values, causing the BOL PDF to not
      *                 generate with the overlay and the data matching.
      *  02/28/17  MCA  Print "LIFTGATE SERVICE REQUIRED" when necessary.
      *  08/16/17  MCA  Remove any leading blanks from address lines.
      *  09/13/17  MCA  Fix "LIFTGATE SERVICE REQUIRED" SQL.  Use HYPSSCC
      *                 instead of OEPKLH/OEPKLD to retrieve items on shipment.
      *  09/27/17  MCA  For drop ships, print email address on BOL if available.
      *  09/27/17  MCA  For residential shipments (non drop ships), print email
      *                 address on BOL if available.
      *  11/14/17  MCA  Print "DELIVER BY xx/xx/xxxx" instruction, if needed.
      *  08/31/18  MH   Print Volume when all pallets have hgt/len/wid values.
      *  11/09/18  MH   Take the Volume off the BOL print.
      *  12/21/18  MCA  Prevent SQL0305 errors when no records found in HYPPLLT
      *                 for the current BOL (because the shipment was not
      *                 palletized) when retrieving wk_VolTot.
      *  01/25/19  MCA  Fix page overflow problem caused by freight classes that
      *                 need a 2nd line in the BOL commodity description.
      *  02/28/19  MCA  Add alternate (10-Char) PRO# for barcode to prevent
      *                 trailing spaces.  Added for FedEx truck (only).
      *  10/06/19  MCA  Remove liftgate service changes made 2/28/2017.
      *  10/12/19  MCA  Re-add liftgate service changes made 2/28/2017, but
      *                 change to only print if order specified by CSR
      *                 to print (even for retail and employee orders).
      *  05/12/20  MCA  Add pallet dimensions to BOL.
      *  10/27/20  MCA  Change pallet dimensions printed on BOL from HxLxW to
      *                 LxWxH.
      *  07/05/21  MCA  Print BLUDA3 in Carrier Information section if non-blank.
      *                 This was needed for new freight class BULTR.
      *  07/28/23  MCA  Add freight charge message.
      *  03/24/25  MCA  Print order-level BOL comments.
      *  03/26/26  MCA  FedEx White Glove service.
      **************************************************************************
      *
     FOEBOLH    IF   E           K DISK
     FHYLBOL1   IF   E           K DISK
     FOEBOLD    IF   E           K DISK
     FHYPBLCD   IF   E           K DISK
     FHDWHSM    IF   E           K DISK
     FHDFTCL    IF   E           K DISK
     FOEORHD    IF   E           K DISK
     FHDCCMT    IF   E           K DISK
     FOEOCMT    IF   E           K DISK
     FHDCTRY    IF   E           K DISK
     FHYPMBLD   IF   E           K DISK    USROPN
     FHYPSLOC   IF   E           K DISK
     FHDCUST    IF   E           K DISK
     FHYLPLLT2  IF   E           K DISK
     FHYPPSCT   IF   E             DISK    USROPN
     FHOEBOLPF  O    E             PRINTER USROPN
     FHOEBOLPF2 O    E             PRINTER USROPN
     F                                     EXTDESC('HOEBOLPF')
     F                                     RENAME(BOLH1:XBOLH1)
     F                                     RENAME(BOLB1:XBOLB1)
     F                                     RENAME(BOLB1X:XBOLB1X)
     F                                     RENAME(BOLB2:XBOLB2)
     F                                     RENAME(BOLB2A:XBOLB2A)
     F                                     RENAME(BOLB2TW:XBOLB2TW)
     F                                     RENAME(BOLB2T:XBOLB2T)
     F                                     RENAME(BOLPD1X:XBOLPD1X)
     F                                     RENAME(BOLPD1:XBOLPD1)

      **************************************************************************
      * Define prototypes.                                                     *
      **************************************************************************
     D QMHSNDM         PR                  EXTPGM('QMHSNDM')                    Send non-pgm message
     D  out_MsgID                     7A   CONST
     D  out_MsgFile                  20A   CONST
     D  out_MsgData                 512A   CONST
     D  out_MsgDtaLen                10I 0 CONST
     D  out_MsgType                  10A   CONST
     D  out_MsgQ                    120A   CONST
     D  out_MsgQCnt                  10I 0 CONST
     D  out_RpyMsgQ                  20A   CONST
     D  in_MsgKey                     4A
     D  io_MsgErrCode                10I 0

      *****************************************************************
      * Program Status Data Structure
      *****************************************************************
     D/COPY QCPYILE,GUIPGMDS

     D                 DS
     D  ADR                    1    132
     D                                     DIM(4)
     D  ADR1                   1     33
     D  ADR2                  34     66
     D  ADR3                  67     99
     D  ADR4                 100    132

     D                 DS
     D work_TotalPage          1      5P 3
     D work_PartlPage          3      5P 3

     D* OVRPRTF data structure *******************************************
     D                 DS
     D  OVRS                   1    500
     D                                     DIM(2)                               OVRPRTF Command
     D  JOVR1                  1    250
     D  JOVR2                251    500
     D  JOVRX                  1    500

     D                 DS
     D  BIFCCQ                 1    140
     D  BIFCCQx                            DIM(10) OVERLAY(BIFCCQ)              Frt cls case qty
     D   BIFCCQ_FTCL                       LIKE(FTCLAS) OVERLAY(BIFCCQx:1)
     D   BIFCCQ_CSQT                  5P 0 OVERLAY(BIFCCQx:6)
     D   BIFCCQ_WGT                   7P 0 OVERLAY(BIFCCQx:9)
     D   BIFCCQ_PLLT                  3P 0 OVERLAY(BIFCCQx:13)

     D                 DS
     D ds_CYMD                 2      8  0
     D ds_MDYY                 5     12  0
     D ds_CYY                  2      4  0
     D ds_YYYY2                9     12  0

     D PDim            S             10A   DIM(42)
     D wk_PDimMax      S              3P 0 INZ(%elem(PDim))

     D entry_BOL#      S                   LIKE(SDBOL#)
     D entry_PDFName   S             60A
     D work_BOL#       S                   LIKE(SDBOL#)
     D work_PDFName    S             60A
     D wk_DatabaseID2  S              2A                                        Database ID
     D wk_BillTo#X     S              7A
     D wk_DBDate       S              7S 0
     D wk_DBMDYY       S              8S 0
     D wk_DlvrBy       S             30A
     D work_OrderCnt   S              5P 0
     D work_FrtClsCnt  S              5P 0
     D work_TotalOrdr  S              5P 0
     D work_TotalFtCl  S              5P 0
     D work_TotalPlts  S              5P 0
     D work_COIPlts    S                   LIKE(R@TPLT)
     D work_COIWgt     S                   LIKE(R@TWGT)
     D work_CIPlts     S                   LIKE(R@TPLT)
     D work_CIPkgs     S                   LIKE(R@TPKG)
     D work_CIWgt      S                   LIKE(R@TWGT)
     D work_PalletWgt  S              3P 0 INZ(40)
     D work_Prepaid    S              1A
     D work_Collect    S              1A
     D work_VCF        S                   LIKE(CXVCF) INZ('C')
     D work_DOCT       S                   LIKE(CXDOCT) INZ('BOL')
     D work_RptType    S              1A
     D wk_BTCls        S              2A
     D wk_STPhone      S             11P 0
     D wk_STEmail      S             64A
     D wk_DSPhone      S             11P 0
     D wk_DSEmail      S             64A
     D wk_BIORD#       S              8S 0
     D wk_OrdCmtCnt    S              5S 0
     D wk_LGCnt        S              7P 0
     D wk_PROBCLen     S             15P 4
     D wk_PrtCtnQty    S              1N
     D wk_MaxOrderCnt  S              3P 0
     D wk_RcdCnt       S              7P 0
     D AllOrdersDone   S              1A
     D AllFrtClsDone   S              1A
     D AllExtCarDta    S              1A
     D AllPlltDimDone  S              1A
     D MasterWhse      S                   LIKE(WHWHS) INZ(8)
     D idx             S              3P 0
     D idx2            S              3P 0
     D idx3            S              3P 0
     D idx4            S              3P 0
     D idx4a           S              3P 0
     D ary_ORRF        S                   LIKE(OEORRF) DIM(MaxOrderCnt)
     D ary_CTNQ        S              5P 0 DIM(MaxOrderCnt2)
     D ary_OCMT        S                   LIKE(OCCMNT) DIM(5)
     D wk_COLines      S              3P 0
     D JAPID           S              2A
     D JUSER           S             10A
     D JWS             S             10A
     D JPGID           S             10A
     D JPRTF           S             10A
     D JPRTFT          S             10A
     D wk_ShipLoc      S              4A
     D wk_SLocFound    S               N   INZ(*OFF)
     D wk_CSZ          S             38A
     D wk_PlltDim      S             10A
     D wk_XTotalPage   S              9A
     D wk_EmergMsg     S             59A   INZ('IN CASE OF EMERGENCY - +
     D                                     CHEMTREC: 800-424-9300 ACCT #10740')
     D wk_PDFClose     S              1N   INZ(*OFF)
     D wk_Pos          S              3P 0
     D wk_OvrCmd       S            512A
     D wk_Ptr1         S              3P 0
     D wk_Ptr2         S              3P 0
     D @TABL           S             10A
     D @KEY1           S             15A
     D wk_Count        S              3P 0
     D wk_Vol          S              6P 2
     D wk_VolTot       S             15P 2
     D wk_MsgID        S              7A
     D wk_MsgData      S            512A
     D wk_MsgDataLen   S              3P 0
     D in_MsgKey       S              4A
     D io_MsgErrCode   S             10I 0

     D Yes             C                   'Y'
     D No              C                   'N'
     D MaxOrderCnt     C                   40
     D MaxOrderCnt2    C                   20
     D MaxOrderLines   C                   10
     D MaxFrtClsCnt    C                   10
     D UnitedStates    C                   'US'
     D FOBDockX        C                   'DK'
     D FOBDestination  C                   'DE'
     D FOBMark         C                   'X'
     D FrtPrepaid1     C                   'PPD'                                Prepaid
     D FrtPrepaid2     C                   'PP'                                 Prepaid
     D FrtPrepaid3     C                   'PC'                                 Prepaid chargeback
     D FrtDefined      C                   'DF'                                 Freight Defined
     D FreightChgMark  C                   'X'
     D at_Attention    C                   CONST('A')                           Addr type: Attention
     D at_POBox        C                   CONST('P')                           Addr type: P.O. Box
     D at_Spacer       C                   CONST('S')                           Addr type: Spacer
     D at_AKA          C                   CONST('K')
     D at_CareOf       C                   CONST('C')
     D ValidBoxChar    C                   CONST(' -0123456789')                Valid P.O. Box Chars
     D TareWgtDsc      C                   'TARE WEIGHT'
     D Print           C                   'P'
     D Imaging         C                   'I'
     D GI              C                   'GI'

      **************************************************************************
      * Define data areas.                                                     *
      **************************************************************************
     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_MsgPP1       S             52A   DTAARA(HYAMSGPP1)                    Frt prepaid msg 1
     D wk_MsgPP2       S             52A   DTAARA(HYAMSGPP2)                    Frt prepaid msg 2
     D wk_MsgPC1       S             52A   DTAARA(HYAMSGPC1)                    Frt collect msg 1
     D wk_MsgPC2       S             52A   DTAARA(HYAMSGPC2)                    Frt collect msg 2

     D********************************************************************
     D* Define procedure prototypes.                                     *
     D********************************************************************
     D spcladdress     PR             1A
     D   address                     30A     VALUE

     D QCMDEXC         PR                  EXTPGM('QCMDEXC')                    Exec system command
     D  out_Command                 512A   CONST
     D  out_CmdLen                   15P 5 CONST

     C/EJECT
     C********************************************************************
     C* Main line.                                                       *
     C********************************************************************

     C                   EVAL      work_BOL# = entry_BOL#
     C                   EVAL      work_PDFName = entry_PDFName

     C                   SELECT

     C                   WHEN      work_PDFName = '*CLOSE'
     C                   CLOSE     HOEBOLPF

     C                   OTHER

     C     work_BOL#     CHAIN(E)  OEBOLH
     C                   IF        not %Error  and  %Found

     C                   EVAL      wk_BillTo#X = %editc(SDBLTO:'X')

     C*
     C* Retrieve ship-from information.
     C*
     C                   EXSR      $RtvShipFrom

     C                   IF        work_PDFName <> *Blanks  and                 .pdf file
     C                               work_PDFName <> '*NONE'
     C                   EXSR      $ChkCtlChar
     C                   ENDIF                                                  work_PDFName<>*Blank
     C                   IF        (work_PDFName = *Blanks  or                  Normal spooled file
     C                               work_PDFName = '*NONE')  or
     C                             (work_PDFName <> *Blanks  and                .pdf file
     C                               work_PDFName <> '*NONE' and
     C                               not %open(HOEBOLPF))

      /Free
       IF work_PDFName <> *Blanks and work_PDFName <> '*NONE';
         IF not %open(HYPPSCT);
           OPEN HYPPSCT;
           CHAIN 1 HYPPSCT;
         ENDIF;                                                              // %open(HYPPSCT)
         wk_OvrCmd = 'OVRPRTF FILE(HOEBOLPF) DEVTYPE(*AFPDS) FRONTOVL(' +
           %trim(CSBLOVL) + ' ' + %triml(%editc(CSBLOVLD:'3')) + ' ' +
           %triml(%editc(CSBLOVLA:'3')) + ') TOSTMF(''' + %trim(CSFLR1) +
           %trim(work_PDFName) + ''') WSCST(*PDF) OVRSCOPE(*CALLLVL)';
         CALLP(E) QCMDEXC (wk_OvrCmd:%len(%trimr(wk_OvrCmd)));
         IF %error;
           DUMP(A);
         ENDIF;
       ENDIF;                                                                // work_PDFName<>*Blank
      /End-Free

     C                   EVAL      JAPID = 'OE'
     C                   IF        work_PDFName = *Blanks or
     C                               work_PDFName = '*NONE'
     C                   EVAL      JUSER = P@USER
     C                   EVAL      JWS = P@JOBN
     C                   ELSE
     C                   EVAL      JUSER = 'ZPDF'
     C                   EVAL      JWS = 'ZPDF'
     C                   ENDIF                                                  work_PDFName=*Blanks
     C                   EVAL      JPGID = 'HOEBOL'
     C                   EVAL      JPRTF = 'HOEBOLPF'
     C                   EVAL      JPRTFT = *Blanks
     C                   CALLB     'HSYSBM'      HSYSBM
     C                   IF        work_PDFName <> *Blanks and                  Rmv FRONTOVL from
     C                               work_PDFName <> '*NONE'                     OVRPRTF so it
     C                   EVAL      wk_Ptr1 = %scan(' FRONTOVL(':JOVRX)           doesn't quash
     C                   IF        wk_Ptr1 > *Zero                               PDF OVRPRTF
     C                   EVAL      wk_Ptr2 = %scan(') ':JOVRX:wk_Ptr1)
     C                   EVAL      JOVRX = %subst(JOVRX:1:wk_Ptr1-1) +
     C                               %subst(JOVRX:wk_Ptr2+1)
     C                   ENDIF                                                  wk_Ptr1>*Zero
     C                   ENDIF                                                  work_PDFName<>*Blank
     C                   CALL(E)   'QCMDEXC'     PLSTOV
     C                   OPEN      HOEBOLPF
     C                   ENDIF                                                  work_PDFName=*Blanks

     C                   EVAL      work_RptType = Print
     C                   EVAL      *IN80 = *OFF
     C                   EXSR      $ProcessDtl

     C                   IF        (work_PDFName = *Blanks  or                  Normal spooled file
     C                               work_PDFName = '*NONE')  or
     C                               wk_PDFClose                                .pdf file
     C                   CLOSE     HOEBOLPF
      /Free
       IF work_PDFName <> *Blanks and work_PDFName <> '*NONE';
         wk_OvrCmd = 'DLTOVR FILE(HOEBOLPF) LVL(*)';
         CALLP QCMDEXC (wk_OvrCmd:%len(%trimr(wk_OvrCmd)));
       ENDIF;                                                                // work_PDFName<>*Blank
     C                   EVAL      wk_PDFClose = *OFF
     C                   ENDIF                                                  work_PDFName=*Blanks

     C                   EVAL      JAPID = 'OE'
     C                   EVAL      JUSER = P@USER
     C                   EVAL      JWS = P@JOBN
     C                   EVAL      JPGID = 'HOEBOL2'
     C****               EVAL      JPRTF = 'HOEBOLPF'
     C****               EVAL      JPRTFT = *Blanks
     C                   EVAL      JPRTF = 'HOEBOLPF2'
     C                   EVAL      JPRTFT = 'HOEBOLPF'
     C                   CALLB     'HSYSBM'      HSYSBM
     C                   EVAL      JOVR2 = %trimr(JOVR2) +
     C                               ' SPLFNAME(HOEBOLPF2)'

     C                   CALL(E)   'QCMDEXC'     PLSTOV
     C                   OPEN      HOEBOLPF2
     C                   EVAL      work_RptType = Imaging
     C                   EVAL      *IN80 = *ON
     C                   EXSR      $ProcessDtl
     C                   CLOSE     HOEBOLPF2

     C                   ENDIF                                                  not %Error and %Foun

     C                   ENDSL

     C                   RETURN
     C/EJECT
     C********************************************************************
     C* Process detail.                                                  *
     C********************************************************************
     C     $ProcessDtl   BEGSR

      *
      * Determine whether to print carton counts for each customer order listed
      * on the BOL. Detemine the maximum number of orders shown on each BOL page.
      *
     C                   IF        SDBHF1 = 'M'                                 Master BOL
     C                   EVAL      wk_PrtCtnQty = *OFF
     C                   EVAL      wk_MaxOrderCnt = MaxOrderCnt
     C                   ELSE
     C                   EVAL      wk_PrtCtnQty = *ON
     C                   EVAL      wk_MaxOrderCnt = MaxOrderCnt2
     C                   ENDIF                                                  SDBHF1='M'

      *
      * If printing a master BOL, open detail file.
      *
     C                   IF        SDBHF1 = 'M'                                 Master BOL
     C                   IF        not %open(HYPMBLD)
     C                   OPEN      HYPMBLD
     C                   ENDIF                                                  not %open(HYPMBLD)
     C                   ENDIF                                                  SDBHF1='M'

     C*
     C* Retrieve total page count.
     C*
     C                   EXSR      $RtvPageCount

     C*
     C* Retrieve ship-to information.
     C*
     C                   EXSR      $RtvShipTo

     C*
     C* Determine FOB point.
     C*
     C                   SELECT
     C                   WHEN      SDFOB = FOBDockX
     C                   EVAL      FOBDOCK = FOBMark
     C                   EVAL      FOBDEST = *Blank
     C                   WHEN      SDFOB = FOBDestination
     C                   EVAL      FOBDOCK = *Blank
     C                   EVAL      FOBDEST = FOBMark
     C                   ENDSL

     C*
     C* Determine freight charge terms.
     C*
     C                   IF        work_Prepaid = Yes
     C                   EVAL      FRTPREPAID = FreightChgMark
     C                   EVAL      FRTPREPAI2 = FreightChgMark
     C                   ELSE
     C                   EVAL      FRTPREPAID = *Blanks
     C                   EVAL      FRTPREPAI2 = *Blanks
     C                   ENDIF
     C                   IF        work_Collect = Yes
     C                   EVAL      FRTCOLLECT = FreightChgMark
     C                   EVAL      FRTCOLLEC2 = FreightChgMark
     C                   ELSE
     C                   EVAL      FRTCOLLECT = *Blanks
     C                   EVAL      FRTCOLLEC2 = *Blanks
     C                   ENDIF
     C                   EVAL      FRT3RDPRTY = *Blanks
     C                   EVAL      FRT3RDPRT2 = *Blanks

     C*
     C* Populate any freight charge message.
     C*
     C                   SELECT
     C                   WHEN      work_Prepaid = Yes
     C                   IN        wk_MsgPP1
     C                   IN        wk_MsgPP2
     C                   IF        wk_MsgPP2 = *Blanks
     C                   EVAL      BOLFCMSG1 = *Blanks
     C                   EVAL      BOLFCMSG2 = wk_MsgPP1
     C                   EVAL      BOLFCMSG3 = *Blanks
     C                   ELSE
     C                   EVAL      BOLFCMSG1 = wk_MsgPP1
     C                   EVAL      BOLFCMSG2 = *Blanks
     C                   EVAL      BOLFCMSG3 = wk_MsgPP2
     C                   ENDIF                                                  wk_MsgPP2=*Blanks
     C                   WHEN      work_Collect = Yes
     C                   IN        wk_MsgPC1
     C                   IN        wk_MsgPC2
     C                   IF        wk_MsgPC2 = *Blanks
     C                   EVAL      BOLFCMSG1 = *Blanks
     C                   EVAL      BOLFCMSG2 = wk_MsgPC1
     C                   EVAL      BOLFCMSG3 = *Blanks
     C                   ELSE
     C                   EVAL      BOLFCMSG1 = wk_MsgPC1
     C                   EVAL      BOLFCMSG2 = *Blanks
     C                   EVAL      BOLFCMSG3 = wk_MsgPC2
     C                   ENDIF                                                  wk_MsgPC2=*Blanks
     C                   ENDSL

     C*
     C* Calculate pallet volume.
     C*
     C                   EXSR      $CalcVol

     C*
     C* Retrieve bill of lading comments / special instructions.
     C*
     C                   EXSR      $RtvInstr

     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   MOVE      work_BOL#     BOLBARCD
     C                   EVAL      PROBARCD = SDPRO#
     C                   EVAL      PROBARCD2 = SDPRO#
     C                   WHEN      work_RptType = Imaging
     C                   EVAL      BOLBARCD = *Blanks
     C                   EVAL      PROBARCD = *Blanks
     C                   EVAL      PROBARCD2 = *Blanks
     C                   ENDSL

      /Free
       wk_PROBCLen = *Zero;
       EXEC SQL select TDNF02 into :wk_PROBCLen
                from GUPTDAT
                where TDTABL='SHIPVIA' and TDKEY1=:SDSHPV;
       SELECT;
       WHEN wk_PROBCLen = 10;
         *IN81 = *ON;
       OTHER;
         *IN81 = *OFF;
       ENDSL;
      /End-Free

     C                   EVAL      CURPAGENBR = *Zero
     C                   EVAL      AllOrdersDone = No
     C                   EVAL      AllFrtClsDone = No
     C                   EVAL      AllExtCarDta = No
     C                   EVAL      AllPlltDimDone = No
     C                   EVAL      work_COIPlts = *Zero
     C                   EVAL      work_COIWgt = *Zero
     C                   EVAL      work_CIPlts = *Zero
     C                   EVAL      work_CIPkgs = *Zero
     C                   EVAL      work_CIWgt = *Zero

     C     work_BOL#     SETLL     HYLBOL1
     C     work_BOL#     SETLL     OEBOLD
     C     SDBLTO        SETLL     HYPBLCD
     C                   IF        SDBHF1 = 'M'                                 Master BOL
     C     work_BOL#     SETLL     HYPMBLD
     C                   ENDIF                                                  SDBHF1='M'

     C                   DOU       AllOrdersDone = Yes  and  AllFrtClsDone = Yes
     C                               and  AllExtCarDta = Yes

     C                   EVAL      work_OrderCnt = *Zero
     C                   EVAL      work_FrtClsCnt = *Zero

     C*
     C* Print bill of lading header information.
     C*
     C                   EXSR      $Header

     C*
     C* Process customer order information.
     C*
     C                   IF        AllOrdersDone = No
     C                   EXSR      $CustOrdInfo
     C                   ENDIF                                                  AllOrdersDone=No

     C*
     C* Process carrier information.
     C*
     C                   IF        AllFrtClsDone = No
     C                               or  AllExtCarDta = No
     C                   EXSR      $CarrierInfo
     C                   ENDIF                                                  AllFrtClsDone=No

     C*
     C* Process pallet dimensions.
     C*
     C                   IF        AllPlltDimDone = No
     C                   EXSR      $PlltDim
     C                   ENDIF                                                  AllPlltDimDone=No

     C                   ENDDO                                                  AllOrdersDone=Yes an

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Print bill of lading header information.                         *
     C********************************************************************
     C     $Header       BEGSR

     C                   EVAL      CURPAGENBR = CURPAGENBR + 1

     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLH1
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLH1
     C                   ENDSL

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Process customer order information.                              *
     C********************************************************************
     C     $CustOrdInfo  BEGSR

     C                   EVAL      idx = *Zero
     C                   EVAL      ary_ORRF = *Blanks
     C                   EVAL      ary_CTNQ = *Zero

     C                   IF        SDBHF1 = 'M'                                 Master BOL

     C     work_BOL#     READE     HYPMBLD
     C                   IF        %eof(HYPMBLD)
     C                   EVAL      AllOrdersDone = Yes
     C                   ENDIF
     C                   DOW       not %eof(HYPMBLD)
     C                   EVAL      idx = idx + 1
     C                   EVAL      ary_ORRF(idx) = %triml(%editc(MDSHTO:'Z')) +
     C                               '-' + %triml(%editc(MDUBOL:'Z'))
     C***                WRITE     BOLB1
     C                   EVAL      work_COIPlts = work_COIPlts + MDTPLT
     C                   EVAL      work_COIWgt = work_COIWgt + MDTWGT
     C                   EVAL      work_OrderCnt = work_OrderCnt + 1
     C     work_BOL#     READE(E)  HYPMBLD
     C                   IF        not %Error  and  not %EOF
     C                   IF        work_OrderCnt = wk_MaxOrderCnt
     C     MDKEY         SETLL     HYPMBLD
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   EVAL      R@TPLT = work_COIPlts
     C                   EVAL      R@TWGT = work_COIWgt +
     C                               (work_COIPlts * work_PalletWgt)
     C                   EVAL      AllOrdersDone = Yes
     C                   ENDIF
     C                   ENDDO                                                  not %eof(HYPMBLD)

     C                   ELSE

     C     work_BOL#     READE(E)  HYLBOL1
     C                   IF        %Error  or  %EOF
     C                   EVAL      AllOrdersDone = Yes
     C                   ENDIF
     C                   DOW       not %Error  and  not %EOF
     C     BIORD#        CHAIN(E)  OEORHD
     C                   IF        not %Error  and  %Found
     C                   EVAL      idx = idx + 1
     C                   EVAL      ary_ORRF(idx) = OEORRF
     C                   ELSE
     C                   EVAL      idx = idx + 1
     C                   EVAL      ary_ORRF(idx) =
     C                               ':' + %triml(%editc(BIORD#:'Z'))
     C                   ENDIF                                                  not %Error and %Foun
     C                   IF        wk_PrtCtnQty
     C                   EVAL      ary_CTNQ(idx) = %xfoot(BIFCCQ_CSQT)
     C                   ENDIF                                                  wk_PrtCtnQty
     C***                WRITE     BOLB1
     C                   EVAL      work_COIPlts = work_COIPlts + BIPLLT
     C                   EVAL      work_COIWgt = work_COIWgt + BIWGT
     C                   EVAL      work_OrderCnt = work_OrderCnt + 1
     C     work_BOL#     READE(E)  HYLBOL1
     C                   IF        not %Error  and  not %EOF
     C                   IF        work_OrderCnt = wk_MaxOrderCnt
     C     BIKEY         SETLL     HYLBOL1
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   EVAL      R@TPLT = work_COIPlts
     C                   EVAL      R@TWGT = work_COIWgt +
     C                               (work_COIPlts * work_PalletWgt)
     C                   EVAL      AllOrdersDone = Yes
     C                   ENDIF
     C                   ENDDO                                                  not %Error and not %

     C                   ENDIF                                                  SDBHF1='M'

     C                   IF        idx > MaxOrderLines
     C                   EVAL      wk_COLines = MaxOrderLines
     C                   ELSE
     C                   EVAL      wk_COLines = idx
     C                   ENDIF                                                  idx>MaxOrderLines
     C                   FOR       idx2 = 1 to wk_COLines
     C                   EVAL      R@ORRF01 = ary_ORRF(idx2)
     C                   IF        not wk_PrtCtnQty
     C                   EVAL      R@ORRF02 = ary_ORRF(idx2 + MaxOrderLines)
     C                   EVAL      R@ORRF03 =
     C                               ary_ORRF(idx2 + (MaxOrderLines * 2))
     C                   EVAL      R@ORRF04 =
     C                               ary_ORRF(idx2 + (MaxOrderLines * 3))
     C                   ELSE
     C                   IF        ary_CTNQ(idx2) <> *Zero
     C                   EVAL      R@ORRF02 = '      ' +
     C                               %editc(ary_CTNQ(idx2):'J') + 'CTNS'
     C                   ELSE
     C                   EVAL      R@ORRF02 = *Blanks
     C                   ENDIF                                                  ary_CTNQ(idx2)<>*Zer
     C                   EVAL      R@ORRF03 = ary_ORRF(idx2 + MaxOrderLines)
     C                   IF        ary_CTNQ(idx2 + MaxOrderLines) <> *Zero
     C                   EVAL      R@ORRF04 = '      ' + %editc(ary_CTNQ(
     C                               idx2 + MaxOrderLines):'J') + 'CTNS'
     C                   ELSE
     C                   EVAL      R@ORRF04 = *Blanks
     C                   ENDIF                                                  ary_CTNQ(idx2+MaxOrd
     C                   ENDIF                                                  not wk_PrtCtnQty
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB1
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB1
     C                   ENDSL
     C                   ENDFOR                                                 idx2=1 to wk_COLines

     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB1X
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB1X
     C                   ENDSL

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Process carrier information.                                     *
     C********************************************************************
     C     $CarrierInfo  BEGSR


     C                   IF        AllFrtClsDone = No
     C     work_BOL#     READE(E)  OEBOLD
     C                   IF        %Error  or  %EOF
     C                   IF        work_CIPlts > *Zero
     C                               or  (work_PDFName <> *Blanks  and          .pdf file
     C                                    work_PDFName <> '*NONE')
     C                   EVAL      R@WGT = work_CIPlts * work_PalletWgt
     C                   EVAL      R@FDS1 = TareWgtDsc
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2TW
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2TW
     C                   ENDSL
     C                   EVAL      work_CIWgt = work_CIWgt + R@WGT
     C                   EVAL      work_FrtClsCnt = work_FrtClsCnt + 1
     C                   ENDIF                                                  work_CIPlts>*Zero
     C                   EVAL      AllFrtClsDone = Yes
     C                   ENDIF
     C                   DOW       not %Error  and  not %EOF
     C                   EVAL      R@PLLT = BLUDN1
     C                   EVAL      R@PKGS = BLPKGS
     C                   EVAL      R@PKUM = BLPKUM
     C                   EVAL      R@WGT = BLWGT
     C                   EVAL      R@HAZM = BLHAZM
     C                   EVAL      R@FDS1 = BLFDS1
     C                               + BLUDA2
     C                   EVAL      R@NMFC = %subst(BLFDS2:6:9)
     C     BLFTCL        CHAIN(E)  HDFTCL
     C                   IF        not %Error  and  %FOUND
     C                   EVAL      R@CLAS = FTNMFC
     C                   ELSE
     C                   EVAL      R@CLAS = *Blanks
     C                   ENDIF
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2
     C                   ENDSL
     C                   IF        BLUDA1 <> *Blanks
     C                   EVAL      R@PLLT = *Zero
     C                   EVAL      R@PLTY = *Blanks
     C                   EVAL      R@PKGS = *Zero
     C                   EVAL      R@PKUM = *Blanks
     C                   EVAL      R@WGT = *Zero
     C                   EVAL      R@HAZM = *Blanks
     C                   EVAL      R@FDS2 = BLUDA1
     C                   EVAL      R@NMFC = *Blanks
     C                   EVAL      R@CLAS = *Blanks
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2A
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2A
     C                   ENDSL
     C                   ENDIF                                                  BLUDA1<>*Blanks
     C                   IF        BLUDA3 <> *Blanks
     C                   EVAL      R@PLLT = *Zero
     C                   EVAL      R@PLTY = *Blanks
     C                   EVAL      R@PKGS = *Zero
     C                   EVAL      R@PKUM = *Blanks
     C                   EVAL      R@WGT = *Zero
     C                   EVAL      R@HAZM = *Blanks
     C                   EVAL      R@FDS2 = BLUDA3
     C                   EVAL      R@NMFC = *Blanks
     C                   EVAL      R@CLAS = *Blanks
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2A
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2A
     C                   ENDSL
     C                   ENDIF                                                  BLUDA3<>*Blanks
     C***                EVAL      R@TPLT = R@TPLT + BLUDN1
     C***                EVAL      R@TPKG = R@TPKG + BLPKGS
     C***                EVAL      R@TWGT = R@TWGT + BLWGT
     C                   EVAL      work_CIPlts = work_CIPlts + BLUDN1
     C                   EVAL      work_CIPkgs = work_CIPkgs + BLPKGS
     C                   EVAL      work_CIWgt = work_CIWgt + BLWGT
     C                   EVAL      work_FrtClsCnt = work_FrtClsCnt + 1
     C                   IF        BLUDA1 <> *Blanks
     C                   EVAL      work_FrtClsCnt = work_FrtClsCnt + 1
     C                   ENDIF                                                  BLUDA1<>*Blanks
     C                   IF        BLUDA3 <> *Blanks
     C                   EVAL      work_FrtClsCnt = work_FrtClsCnt + 1
     C                   ENDIF                                                  BLUDA3<>*Blanks
     C     work_BOL#     READE(E)  OEBOLD
     C                   IF        not %Error  and  not %EOF
     C                   IF        work_FrtClsCnt = MaxFrtClsCnt
     C     BLKEY         SETLL     OEBOLD
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   IF        work_FrtClsCnt = MaxFrtClsCnt
     C     BLKEY         SETGT     OEBOLD
     C                   LEAVE
     C                   ENDIF                                                  work_FrtClsCnt=MaxFr
     C                   IF        work_CIPlts > *Zero
     C                               or  (work_PDFName <> *Blanks  and          .pdf file
     C                                    work_PDFName <> '*NONE')
     C                   EVAL      R@WGT = work_CIPlts * work_PalletWgt
     C                   EVAL      R@FDS1 = TareWgtDsc
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2TW
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2TW
     C                   ENDSL
     C                   EVAL      work_CIWgt = work_CIWgt + R@WGT
     C                   EVAL      work_FrtClsCnt = work_FrtClsCnt + 1
     C                   ENDIF                                                  work_CIPlts>*Zero
     C                   EVAL      AllFrtClsDone = Yes
     C                   ENDIF
     C                   ENDDO                                                  not %Error and not %
     C                   ENDIF                                                  AllFrtClsDone=No

     C                   IF        AllFrtClsDone = Yes
     C     SDBLTO        READE(E)  HYPBLCD
     C                   IF        not %Error  and  not %EOF
     C                   IF        work_FrtClsCnt = MaxFrtClsCnt
     C     CDKEY         SETLL     HYPBLCD
     C                   ENDIF
     C                   ELSE
     C                   EVAL      R@TPLT = work_CIPlts
     C                   EVAL      R@TPKG = work_CIPkgs
     C                   IF        work_PDFName = *Blanks  or                   not .pdf file
     C                               work_PDFName = '*NONE'
     C                   EVAL      R@TWGT = work_CIWgt
     C                   ELSE
     C                   EVAL      R@TWGT = *Zero
     C                   ENDIF                                                  work_PDFName=*Blanks
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2T
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2T
     C                   ENDSL
     C                   EVAL      AllExtCarDta = Yes
     C                   ENDIF
     C                   DOW       not %Error  and  not %EOF
     C                   EVAL      R@PLLT = CDHQTY
     C                   EVAL      R@PLTY = CDHTYP
     C                   EVAL      R@PKGS = CDPQTY
     C                   EVAL      R@PKUM = CDPTYP
     C                   EVAL      R@WGT = CDWGT
     C                   EVAL      R@HAZM = CDHAZM
     C                   EVAL      R@FDS2 = CDDESC
     C                   EVAL      R@NMFC = CDNMF#
     C                   EVAL      R@CLAS = CDNMFC
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2A
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2A
     C                   ENDSL
     C                   EVAL      work_CIPlts = work_CIPlts + CDHQTY
     C                   EVAL      work_CIPkgs = work_CIPkgs + CDPQTY
     C                   EVAL      work_CIWgt = work_CIWgt + CDWGT
     C                   EVAL      work_FrtClsCnt = work_FrtClsCnt + 1
     C     SDBLTO        READE(E)  HYPBLCD
     C                   IF        not %Error  and  not %EOF
     C                   IF        work_FrtClsCnt = MaxFrtClsCnt
     C     CDKEY         SETLL     HYPBLCD
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C                   EVAL      R@TPLT = work_CIPlts
     C                   EVAL      R@TPKG = work_CIPkgs
     C                   IF        work_PDFName = *Blanks  or                   not .pdf file
     C                               work_PDFName = '*NONE'
     C                   EVAL      R@TWGT = work_CIWgt
     C                   ELSE
     C                   EVAL      R@TWGT = *Zero
     C                   ENDIF                                                  work_PDFName=*Blanks
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLB2T
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLB2T
     C                   ENDSL
     C                   EVAL      AllExtCarDta = Yes
     C                   ENDIF
     C                   ENDDO                                                  not %Error and not %
     C                   ENDIF                                                  AllFrtClsDone=Yes

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Process pallet dimensions.                                       *
     C********************************************************************
     C     $PlltDim      BEGSR

     C                   EVAL      AllPlltDimDone = Yes

      /Free
       wk_RcdCnt= *Zero;
       EXEC SQL select count(*) into :wk_RcdCnt
                from HYPPLLT
                where PMBOL#=:work_BOL# and PMHGT<>0;
      /End-Free

     C                   IF        wk_RcdCnt = *Zero
     C                   LEAVESR
     C                   ENDIF                                                  wk_RcdCnt=*Zero

     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLPD1X
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLPD1X
     C                   ENDSL

     C                   EVAL      PDim = *Blanks
     C                   EVAL      idx3 = *Zero

     C     work_BOL#     SETLL     HYLPLLT2
     C                   DOU       %eof(HYLPLLT2)
     C     work_BOL#     READE     HYLPLLT2
     C                   IF        %eof(HYLPLLT2)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYLPLLT2)
     C                   IF        PMHGT = *Zero
     C                   ITER
     C                   ENDIF                                                  PMHGT=*Zero
     C****               EVAL      wk_PlltDim =
     C****                           %triml(%editc(PMHGT:'Z')) + 'X' +
     C****                           %triml(%editc(PMLEN:'Z')) + 'X' +
     C****                           %triml(%editc(PMWID:'Z'))
     C                   EVAL      wk_PlltDim =
     C                               %triml(%editc(PMLEN:'Z')) + 'X' +
     C                               %triml(%editc(PMWID:'Z')) + 'X' +
     C                               %triml(%editc(PMHGT:'Z'))
     C                   EVAL      idx3 = idx3 +1
     C                   IF        idx3 <= wk_PDimMax
     C                   EVAL      PDim(idx3) = wk_PlltDim
     C                   ENDIF                                                  idx3<=wk_PDimMax
     C                   ENDDO                                                  %eof(HYLPLLT2)

     C                   FOR       idx3 = 1 to 6                                6 rows, 7 columns
     C                   EVAL      R@PLTDIM01 = PDim(idx3)
     C                   EVAL      R@PLTDIM02 = PDim(idx3+6)
     C                   EVAL      R@PLTDIM03 = PDim(idx3+12)
     C                   EVAL      R@PLTDIM04 = PDim(idx3+18)
     C                   EVAL      R@PLTDIM05 = PDim(idx3+24)
     C                   EVAL      R@PLTDIM06 = PDim(idx3+30)
     C                   EVAL      R@PLTDIM07 = PDim(idx3+36)
     C                   SELECT
     C                   WHEN      work_RptType = Print
     C                   WRITE     BOLPD1
     C                   WHEN      work_RptType = Imaging
     C                   WRITE     XBOLPD1
     C                   ENDSL
     C                   ENDFOR                                                 idx3=1 to 9

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Retrieve ship-to information.                                    *
     C********************************************************************
     C     $RtvShipTo    BEGSR

     C                   EVAL      idx = *Zero
     C                   EVAL      ADR = *Blanks

     C                   IF        SDNAME <> *Blanks
     C                   EVAL      idx = idx + 1
     C                   EVAL      ADR(idx) = SDNAME
     C                   ENDIF
     C                   IF        SDADR1 <> *Blanks  and
     C                               spcladdress(SDADR1) = *Blank
     C                   EVAL      idx = idx + 1
     C                   EVAL      ADR(idx) = %triml(SDADR1)
     C                   ENDIF
     C                   IF        SDADR2 <> *Blanks  and  idx < 4  and
     C                               spcladdress(SDADR2) = *Blank
     C                   EVAL      idx = idx + 1
     C                   EVAL      ADR(idx) = %triml(SDADR2)
     C                   ENDIF
     C                   IF        SDADR3 <> *Blanks  and  idx < 4  and
     C                               spcladdress(SDADR3) = *Blank
     C                   EVAL      idx = idx + 1
     C                   EVAL      ADR(idx) = %triml(SDADR3)
     C                   ENDIF
     C                   IF        SDCITY <> *Blanks  or  SDST <> *Blanks  or
     C                               SDZIP <> *Blanks
     C                   EVAL      ADR5 = %trimr(SDCITY) + ', ' + SDST +
     C                               '  ' + SDZIP
     C                   ELSE
     C                   EVAL      ADR5 = *Blanks
     C                   ENDIF
     C                   IF        SDCTRY <> UnitedStates
     C                   IF        idx < 4
     C     SDCTRY        CHAIN     HDCTRY
     C                   IF        not %found(HDCTRY)
     C                   EVAL      CNCDES = SDCTRY
     C                   ENDIF                                                  %found(HDCTRY)
     C                   EVAL      idx = 4
     C                   EVAL      ADR(idx) = SDCTRY
     C                   ELSE
     C                   EVAL      %subst(ADR5:31:3) =
     C                               ' ' + %subst(SDCTRY:1:2)
     C                   ENDIF
     C                   ENDIF

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Retrieve ship-from information.                                  *
     C********************************************************************
     C     $RtvShipFrom  BEGSR

     C                   EXSR      $ShipLoc

     C                   IF        wk_SLocFound = *OFF
     C     MasterWhse    CHAIN(E)  HDWHSM
     C                   EVAL      TTL1 = WHWHAD
     C                   EVAL      TTL2 = WHWHA2
     C                   EVAL      TTL3 = %trimr(WHWHCT) + ', ' + WHWHST +
     C                               '  ' + WHHWZP
     C                   ENDIF                                                  wk_SLocFound=*OFF

     C                   IF        wk_SLocFound = *ON
     C                   EVAL      TTL1 = SLNAME
     C                   EVAL      TTL2 = SLADDR1
     C                   EVAL      TTL3 = %trimr(SLCITY) + ', ' + SLST +
     C                               '  ' + SLZIP
     C                   ENDIF                                                  wk_SLocFound=*ON


     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Get Shipping Location Address Info.                              *
     C********************************************************************
     C     $ShipLoc      BEGSR

      *
      *  Get the shipping location address info to print on the label
      *
     C                   EVAL      wk_ShipLoc = %subst(SDBCM8:29:4)

     C                   IF        wk_ShipLoc = *BLANKS
     C                   EVAL      wk_ShipLoc = GI
     C                   ENDIF                                                  wk_ShipLoc=*Blanks

     C     wk_ShipLoc    CHAIN     HYPSLOC
     C                   IF        %found(HYPSLOC)
     C                   EVAL      wk_SLocFound = *ON
     C                   ELSE
     C                   EVAL      wk_SLocFound = *OFF
     C                   ENDIF
     C

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Calculate volume of pallets on BOL.                              *
     C********************************************************************
     C     $CalcVol      BEGSR

     C                   EVAL      wk_Count = 0
     C                   EVAL      wk_VolTot = 0
     C                   EVAL      wk_Vol = 0
     C                   EVAL      PVOL = *Blanks
      *
      *  All pallets on this BOL must have all dimensions present in order to calc
      *  the volume.
      *
      /Free
       EXEC SQL SELECT count(*) INTO :wk_Count
                  FROM HYPPLLT a
                  WHERE a.PMBOL# = :SDBOL#
                  AND (a.PMLEN = 0 OR a.PMWID = 0 OR a.PMHGT = 0);
      //
      // count must be zero to print the volume.
      //
       If wk_Count = 0;
         EXEC SQL SELECT
              round(coalesce(sum((a.PMHGT * a.PMWID * a.PMLEN)/1728),0),2)
              INTO :wk_VolTot
              FROM HYPPLLT a  WHERE a.PMBOL#  = :SDBOL#;
       EndIf;

      /End-Free

      *
      *  11/9/2018 force BOL to not print volume per Lance.
      *
     C                   IF        wk_VolTot > 0 and wk_VolTot <= 9999.99
     C                              and 1 = 0
     C                   EVAL      wk_Vol = wk_VolTot
     C                   EVAL      PVOL = 'VOL:' + %editc(wk_Vol:'3')
     C                   ENDIF                                                  wk_Vol > 0

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Retrieve bill of lading comments / special instructions.         *
     C********************************************************************
     C     $RtvInstr     BEGSR

     C                   IF        SDBHF1 = 'M'                                 Master BOL

     C                   IF        %subst(SDBCM3:31) = *Blanks
     C                   EVAL      BOLCMT1 = %subst(SDBCM3:1:30) + SDBCM4
     C                   ELSE
     C                   EVAL      BOLCMT1 = SDBCM3 + SDBCM4
     C                   ENDIF                                                  %subst(SDBCM3:31)=
     C                   IF        %subst(SDBCM5:31) = *Blanks
     C                   EVAL      BOLCMT2 = %subst(SDBCM5:1:30) + SDBCM6
     C                   ELSE
     C                   EVAL      BOLCMT2 = SDBCM5 + SDBCM6
     C                   ENDIF                                                  %subst(SDBCM5:31)=
     C                   IF        %subst(SDBCM7:31) = *Blanks
     C                   EVAL      BOLCMT3 = %subst(SDBCM7:1:30) + SDBCM8
     C                   ELSE
     C                   EVAL      BOLCMT3 = SDBCM7 + SDBCM8
     C                   ENDIF                                                  %subst(SDBCM7:31)=
     C                   EVAL      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT5 = *Blanks

     C                   ELSE

      /Free
       wk_DBDate = *Zero;
       wk_DlvrBy = *Blanks;
       EXEC SQL select TDCF02 into :wk_DlvrBy
                from GUPTDAT
                where TDTABL='CUSTOMER' and TDKEY1=:wk_BillTo#X and
                      TDKEY2=:wk_DatabaseID2;
       SELECT;
       WHEN wk_DlvrBy = 'OEDTE2-EARLIEST';
         EXEC SQL select coalesce(min(b.OEDTE2),9999999) into :wk_DBDate
                  from HYPBOL a join OEORHD b on a.BIORD#=b.OEORD#
                  where a.BIBOL#=:work_BOL# and b.OEDTE2<>0;
       ENDSL;

       work_OrderCnt = *Zero;
       wk_BIORD# = *Zero;
       wk_OrdCmtCnt = *Zero;
       EXEC SQL select count(distinct BIORD#), min(BIORD#)
                  into :work_OrderCnt, :wk_BIORD#
                from HYPBOL
                where BIBOL#=:work_BOL#;
       IF work_OrderCnt > *Zero;
         EXEC SQL select count(*) into :wk_OrdCmtCnt
                  from OEOCMT
                  where OCORD#=:wk_BIORD# and OCDOCT='BOL';
       ENDIF;                                                                // work_OrderCnt>*Zero
      /End-Free

     C                   EVAL      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT5 = *Blanks

     C                   IF        SDERPH = *Zero

      *
      * Retrieve any ship-to customer comments.
      *
     C     CXKEY2        SETLL     HDCCMT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT1 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT2 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT3 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT4 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT5 = CXCMNT
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF
     C                   ENDIF

      *
      * If ship-to customer comments not found, retrieve any bill-to customer comments.
      *
     C                   IF        BOLCMT1 = *Blanks  and  SDSHTO <> SDBLTO

     C     CXKEY         SETLL     HDCCMT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT1 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT2 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT3 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT4 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT5 = CXCMNT
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF
     C                   ENDIF

     C                   ENDIF                                                  BOLCMT1=*Blanks and

     C                   ELSE

     C                   EVAL      BOLCMT1 = wk_EmergMsg

      *
      * Retrieve any ship-to customer comments.
      *
     C     CXKEY2        SETLL     HDCCMT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT2 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT3 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT4 = CXCMNT
     C     CXKEY2        READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT5 = CXCMNT
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF
     C                   ENDIF

      *
      * If ship-to customer comments not found, retrieve any bill-to customer comments.
      *
     C                   IF        BOLCMT2 = *Blanks  and  SDSHTO <> SDBLTO

     C     CXKEY         SETLL     HDCCMT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT2 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT3 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT4 = CXCMNT
     C     CXKEY         READE(E)  HDCCMT
     C                   IF        not %Error  and  not %EOF
     C                   EVAL      BOLCMT5 = CXCMNT
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF                                                  not %Error and not
     C                   ENDIF
     C                   ENDIF

     C                   ENDIF                                                  BOLCMT2=*Blanks and

     C                   ENDIF                                                  SDERPH=*Zero

     C                   IF        SDDSHP <> *Zero
      /Free
       wk_DSPhone = *Zero;
       EXEC SQL select DSPHON into :wk_DSPhone
                from HDDSHP
                where DSVCF='C' and DSVNCS=:SDSHTO and DSNMBR=:SDDSHP;
       wk_DSEmail = *Blanks;
       EXEC SQL select DSEMAL into :wk_DSEmail
                from HDDSHP2
                where DSNMBR=:SDDSHP;
       IF %len(%trimr(wk_DSEmail)) >  59;                                    // Don't print if >
         wk_DSEmail = *Blanks;                                               //   max comment length
       ENDIF;
      /End-Free
     C                   IF        wk_DSPhone <> *Zero
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = 'Phone: ' +
     C                               %triml(%editw(wk_DSPhone:' -   -   -    '))
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = 'Phone: ' +
     C                               %triml(%editw(wk_DSPhone:' -   -   -    '))
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = 'Phone: ' +
     C                               %triml(%editw(wk_DSPhone:' -   -   -    '))
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = 'Phone: ' +
     C                               %triml(%editw(wk_DSPhone:' -   -   -    '))
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = 'Phone: ' +
     C                               %triml(%editw(wk_DSPhone:' -   -   -    '))
     C                   ENDSL
     C                   ENDIF                                                  wk_DSPhone<>*Zero;
     C                   IF        wk_DSEmail <> *Blanks
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = wk_DSEmail
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = wk_DSEmail
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = wk_DSEmail
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = wk_DSEmail
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = wk_DSEmail
     C                   ENDSL
     C                   ENDIF                                                  wk_DSEmail<>*Blanks;
     C                   ELSE
      /Free
       wk_BTCls = *Blanks;
       wk_STPhone = *Zero;
       EXEC SQL select CMCCLS into :wk_BTCls
                from HDCUST
                where CMCUST=:SDBLTO;
       EXEC SQL select CMPHON into :wk_STPhone
                from HDCUST
                where CMCUST=:SDSHTO;
       wk_STEmail = *Blanks;
       EXEC SQL select CLEMAL into :wk_STEmail
                from HYPCLMS
                where CLCLNO=:SDSHTO;
       IF %len(%trimr(wk_STEmail)) >  59;                                    // Don't print if >
         wk_STEmail = *Blanks;                                               //   max comment length
       ENDIF;
      /End-Free
     C                   IF        wk_STPhone <> *Zero  and
     C                               (wk_BTCls = 'RT'  or  wk_BTCls = 'RF'  or
     C                                wk_BTCls = 'RP'  or  wk_BTCls = 'SF'  or
     C                                wk_BTCls = 'IP'  or  wk_BTCls = 'IR'  or
     C                                wk_BTCls = 'SS'  or  wk_BTCls = 'WR'  or
     C                                wk_BTCls = 'TV'  or  wk_BTCls = 'EM')
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = 'Phone: ' +
     C                               %triml(%editw(wk_STPhone:' -   -   -    '))
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = 'Phone: ' +
     C                               %triml(%editw(wk_STPhone:' -   -   -    '))
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = 'Phone: ' +
     C                               %triml(%editw(wk_STPhone:' -   -   -    '))
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = 'Phone: ' +
     C                               %triml(%editw(wk_STPhone:' -   -   -    '))
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = 'Phone: ' +
     C                               %triml(%editw(wk_STPhone:' -   -   -    '))
     C                   ENDSL
     C                   ENDIF                                                  wk_STPhone<>*Zero
     C                   IF        wk_STEmail <> *Blanks  and
     C                               (wk_BTCls = 'RT'  or  wk_BTCls = 'RF'  or
     C                                wk_BTCls = 'RP'  or  wk_BTCls = 'SF'  or
     C                                wk_BTCls = 'IP'  or  wk_BTCls = 'IR'  or
     C                                wk_BTCls = 'SS'  or  wk_BTCls = 'WR'  or
     C                                wk_BTCls = 'TV'  or  wk_BTCls = 'EM')
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = wk_STEmail
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = wk_STEmail
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = wk_STEmail
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = wk_STEmail
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = wk_STEmail
     C                   ENDSL
     C                   ENDIF                                                  wk_STEmail<>*Blanks;
     C                   ENDIF                                                  SDDSHP<>*Zero

      *
      * Print "DELIVER BY xx/xx/xxxx" instruction, if needed.
      *
     C                   IF        wk_DBDate <> *Zero  and                      Cust needs DlvrBy dt
     C                               wk_DBDate <> 9999999                       DlvrBy date found
     C                   EVAL      ds_CYMD = wk_DBDate
     C                   EVAL      ds_YYYY2 = ds_CYY + 1900
     C                   EVAL      wk_DBMDYY = ds_MDYY
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = 'DELIVER BY ' +
     C                               %triml(%editw(wk_DBMDYY:'  /  /    '))
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = 'DELIVER BY ' +
     C                               %triml(%editw(wk_DBMDYY:'  /  /    '))
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = 'DELIVER BY ' +
     C                               %triml(%editw(wk_DBMDYY:'  /  /    '))
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = 'DELIVER BY ' +
     C                               %triml(%editw(wk_DBMDYY:'  /  /    '))
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = 'DELIVER BY ' +
     C                               %triml(%editw(wk_DBMDYY:'  /  /    '))
     C                   ENDSL
     C                   ENDIF                                                  wk_DBDate<>*Zero and

      *
      * Handle FedEx White Glove service shipments.
      *
     C                   IF        SDSHPV = 'FW'
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = 'BASIC BY APPOINTMENT'
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = 'BASIC BY APPOINTMENT'
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = 'BASIC BY APPOINTMENT'
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = 'BASIC BY APPOINTMENT'
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = 'BASIC BY APPOINTMENT'
     C                   OTHER
     C                   IF        %len(%trimr(BOLCMT5)) < 38
     C                   EVAL      BOLCMT5 = %trimr(BOLCMT5) + '  ' +
     C                               'BASIC BY APPOINTMENT'
     C                   ELSE
     C                   EVAL      wk_MsgID = 'ERR1539'
     C                   EVAL      wk_MsgData = %triml(%editc(work_BOL#:'Z'))
     C                   EVAL      %subst(wk_MsgData:9:2) = SDSHPV
     C                   EVAL      %subst(wk_MsgData:11:30) =
     C                               'BASIC BY APPOINTMENT'
     C                   EXSR      $SendITMsg
     C                   ENDIF                                                  %len(%trimr(BOLCMT5)
     C                   ENDSL
     C                   ENDIF                                                  SDSHPV='FW'

      *
      * If the shipment contains any single item weighing at least 150 lbs,
      * and its order was marked in O/E as requiring liftgate
      * service (OEUCA1='LG'), print "LIFTGATE SERVICE REQUIRED" in the BOL
      * special instructions section.
      *
      /Free
       wk_LGCnt = *Zero;
       EXEC SQL select count(*) into :wk_LGCnt
                from HYPSSCC a
                  join HDIMST b on a.SCITEM=b.IMITEM
                  join OEORHD c on a.SCORD#=c.OEORD#
                where a.SCBOL#=:work_BOL# and b.IMIMWG>=150 and
                  c.OEUCA1='LG';
      /End-Free
     C                   IF        wk_LGCnt > *Zero
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = 'LIFTGATE SERVICE REQUIRED'
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = 'LIFTGATE SERVICE REQUIRED'
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = 'LIFTGATE SERVICE REQUIRED'
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = 'LIFTGATE SERVICE REQUIRED'
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = 'LIFTGATE SERVICE REQUIRED'
     C                   OTHER
     C                   IF        %len(%trimr(BOLCMT5)) < 51
     C                   EVAL      BOLCMT5 = %trimr(BOLCMT5) + ' ' +
     C                               'LIFTGATE SERVICE REQUIRED'
     C                   ENDIF                                                  %len(%trimr(BOLCMT5)
     C                   ENDSL
     C                   ENDIF                                                  wk_LGCnt>*Zero

      *
      * If there are order-level BOL comments, print them (if there
      * is room).
      *
     C                   IF        wk_OrdCmtCnt > *Zero
     C                   IF        work_OrderCnt = 1
     C     wk_BIORD#     SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     wk_BIORD#     READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        OCDOCT <> 'BOL'  or  OCCMNT = *Blanks
     C                   ITER
     C                   ENDIF                                                  OCDOCT<>'BOL' or
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = OCCMNT
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = OCCMNT
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = OCCMNT
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = OCCMNT
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = OCCMNT
     C                   OTHER
     C                   IF        %subst(BOLCMT5:51:9) = *Blanks
     C                   EVAL      %subst(BOLCMT5:52:8) = 'MORE . .'
     C                   ENDIF
     C                   ENDSL
     C                   ENDDO                                                  %eof(OEOCMT)
     C                   ELSE
     C                   EVAL      idx4 = *Zero
     C                   EVAL      ary_OCMT = *Blanks
     C     work_BOL#     SETLL     HYLBOL1
     C                   DOU       %eof(HYLBOL1)
     C     work_BOL#     READE     HYLBOL1
     C                   IF        %eof(HYLBOL1)
     C                   LEAVE
     C                   ENDIF                                                  %eof(work_BOL#)
     C     BIORD#        SETLL     OEOCMT
     C                   DOU       %eof(OEOCMT)
     C     BIORD#        READE     OEOCMT
     C                   IF        %eof(OEOCMT)
     C                   LEAVE
     C                   ENDIF                                                  %eof(OEOCMT)
     C                   IF        OCDOCT <> 'BOL'  or  OCCMNT = *Blanks
     C                   ITER
     C                   ENDIF                                                  OCDOCT<>'BOL' or
     C                   IF        idx4 = *Zero
     C                   EVAL      idx4 = idx4 + 1
     C                   EVAL      ary_OCMT(idx4) = OCCMNT
     C                   ELSE
     C                   EVAL      idx4a = %lookup(OCCMNT:ary_OCMT:1:idx4)      Check for dup cmnts
     C                   IF        idx4a > *Zero
     C                   ITER
     C                   ENDIF                                                  idx4a>*Zero
     C                   EVAL      idx4 = idx4 + 1
     C                   EVAL      ary_OCMT(idx4) = OCCMNT
     C                   ENDIF                                                  idx4=*Zero
     C                   SELECT
     C                   WHEN      BOLCMT1 = *Blanks
     C                   EVAL      BOLCMT1 = OCCMNT
     C                   WHEN      BOLCMT2 = *Blanks
     C                   EVAL      BOLCMT2 = OCCMNT
     C                   WHEN      BOLCMT3 = *Blanks
     C                   EVAL      BOLCMT3 = OCCMNT
     C                   WHEN      BOLCMT4 = *Blanks
     C                   EVAL      BOLCMT4 = OCCMNT
     C                   WHEN      BOLCMT5 = *Blanks
     C                   EVAL      BOLCMT5 = OCCMNT
     C                   OTHER
     C                   IF        %subst(BOLCMT5:51:9) = *Blanks
     C                   EVAL      %subst(BOLCMT5:52:8) = 'MORE . .'
     C                   ENDIF
     C                   ENDSL
     C                   ENDDO                                                  %eof(OEOCMT)
     C                   ENDDO                                                  %eof(HYLBOL1)
     C                   ENDIF                                                  work_OrderCnt=1
     C                   ENDIF                                                  wk_OrdCmtCnt>*Zero

     C                   ENDIF                                                  SDBHF1='M'

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Retrieve total page count.                                       *
     C********************************************************************
     C     $RtvPageCount BEGSR

     C                   EVAL      work_TotalOrdr = *Zero
     C                   EVAL      work_TotalFtCl = *Zero
     C                   EVAL      work_TotalPlts = *Zero
     C                   EVAL      work_Prepaid = No
     C                   EVAL      work_Collect = No

     C                   IF        SDBHF1 = 'M'                                 Master BOL

     C     work_BOL#     SETLL     HYPMBLD
     C                   DOU       %eof(HYPMBLD)
     C     work_BOL#     READE     HYPMBLD
     C                   IF        %eof(HYPMBLD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPMBLD)
     C                   EVAL      work_TotalOrdr = work_TotalOrdr + 1
     C                   ENDDO                                                  %eof(HYPMBLD)

     C                   SELECT
     C                   WHEN      SDBHF2 = 'P'
     C                   EVAL      work_Prepaid = Yes
     C                   WHEN      SDBHF2 = 'C'
     C                   EVAL      work_Collect = Yes
     C                   ENDSL

     C                   ELSE

     C     work_BOL#     SETLL     HYLBOL1
     C     work_BOL#     READE(E)  HYLBOL1
     C                   DOW       not %Error  and  not %EOF
     C                   EVAL      work_TotalOrdr = work_TotalOrdr + 1
     C     BIORD#        CHAIN(E)  OEORHD
     C                   IF        not %Error  and  %Found
     C                   IF        %subst(OEUDF1:1:4) = FrtPrepaid1  or
     C                               %subst(OEUDF1:1:3) = FrtPrepaid2  or
     C                               %subst(OEUDF1:1:3) = FrtPrepaid3
     C                   EVAL      work_Prepaid = Yes
     C                   ELSE
     C                   IF        %subst(OEUDF1:1:3) <> FrtDefined
     C                   EVAL      work_Collect = Yes
     C                   ENDIF                                                  %subst(OEUDF1:1:4)=F
     C                   ENDIF                                                  %subst(OEUDF1:1:4)=F
     C                   ENDIF                                                  not %Error and %Foun
     C     work_BOL#     READE(E)  HYLBOL1
     C                   ENDDO                                                  not %Error and not %

     C                   ENDIF                                                  SDBHF1='M'

     C     work_BOL#     SETLL     OEBOLD
     C     work_BOL#     READE(E)  OEBOLD
     C                   DOW       not %Error  and  not %EOF
     C                   EVAL      work_TotalFtCl = work_TotalFtCl + 1
     C                   IF        BLUDA1 <> *Blanks
     C                   EVAL      work_TotalFtCl = work_TotalFtCl + 1
     C                   ENDIF                                                  BLUDA1<>*Blanks
     C                   IF        BLUDA3 <> *Blanks
     C                   EVAL      work_TotalFtCl = work_TotalFtCl + 1
     C                   ENDIF                                                  BLUDA3<>*Blanks
     C                   EVAL      work_TotalPlts = work_TotalPlts + BLUDN1
     C     work_BOL#     READE(E)  OEBOLD
     C                   ENDDO                                                  not %Error and not %

     C                   IF        work_TotalPlts > *Zero
     C                   EVAL      work_TotalFtCl = work_TotalFtCl + 1
     C                   ENDIF                                                  work_TotalPlts>*Zero

     C                   IF        SDBHF1 <> 'M'                                Not Master BOL
     C     SDBLTO        SETLL     HYPBLCD
     C                   DOU       %eof(HYPBLCD)
     C     SDBLTO        READE     HYPBLCD
     C                   IF        %eof(HYPBLCD)
     C                   LEAVE
     C                   ENDIF                                                  %eof(HYPBLCD)
     C                   EVAL      work_TotalFtCl = work_TotalFtCl + 1
     C                   ENDDO                                                  %eof(HYPBLCD)
     C                   ENDIF                                                  SDBHF1<>'M'

     C                   IF        work_TotalOrdr / wk_MaxOrderCnt >
     C                               work_TotalFtCl / MaxFrtClsCnt
     C                   EVAL      work_TotalPage =
     C                               work_TotalOrdr / wk_MaxOrderCnt
     C                   ELSE
     C                   EVAL      work_TotalPage =
     C                               work_TotalFtCl / MaxFrtClsCnt
     C                   ENDIF
     C                   EVAL      wk_XTotalPage = %editc(work_TotalPage:'X')
     C                   EVAL      TOTPAGECNT = work_TotalPage
     C                   IF        %subst(wk_XTotalPage:7:3) <> '000'
     C                   EVAL      TOTPAGECNT = TOTPAGECNT + 1
     C                   ENDIF

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
     C**************************************************************************
     C* Send I.T. personnel message.                                           *
     C**************************************************************************
     C     $SendITMsg    BEGSR

     C                   EVAL      wk_MsgDataLen = %len(%trimr(wk_MsgData))

     C                   CALLP(E)  QMHSNDM (wk_MsgID : 'GUPMSGF   *LIBL     ' :
     C                               wk_MsgData : wk_MsgDataLen : '*INFO' :
     C                               'BEARL     *LIBL     JELLIS    *LIBL     +
     C                                *SYSOPR' :
     C                               3 : *Blanks : in_MsgKey : io_MsgErrCode)

     C                   ENDSR

     C/EJECT
     C********************************************************************
     C* Initialization.                                                  *
     C********************************************************************
     C     *INZSR        BEGSR

     C*
     C* Define *ENTRY parameter list.
     C*
     C     *ENTRY        PLIST
     C                   PARM                    entry_BOL#
     C                   PARM                    entry_PDFName

     C*
     C* Define parameter lists.
     C*
     C     HSYSBM        PLIST
     C                   PARM                    JAPID
     C                   PARM                    JUSER
     C                   PARM                    JWS
     C                   PARM                    JPGID
     C                   PARM                    JPRTF
     C                   PARM                    JPRTFT
     C                   PARM                    JJNAM            10
     C                   PARM                    JJOBD            10
     C                   PARM                    JJOBQ            10
     C                   PARM                    JOUTQ            10
     C                   PARM                    JOVR1
     C                   PARM                    JOVR2

     C     PLSTOV        PLIST
     C                   PARM                    OVRS
     C                   PARM      500           LEN              15 5
     C*
     C* Define key lists.
     C*
     C     BIKEY         KLIST                                                  HYLBOL1
     C                   KFLD                    BIBOL#
     C                   KFLD                    BIORD#
     C                   KFLD                    BITURN

     C     MDKEY         KLIST                                                  HYPMBLD
     C                   KFLD                    MDMBOL
     C                   KFLD                    MDSHTO
     C                   KFLD                    MDUBOL

     C     BLKEY         KLIST                                                  OEBOLD
     C                   KFLD                    BLBOL#
     C                   KFLD                    BLFTCL

     C     CDKEY         KLIST                                                  HYPBLCD
     C                   KFLD                    CDCUST
     C                   KFLD                    CDSEQ#

     C     CXKEY         KLIST                                                  HDCCMT
     C                   KFLD                    work_VCF
     C                   KFLD                    SDBLTO
     C                   KFLD                    work_DOCT

     C     CXKEY2        KLIST                                                  HDCCMT
     C                   KFLD                    work_VCF
     C                   KFLD                    SDSHTO
     C                   KFLD                    work_DOCT

      *
      * Retrieve current database ID.
      *
     C                   IN        wk_DatabaseID
     C                   SELECT
     C                   WHEN      wk_DatabaseID = 'HY'  or
     C                               wk_DatabaseID = 'XX'
     C                   EVAL      wk_DatabaseID2 = 'HY'
     C                   WHEN      wk_DatabaseID = 'SS'  or
     C                               wk_DatabaseID = 'SX'
     C                   EVAL      wk_DatabaseID2 = 'SS'
     C                   WHEN      wk_DatabaseID = 'PV'  or
     C                               wk_DatabaseID = 'PX'
     C                   EVAL      wk_DatabaseID2 = 'PV'
     C                   OTHER
     C                   EVAL      wk_DatabaseID2 = wk_DatabaseID
     C                   ENDSL

     C                   ENDSR
     C/EJECT
     P**************************************************************************
     P* Check for special address field.  Return a character field.            *
     P**************************************************************************
     P spcladdress     B
     D spcladdress     PI             1A
     D   address                     30A   VALUE
     D addresstype     S              1A
     C                   EVAL      addresstype=*Blank
     C                   SELECT
     C                   WHEN      %subst(address:1:4) = 'ATT:' or
     C                             %subst(address:1:5) = 'ATTN:' or
     C                             %subst(address:1:4) = 'ATT;' or
     C                             %subst(address:1:5) = 'ATTN;' or
     C                             %subst(address:1:4) = 'ATT ' or
     C                             %subst(address:1:5) = 'ATTN '
     C                   EVAL      addresstype = at_Attention
     C                   WHEN      %subst(address:1:4) = 'BOX ' or
     C                             %subst(address:1:4) = 'Box '
     C     ValidBoxChar  CHECK     address:5                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:5) = 'PO BX'
     C     ValidBoxChar  CHECK     address:6                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:6) = 'PO BOX'
     C     ValidBoxChar  CHECK     address:7                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:7) = 'PO  BOX'
     C     ValidBoxChar  CHECK     address:8                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:9) = 'PO DRAWER'
     C     ValidBoxChar  CHECK     address:10                             90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:4) = 'P.O.' or
     C                             %subst(address:1:4) = 'P O '
     C     ValidBoxChar  CHECK     address:9                              90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      %subst(address:1:6) = 'P. O. '
     C     ValidBoxChar  CHECK     address:11                             90    Rest of field
     C                   IF        *IN90 = *OFF                                  must be digits
     C                   EVAL      addresstype = at_POBox
     C                   ENDIF
     C                   WHEN      address = '.'
     C                   EVAL      addresstype = at_Spacer
     C                   WHEN      %subst(address:1:4) = 'AKA '
     C                   EVAL      addresstype = at_AKA
     C                   WHEN      %subst(address:1:1) = '%'
     C                               or  %subst(address:1:4) = 'C/O '
     C                   EVAL      addresstype = at_CareOf
     C                   ENDSL
     C                   RETURN    addresstype
     P spcladdress     E
     C/EJECT
