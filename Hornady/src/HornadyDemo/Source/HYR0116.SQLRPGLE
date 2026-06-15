     H COPYRIGHT('(C) Copyright Hornady Manufacturing Company 1999')
     H BNDDIR('HYR0116')
      **************************************************************************
      * TITLE: Order Ship (Scanner) Shipping Label Print
      *    BY: William Earl (MCA)
      *  DATE: November 1999
      *
      *  REV DATE  BY   CHANGE
      *  --------  ---  -----------------------------------------------------
      *  04/11/00  MCA  Retrieve T.L. Ashford label print program name from
      *                 SHPLBLPGM data area.
      *  04/20/00  MCA  Add partial-carton shipping label print.
      *  04/20/00  MCA  Place vendor item number in S2ITEM, if available.
      *  09/18/01  MCA  Add class I explosives label print.
      *  03/23/04  MCA  Add 2nd shipping label output queue.
      *  10/11/04  MCA  Expand UPC from 5 to 12 digits.  Add 2nd field
      *                 for customer item number, instead of using same
      *                 field for either Hornady item number or customer
      *                 item number.  Add SCAC field.
      *  10/12/04  MCA  Use data areas to determine shipping label name.
      *  09/06/05  MCA  Add 3rd shipping label output queue.
      *  07/05/07  RJS  Add 4rd shipping label output queue.
      *  08/13/07  RJS  Add pallet label print.
      *  09/25/07  MCA  For Academy Corporation, clear item number to print.
      *  02/26/08  RJS  Add 5th shipping label output queue.
      *  02/11/09 MCA  Changes to utilize AS/400 job number field in label
      *                work file records.  (This was changed to enable
      *                multiple user shipping.)
      *  06/14/09 MCA  DC move changes for new user, workstation and printer
      *                names.
      *  10/07/09 RJS  Add 9th shipping label output queue.
      *  04/07/10 MCA  Change label output queue and program determination
      *                process.
      *  01/12/11 MCA  Change pallet label quantity API parameter population
      *                due to removal of auto-incrementing number on label.
      *  04/27/11 MCA  Hard-code Dick's label for Rick's testing.
      *  08/26/11 MCA  Add functionality to print explosive label without printing
      *                carton label.  This was added for reprint purposes.
      *  12/19/12 MCA  Add Dick's partial-case label.
      *  03/18/15 MCA  New printer hardware.  Closing program after each
      *                label print to fix print errors.
      *  08/04/15 MCA  Dick's Field and Stream use DSG label.
      *  02/29/16 MCA  Add "Heavy" label generation on request.
      *  08/02/16 MCA  Add Amazon "Heavy" label generation on request.
      *  01/16/17 MCA  Bartender interface.
      *  12/12/17 MCA  Allow a customer's item number and UPC to be printed on
      *                on the UCC-128 shipping label, instead of Hornady's
      *                item number.  This change was made specifically for
      *                Cabela's Herter's ammo.
      *  02/07/19 MCA  Convert Explosive (aka, Hazard) label from T.L. Ashford
      *                to Bartender.
      *  02/19/19 MCA  Close T.L. Ashford program after label print for all
      *                Grand Island and West DC workstations.
      *  10/21/20 MCA  Add Amazon "Single ASIN" label generation on request.
      *  02/16/21 MCA  Change explosive label address from 3625 to 8350.
      *  12/15/22 MCA  Always close (and reset) T.L. Ashford print program,
      *                (tla_DEMAND='Y'), to prevent a prior label's image that
      *                was generated from a different job/program from being
      *                backgrounded on any label produced through this program.
      *  02/05/24 MCA  Add group separator shipping label print.  This will
      *                handle TL Ashford labels.  This program and HYR0117 will
      *                need to be changed when shipping labels are switched to
      *                Bartender.
      **************************************************************************
      *
     FHYJSLBL   IF   E           K DISK
     FHDVCIT    IF   E           K DISK
     FHYPELBL   IF   E           K DISK
     FHYJPLBL   IF   E           K DISK    USROPN
     FHYPWSOQ   IF   E           K DISK    USROPN
     FHYPOQLP   IF   E           K DISK    USROPN
     FGUPTDAT   IF   E           K DISK
     F***GUPTDAT   IF   E           K DISK
     D********************************************************************

      /COPY QPRPSRC,HYR0117

      /DEFINE LblHaz
      /COPY QPRPSRC,HYR2816
      /UNDEFINE LblHaz

     D                SDS
     D ps_Wkstn              244    253
     D ps_User               254    263
     D ps_Job#               264    269
     D tla_MRGDTA    E DS          1024    EXTNAME(HYJSLBL)
     D tla_MRGDTA2   E DS          1024    EXTNAME(HYPELBL)
     D tla_MRGDTA3   E DS          1024    EXTNAME(HYJPLBL)
     D tla_MRGDTA4     DS          1024    INZ

      /COPY QPRPSRC,HYR2816DS3

     D wk_DatabaseID   S              2A   DTAARA(DATABASEID)                   Database ID
     D wk_DatabaseID2  S              2A

     D*** @TABL           S                   LIKE(TDTABL)
     D*** @KEY1           S                   LIKE(TDKEY1)
     D*** wk_LblQ#        S              1P 0
     D wk_ShpLblOutq   S             10A
     D wk_ShpXLblOutq  S             10A
     D wk_PltLblOutq   S             10A
     D wk_ChkItem      S             15A
     D wk_OEORD#       S              8P 0
     D wk_OEBLTO       S              7P 0
     D wk_CBITEM       S             15A
     D wk_CBCUST       S              7P 0
     D wk_CBCITEM      S             30A
     D wk_CBCUPC       S             12A
     D wk_CBIPL        S              1A
     D wk_HazLblFmt    S             20A   INZ('H-HAZLBL')
     D wk_HazLblQty    S              4P 0 INZ(1)
     D wk_Error        S             10I 0
     D wk_RC0117       S             10I 0
     D LBLPGM          S             10A
     D LBLPGM2         S             10A
     D kf_WSID         S                   LIKE(WOWSID)
     D kf_OQTY         S                   LIKE(WOOQTY)
     D kf_OUTQ         S                   LIKE(LPOUTQ)
     D Yes             C                   CONST('Y')
     D No              C                   CONST('N')
     D Only            C                   CONST('O')
     D Full            C                   CONST('F')
     D Partial         C                   CONST('P')
     D Mixed           C                   CONST('M')
     D GrpSep          C                   CONST('G')
     D MstCtn          C                   CONST('A')
     D Pallet          C                   CONST('T')
     D Heavy           C                   CONST('H')
     D HeavyAmz        C                   CONST('Z')
     D SnglASIN        C                   CONST('S')
     D*** WkstnAttr       C                   CONST('WKSTNATTR')
     C* Mainline *********************************************************
     C                   DO
     C*
     C* If no labels are to be generated, exit program.
     C*
     C                   IF        work_CrtnType = Pallet
     C***  PLTLBLOUTQ    IFEQ      '*NONE'
     C                   IF        wk_PltLblOutq = '*NONE'
     C                   LEAVE
     C                   ENDIF                                                  PLTLBLOUTQ='*NONE'
     C                   ELSE
     C***  SHPLBLOUTQ    IFEQ      '*NONE'
     C                   IF        wk_ShpLblOutq = '*NONE'
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF                                                  work_CrtnType=Pallet

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

      *
      * Bartendar interface.
      *
      /Free
       CHAIN ('BTWKSTN':ps_wkstn) GUPTDAT;
       IF %found(GUPTDAT);
         IF ((wk_DatabaseID = 'HY' or wk_DatabaseID = 'XX') and
                TDFF01 = 'Y')  or
            ((wk_DatabaseID = 'SS' or wk_DatabaseID = 'SX') and
                TDFF02 = 'Y')  or
            ((wk_DatabaseID = 'PV' or wk_DatabaseID = 'PX') and
                TDFF03 = 'Y');
           SELECT;
           WHEN work_CrtnType = Full  or  work_CrtnType = Partial  or
                 work_CrtnType = Mixed;
             IF work_ExplsLbl <> Only;
               wk_RC0117 = LblUCC128 (work_ORD#:work_ORL#:work_BLN#:
                                           work_CrtnType:1);
             ENDIF;                                                          // work_ExplsLbl<>Only
             IF work_ExplsLbl = Yes  or  work_ExplsLbl = Only;
           //  wk_RC0117 = LblHazard (work_ORD#:1);
             ENDIF;                                                          // work_ExplsLbl=Yes or
           WHEN work_CrtnType = MstCtn;
             IF work_ExplsLbl <> Only;
           //  wk_RC0117 = LblMstCtn (work_ORD#:1);
             ENDIF;                                                          // work_ExplsLbl<>Only
             IF work_ExplsLbl = Yes  or  work_ExplsLbl = Only;
           //  wk_RC0117 = LblHazard (work_ORD#:1);
             ENDIF;                                                          // work_ExplsLbl=Yes or
           WHEN work_CrtnType = Pallet;
             wk_RC0117 = LblPallet (work_ORD#:1);
           WHEN work_CrtnType = Heavy  or  work_CrtnType = HeavyAmz;
         //  wk_RC0117 = LblHeavy (1);
           ENDSL;
           IF wk_RC0117 <> *Zero;
             EVAL out_ErrCod = '1001';
           ENDIF;                                                            // wk_RC0117<>*Zero
           LEAVE;
         ENDIF;                                                              // wk_DatabaseID='HY'
       ENDIF;                                                                // %found(GUPTDAT)
      /End-Free

     C*
     C* Retrieve label data from database, and call T.L. Ashford program
     C* to print it.
     C*
     C                   MOVEL     '0000'        out_ErrCod
     C                   SELECT
     C     work_CrtnType WHENEQ    Full
     C***  SLKEY         CHAIN     HYJSLBL                            95
     C     SLKEY         SETLL     HYJSLBL
     C                   DOU       *IN95  or  S2JOBN = ps_JOB#
     C     SLKEY         READE     HYJSLBL                                95
     C                   ENDDO                                                  *IN95 or
     C     *IN95         IFEQ      *OFF
     C                   IF        SLBLTO = 52                                  Academy corporation
     C                   EVAL      S2CITM = *Blanks
     C                   ELSE
     C                   MOVE      SLSHTO        ##CUST
     C                   MOVEL     SLITEM        ##ITEM
     C     VCKEY         CHAIN     HDVCIT                             95
     C     *IN95         IFEQ      *ON
     C                   MOVE      SLBLTO        ##CUST
     C                   MOVEL     SLITEM        ##ITEM
     C     VCKEY         CHAIN     HDVCIT                             95
     C                   ENDIF                                                  *IN95=*ON
     C     *IN95         IFEQ      *OFF
     C***                MOVEL     VCVCIT        S2ITEM
     C***                MOVEL     SLIMDS        S2IMDS
     C                   MOVEL     VCVCIT        S2CITM
     C***                CLEAR                   S2UPC
     C                   ELSE
     C***                MOVEL     SLITEM        S2ITEM
     C***                MOVEL     SLIMDS        S2IMDS
     C                   MOVEL(P)  SLITEM        S2CITM
     C***                MOVEL     SLUPC         S2UPC
     C                   ENDIF                                                  *IN95=*OFF
     C                   ENDIF                                                  SLBLTO=52
     C                   MOVEL     SLITEM        S2ITEM
     C                   MOVEL     SLIMDS        S2IMDS
     C                   EVAL      wk_ChkItem = SLITEM
     C                   EXSR      $ChkCustItem
     C***                MOVEL(P)  'DCSHIP'      tla_LBLNAM
     C                   IF        SLBLTO = 337
     C                               or  SLBLTO = 536527
     C                   EVAL      tla_LBLNAM = 'DCUCCDSG'                      Full label - Dick's
     C                   ELSE
     C                   EVAL      tla_LBLNAM = SHPLBLNAMF                      Full label name
     C                   ENDIF                                                  SLBLTO=337
     C                   IF        work_ExplsLbl <> Only
     C                   EXSR      $PrintLabel
     C                   ENDIF                                                  work_ExplsLbl<>Only
     C                   IF        work_ExplsLbl = Yes
     C                               or  work_ExplsLbl = Only
     C                   MOVEL     'DCEXPLOS'    tla_LBLNAM
     C                   EXSR      $PrintLabel2
     C                   ENDIF                                                  work_ExplsLbl=Yes
     C                   ELSE
     C                   MOVEL     '0001'        out_ErrCod                     Record not found
     C                   ENDIF                                                  *IN95=*OFF
     C     work_CrtnType WHENEQ    Partial
     C     work_ORD#     SETLL     HYJSLBL
     C                   DOU       *IN95  or  S2JOBN = ps_JOB#
     C     work_ORD#     READE     HYJSLBL                                95
     C                   ENDDO                                                  *IN95 or
     C     *IN95         IFEQ      *OFF
     C                   CLEAR                   SLORL#
     C                   CLEAR                   SLBLN#
     C                   CLEAR                   SLBLKT
     C                   CLEAR                   SLITEM
     C                   CLEAR                   SLIMDS
     C***                CLEAR                   SLUPC
     C                   CLEAR                   SLQTY
     C                   IF        SLBLTO = 52                                  Academy corporation
     C                   EVAL      S2CITM = *Blanks
     C                   ELSE
     C                   MOVE      SLSHTO        ##CUST
     C                   MOVEL     S2ITEM        ##ITEM
     C     VCKEY         CHAIN     HDVCIT                             95
     C     *IN95         IFEQ      *ON
     C                   MOVE      SLBLTO        ##CUST
     C                   MOVEL     S2ITEM        ##ITEM
     C     VCKEY         CHAIN     HDVCIT                             95
     C                   ENDIF                                                  *IN95=*ON
     C     *IN95         IFEQ      *OFF
     C***                MOVEL     VCVCIT        S2ITEM
     C***                CLEAR                   S2UPC
     C                   MOVEL     VCVCIT        S2CITM
     C                   ELSE
     C                   MOVEL(P)  S2ITEM        S2CITM
     C                   ENDIF                                                  *IN95=*OFF
     C                   ENDIF                                                  SLBLTO=52
     C                   EVAL      wk_ChkItem = S2ITEM
     C                   EXSR      $ChkCustItem
     C***                MOVEL(P)  'DCSHIPP'     tla_LBLNAM
     C                   IF        SLBLTO = 337
     C                               or  SLBLTO = 536527
     C                   EVAL      tla_LBLNAM = 'DCUCCDSP'                      Partial label-Dick's
     C                   ELSE
     C                   EVAL      tla_LBLNAM = SHPLBLNAMP                      Partial label name
     C                   ENDIF                                                  SLBLTO=337
     C                   IF        work_ExplsLbl <> Only
     C                   EXSR      $PrintLabel
     C                   ENDIF                                                  work_ExplsLbl<>Only
     C                   IF        work_ExplsLbl = Yes
     C                               or  work_ExplsLbl = Only
     C                   MOVEL     'DCEXPLOS'    tla_LBLNAM
     C                   EXSR      $PrintLabel2
     C                   ENDIF                                                  work_ExplsLbl=Yes
     C                   ELSE
     C                   MOVEL     '0001'        out_ErrCod                     Record not found
     C                   ENDIF                                                  *IN95=*OFF
     C     work_CrtnType WHENEQ    Mixed
     C     work_ORD#     SETLL     HYJSLBL
     C                   DOU       *IN95  or  S2JOBN = ps_JOB#
     C     work_ORD#     READE     HYJSLBL                                95
     C                   ENDDO                                                  *IN95 or
     C     *IN95         IFEQ      *OFF
     C                   CLEAR                   SLORL#
     C                   CLEAR                   SLBLN#
     C                   CLEAR                   SLBLKT
     C                   CLEAR                   SLITEM
     C                   CLEAR                   SLIMDS
     C***                CLEAR                   SLUPC
     C                   CLEAR                   SLQTY
     C                   CLEAR                   S2ITEM
     C                   CLEAR                   S2IMDS
     C                   CLEAR                   S2CITM
     C                   CLEAR                   S2UPC
     C                   CLEAR                   S2QTY
     C***                MOVEL(P)  'DCSHIPM'     tla_LBLNAM
     C                   EVAL      tla_LBLNAM = SHPLBLNAMM                      Mixed label name
     C                   IF        work_ExplsLbl <> Only
     C                   EXSR      $PrintLabel
     C                   ENDIF                                                  work_ExplsLbl<>Only
     C                   IF        work_ExplsLbl = Yes
     C                               or  work_ExplsLbl = Only
     C                   MOVEL     'DCEXPLOS'    tla_LBLNAM
     C                   EXSR      $PrintLabel2
     C                   ENDIF                                                  work_ExplsLbl=Yes
     C                   ELSE
     C                   MOVEL     '0001'        out_ErrCod                     Record not found
     C                   ENDIF                                                  *IN95=*OFF
     C     work_CrtnType WHENEQ    GrpSep
     C     work_ORD#     SETLL     HYJSLBL
     C                   DOU       *IN95  or  S2JOBN = ps_JOB#
     C     work_ORD#     READE     HYJSLBL                                95
     C                   ENDDO                                                  *IN95 or
     C     *IN95         IFEQ      *OFF
     C                   CLEAR                   SLORL#
     C                   CLEAR                   SLBLN#
     C                   CLEAR                   SLBLKT
     C                   CLEAR                   SLITEM
     C                   CLEAR                   SLIMDS
     C                   CLEAR                   SLQTY
     C                   IF        SLBLTO = 52                                  Academy corporation
     C                   EVAL      S2CITM = *Blanks
     C                   ELSE
     C                   MOVE      SLSHTO        ##CUST
     C                   MOVEL     S2ITEM        ##ITEM
     C     VCKEY         CHAIN     HDVCIT                             95
     C     *IN95         IFEQ      *ON
     C                   MOVE      SLBLTO        ##CUST
     C                   MOVEL     S2ITEM        ##ITEM
     C     VCKEY         CHAIN     HDVCIT                             95
     C                   ENDIF                                                  *IN95=*ON
     C     *IN95         IFEQ      *OFF
     C                   MOVEL     VCVCIT        S2CITM
     C                   ELSE
     C                   MOVEL(P)  S2ITEM        S2CITM
     C                   ENDIF                                                  *IN95=*OFF
     C                   ENDIF                                                  SLBLTO=52
     C                   EVAL      wk_ChkItem = S2ITEM
     C                   EXSR      $ChkCustItem
     C                   EVAL      tla_LBLNAM = SHPLBLNAMG                      Group separator name
     C                   EVAL      tla_LBLDEV = wk_ShpXLblOUtq
     C                   EXSR      $PrintLabel
     C                   EVAL      tla_LBLDEV = wk_ShpLblOutq
     C                   ELSE
     C                   MOVEL     '0001'        out_ErrCod                     Record not found
     C                   ENDIF                                                  *IN95=*OFF
     C     work_CrtnType WHENEQ    MstCtn
     C     work_ORD#     SETLL     HYJSLBL
     C                   DOU       *IN95  or  S2JOBN = ps_JOB#
     C     work_ORD#     READE     HYJSLBL                                95
     C                   ENDDO                                                  *IN95 or
     C     *IN95         IFEQ      *OFF
     C                   CLEAR                   SLORL#
     C                   CLEAR                   SLBLN#
     C                   CLEAR                   SLBLKT
     C                   CLEAR                   SLITEM
     C                   CLEAR                   SLIMDS
     C                   CLEAR                   SLQTY
     C                   CLEAR                   S2ITEM
     C                   CLEAR                   S2IMDS
     C                   CLEAR                   S2CITM
     C                   CLEAR                   S2UPC
     C                   CLEAR                   S2QTY
     C                   EVAL      tla_LBLNAM = SHPLBLNAMA                      Master ctn name
     C                   IF        work_ExplsLbl <> Only
     C                   EXSR      $PrintLabel
     C                   ENDIF                                                  work_ExplsLbl<>Only
     C                   IF        work_ExplsLbl = Yes
     C                               or  work_ExplsLbl = Only
     C                   MOVEL     'DCEXPLOS'    tla_LBLNAM
     C                   EXSR      $PrintLabel2
     C                   ENDIF                                                  work_ExplsLbl=Yes
     C                   ELSE
     C                   MOVEL     '0001'        out_ErrCod                     Record not found
     C                   ENDIF                                                  *IN95=*OFF
     C     work_CrtnType WHENEQ    Pallet
     C                   IF        not %open(HYJPLBL)
     C                   OPEN      HYJPLBL
     C                   ENDIF                                                  not %open(HYJPLBL)
     C***  work_ORD#     CHAIN     HYJPLBL
     C     work_ORD#     SETLL     HYJPLBL
     C                   DOU       %eof(HYJPLBL)  or  PLJOBN = ps_JOB#
     C     work_ORD#     READE     HYJPLBL
     C                   ENDDO                                                 %eof(HYJPLBL) or
     C***                IF        %found(HYJPLBL)
     C                   IF        not %eof(HYJPLBL)
     C***                MOVEL     PLTLBLOUTQ    tla_LBLDEV
     C                   EVAL      tla_LBLDEV = wk_PltLblOutq
     C                   IF        PLITEM = '*MIXED'
     C                   EVAL      tla_LBLNAM = 'DCPALETM'
     C                   ELSE
     C                   EVAL      tla_LBLNAM = 'DCPALLET'
     C                   ENDIF                                                  PLITEM='*MIXED'
     C***                EVAL      tla_PERSET = 3
     C                   EVAL      tla_PERSET = 1
     C                   EVAL      tla_LBLQTY = 3
     C                   EVAL      tla_DEMAND = 'Y'
     C                   EXSR      $PrintLabel3
     C***                MOVEL     SHPLBLOUTQ    tla_LBLDEV
     C                   EVAL      tla_LBLDEV = wk_ShpLblOutq
     C                   EVAL      tla_PERSET = 1
     C                   EVAL      tla_LBLQTY = 1
     C****               IF        ps_Wkstn = 'DCUPS1'  or
     C****                           ps_Wkstn = 'DCUPS2'  or
     C****                           ps_Wkstn = 'DCUPS3'  or
     C****                           ps_Wkstn = 'DCUPS4'  or
     C****                           ps_Wkstn = 'DCTRK1'  or
     C****                           ps_Wkstn = 'DCTRK2'  or
     C****                           ps_Wkstn = 'DCTRK3'  or
     C****                           ps_Wkstn = 'DCTRK4'
     C****               IF        %subst(ps_Wkstn:1:5) = 'DCUPS'  or
     C****                           %subst(ps_Wkstn:1:5) = 'DCTRK'  or
     C****                           %subst(ps_Wkstn:1:6) = 'DCLINE'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPARCEL'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPALLET'  or
     C****                           %subst(ps_Wkstn:1:9) = 'DCJACKPOT'
     C                   EVAL      tla_DEMAND = 'Y'
     C****               ELSE
     C****               EVAL      tla_DEMAND = 'D'
     C****               ENDIF                                                  ps_Wkstn='DCUPS1'
     C                   ELSE
     C                   MOVEL     '0001'        out_ErrCod                     Record not found
     C***                ENDIF                                                  %found(HYJPLBL)
     C                   ENDIF                                                  not %eof(HYJPLBL)
     C     work_CrtnType WHENEQ    Heavy
     C                   EVAL      tla_LBLNAM = 'DC-HEAVY'
     C                   EVAL      tla_DEMAND = 'Y'
     C                   EXSR      $PrintLabel4
     C****               IF        ps_Wkstn = 'DCUPS1'  or
     C****                           ps_Wkstn = 'DCUPS2'  or
     C****                           ps_Wkstn = 'DCUPS3'  or
     C****                           ps_Wkstn = 'DCUPS4'  or
     C****                           ps_Wkstn = 'DCTRK1'  or
     C****                           ps_Wkstn = 'DCTRK2'  or
     C****                           ps_Wkstn = 'DCTRK3'  or
     C****                           ps_Wkstn = 'DCTRK4'
     C****               IF        %subst(ps_Wkstn:1:5) = 'DCUPS'  or
     C****                           %subst(ps_Wkstn:1:5) = 'DCTRK'  or
     C****                           %subst(ps_Wkstn:1:6) = 'DCLINE'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPARCEL'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPALLET'  or
     C****                           %subst(ps_Wkstn:1:9) = 'DCJACKPOT'
     C                   EVAL      tla_DEMAND = 'Y'
     C****               ELSE
     C****               EVAL      tla_DEMAND = 'D'
     C****               ENDIF                                                  ps_Wkstn='DCUPS1'
     C     work_CrtnType WHENEQ    HeavyAmz
     C                   EVAL      tla_LBLNAM = 'DC-HEAMZ'
     C                   EVAL      tla_DEMAND = 'Y'
     C                   EXSR      $PrintLabel4
     C****               IF        ps_Wkstn = 'DCUPS1'  or
     C****                           ps_Wkstn = 'DCUPS2'  or
     C****                           ps_Wkstn = 'DCUPS3'  or
     C****                           ps_Wkstn = 'DCUPS4'  or
     C****                           ps_Wkstn = 'DCTRK1'  or
     C****                           ps_Wkstn = 'DCTRK2'  or
     C****                           ps_Wkstn = 'DCTRK3'  or
     C****                           ps_Wkstn = 'DCTRK4'
     C****               IF        %subst(ps_Wkstn:1:5) = 'DCUPS'  or
     C****                           %subst(ps_Wkstn:1:5) = 'DCTRK'  or
     C****                           %subst(ps_Wkstn:1:6) = 'DCLINE'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPARCEL'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPALLET'  or
     C****                           %subst(ps_Wkstn:1:9) = 'DCJACKPOT'
     C                   EVAL      tla_DEMAND = 'Y'
     C****               ELSE
     C****               EVAL      tla_DEMAND = 'D'
     C****               ENDIF                                                  ps_Wkstn='DCUPS1'
     C     work_CrtnType WHENEQ    SnglASIN                                     Amazon Single ASIN
     C                   EVAL      tla_LBLNAM = 'DC-SAAMZ'
     C                   EVAL      tla_PERSET = 1
     C                   EVAL      tla_LBLQTY = 2
     C                   EVAL      tla_DEMAND = 'Y'
     C                   EXSR      $PrintLabel4
     C                   EVAL      tla_LBLQTY = 1
     C****               IF        %subst(ps_Wkstn:1:5) = 'DCUPS'  or
     C****                           %subst(ps_Wkstn:1:5) = 'DCTRK'  or
     C****                           %subst(ps_Wkstn:1:6) = 'DCLINE'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPARCEL'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPALLET'  or
     C****                           %subst(ps_Wkstn:1:9) = 'DCJACKPOT'
     C                   EVAL      tla_DEMAND = 'Y'
     C****               ELSE
     C****               EVAL      tla_DEMAND = 'D'
     C****               ENDIF                                                  %subst(ps_Wkstn:1:5)
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C                   RETURN
     C/EJECT
     C********************************************************************
     C* Check for customer-item to print on the label.                   *
     C********************************************************************
     C     $ChkCustItem  BEGSR
      /Free
       wk_OEORD# = *Zero;
       wk_OEBLTO = *Zero;
       EXEC SQL select OEORD#, OEBLTO into :wk_OEORD#, :wk_OEBLTO
                from OEORHD
                where OEORD#=:work_ORD#;
       IF wk_OEORD# = work_ORD#;
         wk_CBITEM = *Blanks;
         wk_CBCUST = *Zero;
         wk_CBCITEM = *Blanks;
         wk_CBCUPC = *Blanks;
         wk_CBIPL = *Blank;
         EXEC SQL select CBITEM, CBCUST, CBCITEM, CBCUPC, CBIPL
                  into :wk_CBITEM, :wk_CBCUST, :wk_CBCITEM, :wk_CBCUPC,
                       :wk_CBIPL
                  from BARCUST
                  where CBITEM=:wk_ChkItem and CBCUST=:wk_OEBLTO;
         IF wk_CBITEM = wk_ChkItem  and  wk_CBCUST = wk_OEBLTO  and
              wk_CBCITEM <> *Blanks  and  wk_CBIPL = 'C';
           SLITEM = wk_CBCITEM;
           S2ITEM = wk_CBCITEM;
           IF wk_CBCUPC <> *Blanks;
             S2UPC = wk_CBCUPC;
           ENDIF;                                                            // wk_CBCUPC<>*Blanks
         ENDIF;                                                              // wk_CBITEM=wk_ChkItem
       ENDIF;                                                                // wk_OEORD#=work_ORD#
      /End-Free
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Call T.L. Ashford to print label.                                *
     C********************************************************************
     C     $PrintLabel   BEGSR
     C*
     C                   CALL      LBLPGM        LBLPGM_plist           99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     '0002'        out_ErrCod                     T.L.Ashford error
     C                   ENDIF                                                  *IN99=*ON
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Call T.L. Ashford to print label - Class I Explosives.           *
     C********************************************************************
     C**** $PrintLabel2  BEGSR
     C*
     C***  work_ORD#     CHAIN     HYPELBL                            95
     C**** ELKEY         CHAIN     HYPELBL                            95
     C****               IF        *IN95 = *OFF
     C****               CALL      LBLPGM        LBLPGM_plist2          99
     C**** *IN99         IFEQ      *ON
     C****               MOVEL     '0003'        out_ErrCod                     T.L.Ashford error
     C****               ENDIF                                                  *IN99=*ON
     C****               ELSE
     C****               MOVEL     '0004'        out_ErrCod                     No HYPELBL record
     C****               ENDIF
     C*
     C****               ENDSR
     C/EJECT
     C********************************************************************
     C* Create Bartender file to print label - Class I Explosives.       *
     C********************************************************************
     C     $PrintLabel2  BEGSR

     C     ELKEY         CHAIN     HYPELBL
     C                   IF        %found(HYPELBL)

     C                   CLEAR                   HYR2816DS_3
     C                   EVAL      pl3_Comp = wk_DatabaseID2
     C                   EVAL      pl3_Order# = work_ORD#
     C                   EVAL      pl3_Item# = ELITEM
     C                   EVAL      pl3_FrtCls1 = 'CARTRIDGES, SMALL ARMS'
     C                   EVAL      pl3_FrtCls2 = '1.4S UN0012'
     C                   EVAL      pl3_SFName = 'HORNADY MANUFACTURING COMPANY'
     C                   EVAL      pl3_SFAddr1 = '8350 W OLD POTASH HWY'
     C                   EVAL      pl3_SFAddr2 = 'ALDA, NE  68810'
     C                   EVAL      pl3_SFAddr3 = 'USA'
     C                   EVAL      pl3_SFAddr4 = 'PHONE #: 1-308-382-1390'
     C                   EVAL      pl3_GQHdg = 'GROSS QTY:'
     C                   EVALR     pl3_GrossQty = %editc(ELBWGT:'3') + ' KG'
     C                   EVAL      pl3_NQHdg = 'NET QTY:'
     C                   EVALR     pl3_NetQty = %editc(ELHMNQ:'3') + ' KG'
     C                   EVAL      pl3_NEQHdg = 'NEQ:'
     C                   EVALR     pl3_NEQ = %editc(ELPWGT:'3') + ' KG'
     C                   EVAL      pl3_SSCC = *Blanks

     C                   EVAL      wk_Error = LblHaz (HYR2816DS_3:wk_HazLblFmt:
     C                               wk_HazLblQty:ps_User)

     C                   IF        wk_Error <> *Zero
     C                   EVAL      out_ErrCod = '0003'                          LblHaz error
     C                   ENDIF                                                  wk_Error<>*Zero

     C                   ELSE
     C                   EVAL      out_ErrCod = '0004'                          No HYPELBL record
     C                   ENDIF                                                  %found(HYPELBL)

     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Call T.L. Ashford to print label.                                *
     C********************************************************************
     C     $PrintLabel3  BEGSR
     C*
     C                   CALL      LBLPGM2       LBLPGM_plist3          99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     '0002'        out_ErrCod                     T.L.Ashford error
     C                   ENDIF                                                  *IN99=*ON
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Call T.L. Ashford to print label.                                *
     C********************************************************************
     C     $PrintLabel4  BEGSR
     C*
     C                   CALL      LBLPGM        LBLPGM_plist4          99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     '0002'        out_ErrCod                     T.L.Ashford error
     C                   ENDIF                                                  *IN99=*ON
     C*
     C                   ENDSR
     C/EJECT
     C********************************************************************
     C* Initialization.                                                  *
     C********************************************************************
     C     *INZSR        BEGSR
     C* Entry Section ****************************************************
     C     *ENTRY        PLIST
     C     work_APID     PARM                    entparm_APID      2
     C     work_ORD#     PARM                    entparm_ORD#
     C     work_ORL#     PARM                    entparm_ORL#
     C     work_BLN#     PARM                    entparm_BLN#
     C     work_CrtnType PARM                    entparm_CrtnTy    1
     C     work_ExplsLbl PARM                    entparm_ExpLbl    1
     C                   PARM      out_ErrCod    entparm_ErrCod    4
     C* PLIST Section ****************************************************
     C     LBLPGM_plist  PLIST
     C                   PARM                    tla_LBLNAM        8
     C                   PARM                    tla_ACROSS        2 0
     C                   PARM                    tla_LBLDEV       10
     C                   PARM                    tla_LBLQTY        4 0
     C                   PARM                    tla_DEMAND        1
     C                   PARM                    tla_MRGDTA
     C                   PARM                    tla_PERSET        4 0

     C     LBLPGM_plist2 PLIST
     C                   PARM                    tla_LBLNAM
     C                   PARM                    tla_ACROSS
     C                   PARM                    tla_LBLDEV
     C                   PARM                    tla_LBLQTY
     C                   PARM                    tla_DEMAND
     C                   PARM                    tla_MRGDTA2
     C                   PARM                    tla_PERSET

     C     LBLPGM_plist3 PLIST
     C                   PARM                    tla_LBLNAM
     C                   PARM                    tla_ACROSS
     C                   PARM                    tla_LBLDEV
     C                   PARM                    tla_LBLQTY
     C                   PARM                    tla_DEMAND
     C                   PARM                    tla_MRGDTA3
     C                   PARM                    tla_PERSET

     C     LBLPGM_plist4 PLIST
     C                   PARM                    tla_LBLNAM
     C                   PARM                    tla_ACROSS
     C                   PARM                    tla_LBLDEV
     C                   PARM                    tla_LBLQTY
     C                   PARM                    tla_DEMAND
     C                   PARM                    tla_MRGDTA4
     C                   PARM                    tla_PERSET

     C* KLIST Section ****************************************************
     C     SLKEY         KLIST                                                  HYJSLBL
     C                   KFLD                    work_ORD#
     C                   KFLD                    work_ORL#
     C                   KFLD                    work_BLN#
     C     ELKEY         KLIST                                                  HYPELBL
     C                   KFLD                    work_ORD#
     C                   KFLD                    ps_JOB#
     C     VCKEY         KLIST                                                  HDVCIT
     C                   KFLD                    ##VCF
     C                   KFLD                    ##CUST
     C                   KFLD                    ##ITEM
     C     key_WO        KLIST                                                  HYPWSOQ
     C                   KFLD                    kf_WSID
     C                   KFLD                    kf_OQTY
     C***  TDKY1         KLIST                                                  GUPTDAT
     C***                KFLD                    @TABL
     C***                KFLD                    @KEY1
     C* Field Definition Section *****************************************
     C     *LIKE         DEFINE    entparm_APID  work_APID
     C     *LIKE         DEFINE    SLORD#        entparm_ORD#
     C     *LIKE         DEFINE    SLORD#        work_ORD#
     C     *LIKE         DEFINE    SLORL#        entparm_ORL#
     C     *LIKE         DEFINE    SLORL#        work_ORL#
     C     *LIKE         DEFINE    SLBLN#        entparm_BLN#
     C     *LIKE         DEFINE    SLBLN#        work_BLN#
     C     *LIKE         DEFINE    entparm_CrtnTywork_CrtnType
     C     *LIKE         DEFINE    entparm_ExpLblwork_ExplsLbl
     C     *LIKE         DEFINE    entparm_ErrCodout_ErrCod
     C     *LIKE         DEFINE    VCVCF         ##VCF
     C     *LIKE         DEFINE    VCVNCS        ##CUST
     C     *LIKE         DEFINE    VCITEM        ##ITEM
     C*
     C* Initialize work fields.
     C*
     C                   Z-ADD     1             tla_ACROSS
     C***                MOVEL     'PRT11'       tla_LBLDEV
     C                   Z-ADD     1             tla_LBLQTY
     C****               IF        ps_Wkstn = 'DCUPS1'  or
     C****                           ps_Wkstn = 'DCUPS2'  or
     C****                           ps_Wkstn = 'DCUPS3'  or
     C****                           ps_Wkstn = 'DCUPS4'  or
     C****                           ps_Wkstn = 'DCTRK1'  or
     C****                           ps_Wkstn = 'DCTRK2'  or
     C****                           ps_Wkstn = 'DCTRK3'  or
     C****                           ps_Wkstn = 'DCTRK4'
     C****               IF        %subst(ps_Wkstn:1:5) = 'DCUPS'  or
     C****                           %subst(ps_Wkstn:1:5) = 'DCTRK'  or
     C****                           %subst(ps_Wkstn:1:6) = 'DCLINE'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPARCEL'  or
     C****                           %subst(ps_Wkstn:1:8) = 'DCPALLET'  or
     C****                           %subst(ps_Wkstn:1:9) = 'DCJACKPOT'
     C                   EVAL      tla_DEMAND = 'Y'
     C****               ELSE
     C****               MOVEL     'D'           tla_DEMAND
     C****               ENDIF                                                  ps_Wkstn='DCUPS1'
     C                   Z-ADD     1             tla_PERSET
     C                   MOVEL     'C'           ##VCF
     C*
     C* Retrieve shipping label output queue.
     C*
     C***                EVAL      @TABL = WkstnAttr
     C***                EVAL      @KEY1 = ps_Wkstn
     C***  TDKY1         CHAIN     GUPTDAT
     C***                IF        %found(GUPTDAT)
     C***                EVAL      wk_LblQ# = TDNF01
     C***                ELSE
     C***                EVAL      wk_LblQ# = 1
     C***                ENDIF                                                  %found(GUPTDAT)
     C*
     C***  *DTAARA       DEFINE                  SHPLBLOUTQ       10
     C***  *DTAARA       DEFINE                  SHPLBLOQ2        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ3        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ4        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ5        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ6        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ7        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ8        10
     C***  *DTAARA       DEFINE                  SHPLBLOQ9        10
     C***  *DTAARA       DEFINE                  SHPLBLPGM        10
     C***  *DTAARA       DEFINE                  SHPLBLPGM2       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM3       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM4       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM5       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM6       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM7       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM8       10
     C***  *DTAARA       DEFINE                  SHPLBLPGM9       10
     C***  *DTAARA       DEFINE                  PLTLBLOUTQ       10
     C***  *DTAARA       DEFINE                  PLTLBLOQ2        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ3        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ4        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ5        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ6        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ7        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ8        10
     C***  *DTAARA       DEFINE                  PLTLBLOQ9        10
     C***                SELECT
     C***                WHEN       wk_LblQ# = 1
     C***                IN        SHPLBLOUTQ
     C***                MOVEL     SHPLBLOUTQ    tla_LBLDEV
     C***                IN        SHPLBLPGM
     C***                MOVEL     SHPLBLPGM     LBLPGM           10
     C***                IN        PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 2
     C***                IN        SHPLBLOQ2
     C***                MOVEL     SHPLBLOQ2     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ2     SHPLBLOUTQ
     C***                IN        SHPLBLPGM2
     C***                MOVEL     SHPLBLPGM2    LBLPGM
     C***                IN        PLTLBLOQ2
     C***                MOVEL     PLTLBLOQ2     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 3
     C***                IN        SHPLBLOQ3
     C***                MOVEL     SHPLBLOQ3     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ3     SHPLBLOUTQ
     C***                IN        SHPLBLPGM3
     C***                MOVEL     SHPLBLPGM3    LBLPGM
     C***                IN        PLTLBLOQ3
     C***                MOVEL     PLTLBLOQ3     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 4
     C***                IN        SHPLBLOQ4
     C***                MOVEL     SHPLBLOQ4     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ4     SHPLBLOUTQ
     C***                IN        SHPLBLPGM4
     C***                MOVEL     SHPLBLPGM4    LBLPGM
     C***                IN        PLTLBLOQ4
     C***                MOVEL     PLTLBLOQ4     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 5
     C***                IN        SHPLBLOQ5
     C***                MOVEL     SHPLBLOQ5     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ5     SHPLBLOUTQ
     C***                IN        SHPLBLPGM5
     C***                MOVEL     SHPLBLPGM5    LBLPGM
     C***                IN        PLTLBLOQ5
     C***                MOVEL     PLTLBLOQ5     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 6
     C***                IN        SHPLBLOQ6
     C***                MOVEL     SHPLBLOQ6     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ6     SHPLBLOUTQ
     C***                IN        SHPLBLPGM6
     C***                MOVEL     SHPLBLPGM6    LBLPGM
     C***                IN        PLTLBLOQ6
     C***                MOVEL     PLTLBLOQ6     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 7
     C***                IN        SHPLBLOQ7
     C***                MOVEL     SHPLBLOQ7     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ7     SHPLBLOUTQ
     C***                IN        SHPLBLPGM7
     C***                MOVEL     SHPLBLPGM7    LBLPGM
     C***                IN        PLTLBLOQ7
     C***                MOVEL     PLTLBLOQ7     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 8
     C***                IN        SHPLBLOQ8
     C***                MOVEL     SHPLBLOQ8     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ8     SHPLBLOUTQ
     C***                IN        SHPLBLPGM8
     C***                MOVEL     SHPLBLPGM8    LBLPGM
     C***                IN        PLTLBLOQ8
     C***                MOVEL     PLTLBLOQ8     PLTLBLOUTQ
     C***                WHEN       wk_LblQ# = 9
     C***                IN        SHPLBLOQ9
     C***                MOVEL     SHPLBLOQ9     tla_LBLDEV
     C***                MOVEL     SHPLBLOQ9     SHPLBLOUTQ
     C***                IN        SHPLBLPGM9
     C***                MOVEL     SHPLBLPGM9    LBLPGM
     C***                IN        PLTLBLOQ9
     C***                MOVEL     PLTLBLOQ9     PLTLBLOUTQ
     C***                OTHER
     C***                IN        SHPLBLOUTQ
     C***                MOVEL     SHPLBLOUTQ    tla_LBLDEV
     C***                IN        SHPLBLPGM
     C***                MOVEL     SHPLBLPGM     LBLPGM
     C***                IN        PLTLBLOUTQ
     C***                ENDSL

     C                   IF        not %open(HYPWSOQ)
     C                   OPEN      HYPWSOQ
     C                   ENDIF                                                  not %open(HYPWSOQ)

      *
      * Retrieve shipping label output queue.
      *
     C                   EVAL      kf_WSID = ps_Wkstn
     C                   EVAL      kf_OQTY = 'SHIPCTNLBL'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      kf_WSID = '*DFT'
     C     key_WO        CHAIN     HYPWSOQ
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      WOOUTQ = '*NONE'
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   EVAL      wk_ShpLblOutq = WOOUTQ
     C                   EVAL      tla_LBLDEV = WOOUTQ

      *
      * Retrieve extra shipping label output queue.
      *
     C                   EVAL      kf_WSID = ps_Wkstn
     C                   EVAL      kf_OQTY = 'SHIPXCTLBL'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      kf_WSID = '*DFT'
     C     key_WO        CHAIN     HYPWSOQ
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      WOOUTQ = '*NONE'
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   EVAL      wk_ShpXLblOutq = WOOUTQ

      *
      * Retrieve pallet label output queue.
      *
     C                   EVAL      kf_WSID = ps_Wkstn
     C                   EVAL      kf_OQTY = 'PALLETLBL'
     C     key_WO        CHAIN     HYPWSOQ
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      kf_WSID = '*DFT'
     C     key_WO        CHAIN     HYPWSOQ
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   IF        not %found(HYPWSOQ)
     C                   EVAL      WOOUTQ = '*NONE'
     C                   ENDIF                                                  not %found(HYPWSOQ)
     C                   EVAL      wk_PltLblOutq = WOOUTQ

     C                   CLOSE     HYPWSOQ

     C                   IF        not %open(HYPOQLP)
     C                   OPEN      HYPOQLP
     C                   ENDIF                                                  not %open(HYPOQLP)

      *
      * Retrieve shipping label program.
      *
     C                   EVAL      kf_OUTQ = wk_ShpLblOutq
     C     kf_OUTQ       CHAIN     HYPOQLP
     C                   IF        not %found(HYPOQLP)
     C                   EVAL      kf_OUTQ = '*DFT'
     C     kf_OUTQ       CHAIN     HYPOQLP
     C                   ENDIF                                                  not %found(HYPOQLP)
     C                   IF        not %found(HYPOQLP)
     C                   EVAL      LPLPGM = 'Z4XI1024'
     C                   ENDIF                                                  not %found(HYPOQLP)
     C                   EVAL      LBLPGM = LPLPGM

      *
      * Retrieve pallet label program.
      *
     C                   IF        wk_PltLblOutq = wk_ShpLblOutq
     C                   EVAL      LBLPGM2 = LBLPGM
     C                   ELSE
     C                   EVAL      kf_OUTQ = wk_PltLblOutq
     C     kf_OUTQ       CHAIN     HYPOQLP
     C                   IF        not %found(HYPOQLP)
     C                   EVAL      kf_OUTQ = '*DFT'
     C     kf_OUTQ       CHAIN     HYPOQLP
     C                   ENDIF                                                  not %found(HYPOQLP)
     C                   IF        not %found(HYPOQLP)
     C                   EVAL      LPLPGM = 'Z4XI1024'
     C                   ENDIF                                                  not %found(HYPOQLP)
     C                   EVAL      LBLPGM2 = LPLPGM
     C                   ENDIF                                                  wk_PltLblOutq=wk_Shp

     C*
     C* Retrieve shipping label names.
     C*
     C     *DTAARA       DEFINE                  SHPLBLNAMF       10
     C     *DTAARA       DEFINE                  SHPLBLNAMP       10
     C     *DTAARA       DEFINE                  SHPLBLNAMM       10
     C     *DTAARA       DEFINE                  SHPLBLNAMG       10
     C     *DTAARA       DEFINE                  SHPLBLNAMA       10
     C                   IN        SHPLBLNAMF
     C                   IN        SHPLBLNAMP
     C                   IN        SHPLBLNAMM
     C                   IN        SHPLBLNAMG
     C                   IN        SHPLBLNAMA
     C*
     C                   ENDSR
     C/EJECT
