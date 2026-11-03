      // @MBRTEXT: Ticket Item Inventory - Variety Wholesalers web (EJS)
      *================================================================
      *
      *  Program........: TS0110
      *  Written by.....: M. Pryor
      *  Written for....: Variety Wholesalers, Inc.
      *  Date written...: February 2005
      *  Compiler.......: RPG/400
      *
      *  Purpose........: Create distribution for store from list of
      *                   available skus for a department. List is
      *                   sorted by sub-department.
      *
      *  03/10/05: PRYOR - Program was using IBARQT for reserve qty.
      *            Changed to use IBTOOQ.
      *
      *  03/24/05: Fox   - Users are complaining that reserve qty SHOULD
      *            contain IBARQT. Ibarqt is the summation of all allocated
      *            transfers from DC (9001). Updated with batch job twice a day (see
      *            job scheduler). Ibtooq reflects transfer on order INTO invbal
      *            location (ex 9001) not on order from location.
      *
      *  03/28/05: Fox   - Users don't like IBARQT because it's updated only twice a
      *            day. Wrote a trf840 as a utility pgm to retreive allocated qty's from
      *            trfdtlx6.
      *
      *  04/08/05: PRYOR - Subfile changed to show negative signs. This
      *            program changed to check for qtys > 9999 to prevent
      *            field size errors.
      *
      *  05/02/05: PRYOR - Check for field size overflow when calcu-
      *            lation pct sold.
      *
      *  12/07/05: Turn off subfile display if no records written.
      *
      *  03/21/06: PRYOR - Make sure sub-department totals don't
      *            exceed the maximum value of 9999 before moving
      *            them to the subfile record.
      *
      *  04/03/09: PRYOR - Include Ad sales (Ibasuy).
      *
      *  03/12/14: PRYOR - Change subroutine, "LoadSfl", to reset the
      *            total fields before rebuilding the subfile.
      *
      *  11/13/14: PRYOR - Enlarge some of the columns to get rid of
      *            the size over flow problem. Shipments, on hand  and
      *            sold columns changed from 4,0 to 5,0. The warehouse
      *            available changed to 6,0.
      *
      *  03/28/16: PRYOR - Filter out dept price skus.
      *
      *  10/21/16: PRYOR - Read TBLSTR instead of STRFIL. Get warehouse
      *            number from TBLSTR and use for accessing INVBAL to
      *            get warehouse on hand.
      *
      *  03/19/17: PRYOR - Work file, STR001WK, now has a whse number
      *            field. Put the warehouse number from TBLSTR in it.
      *
      *  03/21/17: PRYOR - Forgot to uncomment the call to QCMDEXC
      *            that submits CLTS0111.
      *
      *  05/08/17: PRYOR - Call TRF840A instead of TRF840. The former
      *            requires a warehouse number parameter.
      *
      *  01/21/18: PRYOR - Show pack and hold qty for the store's
      *            warehouse. Change display to wide screen format.
      *
v110  *  03/04/25: Angela Kearsley - RTC VWS110
v110  *            Added BL PHPF* files PHPFB1 & PHPFB2
      *
0001  *  05/20/25: Stacy Jarvis - RTC
0001  *            Pull correct price based on location/price zone
      *
PRF1  *  11/03/26: Demo build - F10 submits CLTS0111 with CURLIB set to
PRF1  *            the library TS0110 runs from, not JOBD(IMSJOB1).
      *
VW01  *  11/03/26: Web clone TS0110W (menu option 12). Same logic as
VW01  *            TS0110; display file TS0110WD is an EJS (agentic) Rich
VW01  *            Display driven by the Profound UI Open Access handler.
VW01  *            Changes, all tagged VW01:
VW01  *            - TS0110WD + INFDS + HANDLER('PROFOUNDUI(HANDLER)')
VW01  *            - EjsOut/EjsIn around each EXFMT: publish *IN01-99 to
VW01  *              the template; map the browser ACTION (or the INFDS AID
VW01  *              of a real key) onto *INKA/KB/KG/KH/KJ so the existing
VW01  *              function key tests are unchanged
VW01  *            - EjsRow before each WRITE SkuSfl: the row's display
VW01  *              indicators 10-15 travel with the row (heading/total)
VW01  *            - CFKEYS write removed (SKUCNT is part of SKUCTL)
VW01  *            - SFLCLR (*IN52) mirrored into the EJSCLR field
      *
      *================================================================

      * codermake: compile options from the PRFDEMOLIB README (CRTBNDRPG
      * DFTACTGRP(*NO) ACTGRP(*CALLER)) carried in the source.
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
VW01 FTs0110wd  CF   E             WORKSTN SFILE(SkuSfl:Rrn)
     F                                     USROPN
VW01 F                                     INFDS(EjsDs)
VW01 F                                     HANDLER('PROFOUNDUI(HANDLER)')
     FTblStr    IF   E           K DISK
     FInvMstZ3  IF   E           K DISK    PREFIX(Inv_)
     FInvBal    IF   E           K DISK    PREFIX(Bal_)
     FInvBal1   IF   E           K DISK    PREFIX(Ly_)
     F                                     RENAME(rInvBal:LyBalRec)
     FInvDpt    IF   E           K DISK
     FItmRtl    IF   E           K DISK    PREFIX(Rtl_)
     FRplPrf    IF   E           K DISK    PREFIX(Rpl_)
     FStr001Wk  UF A E           K DISK    USROPN
     FPhPf      IF   E           K DISK    USROPN
     FPhPfw2    IF   E           K DISK    USROPN RENAME(rPhPf:rPhPfw2)
v110 FPhPfb1    IF   E           K DISK    USROPN RENAME(rPhPf:rPhPfb1)
v110 FPhPfb2    IF   E           K DISK    USROPN RENAME(rPhPf:rPhPfb2)

VW01  * EJS (web) display support
VW01 D EjsDs           DS
VW01 D  EjsKey               369    369
VW01 D EjsAid          S              1
VW01 D EjsN            S              3S 0
VW01 D EjsAids         C                   CONST(x'3132333435363738393A3B3C-
VW01 D                                     B1B2B3B4B5B6B7B8B9BABBBC')
VW01 D EjsInP          S               *   INZ(%ADDR(*IN))
VW01 D EjsInA          S             99    BASED(EjsInP)

      * ...............................................................
      * : Get program name from program information data structure.   :
      * :.............................................................:
     D                SDS
     D  Prog                   1     10
PRF1 D  PgmLib                81     90

      * ...............................................................
      * : Define work variables.                                      :
      * :.............................................................:

     D DfsDpt          S              3S 0 INZ(0)
     D DfClas          S              3S 0 INZ(0)
     D DfsCls          S              3S 0 INZ(0)

     D WhsNo           S              5P 0 INZ(9001)
     D Rrn             S              4P 0 INZ(0)
     D LastRrn         S              4P 0 INZ(0)
     D RrNo            S              4P 0 INZ(0)
     D RecCnt          S              4P 0 INZ(0)
     D TotShp          S              7P 0 INZ(0)
     D TotSld          S              7P 0 INZ(0)
     D WhsAvl          S              7P 0 INZ(0)
     D SubDpt          S              3S 0 INZ(999)
     D Class           S              3S 0 INZ(999)
     D TckYn           S              1    INZ('Y')
     D AvlYn           S              1    INZ('Y')
     D DfSeas          S              3    INZ('ALL')
     D NewMbr          S              8
     D SeqNo           S              7
     D CmdTxt          S            100
     D CmdLen          S             15P 5
     D wQty            S              7P 0
     D PahQty          S              7P 0
     D NewQty          S              6P 0
     D Index           S              5P 0

      * ...............................................................
      * : Define qty fields as an array for convient processing.      :
      * :.............................................................:

     D Wqt             DS
     D PhQty
     D PhQty2
     D PhQty3
     D PhQty4
     D PhQty5
     D PhQ                            6S 0 OVERLAY(Wqt) DIM(5)

     D SfDes1          DS            15
     D   Mflg                 14     15A

     D SysDatx         DS             8
     D   SysDat                       8S 0

     D FinTotals       DS
     D   FluShp                       6S 0 INZ(0)
     D   FluOnh                       6S 0 INZ(0)
     D   FluSld                       6S 0 INZ(0)
     D   FluRes                       6S 0 INZ(0)
     D   FlpAvl                       7S 0 INZ(0)
     D   FlphQt                       7S 0 INZ(0)
     D   FlnewQ                       7S 0 INZ(0)

     D SubTotals       DS
     D   TluShp                       5S 0 INZ(0)
     D   TluOnh                       5S 0 INZ(0)
     D   TluSld                       5S 0 INZ(0)
     D   TluRes                       6S 0 INZ(0)
     D   TlpAvl                       7S 0 INZ(0)
     D   TlphQt                       7S 0 INZ(0)
     D   TlnewQ                       7S 0 INZ(0)

     D ClsTotals       DS
     D   CluShp                       5S 0 INZ(0)
     D   CluOnh                       5S 0 INZ(0)
     D   CluSld                       5S 0 INZ(0)
     D   CluRes                       6S 0 INZ(0)
     D   ClpAvl                       7S 0 INZ(0)
     D   ClphQt                       7S 0 INZ(0)
     D   ClnewQ                       7S 0 INZ(0)

      * ...............................................................
      * : Define command strings for opening work file member.        :
      * :.............................................................:

     D AddMbrCmd       S             35    INZ('ADDPFM FILE(STR001WK) -
     D                                     MBR(XXXXXXXX)')

     D OvrMbrCmd       S             35    INZ('OVRDBF FILE(STR001WK) -
     D                                     MBR(XXXXXXXX)')

     D DltOvrCmd       S             21    INZ('DLTOVR FILE(STR001WK)')

      * ...............................................................
      * : Define some useful indicators.                              :
      * :.............................................................:

     D Ticket_Item     S               N   INZ(*OFF)
     D FirstTime       S               N   INZ(*ON)
     D First_Sku       S               N   INZ(*ON)
     D Avail_Only      S               N   INZ(*ON)
     D Ticket_Only     S               N   INZ(*ON)
     D Selected_Sku    S               N   INZ(*ON)
     D F1_Pressed      S               N   INZ(*OFF)

      * ...............................................................
      * : Prototype definition for external call (CALLP).             :
      * :.............................................................:
     DQcmdExc          PR                  EXTPGM('QCMDEXC')
     D  CmdTxt                      100    OPTIONS(*VARSIZE)
     D  CmdLen                       15P 5

     DNxtSeq           PR                  EXTPGM('NXTSEQ')
     D  SeqNo                         7

     D*AllocatedQty    S              7  0
     D AlcQty          S              7  0

     DTrf840           PR                  EXTPGM('TRF840A')
     D  SkuNo                         9  0
     D  WhsNo                         5  0
     D  AlcQt                         7  0

     D DspSiz          PR                  EXTPGM('DSPSIZ')
     D   PgmName                     10

      * ...............................................................
      * : Define file access keys.                                    :
      * :.............................................................:

     C     DptKey        KLIST
     C                   KFLD                    DfDept
     C                   KFLD                    DfsDpt
     C                   KFLD                    DfClas
     C                   KFLD                    DfsCls

     C     ClsKey        KLIST
     C                   KFLD                    Inv_Idept
     C                   KFLD                    Inv_Isdept
     C                   KFLD                    Inv_Iclas

     C     BalKey        KLIST
     C                   KFLD                    DfStor
     C                   KFLD                    DfDept

     C     WhsKey        KLIST
     C                   KFLD                    Inv_Inumbr
     C                   KFLD                    WhsNo

     C     StrKey        KLIST
     C                   KFLD                    Inv_Inumbr
     C                   KFLD                    DfStor

     C     PahKey        KLIST
     C                   KFLD                    WhsNo
     C                   KFLD                    Inv_Inumbr

      *================================================================
      * MAIN - Prompt for store number and dept until user presses F1.
      *================================================================
      /FREE

        MONITOR;
VW01        OPEN Ts0110wd;
        ON-ERROR *FILE;
            CALLP DspSiz(Prog);
            *INLR = *ON;
            RETURN;
        ENDMON;

        DOW NOT F1_Pressed;
            DFYear = 'Y';
VW01        EXSR EjsOut;
            EXFMT Prompt;
VW01        EXSR EjsIn;
            IF *INKA;
                F1_Pressed = *ON;
                ITER;
            ENDIF;
            if DFYear = 'Y';
                DFYrd = 'TY & LY';
            else;
                DFYrd = 'TY Only';
            endif;
            *IN90 = *OFF;
            CHAIN DfStor rTblStr;
            IF NOT %FOUND;
                *IN90 = *ON;
                ITER;
            ENDIF;
            WhsTxt = 'WH' + %TRIM(%EDITW(StrWhs:'0    '));
            WhsNo = StrWhs;
            DfName = StCity;
            DfStat = StpVst;
            EXSR Open_PhPf;
            EXSR DspSkus;
        ENDDO;

VW01    CLOSE Ts0110wd;

        *INLR = *ON;

      /END-FREE

      *================================================================
      * This routine displays the subfile of skus.
      *================================================================
      /FREE
       BEGSR DspSkus;

           DfsDpt = 0;
           DfClas = 0;
           DfsCls = 0;
           *IN91 = *OFF;

           CHAIN DptKey rInvDpt;

           IF NOT %FOUND;
               DptNam = *BLANKS;
               *IN91 = *ON;
               LEAVESR;
           ENDIF;

           DfDnam = %SUBST(DptNam:1:25);

           EXSR LoadSfl;

           DOW NOT *INKA;
VW01   //      WRITE CfKeys;
VW01           EXSR EjsOut;
               EXFMT SkuCtl;
VW01           EXSR EjsIn;
               SELECT;
                   WHEN *INKA OR *INKB;   // F1 or F2 = exit
                       LEAVE;
                   WHEN *INKG;            // F7 = show beginning of SFL
                       Nbr = 1;
                   WHEN *INKH;            // F8 = show end of SFL
                       Nbr = LastRrn;
                   WHEN *INKJ;            // F10 = create distribution
VW01                   EXSR EjsOut;
                       EXFMT DistWin;
VW01                   EXSR EjsIn;
                       IF *INKA;
                           *INKA = *OFF;
                           ITER;
                       ENDIF;
                       EXSR CrtDist;
                       IF RecCnt > 0;
                           LEAVE;
                       ENDIF;
                   OTHER;                 // ENTER pressed
                       IF CsrPos <> 0;
                           Nbr = CsrPos;
                       ENDIF;
               ENDSL;
           ENDDO;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine loads the subfile of skus.
      *================================================================
      /FREE

       BEGSR LoadSfl;

           EXSR Filter;

           WRITE WaitWin;

           *IN50 = *OFF;
           *IN51 = *OFF;
           *IN52 = *ON;
VW01       EjsClr = *IN52;
           WRITE SkuCtl;
VW01       EjsClr = '0';
           *IN50 = *ON;
           *IN51 = *ON;
           *IN52 = *OFF;
           Rrn = 0;
           Nbr = 1;
           SkuCnt = 0;
           First_Sku = *ON;
           RESET FinTotals;
           RESET SubTotals;
           RESET ClsTotals;

           CHAIN DfDept rInvMst;

           DOW NOT %EOF(InvMstZ3);
               EXSR GetWhsAvl;
               EXSR GetPahQty;
               TotShp = Bal_IbHand + Bal_Ibrsuy + Bal_Ibasuy +
                        Ly_Ibrsuy + Ly_Ibasuy;
               EXSR Record_Select;
               IF Selected_Sku;
                   SELECT;
                   WHEN First_Sku;
                       EXSR SubBeg;
                       EXSR ClsBeg;
                       First_Sku = *OFF;
                   WHEN Inv_IsDept <> SubDpt;
                       EXSR ClsEnd;
                       EXSR SubEnd;
                       EXSR SubBeg;
                       EXSR ClsBeg;
                   WHEN Inv_Iclas <> Class;
                       EXSR ClsEnd;
                       EXSR ClsBeg;
                   OTHER;
                   ENDSL;
                   SfSku  = Inv_Inumbr;
                   SfDes1 = %SUBST(Inv_Idescr:1:15);
 0001              SELECT;
 0001              WHEN ZONNUM = 1;
 0001                SfPric = Rtl_Z1rtl;
 0001              WHEN ZONNUM = 2;
 0001                SfPric = Rtl_Z2rtl;
 0001              WHEN ZONNUM = 3;
 0001                SfPric = Rtl_Z3rtl;
 0001              OTHER;
                   SfPric = Rtl_RegPrc;
 0001              ENDSL;
                   SfPack = Inv_iMinPk;
                   IF %ABS(TotShp) < 99999;
                       SfuShp = TotShp;
                   ELSE;
                       IF TotShp < 0;
                           SfuShp = 0;
                       ELSE;
                           SfuShp = 99999;
                       ENDIF;
                   ENDIF;
                   IF %ABS(Bal_Ibhand) < 99999;
                       SfuOnh = Bal_Ibhand;
                   ELSE;
                       IF Bal_Ibhand < 0;
                           SfuOnh = 0;
                       ELSE;
                           SfuOnh = 99999;
                       ENDIF;
                   ENDIF;
                   TotSld = TotShp - Bal_Ibhand;
                   IF %ABS(TotSld) < 99999;
                       SfuSld = TotSld;
                   ELSE;
                       IF TotSld < 0;
                           SfuSld = 0;
                       ELSE;
                           SfuSld = 99999;
                       ENDIF;
                   ENDIF;
                   IF TotShp > 0;
                       IF (SfuSld/TotShp) * 100.0 <= 999.9;
                           EVAL(H) SfPcts = (SfuSld/TotShp) * 100.0;
                       ELSE;
                           SfPcts = 999.9;
                       ENDIF;
                   ELSE;
                       SfPcts = 0;
                   ENDIF;
                   SfuRes = Bal_Ibtooq;
                   SfpAvl = WhsAvl;
                   SfphQt = PahQty;
                   SfnewQ = NewQty;
                   FluShp += SfuShp;  // Final totals
                   FluOnh += SfuOnh;
                   Flusld += SfuSld;
                   FluRes += SfuRes;
                   FlpAvl += SfpAvl;
                   FlphQt += SfphQt;
                   FlnewQ += SfnewQ;
                   TluShp += SfuShp;  // Sub-dept totals
                   TluOnh += SfuOnh;
                   Tlusld += SfuSld;
                   TluRes += SfuRes;
                   TlpAvl += SfpAvl;
                   TlphQt += SfphQt;
                   TlnewQ += SfnewQ;
                   CluShp += SfuShp;  // Class totals
                   CluOnh += SfuOnh;
                   Clusld += SfuSld;
                   CluRes += SfuRes;
                   ClpAvl += SfpAvl;
                   ClphQt += SfphQt;
                   ClnewQ += SfnewQ;

                   CHAIN StrKey rRplPrf;

                   IF NOT %FOUND;
                      Mflg   = ' N';
                   ENDIF;
                   IF %FOUND;
                      Mflg   = ' Y';
                   ENDIF;
                   Rrn = rrn + 1;
VW01               EXSR EjsRow;
                   WRITE SkuSfl;
                   SkuCnt = SkuCnt + 1;
               ENDIF;
               READE DfDept rInvMst;
           ENDDO;

           IF Rrn > 0;
               EXSR ClsEnd;
               EXSR SubEnd;
               EXSR FinEnd;
           ELSE;
               *IN50 = *OFF;
           ENDIF;

           LastRrn = Rrn;

       ENDSR;

      /END-FREE

      *================================================================
      * Open the correct pack and hold file for the store's warehouse.
      *================================================================
      /FREE

       BEGSR Open_PhPf;

           IF %OPEN(PhPf);
               CLOSE PhPf;
           ELSEIF %OPEN(PhPfw2);
               CLOSE PhPfW2;
v110       ELSEIF %OPEN(PhPfb1);
v110           CLOSE PhPfb1;
v110       ELSEIF %OPEN(PhPfb2);
v100           CLOSE PhPfb2;
           ENDIF;

           IF WhsNo = 9001;
              OPEN PhPf;
           ELSEIF WhsNo = 9002;
              OPEN PhPfw2;
v110       ELSEIF WhsNo = 9151;
v110          OPEN PhPfb1;
v110       ELSEIF WhsNo = 9152;
v110          OPEN PhPfb2;
           ENDIF;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine gets information for each sku regarding warehouse
      * available qty, store on hand and total sales.
      *================================================================
      /FREE

       BEGSR GetWhsAvl;

           IF Inv_IminPk = 0;
               LEAVESR;
           ENDIF;

           CHAIN WhsKey rInvBal;

           IF NOT %FOUND;
               Bal_Ibhand = 0;
               Bal_Ibtooq = 0;
           ENDIF;

           // WhsAvl = %INT((Bal_Ibhand-Bal_Ibtooq)/Inv_Iminpk);

           AlcQty = 0;
           CALLP Trf840 (Inv_Inumbr:WhsNo:AlcQty);
           WhsAvl = %INT((Bal_Ibhand-Bal_Ibtooq-AlcQty)/Inv_Iminpk);

           IF WhsAvl < 0;
               WhsAvl = 0;
           ENDIF;

           IF Inv_Itkttr = '1' OR Inv_Itkttr = '3' or Inv_Itkttr = '4';
               Ticket_Item = *ON;
           ELSE;
               Ticket_Item = *OFF;
           ENDIF;

        // Get unit retail from ITMRTL file:

           CHAIN Inv_Inumbr Itmrtlr;

           IF NOT %FOUND;
               Rtl_RegPrc = 0.0;
0001           Rtl_Z1rtl  = 0.0;
0001           Rtl_Z2rtl  = 0.0;
0001           Rtl_Z3rtl  = 0.0;
           ENDIF;

        // Get store information from INVBAL:

           CHAIN StrKey rInvBal;

           IF NOT %FOUND;
               Bal_Ibhand = 0;
               Bal_Ibtooq = 0;
               Bal_Ibtiuy = 0;
               Bal_Ibrsuy = 0;
               Bal_Ibasuy = 0;
           ENDIF;

        // Get last year store information from INVBAL1:

           if DFYear = 'Y';
           CHAIN StrKey LyBalRec;

           IF NOT %FOUND;
               Ly_Ibrsuy = 0;
               Ly_Ibasuy = 0;
           ENDIF;
           else;
               Ly_Ibrsuy = 0;
               Ly_Ibasuy = 0;
           ENDIF;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine reads PHPFL1 to get the current pack and hold qty
      * for the warehouse of the store.
      *================================================================
      /FREE

       BEGSR GetPahQty;

           PahQty = 0;
           NewQty = 0;

           IF WhsNo = 9001;
               CHAIN Inv_Inumbr rPhPf;
           ELSEIF WhsNo = 9002;
               CHAIN Inv_Inumbr rPhPfw2;
v110       ELSEIF WhsNo = 9151;
v110           CHAIN Inv_Inumbr rPhPfb1;
v110       ELSEIF WhsNo = 9152;
v110           CHAIN Inv_Inumbr rPhPfb2;
           ENDIF;

           IF %FOUND;
               FOR Index = 1 TO 5;
                   wQty = PhQ(Index);
                   IF wQty <> 777777 AND wQty <> 999999;
                      PahQty += wQty;
                   ENDIF;
               ENDFOR;
               PahQty = %INT(PahQty/Inv_iMinPk);
               IF PahQty > 999999;
                   PahQty = 999999;
               ENDIF;
               NewQty = PhNew;
           ENDIF;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine determines whether a sku should be added to the
      * subfile based on parameters specified by the user.
      *================================================================
      /FREE

       BEGSR Record_Select;

           SELECT;
               WHEN Inv_Inumbr = Inv_Idept;
                   Selected_Sku = *OFF;
               WHEN Avail_Only AND WhsAvl <= 0;
                   Selected_Sku = *OFF;
               WHEN Ticket_Only AND Inv_Itkttr <> '1' AND Inv_Itkttr <> '3'
                   AND Inv_Itkttr <> '4';
                   Selected_Sku = *OFF;
               WHEN DfSeas <> 'ALL' AND Inv_Iseasn <> DfSeas;
                   Selected_Sku = *OFF;
               WHEN NOT Avail_Only AND TotShp = 0 AND WhsAvl = 0 AND
                 mflg = ' Y';
                  Selected_Sku = *ON;
               WHEN NOT Avail_Only AND TotShp = 0 AND WhsAvl = 0 AND
                 mflg = ' N';
                  Selected_Sku = *OFF;
               OTHER;
                   Selected_Sku = *ON;
           ENDSL;

       ENDSR;

      /END-FREE

      *================================================================
      *
      *================================================================
      /FREE

       BEGSR SubBeg;

           DfsDpt = Inv_Isdept;

           CHAIN DptKey rInvDpt;

           IF NOT %FOUND;
               DptNam = *BLANKS;
           ENDIF;

           Rrn = Rrn + 1;
           SfSku = Inv_Isdept;
           SfDes1 = DptNam;
           *IN10 = *ON;
           *IN11 = *ON;
           *IN13 = *ON;
VW01       EXSR EjsRow;
           WRITE SkuSfl;
           *IN10 = *OFF;
           *IN11 = *OFF;
           *IN13 = *OFF;
           SubDpt = Inv_Isdept;

           CLEAR SubTotals;

       ENDSR;

      /END-FREE

      *================================================================
      *
      *================================================================
      /FREE

       BEGSR ClsBeg;

           CHAIN ClsKey rInvDpt;

           IF NOT %FOUND;
               DptNam = *BLANKS;
           ENDIF;

           Rrn = Rrn + 1;
           SfSku = Inv_Iclas;
           SfDes1 = DptNam;
           *IN10 = *ON;
           *IN11 = *ON;
           *IN12 = *ON;
           *IN15 = *ON;
VW01       EXSR EjsRow;
           WRITE SkuSfl;
           *IN10 = *OFF;
           *IN11 = *OFF;
           *IN12 = *OFF;
           *IN15 = *OFF;
           Class = Inv_Iclas;

           CLEAR ClsTotals;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine creates a sub-department total record and writes
      * it to the subfile.
      *================================================================
      /FREE

       BEGSR SubEnd;

           CLEAR SkuSfl;
           SfDes1 = 'SubDpt Totals.:';
           SfSku  = SubDpt;

           IF TluShp <= 99999;
               SfuShp = TluShp;
           ELSE;
               SfuShp = 99999;
           ENDIF;

           IF TluOnh <= 99999;
               SfuOnh = TluOnh;
           ELSE;
               SfuOnh = 99999;
           ENDIF;

           IF TluSld <= 99999;
               SfuSld = TluSld;
           ELSE;
               SfuSld = 99999;
           ENDIF;

           IF TluRes <= 99999;
               SfuRes = TluRes;
           ELSE;
               SfuRes = 99999;
           ENDIF;

           IF TlpAvl <= 999999;
               SfpAvl = TlpAvl;
           ELSE;
               SfpAvl = 999999;
           ENDIF;

           IF TlphQt <= 999999;
               SfphQt = TlphQt;
           ELSE;
               SfphQt = 999999;
           ENDIF;

           IF SfuShp > 0;
             MONITOR;
               EVAL(H) SfPcts = (SfuSld/SfuShp) * 100.0;
             ON-ERROR;
               SfPcts = 999.9;
             ENDMON;
           ELSE;
               SfPcts = 0;
           ENDIF;

           *IN11 = *ON;
           *IN13 = *ON;
           Rrn = Rrn + 1;
VW01       EXSR EjsRow;
           WRITE SkuSfl;
           *IN11 = *OFF;
           *IN13 = *OFF;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine creates a class total record and writes it to the
      * subfile.
      *================================================================
      /FREE

       BEGSR ClsEnd;

           CLEAR SkuSfl;
           SfDes1 = 'Class Totals..:';
           SfSku  = Class;

           IF CluShp <= 99999;
               SfuShp = CluShp;
           ELSE;
               SfuShp = 99999;
           ENDIF;

           IF CluOnh <= 99999;
               SfuOnh = CluOnh;
           ELSE;
               SfuOnh = 99999;
           ENDIF;

           IF CluSld <= 99999;
               SfuSld = CluSld;
           ELSE;
               SfuSld = 99999;
           ENDIF;

           IF CluRes <= 99999;
               SfuRes = CluRes;
           ELSE;
               SfuRes = 99999;
           ENDIF;

           IF ClpAvl <= 999999;
               SfpAvl = ClpAvl;
           ELSE;
               SfpAvl = 999999;
           ENDIF;

           IF ClphQt <= 999999;
               SfphQt = ClphQt;
           ELSE;
               SfphQt = 999999;
           ENDIF;

           IF SfuShp > 0;
             MONITOR;
               EVAL(H) SfPcts = (SfuSld/SfuShp) * 100.0;
             ON-ERROR;
               Sfpcts = 999.9;
             ENDMON;
           ELSE;
               SfPcts = 0;
           ENDIF;

           *IN11 = *ON;
           *IN12 = *ON;
           Rrn = Rrn + 1;
VW01       EXSR EjsRow;
           WRITE SkuSfl;
           *IN11 = *OFF;
           *IN12 = *OFF;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine creates a final total record and writes
      * it to the subfile.
      *================================================================
      /FREE

       BEGSR FinEnd;

           CLEAR SkuSfl;
           SfDes1 = 'Final Totals..:';
           SfSku  = 0;

           IF FluShp <= 99999;
               SfuShp = FluShp;
           ELSE;
               SfuShp = 99999;
           ENDIF;

           IF FluOnh <= 99999;
               SfuOnh = FluOnh;
           ELSE;
               SfuOnh = 99999;
           ENDIF;

           IF FluSld <= 99999;
               SfuSld = FluSld;
           ELSE;
               SfuSld = 99999;
           ENDIF;

           IF FluRes <= 99999;
               SfuRes = FluRes;
           ELSE;
               SfuRes = 99999;
           ENDIF;

           IF FlpAvl <= 999999;
               SfpAvl = FlpAvl;
           ELSE;
               SfpAvl = 999999;
           ENDIF;

           IF FlphQt <= 999999;
               SfphQt = FlphQt;
           ELSE;
               SfphQt = 999999;
           ENDIF;

           IF SfuShp > 0;
             MONITOR;
               EVAL(H) SfPcts = (SfuSld/SfuShp) * 100.0;
             ON-ERROR;
               Sfpcts = 999.9;
             ENDMON;
           ELSE;
               SfPcts = 0;
           ENDIF;

           *IN11 = *ON;
           *IN14 = *ON;
           Rrn = Rrn + 1;
VW01       EXSR EjsRow;
           WRITE SkuSfl;
           *IN11 = *OFF;
           *IN14 = *OFF;

       ENDSR;

      /END-FREE


      *================================================================
      * This routine sets on or off some work indicators that control
      * whether or not a sku will be included.
      *================================================================
      /FREE

       BEGSR Filter;

           IF TckYn = 'Y';
               Ticket_Only = *ON;
           ELSE;
               Ticket_Only = *OFF;
           ENDIF;

           IF AvlYn = 'Y';
               Avail_Only = *ON;
           ELSE;
               Avail_Only = *OFF;
           ENDIF;

       ENDSR;

      /END-FREE

      *================================================================
      * This routine creates reads the subfile and writes a record to
      * STR001WK for every sku with a non-zero qty.  Then, CLTS0111 is
      * is submitted to create transfers.
      *================================================================
      /FREE

       BEGSR CrtDist;

           EXSR AddMbr;

           SysDatX = %CHAR(%DATE:*ISO0);
           RecCnt = 0;

           OPEN Str001Wk;

           FOR RrNo = 1 to LastRrn;
              CHAIN RrNo SkuSfl;
              IF %FOUND;
                  IF SfQty > 0;
                      StrNo = DfStor;
                      DptNo = DfDept;
                      SkuNo = SfSku;
                      OrQty = SfQty * SfPack;
                      OrDat = SysDat;
                      Type  = '2';
                      Str_Dkey = ' ';
                      Whse = WhsNo;
                      WRITE rStr001;
                      RecCnt = RecCnt + 1;
                  ENDIF;
               ENDIF;
           ENDFOR;

           CLOSE Str001Wk;

           CmdTxt = 'DLTOVR FILE(STR001WK)';
           CmdLen =  %LEN(%TRIM(CmdTxt));
           CALLP QcmdExc (CmdTxt:CmdLen);

           IF RecCnt = 0;
VW01           EXSR EjsOut;
               EXFMT NoneWin;
VW01           EXSR EjsIn;
               CmdTxt = 'RMVM FILE(STR001WK) MBR(' + NewMbr + ')';
               CmdLen =  %LEN(%TRIM(CmdTxt));
               CALLP QcmdExc (CmdTxt:CmdLen);
               LEAVESR;
           ENDIF;

      //PRF1: Production submits with JOBD(IMSJOB1) INLLIBL(*JOBD), a job
      //      description whose library list names the demo library. A task
      //      or demo library built by codermake has no IMSJOB1 (and the
      //      demo profile may not create one), so the submitted job gets
      //      the library TS0110 runs from as its current library instead.
      //CmdTxt = 'SBMJOB CMD(CALL PGM(CLTS0111) PARM(' + NewMbr +
      //         ')) JOB(CLTS0111) JOBD(IMSJOB1) INLLIBL(*JOBD)';
           CmdTxt = 'SBMJOB CMD(CALL PGM(CLTS0111) PARM(' + NewMbr +
                    ')) JOB(CLTS0111) CURLIB(' + %TRIM(PgmLib) + ')';
           CmdLen =  %LEN(%TRIM(CmdTxt));
           CALLP QcmdExc (CmdTxt:CmdLen);

       ENDSR;

      /END-FREE

      *================================================================
      * This routine adds a member to the work file, STR001WK.
      *================================================================
      /FREE

       BEGSR AddMbr;

           CALLP NxtSeq (SeqNo);

           NewMbr = 'T' + SeqNo;


           CmdTxt = 'ADDPFM FILE(STR001WK) MBR(' + NewMbr + ')';
           CmdLen =  %LEN(%TRIM(CmdTxt));
           CALLP QcmdExc (CmdTxt:CmdLen);

           CmdTxt = 'OVRDBF FILE(STR001WK) MBR(' + NewMbr + ')';
           CmdLen =  %LEN(%TRIM(CmdTxt));
           CALLP QcmdExc (CmdTxt:CmdLen);

       ENDSR;

      /END-FREE

VW01  *================================================================
VW01  * EJS (web) display support - added for TS0110W
VW01  *================================================================
VW01  /FREE

VW01   // Before EXFMT: indicators to the template, clear the action
VW01   BEGSR EjsOut;
VW01       Inds = EjsInA;
VW01       EjsClr = '0';
VW01       Action = *BLANKS;
VW01   ENDSR;

VW01   // After EXFMT: a browser button posts ACTION (F01..F24). A real
VW01   // keyboard key leaves it blank and the handler fills the INFDS AID.
VW01   // Either way, set the function key indicators this program tests.
VW01   BEGSR EjsIn;
VW01       EjsAid = EjsKey;
VW01       IF %SUBST(Action:1:1) = 'F'
VW01          AND %LEN(%TRIMR(Action)) > 1
VW01          AND %CHECK('0123456789':%TRIMR(%SUBST(Action:2))) = 0;
VW01           EjsN = %INT(%TRIMR(%SUBST(Action:2)));
VW01           IF EjsN >= 1 AND EjsN <= 24;
VW01               EjsAid = %SUBST(EjsAids:EjsN:1);
VW01           ENDIF;
VW01       ELSEIF Action = 'ENTER';
VW01           EjsAid = x'F1';
VW01       ENDIF;
VW01       *INKA = EjsAid = x'31';
VW01       *INKB = EjsAid = x'32';
VW01       *INKG = EjsAid = x'37';
VW01       *INKH = EjsAid = x'38';
VW01       *INKJ = EjsAid = x'3A';
VW01   ENDSR;

VW01   // Before WRITE SkuSfl: the row's display indicators 10-15
VW01   BEGSR EjsRow;
VW01       SfInd = %SUBST(EjsInA:10:6);
VW01   ENDSR;

VW01  /END-FREE

