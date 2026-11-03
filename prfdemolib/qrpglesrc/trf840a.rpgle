      // @MBRTEXT: Retrieve Allocated Qty's for calling program
      *
      *       Retrieve Allocated Qty's from TrfDtl for program calling this program
      *
      *       Fox - 3/28/05
      *
      *       this is an attempted to standardize allocated qty's across the board for
      *       for interactive programs. Currently, some pgms are getting allocated qty's
      *       from invbal/ibartq which is updated twice a day via wrkjobscde.
      **********************************************************************
      *                    Modification History
      *
      *   3/17/17  Grubb  Add WH as a parameter and key field for TRFDTL.
      *
      *   5/15/17: PRYOR - Program was reading TRFDTL11 with just sku
      *            number key instead of sku/whs.
      *
0001  *   4/30/25: Stacy Jarvis - RTC  allocated qty needs to be allocated
0001  *            and shipped transfers
      *
      **********************************************************************
      *
      * codermake: compile options from the PRFDEMOLIB README (CRTBNDRPG
      * DFTACTGRP(*NO) ACTGRP(*CALLER)) carried in the source.
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H OPTION(*NODEBUGIO)
0001 F***TrfDtl11  IF   E           K DISK
0001 FTrfDtll2  IF   E           K DISK
0001 FTRFZSTL3  IF   E           K DISK    PREFIX(ZST_)
     D*
     D                SDS
     D  PROG                   1     10
     D  QPARMS           *PARMS
     D*
0001 D AlcQty          S              7  0
     D*
     C     *entry        plist
     C                   parm                    Inumbr            9 0
     C                   parm                    frLoc             5 0
     C                   parm                    AllocatedQty      7 0
      *
      **  Key list for TRFDTL11
     C     KTRFDTL11     KLIST
     C                   KFLD                    iNumbr
     C                   KFLD                    frLoc
      *
0001 c     KeyZST        klist
0001 c                   kfld                    TRFTLC
0001 c                   kfld                    TRFBCH
0001 c                   kfld                    INUMBR
      *
      /Free
                    AllocatedQty = 0;
                    setll KTRFDTL11 rTrfDtl;
                    Reade KTRFDTL11 rTrfDtl;
                    //DoW not %Eof(TrfDtl11);                                 //0001
                    //     AllocatedQty = AllocatedQty + TrfAlc;              //0001
                    //     Reade Inumbr rTrfDtl;
                    DoW not %Eof(TrfDtll2);                                   //0001
                      if TRFSTS = 'A' or                                      //0001
                         TRFSTS = 'S' and                                     //0001
                         TRFSHP < TRFALC;                                     //0001
                        //adjust trf allocation for zero shipments            //0001
                        Chain KeyZST TRFZSTL3;                                //0001
                        if %found(TRFZSTL3) and                               //0001
                           zst_TRZSRS  = ' ';                                 //0001
                          AlcQty = TRFALC - zst_TRFALC;                       //0001
                        else;                                                 //0001
                          AlcQty = TRFALC;                                    //0001
                        endif;                                                //0001
                        AllocatedQty += AlcQty - TRFSHP;                      //0001
                      endif;                                                  //0001
                           Reade KTRFDTL11 rTrfDtl;
                    Enddo;

          *inlr = '1';
          return;