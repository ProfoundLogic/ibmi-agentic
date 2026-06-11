     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      //
      //*************************************************************************************
     H/TITLE F0374R                                                                        *
      //*************************************************************************************
      //  Description:  Screen to display the detail from FTPFP                             *
      //                                                                                    *
      //  Created: 04/04/16   Programmer C:212   Project:OK022                              *
      //*************************************************************************************
      //  Revisions:
      //
      //
      //*************************************************************************************
      //
     Ff0374f    cf   e             workstn
      //
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Ffaeul     if   e           k disk
     Fftdgl     if   e           k disk
     Fftgtl     if   e           k disk
     Ffmsvl1ok  if   e           k disk
     Ffdcal18   if   e           k disk
     Fftrtl1    if   e           k disk
      //
      //
     D OPTNM           c                   CONST('DRG Payment Information')
     D COMAPI          c                   CONST('SA005R')
      //
     D ##cpt4          s              5    inz('DRG')
     D ##PDiem         s                   like(RTpdiem) inz
     D kkCo#           s                   like(SVco#) inz
     D kkEfDt          s                   like(EUdcDt) inz
     D xxDrg#          s              5A
      //
     D                 ds
     D kkcmsv                        11
     D  xxsvcd                        3  0 overlay(kkcmsv)
     D  xxcpt4                        5    overlay(kkcmsv:4)
     D  xxunit                        3  0 overlay(kkcmsv:9)
     D*
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
      //
     DPSDS           ESDS                  Extname(SZQ1P)
     d/copy sPrDate
      //
     C     *like         define    CAAST         ##ST
     C     *like         define    EUDCDT        ##EFDT
      //
     C     *entry        plist
     C                   parm                    ##CO#             3 0
     C                   parm                    ##SUB#            5 0
     C                   parm                    ##FND             3 0
     C                   parm                    ##FYR             3 0
     C                   parm                    ##CASE            7 0
     C                   parm                    ##EOB#            9 0
      //
     C     dtcymd1       PLIST
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      *Blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      //
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C     euky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
      //
     C     svky          klist
     C                   kfld                    kkCo#
     C                   kfld                    kkcmSv
     C                   kfld                    kkEfdt
      //
     C     svky1         klist
     C                   kfld                    kkCo#
     C                   kfld                    kkcmSv
      //
     C     ftky          klist
     C                   kfld                    @@prov#
     C                   kfld                    ##efdt
      //
     C     ftky1         klist
     C                   kfld                    @@prov#
      //
     C     fdky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C                   kfld                    ##CPT4

     C     rtky          klist
     C                   kfld                    ##ST
     C                   kfld                    ##EFDT

     C     rtky1         klist
     C                   kfld                    ##ST

     C     compl         plist
     C                   parm                    ##CO#
     C                   parm                    OPTNM1           40
      //
      //----------------------------------------------------------------------
      //  MAINLINE CODE
      //----------------------------------------------------------------------
      //
     C                   CALL      COMAPI        COMPL
      /FREE
       kkCo# = ##Co#;
       exsr pgmstr;
       //
       chain(n) euky peu01;
       *IN99 = NOT %FOUND;
       if *in99 = *off;
       xxDrg# = %trim(%char(EUdrg#));
       Chain (kkCO#:'OKCD':xxDrg#) ftgtl;
       if %found;
         isCatastro = *on;
       else;
         isCatastro = *off;
       endif;
      /END-FREE
     C                   Z-add     eudrg#        @@drg#            5 0
     C                   movel     eumcar        @@prov#          10
     C                   Z-Add     eudcdt        ##EFDT
      //
     C                   Z-Add     eudcdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         @@eudt#           8 0
      /FREE
         endif;
         //
         //          Get FDCAP
         //          ------------------------------------------------------
         Chain fdky PCA01;
         *IN60 = NOT %FOUND;
         IF *in60 = *off;
           ##ST = CAAST;
           SCDRPY  =  CAPAID;
           SCTADJ = CAPAID;
         ENDIF;

         SETGT RTKY PRT01;
         READPE(N) RTKY1 PRT01;
         DOW NOT %EOF;
           IF RTTYPE = 'I';
             ##pdiem = RTPDiem;
             LEAVE;
           ELSE;
             READPE(N) RTKY1 PRT01;
           ENDIF;
         ENDDO;
         //
         SCPDIEM = ##PDIEM;
         //
         //          ------------------------------------------------------
         //          Get FMSVL1OK
         //          ------------------------------------------------------
         kkEfDt = EUdcDt;
         xxSvCd = EUdrg#;
         xxCpt4 = *blanks;
         xxUnit = 0;

         setgt svky psv01;
         readpe svky1 psv01;
         If not %eof (fmsvl1ok);
           scRelW = SVrelW;
           scEfDt=dtMDYY(SVEfDt);
         EndIf;
         //
         //          Get from FAEUP
         //          ------------------------------------------------------
         SCDRG = EUDRG#;
         scDcDt=dtMDYY(EUdcDt);
      /END-FREE
     C                   MOVEL     EUDGDS        SCDGDS
      //
      //          Show Screen
      //          ------------------------------------------------------
      /FREE
         exfmt Scr01;
       //
      /END-FREE
     C                   move      *on           *inlr
      //
      //================================================================
      // PGMSTR - Start of program processing
      //================================================================
      /FREE
       BEGSR PGMSTR;
         //
      /END-FREE
     C                   MOVEL(P)  OPTNM         ##DATA
      //
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      //
     C                   MOVEL     ##DATA        SCPRDS
      //
     C                   MOVEL(P)  OPTNM1        ##DATA1
      //
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA1          80
     C                   PARM      40            ##LENG1           3 0
      //
     C                   MOVEL     ##DATA1       SCCONM
      //
      /FREE
       ENDSR;                                                                 //PGMSTR
       //================================================================
      /END-FREE
