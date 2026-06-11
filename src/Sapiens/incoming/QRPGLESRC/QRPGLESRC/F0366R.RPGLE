     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**************************************************************************************
     H/TITLE F0366R                                                                        *
     H**************************************************************************************
     H*  Description:  Screen to display the detail from FTPFP                             *
     H*                                                                                    *
     H*  Created: 11/28/08   Programmer C:191   Project:MS017                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  10/29/20  C:295  Project: MS039                                                   *
     H*                   MS- 10/01/20 Inpatient and Outlier Update                        *
     H*                                                                                    *
     H*  05/11/11  C:079  Support: 76850  MC133
     H*                   Prevent dividing by zero error
     A*                                                                                    *
     A*  09/27/10  C:227  Project: MS019                                                   *
     A*                   Fix the way to get data from FTDGP and FTIIP                     *
     A*                                                                                    *
     A*  01/29/10  C:XXX  Project: SY215                                                   *
     A*                   Retrieve brand Name                                              *
     A*                                                                                    *
     H*  01/13/09  C:227  Project: MS018                                                   *
     H*                   MS 10/1/2008 Inpat & Outlier Update                              *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Ff0366f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Ffaeul     if   e           K DISK
     Fftiil     if   e           K DISK
     Fftdgl     if   e           k DISK
     Ffdcal18   if   e           k DISK
     F*** BEG ADD *** 01/13/09 ****************************************
     FFTRTL1    if   e           k disk
     F*** END ADD *** 01/13/09 ****************************************
     F*** BEG ADD *** 10/29/20 ****************************************
     Ffminptl1msif   e           k disk
     F*** END ADD *** 10/29/20 ****************************************
      *
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D ##cpt4          s              5    INZ('DRG')
     D OPTNM           C                   CONST('DRG Payment Information')
     I*** BEGIN DEL/ADD ******* 01/29/10 ************************************
     D*** OPTNM1       C                   CONST('FISERV INSURANCE SOLUTIONS')
     D COMAPI          C                   CONST('SA005R')
     I*** END DEL/ADD ******* 01/29/10 **************************************
     D ##CMBR          S              6  2
     D ##OUTT          S              9  2
     D ##OUTL          S              9  2 INZ(0)
     D ##MSBR          S              9  2
     D XXDRPY          S                   like(SCDRPY)
     D*** Beg Add *** 10/29/20 ****************************************
     DkkProv           s             10
     DkkEfDt           s              7S 0
     D*** End Add *** 10/29/20 ****************************************
      *
     DPSDS           ESDS                  Extname(SZQ1P)
     C*
     C*** BEG ADD *** 01/13/09 ****************************************
     C     *like         define    CAAST         ##ST
     C     *like         define    EUDCDT        ##EFDT
     C     *like         define    CAOUTL        XXOUTL
     C*** END ADD *** 01/13/09 ****************************************
     C*
     C     *entry        plist
     C                   parm                    ##CO#             3 0
     C                   parm                    ##SUB#            5 0
     C                   parm                    ##FND             3 0
     C                   parm                    ##FYR             3 0
     C                   parm                    ##CASE            7 0
     C                   parm                    ##EOB#            9 0
     C***                parm                    ##conm           40
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *Blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     dtcymd1       PLIST
     C                   parm                    srdti             8 0
     C                   parm      '*YYMD'       srffmt            8
     C                   parm      *Blanks       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
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
     C*
     C     dgky          klist
     C                   kfld                    @@drg#
     C*** BEG ADD *** 09/27/10 ****************************************
     C                   kfld                    ##efdt
     C*
     C     dgky1         klist
     C                   kfld                    @@drg#
     C*** END ADD *** 09/27/10 ****************************************
     C*
     C     ftky          klist
     C                   kfld                    @@prov#
     C*** BEG ADD *** 09/27/10 ****************************************
     C                   kfld                    ##efdt
     C*
     C     ftky1         klist
     C                   kfld                    @@prov#
     C*** END ADD *** 09/27/10 ****************************************
     C*
     C     fdky          klist
     C                   kfld                    ##CO#
     C                   kfld                    ##SUB#
     C                   kfld                    ##FND
     C                   kfld                    ##FYR
     C                   kfld                    ##CASE
     C                   kfld                    ##EOB#
     C                   kfld                    ##CPT4
     C*** BEG ADD *** 01/13/09 ****************************************
     C     RTKY          KLIST
     C                   KFLD                    ##ST
     C                   KFLD                    ##EFDT
     C     RTKY1         KLIST
     C                   KFLD                    ##ST
     I*****************BEGIN ADD **** 01/26/10 ***********
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    OPTNM1           40
     I*****************END   ADD **** 01/26/10 ***********
     C*** END ADD *** 01/13/09 ****************************************
     C*
     C*----------------------------------------------------------------------
     C*  MAINLINE CODE
     C*----------------------------------------------------------------------
     C     euky          chain(n)  peu01                              99
     C                   if        *in99 = *off
     C                   Z-add     eudrg#        @@drg#            5 0
     C                   movel     eumcar        @@prov#          10
     C*** BEG ADD *** 01/13/09 ****************************************
     C                   Z-Add     eudcdt        ##EFDT
     C*** END ADD *** 01/13/09 ****************************************
     C*
     C                   Z-Add     eudcdt        srdti
     C                   call      'SRCVTDAT'    dtcymd2
     C                   Movel     srdto         @@eudt#           8 0
     C*
     C* Set CMS base rate and Outlier threshold according to discharge date
     C*** BEG CHG *** 01/13/09 ****************************************
     C*          Get FDCAP
     C*          ------------------------------------------------------
     C     fdky          Chain     PCA01                              60
     C     *in60         IFEQ      *off
     C                   Z-add     CAOUTL        XXOUTL
     C                   EVAL      ##ST = CAAST
     C                   ENDIF

     C     RTKY          SETGT     PRT01
     C     RTKY1         READPE(N) PRT01
     C                   DOW       NOT %EOF
     C                   IF        RTTYPE = 'I'
     C                   Eval      ##cmbr = RTOPBR
     C                   Eval      ##outt = RTOUTL
     C                   LEAVE
     C                   ELSE
     C     RTKY1         READPE(N) PRT01
     C                   ENDIF
     C                   ENDDO
     C**                 Select
     C**                 When      EUDCDT >= 1071001
     C**                 Eval      ##CMBR = 4990.60
     C**                 Eval      ##outt = 22185
     C**                 When      EUDCDT >= 1061001
     C**                 Eval      ##CMBR = 4874.49
     C**                 Eval      ##outt= 24485
     C**                 Other
     C**                 Eval      ##cmbr = 0
     C**                 Eval      ##outt = 0
     C**                 Endsl
     C*** END CHG *** 01/13/09 ****************************************
     C                   Eval(h)   ##MSBR = ##CMBR * 2
     C*
     C*** BEG CHG *** 09/27/10 ****************************************
     C*****dgky**        chain     pdg01                              90
     C     dgky          setgt     pdg01                              90
     C     dgky1         readpe    pdg01
     C*** END CHG *** 09/27/10 ****************************************
     C*
     C                   Eval(h)   XXDRPY = ##MSBR * DGRELW
     C*** BEGIN ADD *** 01/29/10 **************************************
     C                   CALL      COMAPI        COMPL
     C*** BEGIN ADD *** 01/29/10 **************************************
     C                   exsr      pgmstr
     C                   exsr      mvdbsc
     C                   exfmt     Scr01
     C                   endif
     C*
     C                   move      *on           *inlr
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   EVAL      SCCMBR  =  ##CMBR
     C                   EVAL      SCDRPY  =  XXDRPY
     C*
     C*          Get from FAEUP
     C*          ------------------------------------------------------
     C                   EVAL      SCDRG   =     EUDRG#
     C                   MOVEL     EUDGDS        SCDGDS
     C                   EVAL      SCPROV  =     EUMCAR
     C                   Z-Add     EUDCDT        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   MOVEL     SRDTO         SCDCDT
     C*          Get FDCAP
     C*          ------------------------------------------------------
     C*** BEG CHG *** 01/13/09 **-(MOVED TO START OF PROG)*************
     C**   fdky          Chain     PCA01                              60
     C**   *in60         IFEQ      *off
     C                   Z-add     XXOUTL        SCOUTL
     C**                 ENDIF
     C*** END CHG *** 01/13/09 ****************************************
     C*          Get FTDGP
     C*          ------------------------------------------------------
     C     *in90         IFEQ      *off
     C                   EVAL      SCRELW = DGRELW
     C                   ENDIF
     C*** Beg chg *** 10/29/20 ****************************************
     C                   Select
     C                   When      EUdcdt >= 1201001
     C                   Exsr      CalcCcrOutl
     C                   Other
     C*          Get FTIIP
     C*          ------------------------------------------------------
     C*** BEG CHG *** 09/27/10 ****************************************
     C*****ftky**        Chain     PII01                              62
     C     ftky          setgt     PII01
     C     ftky1         readpe    PII01                                  62
     C*** END CHG *** 09/27/10 ****************************************
     C     *in62         IFEQ      *off
     C                   EVAL(h)   SCOOCR = IIOCCR
     C                   Z-Add     IIEFDT        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   MOVEL     SRDTO         SCEFDT
     C                   ENDIF
     C*
     C*** BEG ADD *** 05/11/11 ****************************************
     C                   if        iioccr = 0
     C                   eval      ##OUTL = 0
     C                   else
     C*** END ADD *** 05/11/11 ****************************************
     C                   Eval(h)   ##OUTL = ##OUTT/IIOCCR
     C*** BEG ADD *** 05/11/11 ****************************************
     C                   endif
     C                   endsl
     C*** End Chg *** 10/29/20 ****************************************
     C*** END ADD *** 05/11/11 ****************************************
     C                   Eval(h)   SCOUTT = ##OUTL/2
     C
     C                   EVAL(h)   SCTADJ = SCOUTL+SCDRPY
     C************       EVAL(h)   SCTADJ = SCOUTT+SCDRPY
     C
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C****               MOVEL(P)  ##conm        scconm
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C*                  MOVE      *ON           DSPTYP
     C*
     C                   MOVEL(P)  OPTNM1        ##DATA1
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA1          80
     C                   PARM      40            ##LENG1           3 0
     C*
     C                   MOVEL     ##DATA1       SCCONM
     C*                  MOVE      *ON           DSPTYP
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* CalcCcrOutl - Calculate OCCR
     C*================================================================
      // free
       //*** Beg Add *** 10/29/20 ***************************************
          Begsr CalcCcrOutl ;
            ##OutL = 0;
            scOOCR = 0;
            scEfDt = 0;
            kkProv = EUmCar;
            kkEfDt = ##efDt;
            Setgt (kkProv:kkEfDt) pinpt01;
            Readpe (kkProv) pinpt01;
            If Not %Eof(FMINPTL1MS);
               scOocr = INoCCR;
               scEfdt = %Dec(%Char(%Date(INefDt:*CYMD):*usa0):8:0);
               ##OutL = INoCCR/INmedOut;
             Endif;
           Endsr;
       //*** End Add *** 10/29/20 ***************************************
      //end-free
