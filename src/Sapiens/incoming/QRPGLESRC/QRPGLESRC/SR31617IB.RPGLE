      //===================================================================*
     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
      //===================================================================*
     H/TITLE SR31617IB - ADDITIONAL ENTRY PROGRAM FOR WC310617B
      //===================================================================*
      //  DESCRIPTION: Collects additional information for form WC310617B  *
      //                                                                   *
      //  CALLED FROM: SR31617B                                            *
      //                                                                   *
      //  CALLS TO:    none                                                *
      //                                                                   *
      //  CREATED: 03/27/20      C:079     PROJECT: WC2736                 *
      //===================================================================*
      //  REVSIONS:                                                        *
      //                                                                   *
      //                                                                   *
      //===================================================================*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /Endif
     FSR31617f  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /Endif
      //* Additional Information File
     FWXFMP1    uf a e           k DISK
      //* Group Master File
     FWMFNL     IF   E           K DISK
      //* Employer Master File
     FWMEML     IF   E           K DISK
      //
      //================================================================
      // /COPY 'D' specs
      //================================================================
      //
     D/COPY SCOPYPR01D
      //
      //================================================================
      // Stand Alone Fields - TOP
      //================================================================
     D optNm           c                   const('Additional information for WC+
     D                                     310617B')
     D comApi          pr                  Extpgm('WA001R')
     d  inCo#                              Like(##Co#)
     d  inFnd                              Like(##Fnd)
     d  outCoNm                            Like(##CoNm)
     d  outFnam                            Like(##Fnam)

     D CompEmpDivNm    pr                  Extpgm('WA005R')
     d  inEmp#                             Like(EMemp#)
     d  inDiv                              Like(EMdiv)
     d  inName                             Like(EMname)
     d  outEdn                             Like(xxEdn)

     D Center          pr                  Extpgm('SRCENTER')
     d  outData                            Like(##Data)
     d  inLength                           Like(##Leng)

      //
     D @@Call          s              1
     D @@PrDs          s             40
     D @@Co#           s              3
     D @@Emp#          s              9
     D @@Div           s              5
     D @@Fnd           s              3
     D @@Fyr           s              3
      //
     D ##Co#           s              3  0
     D ##Fnam          s             40
     D ##Fnd           s              3  0
      //
     D xxFrm1          s             15
     D xxCall          s                   like(@@call)
     D xxForm          s             10    inz('WC310617B ')
     D xxCo#           s                   like(emco#)
     D xxFnd           s                   like(emfnd)
     D xxEmp#          s                   like(ememp#)
     D xxDiv           s                   like(emdiv)
     D xxFyr           s              3  0
     D xxEdn           s             80
      //
     C/COPY SCOPYDTPL
      //
      //================================================================
      //  PLIST definitions
      //================================================================
      //* Entry parameters
     C     *entry        plist
     C                   parm                    @@PrDs
     C                   parm                    @@Call
     C                   parm                    @@Co#
     C                   parm                    @@Fnd
     C                   parm                    @@Emp#
     C                   parm                    @@Div
     C                   parm                    @@Fyr
      //
      //* Company file/fund (group) retrieve program parameter list
     C     compl         plist
     C                   parm                    ##Co#
     C                   parm                    ##Fnd
     C                   parm                    ##CoNm
     C                   parm                    ##Fnam
      //================================================================
      //  KLIST definitions
      //================================================================
      //
      //* Employer Master file
     C     emKy          klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
      //
      //* Additional Information file
     C     fmKy1         klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxfyr
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFrm1
      //
      //* Group Master file
     C     fnKy          klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
      //================================================================
      // Main
      //================================================================
      //
     C/COPY SCOPYPR01C
      //
      //====================================================================
      // DSCAPI - Call API for getting descriptions (use 91 on error)
      //====================================================================
      /FREE
       Begsr DscApi;
          //
      /END-FREE
          Monitor;
          Callp ComApi(##Co#:##Fnd:##CoNm:##Fnam);
          On-Error;
             *in91 = *on;
          Endmon;
      //
      /FREE
       Endsr;
       //====================================================================
       // ERRCK1 - Error checking for srceen 1 (SCR01)
       //====================================================================
       Begsr ErrCk1;
          //
          exsr OthFks;
          If not *in90;
             Exfmt scr02;
          Endif;
      //
       Endsr;
       //====================================================================
       // KJDO   - Processing for F10
       //====================================================================
       Begsr KjDo;
       Endsr;
       //====================================================================
       // OTHFKS - Processing for other functions keys
       //====================================================================
       Begsr OthFks;
          //
          Select;
          When *inkj = *on;
             Exsr KjDo;
             *in90 = *on;
          Endsl;
          //
       Endsr;
       //====================================================================
       // PGMEND - End of program processing
       //====================================================================
       Begsr PgmEnd;
          //
          If *inkc = *on Or *inkl = *on;
             @@Call = 'F';
          Endif;
          //
       Endsr;
       //====================================================================
       // PGMSTR - Used for any start of program processing (one-time)
       //====================================================================
       Begsr PgmStr;
          //
          If Q1prms >= 1 and @@PrDs <> *blanks;
             ##data = @@PrDs;
          Else;
             ##data = Optnm;
          Endif;
          //
          ##Leng = 40;
          Monitor;
             Callp Center(##Data:##Leng);
             On-Error;
          Endmon;

          If Q1prms >= 2;
             xxCall = @@Call;
          Else;
             Clear xxCall;
          Endif;
          //
          xxFyr = 0;
          xxCo#  = %Dec(@@Co#:3:0);
          xxFnd  = %Dec(@@Fnd:3:0);
          xxEmp# = %Dec(@@Emp#:9:0);
          xxDiv  = %Dec(@@Div:5:0);
          xxFyr  = %Dec(@@Fyr:3:0);

          scFnd = xxFnd;
          scFyr = xxfyr;
          xxFrm1 = xxForm;
          //
          Chain fnKy pfn01;
          scFnam = FNname;
          //
          Chain emKy pem01;
          //
          Monitor;
             Callp CompEmpDivNm(EMemp#:EMdiv:EMname:xxEdn);
          On-Error;
          Endmon;
          scEnam = xxEdn;
          //
       Endsr;
       //====================================================================
       // PREDSP - Pre-processing before displaying (every time)
       //====================================================================
       Begsr PreDsp;
          //
          //* Get endorsement additional information
          Chain fmKy1 pfm01;
          //
          If %Found(WXFMP1);
             //
             //* Move the custom fields to the screen
             scCty1 = F3c411;
             scCty2 = F3c412;

             scEmp1 = F3c401;
             scEmp2 = F3c402;
             scEmp3 = F3c403;
             scEmp4 = F3c404;
             scEmp5 = F3c405;
             scEmp6 = F3c406;
             scEmp7 = F3c407;
             scEmp8 = F3c408;
             scEmp9 = F3c409;
             scem10 = F3c410;

             scStc1 = F3c201;
             scStc2 = F3c202;
             scStc3 = F3c203;
             scStc4 = F3c204;
             scStc5 = F3c205;
             scStc6 = F3c206;
             scStc7 = F3c207;
             scStc8 = F3c208;
             scStc9 = F3c209;
             scSt10 = F3c210;

             scLaw1 = F3c101;
             scLaw2 = F3c102;
             scLaw3 = F3c103;
             scLaw4 = F3c104;
             scLaw5 = F3c105;
             scLaw6 = F3c106;
             scLaw7 = F3c107;
             scLaw8 = F3c108;
             scLaw9 = F3c109;
             scLw10 = F3c110;

             scExr1 = F3am01;
             scExr2 = F3am02;
             scBod1 = F3am03;
             scBod2 = F3am04;
             scBod3 = F3am05;
             //
          Else;
             //
             scCty1 = *blanks;
             scCty2 = *blanks;
             scEmp1 = *blanks;
             scEmp2 = *blanks;
             scEmp3 = *blanks;
             scEmp4 = *blanks;
             scEmp5 = *blanks;
             scEmp6 = *blanks;
             scEmp7 = *blanks;
             scEmp8 = *blanks;
             scEmp9 = *blanks;
             scEm10 = *blanks;

             scStc1 = *blanks;
             scStc2 = *blanks;
             scStc3 = *blanks;
             scStc4 = *blanks;
             scStc5 = *blanks;
             scStc6 = *blanks;
             scStc7 = *blanks;
             scStc8 = *blanks;
             scStc9 = *blanks;
             scSt10 = *blanks;

             scLaw1 = *blanks;
             scLaw2 = *blanks;
             scLaw3 = *blanks;
             scLaw4 = *blanks;
             scLaw5 = *blanks;
             scLaw6 = *blanks;
             scLaw7 = *blanks;
             scLaw8 = *blanks;
             scLaw9 = *blanks;
             scLw10 = *blanks;

             scExr1 = *zeros;
             scExr2 = *zeros;
             scBod1 = *zeros;
             scBod2 = *zeros;
             scBod3 = *zeros;
             //
          Endif;
          //
       Endsr;
       //====================================================================
       // PRSCTL - Process control after no errors found (Call Submitter)
       //====================================================================
       Begsr PRSCTL;
          //
          Chain fmky1 pfm01;
          //
          If Not %Found(WXFMP1);
             clear pfm01;
          Endif;
          //
          //* Move the additional information fields to the file
          F3co# = xxCo#;
          f3fnd = xxFnd;
          f3fyr = xxfyr;
          f3emp# = xxEmp#;
          f3div = xxDiv;
          f3form = xxform;

          F3c411 = scCty1;
          F3c412 = scCty2;

          F3c401 = scEmp1;
          F3c402 = scEmp2;
          F3c403 = scEmp3;
          F3c404 = scEmp4;
          F3c405 = scEmp5;
          F3c406 = scEmp6;
          F3c407 = scEmp7;
          F3c408 = scEmp8;
          F3c409 = scEmp9;
          F3c410 = scem10;

          F3c201 = scStc1;
          F3c202 = scStc2;
          F3c203 = scStc3;
          F3c204 = scStc4;
          F3c205 = scStc5;
          F3c206 = scStc6;
          F3c207 = scStc7;
          F3c208 = scStc8;
          F3c209 = scStc9;
          F3c210 = scSt10;

          F3c101 = scLaw1;
          F3c102 = scLaw2;
          F3c103 = scLaw3;
          F3c104 = scLaw4;
          F3c105 = scLaw5;
          F3c106 = scLaw6;
          F3c107 = scLaw7;
          F3c108 = scLaw8;
          F3c109 = scLaw9;
          F3c110 = scLw10;

          F3am01 = scexr1;
          F3am02 = scexr2;
          F3am03 = scbod1;
          F3am04 = scbod2;
          F3am05 = scbod3;
          //
          If Not %Found(WXFMP1);
             Write pfm01;
          Else;
             Update pfm01;
          Endif;
      //
       Endsr;
      /END-FREE
