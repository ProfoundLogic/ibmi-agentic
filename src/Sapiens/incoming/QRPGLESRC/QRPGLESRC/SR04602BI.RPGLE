     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *
     H/TITLE SR04602BI - CA Small Deductible Endorsement Additional Input Program
      *===================================================================*
      *  DESCRIPTION: Collects additional information for form WC040602B. *
      *                                                                   *
      *  CALLED FROM: SR04602B                                            *
      *                                                                   *
      *  CALLS TO:    none                                                *
      *                                                                   *
      *  CREATED: 09/07/12      C:282    PROJECT: WC2159                  *
      *===================================================================*
     fSR04602Bf cf   e             workstn handler('PROFOUNDUI(HANDLER)')
    
      *  Physical file definition (for maintaining record)
     fwledp     uf a e             disk    rename(ped01:dbrcdf)

     fwledl     if   e           k disk    infds(edfds)

      ** Group Master File
     fwmfnl     if   e           k disk

      ** Employer Master File
     fwmeml     if   e           k disk

     d edfds           ds
     d  edrrn                397    400B 0
      *================================================================
      *  /COPY required constants
      *================================================================
     d optnm           c                   const('WC040602B  Additional +
     d                                     Information')
     d comapi          c                   'SA005R'
      *---------------------------------------------------------------------
      * Stand Alone Fields
      *---------------------------------------------------------------------
     d ##Co#           s              3  0
     d ##CoNm          s             40
     d ##Fnam          s             40
     d ##Fnd           s              3  0
     d @@kc            s              1
     d @@kl            s              1
     d @@Mode          s              1
     d @@rrn           s              9  0
      *---------------------------------------------------------------------
     d @@Call          s              1
     d @@Prds          s             40
     d @@Co#           s              3
     d @@Emp#          s              9
     d @@Div           s              5
     d @@Fnd           s              3
     d @@Fyr           s              3
      *---------------------------------------------------------------------
     d xxForm          s             10    inz('WC040602B ')
     d kkCo#           s                   like(EMco#)
     d kkFnd           s                   like(EMfnd)
     d kkEmp#          s                   like(EMemp#)
     d kkDiv           s                   like(EMdiv)
     d kkFyr           s              3  0
     d kkForm          s             15
     d ##Edn           s             80
     d #ofscr          s              1  0
      *=========================================================================
      * Prototype Declaration
      *=========================================================================
     d CenterFld       pr                  extPgm('SRCENTER')
     d ##data                        80
     d ##leng                         3  0

     d CompEmpDvNm     pr                  extPgm('WA005R')
     d EMemp#                         9  0
     d EMdiv#                         5  0
     d EMname                        40
     d ##Edn                         80
      *
     c/copy SCOPYMT01C
      *================================================================
      *  PLIST definitions
      *================================================================
     c     *Entry        Plist
     c                   Parm                    @@Prds
     c                   Parm                    @@Call
     c                   Parm                    @@Co#
     c                   Parm                    @@Fnd
     c                   Parm                    @@Emp#
     c                   Parm                    @@Div
     c                   Parm                    @@Fyr

      *  Company file/fund (group) retrieve program parameter list

     c     compl         Plist
     c                   Parm                    ##Co#
     c                   Parm                    ##Conm
      *================================================================
      *  KLIST definitions
      *================================================================

      ** Employer Master file
     c     keyWMEML      Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkFnd
     c                   Kfld                    kkEmp#
     c                   Kfld                    kkDiv

      ** Additional Information file
     c     keyWLEDL      Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkFnd
     c                   Kfld                    kkEmp#
     c                   Kfld                    kkDiv
     c                   Kfld                    kkFyr
     c                   Kfld                    kkForm

      ** Group Master file
     c     keyWMFNL      Klist
     c                   Kfld                    kkCo#
     c                   Kfld                    kkFnd
      *================================================================
      *  *LIKE DEFN statements
      *================================================================

     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
      /free
       BegSr Dltpre;
       EndSr;
      /end-free
     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
      /free
       BegSr Dsplyr;
       EndSr;
      /end-free
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
      /free
       BegSr Errck1;

       //  Allocated loss adjustment expenses included

       If ((*in90 = *off) And
          (scFl01 <> 'Y') And (scFl01 <> 'N'));
          *In90 = *On;
          *In71 = *On;
       EndIf;

       // Will the fixed expense charge be adjusted retroactively

       If ((*in90 = *off) And
          (scFl02 <> 'Y') And (scFl02 <> 'N'));
          *In90 = *On;
          *In72 = *On;
       Endif;

       EndSr;
      /end-free
     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
      /free
       BegSr Errck2;
       EndSr;
      /end-free
     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
      /free
       BegSr Errck3;
       EndSr;
      /end-free
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
      /free
       BegSr Mvdbsc;

       // Default values if add mode
       If @@mode = 'A';
          Clear Dbrcdf;
       Else;
          scAm01 = FMam01;
          scFl01 = FMfl01;
          scFl02 = FMfl02;
       EndIf;

       EndSr;
      /end-free
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
      /free
       BegSr Mvscdb;

       FMco#  = kkCo#;
       FMfnd  = kkFnd;
       FMfyr  = kkFyr;
       FMemp# = kkEmp#;
       FMdiv  = kkDiv;
       FMform = kkForm;

       FMam01 = scAm01;
       FMfl01 = scFl01;
       FMfl02 = scFl02;

       EndSr;
      /end-free
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
      /free
       BegSr Otherr;
       EndSr;
      /end-free
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
      /free
       BegSr Othfks;
       EndSr;
      /end-free
      *================================================================
      * PGMEND - End of program processing
      *================================================================
      /free
       BegSr Pgmend;

       If *InKc = *On Or *InKl = *On;
          @@Call = 'F';
       EndIf;

       Endsr;
      /end-free
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
      /free
       BegSr Pgmstr;
    
       #ofscr = 1;
    
       // Get Mode
       kkCo#  = %Int(@@Co#);
       kkFnd  = %Int(@@Fnd);
       kkEmp# = %Int(@@Emp#);
       kkDiv  = %int(@@Div);
       kkFyr  = %int(@@Fyr);
       kkForm = xxForm;
    
       Chain keyWLEDL ped01;
      IF %Found(wledl);
          @@Mode = 'C';
          @@Rrn  = edRrn;
       Else;
          @@Mode = 'A';
          @@Rrn  = 0;
       EndIf;

       // Get Header
    
       If q1Prms >= 1 And @@Prds <> *blanks;
          ##data = @@prds;
       Else;
          ##data = optnm;
       EndIf;

       // Center a field
       CenterFld(##data:##leng);

       scPrds = ##data;
    
       scFnd = kkFnd;
       scFyr = kkFyr;

       Chain  keyWMFNL pfn01;
       If %Found(wmfnl);
          scfnnm = fnname;
       EndIf;

       Chain  keyWMEML pem01;
       If %Found(wmeml);
          CompEmpDvNm(EMemp#:EMdiv:EMname:##Edn);
          scEdn = ##Edn;
       EndIf;

       Endsr;
      /end-free
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
      /free
       BegSr Preoth;
       EndSr;
      /end-free
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
      /free
       BegSr Updatr;

       Chain @@rrn Dbrcdf;
       If %Found(wledp);

          Exsr Mvscdb;

          FMuser = q1User;
          FMpgm  = q1Pgm;

          Exsr SrCvtDate;

          Update Dbrcdf;
       EndIf;

       OutFlg = *On;

       Endsr;
      /end-free
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
      /free
       BegSr Writer;

       Clear dbrcdf;

       Exsr Mvscdb;
       FMuser = q1User;
       FMiusr = q1User;
       FMpgm  = q1Pgm;
       FMipgm = q1Pgm;

       Exsr SrCvtDate;
       FMidt  = %Int(srDto);

       Write dbrcdf;
       Feod  wledp;
       OutFlg = *on;

       Endsr;
      /end-free
      *================================================================
      * SrCvtDate - Convert date from one format to another
      *================================================================
     c     SrCvtDate     Begsr
      *
     c                   Time                    dsdttm
     c                   Eval      FMtime = dsTime
     c                   Eval      srdti = dsDate
     c                   Call      'SRCVTDAT'    dtcymd
     c                   movel(p)  srdto         FMdt
      *
     c                   Endsr
