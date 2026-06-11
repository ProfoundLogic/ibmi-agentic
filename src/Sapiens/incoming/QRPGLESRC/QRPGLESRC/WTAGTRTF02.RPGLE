     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h*
     h**********************************************************************
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Response program to serve A/R Transaction Detail
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes should be made
     h*
     h*  Called from: WTAGTRTFND
     h*  Calls to:
     h*
     h*==============================================================*
     h*  Created   : 01/11/22                                         *
     h*  Programmer: C:300                                            *
     h*  Project   : WC2395- Redesign of Agency Configuration program *
     h*/===============================================================*
     h**********************************************************************
     h* Revisions:
     h*
     h*
     h**********************************************************************
     fwmeap     if   e             disk    rename(pea01:pea0p)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     Fwtctl     if   e           k disk    rename(pct01:pct00)
     fwtabl     if   e           k disk    include(ptt01)
     fwtavl     if   e           k disk    rename(pav01:pav01)

      /copy sPrDate
      /copy wPrUsrPrms
      /copy sPrAtrium
      /copy wPrTable
      /copy wPrEmpLr
     d/copy sPrString

      * *ENTRY
     DWtAgtRtf02       pr                  ExtPgm('WTAGTRTF02')
     D                                9  0
     D                            32767
     D                                1
      *
     DWtAgtRtf02       pi
     D inRRN                          9  0
     D outJSON                    32767
     D rtnCode                        1
      *

     d xxKey           s             10
     d fndEmpDiv       s             30
     d empName         s            100
     d nameType        s              3
     d x#RRN           s              9  0
     d nofDiv          s              3  0
     d code            s             60
     d category        s             60
     d typeDesc        s             30
     d lateDesc        s             10
     d prvempl         s             20
     d premfinco       s             23
     d EL_rrn          s              9  0
     d cxdate          s              8  0
     d actstatus       s             60


     d* Determine if employer is account current
     Dacct_curr        PR             1
     D co#                                 like(EACO#)  const
     D group                               like(EAFND)  const
     D emp#                                like(EAEMP#) const
     D div                                 like(EADIV)  const
     D yr                                  like(EAFYR)  const

     d* Load Transaction Detail detail info.
     d  LoadTrDtDtl    pr

     c* Employer Detail File
     c     keyWDELL1     Klist
     c                   Kfld                    EAco#
     c                   Kfld                    EAfnd
     c                   Kfld                    EAfyr
     c                   Kfld                    EAemp#
     c                   Kfld                    EAdiv

     c     keyTable      klist
     c                   kfld                    EAco#
     c                   kfld                    xxKey
      *
      /Free
       Chain(n) inRRN pea0p;
       Chain(n) keyWDELL1 pel01;
       LoadTrDtDtl();

       outJSON = ' ';

       BuildJSON(outJSON :'OutTDfndEmpDiv' :%Trim(fndEmpDiv));
       BuildJSON(outJSON :'OutTDempName' :%Trim(empName));
       BuildJSON(outJSON :'OutTrnYear' :%EditC(EAfyr:'X'));
       BuildJSON(outJSON :'OutnofDiv' :%EditC(nofDiv:'X'));
       BuildJSON(outJSON :'DtlOutTrncode' :%Trim(code));
       BuildJSON(outJSON :'DtlOutTDcatgry' :%Trim(category));
       BuildJSON(outJSON :'DtlOuttype' :%Trim(typeDesc));
       BuildJSON(outJSON :'DtlOutLate' :%Trim(lateDesc));
       BuildJSON(outJSON :'DtlOutTrnsDt' :
                         alphaUSA(cvtToDate(dtMDYY(EAtrdt))));
       BuildJSON(outJSON :'DtlOutpgmName' :%Trim(EApgm));
       BuildJSON(outJSON :'DtlOutUser' :%Trim(EAuser));
       BuildJSON(outJSON :'DtlOutMainTime'  :%EditW((EAtime):'  :  :  '));

       If prvempl <> *Blanks;
          BuildJSON(outJSON :'DtlLblPrvGrpEmp'  :'Previous Group/Employer');
          BuildJSON(outJSON :'DtlOutPrvGrpEmp'  :%Trim(prvempl));
       Endif;

       If premfinco <> *Blanks;
          BuildJSON(outJSON :'DtlLblPremFinCo'  :'Premium Finance Company');
          BuildJSON(outJSON :'DtlOutPremFinCo'  :%Trim(premfinco));
       Endif;

       If cxdate <> 0;
          BuildJSON(outJSON :'DtlLblCxDate'  :'Cancellation Date');
          BuildJSON(outJSON :'DtlOutCxDate'  :%EditW((cxdate):'  /  /    '));
       Endif;

       If actstatus <> *Blanks;
          BuildJSON(outJSON :'DtlLblActivitysts'  :'Activity Status');
          BuildJSON(outJSON :'DtlOutActivitysts'  :%Trim(actstatus));
       Endif;

       If acct_curr(EAco#:EAfnd:EAemp#:EAdiv:EAfyr) = *On;
          BuildJSON(outJSON :'DtlLblAccCur'  :'Account Current');
       Endif;

       rtnCode = ' ';
       Return;
      /End-Free

    P*====================================================================
    P* acct_curr - Determine if employer is account current
    P*====================================================================
    P*
     Pacct_curr        b
    P*
     Dacct_curr        PI             1
     D co#                                 like(EACO#)  const
     D group                               like(EAFND)  const
     D emp#                                like(EAEMP#) const
     D div                                 like(EADIV)  const
     D yr                                  like(EAFYR)  const

     D ##co#           s                   like(EACO#)
     D ##group         s                   like(EAFND)
     D ##emp#          s                   like(EAEMP#)
     D ##div           s                   like(EADIV)
     D ##yr            s                   like(EAFYR)
     D ##agt#          s              5  0
     D ##agt_name      s             60
     D ##prod          s              5  0
     D ##prod_nm       s             60
     D ##area_cd       s              3  0
     D ##phone#        s              7  0
     D ##plan          s              1
     D acct_curr2      s                   like(*in99)

     C                   call      'WA002R'                             99
     C                   parm      co#           ##co#
     C                   parm      group         ##group
     C                   parm      emp#          ##emp#
     C                   parm      div           ##div
     C                   parm      yr            ##yr
     C                   parm                    ##agt#
     C                   parm                    ##agt_name
     C                   parm                    ##prod
     C                   parm                    ##prod_nm
     C                   parm                    ##area_cd
     C                   parm                    ##phone#
     C                   parm                    ##plan
     C*
     C                   eval      acct_curr2 = ##plan = 'A'
     C*
     C                   return    acct_curr2

     Pacct_curr        e

       //===================================================================
       // Load Transaction Detail detail info.
       //===================================================================
     P LoadTrDtDtl     B
     D LoadTrDtDtl     PI
      /free

       Clear xxKey;
       xxKey = EAarcd;
       Chain keyTable PTT01;
       Code = %trimr(xxKey) + ' - ' + TTdesc;

       // Fund, Employer, Division
       fndEmpDiv = %editc(EAfnd:'X') + ' - ' + %editc(EAEmp#:'X') +
                     ' - ' + %editc(EADiv:'X');
       // DBA Name
       nameType= 'DBA';
       empName  = GetEmpFullName(EAEmp#:EADiv:x#RRN:EAfnd:EAco#:nameType);

       // No. of Divisions
       nofDiv = DivCount(EAco#:EAfnd:EAEmp#);

       // Category with Description
       Chain (EAco#:TTctyp) PCT00;
       Category = TTctyp + ' - ' + %trim(CTdesc);

       // A/R Type with Description
       clear typeDesc;
       Select;
       When EAarty = 'P' or EAarty = 'I'
        or EAarty = 'Q';
          typeDesc = EAarty + ' - Payment';
       When EAarty = 'B' or EAarty = 'F'
        or EAarty = 'H' or EAarty = 'A'
        or EAarty = 'D';
          typeDesc = EAarty + ' - Billed';
       When EAarty = 'R';
          typeDesc = EAarty + ' - Refund';
       When EAarty = 'N';
          typeDesc = EAarty + ' - NSF Charge';
       When EAarty = 'V';
          typeDesc = EAarty + ' - Void';
       When EAarty = 'T';
          typeDesc = EAarty + ' - Return Premium Void';
       Endsl;

       // Late Payment Flag
       Select;
       When EAlate = 'Y';
          lateDesc = 'Yes';
       Other;
          lateDesc = ' ';
       Endsl;

       // Previous Employer
       prvempl = GetPrvEmp#(EAco#:EAfnd:EAemp#:EAdiv);

       // Premium Finance Company
       EL_rrn = GetEmpYrRR(EAfyr:EAemp#:EAdiv:EAfnd:EAco#);
       premfinco = GetPremFin(EL_rrn);

       // Cancellation Date
       cxdate =  dtMDYY(GetCxDate(EAco#:EAfnd:EAemp#:EAdiv));

       // Activity Status
       If ELascd <> *Blanks;
          Clear AVdesc;
          Clear xxKey;
          xxKey = ELascd;
          Chain keyTable PAV01;
          actstatus  = %trimr(xxkey) + ' ' + AVdesc;
       Endif;

      /end-free
     P LoadTrDtDtl     E
