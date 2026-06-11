     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H nomain
     H option(*nodebugio:*srcstmt)
     H**************************************************************************************
     H* Revisions:                                                                         *
      *
      * 05/23/24 C:300 Support: 115634 (SUNZ)
      *                Update WMFNP Maintenance fields while updating
      *                Next Available Claim Number
      *
      * 09/06/22 C:079 Project: WC2823
      *                Fix GetFroiState to return n/a.
      *
      * 05/20/22 C:062 Project: 98690(099)
      *                Add Claim Type to json.
      *
      * 12/15/21 C:136 Project: CN028
      *                Add optional AsOfDate to SqlWithClaimInfo
      *                to call SqlWithPolicyInfo with.
      *
      * 11/03/21 C:291 Project: CN020
      *                Add SqlWithClaimInfo
      *
      * 07/07/21 C:62  Project: WC2781
      *                Add new elements to the claim demographics
      *
      * 04/22/21 C:33  Project: SY413
      *               -Calculate total incurred after all paid/reserves
      *                 completed.
      *               -Include closed claims by default
      *
      * 07/27/20 C:33  Project: SY409
      *                Add ClaimJSONData for generating json of claim data.
     H*                                                                                    *
     H* 02/08/18  C:291 Support: 63953                                                     *
     H*                 Change Division length to 5,0 in ClaimCount procedure              *
     H*                                                                                    *
     H* 03/19/15  C:298 Support: 32825                                                     *
     H*                 Added ClaimDOB                                                     *
     H*                                                                                    *
     H* 03/13/14  C:279 Redmine: 3208                                                      *
     H*                 Modified ClaimCount procedure to accept                            *
     H*                 division as optional parameter.                                    *
     H*                                                                                    *
     H* 09/29/12  C:190 Project: WC2115                                                    *
     H*                 Added ClaimCount                                                   *
     H*                                                                                    *
     H* 09/29/12  C:190 Project: WC2115                                                    *
     H*                 Added ChkDupSS#                                                    *

     H* 07/10/12  C:062 Project: WC2115                                                    *
     H*                 Added ChkAuthClm, GetFroiState, ChkDupClm,                         *
     H*                 GetNextClm#
     H*                                                                                    *
     H* 05/14/12  C:189 Project: WC2115                                                    *
     H*                 Added procedure to check whether multi claim                       *
     H*                                                                                    *
     H* 05/07/12  C:191 Project: Transform                                                 *
     H*                 Added procedure to return RRN (GetClaimRRN)                        *
     H*                                                                                    *
     H* 03/06/12  C:189 Project: WC2115                                                    *
     H*                 Added a procedure to get the claimant name on passing the ss#      *
     H*                                                                                    *
     H*                                                                                    *
     H* 05/21/03  C:132 Support: 37011                                                     *
     H*                 Recompiled to include WPRUSRPRMS change  (no chg to this module)   *
     H*                                                                                    *
     H**************************************************************************************
    F*
     Fwmcml     if   e           k disk    rename(pcm01:pcm00) infds(cmfds)
     Fwmcml1    if   e           k disk                        infds(cmfds1)
     Fwmchl     if   e           k disk    rename(pch01:pch00)
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     Fwmcdl     if   e           k Disk    Rename(pcd01:pcd00)
     Fwamcl     if   e           k Disk    ReName(pmc01:pmc00)
     fwmixl     if   e           k Disk    ReName(pix01:pix00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     fwmcmlb    if   e           k Disk    ReName(pcm01:pcm0b)
     fwmfnl     uf   e           k Disk    ReName(pfn01:pfn00)

     d* Check Claim Authority
     d ChkUserSec      pr                  ExtPgm('SRCCSC')
     d   userType                     1
     d   userLevel                    3
     d   claimType1                   1
     d   claimLevel                   3
     d   execType                     1
     d   secResult                    1
     d   claimType2                   1
     d   claimType3                   1
     d   claimType4                   1
     d   claimType5                   1
     d   claimType6                   1

     D cmfds           ds
     D cmrrn                 397    400b 0

     D cmfds1          ds
     D cmrrn1                397    400b 0

      /Include yajl_h
     d/copy sPrDate
     d/copy sPrJson
     d/copy sPrSql
     D/copy wprusrprms
     D/copy wprgroup
     D/copy wprclaims
      //*** Beg Add ***** 11/03/21 **********************
     D/copy wpremplr
      //*** End Add ***** 11/03/21 **********************

     D psds          esds                  extname(szq1p)




    P*====================================================================
    P* GetClmSts - Get claim status
    P*
    P*  GetClmSts - returns claim status for claim
    P*   co#       - accepts the company #                       (required)
    P*   group     - accepts the group                           (required)
    P*   yr        - accepts the policy year                     (required)
    P*   claim#    - accepts the claim#                          (required)
    P*   date      - accepts the date to determine the status
    P*====================================================================
    P*
     PGetClmSts        b                   export
    P*
     DGetClmSts        PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     D date                           7  0

     D effDate         s                   like(CHEFDT)

     C     cmky1         klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    claim#
     C*
     C     chky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    claim#
     C                   kfld                    date
     C*
     C     chky_1        klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    claim#
     C                   kfld                    effDate

     C                   eval      effDate = *hival
     C*
     C                   if        %parms >= 5 and date <> *zero
     C                   eval      effDate = date
     C                   endif
     C*
     C     cmky1         chain     wmcml1                             60
     C     chky_1        setgt     wmchl
     C     chky          readpe    wmchl                                  60
     C*
     C                   if        *in60
     C                   eval      CHOCST = CMSTAT
     C                   endif
     C*
     C                   return    CHOCST

     PGetClmSts        e





    P*====================================================================
    P* GetIncLoss - Get total inccurred loss for the employer
    P*
    P*  GetIncLoss - returns the total incurred loss for the employer
    P*   co#       - accepts the company #   (required)
    P*   group     - accepts the group       (required)
    P*   employer  - accepts the employer    (required)
    P*   division  - accepts the division    (required)
    P*   yr        - accepts the policy year
    P*   openClose - accepts 'O' for open claims, 'C' for closed claims
    P*   fromDate  - accepts the from date
    P*   toDate    - accepts the to date
    P*   payments  - returns the total claim payments for the employer
    P*   reserve   - returns the total claim reserves for the employer
    P*====================================================================
    P*
     PGetIncLoss       b                   export
    P*
     DGetIncLoss       PI            11  2
     D co#                            3  0 const
     D group                          3  0 const
     D employer                       9  0 const
     D division                       5  0 const
     D yr                             3  0 const options(*nopass)
     D openClose                      1    const options(*nopass)
     D fromDate                       7  0 const options(*nopass)
     D toDate                         7  0 const options(*nopass)
     D payments                      11  2       options(*nopass)
     D reserves                      11  2       options(*nopass)

     D incurred        s             11  2
     D totalPay        s             11  2
     D totalRes        s             11  2
     D year            s              3  0
     D fromDt          s              7  0
     D toDt            s              7  0
     D open            s              1
     D close           s              1

     C     cmky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    division
     C                   kfld                    year
     C*
     C     cmky_1        klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    employer
     C                   kfld                    division

     C                   clear                   totalPay
     C                   clear                   totalRes
     C                   clear                   incurred
     C*
     C                   eval      fromDt = *loval
     C                   eval      toDt   = *hival
     C                   eval      open   = *on
     C***  Begin Chg  ***  04/22/21  *************************************
     C**********         eval      close  = *off
     C                   eval      close  = *on
     C***  End   Chg  ***  04/22/21  *************************************
     C*
     C                   if        %parms >= 5
     C                   eval      year = yr
     C                   endif
     C*
     C                   if        %parms >= 6 and openClose <> *blank
     C                   eval      open  = openClose = 'O' or openClose = 'B'
     C                   eval      close = openClose = 'C' or openClose = 'B'
     C                   endif
     C*
     C                   if        %parms >= 7 and fromDate <> *zero
     C                   eval      fromDt = fromDate
     C                   endif
     C*
     C                   if        %parms >= 8 and toDate <> *zero
     C                   eval      toDt = toDate
     C                   endif
     C*
     C                   select
     C                   when      year <> *zero
     C     cmky          chain     wmcml                              60
     C                   when      year = *zero
     C     cmky_1        chain     wmcml                              60
     C                   endsl
     C*
     C                   dow       not *in60
     C*
     C                   if        (commGroup(CMCO#:CMFND) = *off or
     C                              (CMACDT >= fromDt and
     C                               CMACDT <= toDt)) and
     C                             ((getClmSts(CMCO#:CMFND:CMFYR:CMCASE:
     C                               toDt) = 'O' and open  = *on) or
     C                              (getClmSts(CMCO#:CMFND:CMFYR:CMCASE:
     C                               toDt) = 'C' and close = *on))
     C*
     C                   eval      totalPay = totalPay + CMAR01 +
     C                                          CMAR02 + CMAR03 + CMAR04 +
     C                                          CMAR05 + CMAR06 + CMAR07 +
     C                                          CMAR08 + CMAR09 + CMAR10
     C*
     C                   eval      totalRes = totalRes + CMRR01 +
     C                                          CMRR02 + CMRR03 + CMRR04 +
     C                                          CMRR05 + CMRR06 + CMRR07 +
     C                                          CMRR08 + CMRR09 + CMRR10
     C*
     C***  Begin Del  ***  04/22/21  *************************************
     C**********         eval      incurred = incurred + totalPay + totalRes
     C***  End   Del  ***  04/22/21  *************************************
     C*
     C                   endif
     C*
     C                   select
     C                   when      year <> *zero
     C     cmky          reade     wmcml                                  60
     C                   when      year = *zero
     C     cmky_1        reade     wmcml                                  60
     C                   endsl
     C*
     C                   enddo
     C*
     C***  Begin Add  ***  04/22/21  *************************************
     C                   eval      incurred = totalPay + totalRes
     C***  End   Add  ***  04/22/21  *************************************
     C*
     C                   if        %parms >= 9
     C                   eval      payments = totalPay
     C                   endif
     C*
     C                   if        %parms >= 10
     C                   eval      reserves = totalRes
     C                   endif
     C*
     C                   return    incurred
     C*
     PGetIncLoss       e






     PGetClaimantn     b                   export
    P*
     DGetClaimantn     PI            40
     D ss#                            9    const
     D xxClmName       S             40
      /Free

       // Claim Demographic Information
       Chain ss# pcd00;
       If %Found(wmcdl);

          If cdMid<>*Blanks;
             xxClmName=%trim(CDlsn) + ', ' +
              %trim(CDfsn) + ' ' + %trim(CDmid) + '.';
          Else;
             xxClmName=%trim(CDlsn) + ', ' + %trim(CDfsn);
          EndIf;

       Else;
          xxClmName=*Blanks;
       EndIf;

           return    xxClmName;
       /End-Free

     PGetClaimantn     e
    P*====================================================================
    P* GetClaimRRN - Return Claim RRN
    P*   co#       - accepts the company #   (required)
    P*   group     - accepts the group       (required)
    P*   yr        - accepts the policy year
    P*   Case      - accepts the Case#
    P*====================================================================
     PGetClaimRRN      b                   export
    P*
     DGetClaimRRN      PI             9  0
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const

     C     cmky1         klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    claim#

      /Free
        chain cmky1 pcm01;
        If %found;
          return cmrrn1;
        Else;
          return 0;
        EndIf;
      /End-Free

     PGetClaimRRN      e
    P*====================================================================
    P* GetmultiClm - Check whether multi claim# exists
    P*   co#       - accepts the company #   (required)
    P*   group     - accepts the group       (required)
    P*   yr        - accepts the policy year (required)
    P*   Case      - accepts the Case#       (required)
    P*             - Returns whether mutli claim exists
    P*====================================================================
     PGetmultiClm      b                   export
    P*
     DGetmultiClm      PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const

     D case            s             14    inz('00000000000000')

     C     keywamcl      klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    case
      /Free
       case='0000000' + %EditC(claim#:'X');
       chain  keywamcl pmc00;
       If %found(WAMCL);
          return '1';
        Else;
          return '0';
       EndIf;
      /End-Free
     PGetmultiClm      e
       //===================================================================
       // ChkAuthClm - Check for Authority to Add Claim
       //===================================================================
     P ChkAuthClm      B                   Export
     D ChkAuthClm      PI              n
     D company#                       3  0
     D group#                         3  0

     D kkCo#           s                   like(CXco#)
     D kkFnd           s                   like(CXfnd)
     d userType        s              1
     d userLevel       s              3
     d claimType1      s              1
     d claimLevel      s              3
     d execType        s              1
     d secResult       s              1
     d claimType2      s              1
     d claimType3      s              1
     d claimType4      s              1
     d claimType5      s              1
     d claimType6      s              1

     C     keyWMCXL      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkFnd

      /Free

       // get the FROI Information
       kkCo#=company#;
       kkFnd=group#;
       Chain keyWMCXL pcx00;
       If %Found(WMCXL);
          userType=GetUsrType(q1User);
          userLevel=%EditC(GetUsrLvl(q1User):'X');
          claimType1=CXoCat;
          claimLevel=%EditC(CXsecc:'X');
          execType='1';
          secResult=*Blanks;
          claimType2=CXoCa2;
          claimType3=CXoCa3;
          claimType4=CXoCa4;
          claimType5=CXoCa5;
          claimType6=*Blanks;
          ChkUserSec(userType:userLevel:claimType1:claimLevel:execType:
           secResult:claimType2:claimType3:claimType4:claimType5:
           claimType6);
          If secResult='0';
             Return *On;
          Else;
             Return *Off;
          EndIf;
       Else;
          Return *Off;
       Endif;

      /End-Free
     P ChkAuthClm      E
       //===================================================================
       // ChkDupClm - Check for Duplicate Claims by SS#
       //===================================================================
     P ChkDupClm       B                   Export
     D ChkDupClm       PI              n
     D company#                       3  0
     D socialSec#                     9

     D kkCo#           s                   like(CMco#)
     D kkSS#           s                   like(CMss#)

     C     keyWMCMLB     Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkSS#

      /Free

       // get the FROI Information
       kkCo#=company#;
       kkSS#=socialSec#;
       Chain keyWMCMLB pcm0B;
       If %Found(WMCMLB);
          Return *On;
       Else;
          Return *Off;
       Endif;

      /End-Free
     P ChkDupclm       E
       //===================================================================
       // GetFroiState - Get the FROI State
       //===================================================================
     P GetFroiState    B                   Export
     D GetFroiState    PI             3
     D company#                       3  0
     D state                          2
     D froiState       s              3    inz(*Blanks)
     D kkCo#           s                   like(CMco#)
     D kkJrSt          s                   like(CMjrSt)
     D kkTrId          s                   like(IXtrId)
     D edi             s               n

     C     keyWMIXL      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkJrSt
     C                   Kfld                    kkTrId

      /Free

       // get the FROI Information
       edi=*Off;
       kkCo#=company#;
       kkJrSt=state;
       kkTrId='148';
       Chain keyWMIXL pix00;
       If %Found(WMIXL);
          DoU %Eof(WMIXL);
             If IXSTAT<>'I';
                froiState=IXfroi;
                edi=*On;
                Leave;
             EndIf;
             ReadE keyWMIXL pix00;
          EndDo;
       Endif;
       //*** Beg Chg *** 09/06/22 ******************************************
       //***If edi=*Off;
       //***   froiState=state;
       //***EndIf;
       //***If froiState=*Blanks;
       //***   froiState='n/a';
       //***EndIf;
       //*** End Chg *** 09/06/22 ******************************************
       If froiState = *Blanks or edi = *off;
          froiState='n/a';
       EndIf;

       Return froiState;

      /End-Free
     P GetFroiState    E
       //===================================================================
       // GetNextClm# - Get the Next Claim #
       //===================================================================
     P GetNextClm#     B                   Export
     D GetNextClm#     PI             7  0
     D company#                       3  0 const
     D group#                         3  0 const

     D kkCo#           s                   like(CMco#)
     D kkFnd           s                   like(CMfnd)

     C     keyWMFNL      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkFnd

      /Free

       // get the FROI Information
       kkCo#=company#;
       kkFnd=group#;
       Chain keyWMFNL pfn00;
       If %Found(WMFNL);
          FNnacs=FNnacs+1;
          //*** Beg Add ***** 05/23/24 ***********************
          FNpgm  = Q1pgm;
          FNuser = Q1user;
          FNtime = Timeto6(SystemTime());
          FNdt   = Dateto7(SystemDate());
          //*** End Add ***** 05/23/24 ***********************
          Update pfn00;
       Endif;

       Return FNnacs-1;

      /End-Free
     P GetNextClm#     E
       //===================================================================
       // ChkDupSS# - Check for Duplicate SS#
       //===================================================================
     P ChkDupSS#       B                   Export
     D ChkDupSS#       PI              n
     D company#                       3  0
     D socialSec#                     9

     D kkCo#           s                   like(CMco#)
     D kkSS#           s                   like(CMss#)
     D x               s              1  0
     D dupSS#          s               n   inz(*off)

     C     keyWMCMLB     Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkSS#

      /Free

       // get the FROI Information
       x = 0;
       kkCo#=company#;
       kkSS#=socialSec#;
       Chain keyWMCMLB pcm0B;
       If %Found(WMCMLB);
          DoU %Eof(WMCMLB);
              x = x + 1;
              If x > 1;
                 dupSS# = *On;
                 Leave;
              Else;
                 dupSS# = *Off;
              Endif;
          ReadE keyWMCMLB pcm0B;
          EndDo;
          If dupSS# = *On;
             Return *On;
          Else;
             Return *Off;
          Endif;
       Else;
          Return *Off;
       Endif;

      /End-Free
     P ChkDupSS#       E
    P*====================================================================
    P* GetClaimName - Return Claim Name
    P*   co#       - accepts the company #
    P*   group     - accepts the group
    P*   yr        - accepts the policy year
    P*   Case      - accepts the Case#
    P*====================================================================
     PGetClaimName     b                   export
    P*
     DGetClaimName     PI            50
     D co#                            3  0 const
     D group                          3  0 const
     D yr                             3  0 const
     D claim#                         7  0 const
     D rrn                            9  0        options(*nopass)

     D claimName       s             50    inz

     C     cmky1         klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    yr
     C                   kfld                    claim#

      /Free
        claimName = *Blanks;
        chain cmky1 pcm01;
        If %found;
          claimName = %Trim(CMlsn) + ', ' + %Trim(CMFsn);
          If %parms >4;
            rrn = cmrrn1;
          Endif;
        EndIf;
        return claimName;
      /End-Free

     PGetClaimName     e

     P*====================================================================
     P* Claim Count
     P*====================================================================
     PClaimCount       b                   Export
     DClaimCount       pi             9  0
     D inCompany#                     3  0 Const
     D inGroup                        3  0 Const
     D inEmployer                     9  0 Const
     D inDivision                     5  0 Const options(*nopass)

     D clmCount        s              9  0 Inz(0)
      /Free

       If %parms > 3;
          exec sql
          select count(*) into :clmCount from wmcmp
          where CMco#=:inCompany# And CMfnd=:inGroup And CMemp#=:inEmployer
          And CMdiv=:inDivision;
       Else;
       exec sql
        select count(*) into :clmCount from wmcmp
        where CMco#=:inCompany# And CMfnd=:inGroup And CMemp#=:inEmployer;
       Endif;

       Return clmCount;

      /End-Free

     PClaimCount       e

      //===================================================================
      // ClaimDOB - Get claimaint date of birth
      //===================================================================
     p ClaimDOB        b                   export
     d ClaimDOB        pi             7  0
     d inSS#                          9

     d intDOB          s              7  0

      /free
       Chain(n) inSS# pcd00;
       If %found(wmcdl);
          intDOB = CDdtoB;
       Endif;

       return intDOB;

      /end-free

     p ClaimDOB        e


      *====================================================================
      * ClaimJSONData returns JSON string of general claim data
      *  inCo#  - accepts the current company   (required)
      *  inFnd  - accepts the current group     (required)
      *  inFyr  - accepts the current year      (required)
      *  inCase - accepts the current case      (required)
      *====================================================================
     pClaimJSONData    b                   export
     dClaimJSONData    pi              a   len(5000) varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inFyr                          3  0 const
     d inCase                         7  0 const

     dtimeStamp        s               z   inz
     dsysDt7           s              7  0
     djson             s               a   len(5000) varying inz

     ddecTotalInc      s             13s 2 inz
     dstrClaim         s             25    inz varying
     dstrSql           s           1000    inz varying
     dstrStatus        s             25    inz varying
     d*** Begin Add *** 05/20/22 ******************************************
     dstrLtDesc        s             25    inz varying
     d*** End   Add *** 05/20/22 ******************************************

     d*** Begin Add *** 07/07/21 ******************************************
     dchrClaimantAddress...
     d                 s           1024    varying inz
     ddsWMCDP        e ds                  extname(WMCDP) inz
     d*** End   Add *** 07/07/21 ******************************************
     ddsWMCHP        e ds                  extname(WMCHP) inz qualified
     ddsWMCMP        e ds                  extname(WMCMP) inz
      /Free
       sysDt7 = DateTo7(SystemDate());

       Reset dsWMCMP;
       Exec sql
        select * into :dsWMCMP
         from WMCMP
           where CMco#=:inCo# and CMfnd=:inFnd and CMfyr=:inFyr
            and CMcase=:inCase;

       //*** Begin Add *** 07/07/21 ***************************************
       Reset dsWMCDP;
       Exec sql
        select * into :dsWMCDP
         from WMCDP
           where CDss#=:CMss#;
       //*** End   Add *** 07/07/21 ***************************************

       strClaim = %editc(CMfnd:'X') + '-' +
        %editc(CMfyr:'X') + '-' + %editc(CMcase:'X');
       decTotalInc =
        CMrr01 + CMrr02 + CMrr03 + CMrr04 + CMrr05 +
        CMrr06 + CMrr07 + CMrr08 + CMrr09 + CMrr10 +
        CMar01 + CMar02 + CMar03 + CMar04 + CMar05 +
        CMar06 + CMar07 + CMar08 + CMar09 + CMar10;

       json += '"claimInfo":{';
       json += '"claim":{' +
        BuildJsonNode('number':%EditC(CMcase:'X')) + ',' +
        //*** Begin Chg *** 07/07/21 ***********************************
        //***BuildJsonNode('full':strClaim) +
        BuildJsonNode('full':strClaim) + ',' +
        BuildJsonNode('external':CMpds#) +
        //*** End   Chg *** 07/07/21 ***********************************
        '},';
       //*** Begin Add *** 07/07/21 ***********************************
       // Create Address Note and remove end bracket
       chrClaimantAddress =
          BuildJsonAddress(CDadr1:CDadr2:CDcty:CDst:%EditC(CDzip:'X'));
       chrClaimantAddress = %scanrpl('}':',':chrClaimantAddress);
       //*** End   Add *** 0(CDzip:'X'))*******************************
       json += '"claimant":{' +
        //*** Begin Chg *** 07/07/21 ***********************************
        //***BuildJsonName(CMfsN:CMlsN:CMmid:'name') +
        BuildJsonName(CMfsN:CMlsN:CMmid:'name') + ',"address":' +
        %Trim(chrClaimantAddress) +
        BuildJsonNode('countryCode':CDctry) + ',' +
        BuildJsonNode('nonUsa':CDadr3) + '},' +
        BuildJsonNode('gender':CDsex) + ',' +
        BuildJsonNode('maritalStatus':CDmSts) + ',' +
        BuildJsonNode('dob':%char(CDdtob):'date') + ',' +
        BuildJsonNode('claimantSSN':CDss#) +
        //*** End   Chg *** 07/07/21 ***********************************
        '},';
       Exsr BuildJsonClaimStatus;
       json += '"dates":{' +
        BuildJsonNode('accident':%char(CMacDt):'date') + ',' +
        //*** Begin Chg *** 07/07/21 ***********************************
        //*** BuildJsonNode('entry':%char(CMiDt):'date') +
        BuildJsonNode('entry':%char(CMiDt):'date') + ',' +
        BuildJsonNode('lastWorked':%char(CMdtLw):'date') + ',' +
        BuildJsonNode('death':%char(CMdod):'date') + ',' +
        BuildJsonNode('hire':%char(CMhiDt):'date') +
        //*** End   Chg *** 07/07/21 ***********************************
        '},';
       //*** Begin Add *** 05/20/22 ***********************************
       Select;
       When CMltFl = 'M';
          strLtDesc = 'Medical Only';
       When CMltFl = 'I';
          strLtDesc = 'Indemnity';
       When CMltFl = 'R';
          strLtDesc = 'Record Only';
       Other;
          strLtDesc = 'Undefined';
       Endsl;

       json += '"LostTimeStatus":{' +
                    BuildJsonNode('code':CMltFl) + ',' +
                    BuildJsonNode('description':strLtDesc) + '},';
       //*** End   Add *** 05/20/22 ***********************************
       json += BuildJsonNode('jurisdictionState':CMjrSt) + ',';
       json += BuildJsonNode('totalIncurred':%EditC(decTotalInc:'3')) + ',';
       json = %scanrpl(',':'':json:%len(json)-1);
       json += '}';

       Return json;

       //=================================================================
       // Read output json - Claim Status
       //=================================================================
       Begsr BuildJsonClaimStatus;
        strSql = 'select * from WMCHP ' +
         'where CHco# = ' + %char(inCo#) + ' and ' +
         ' CHfnd = ' + %char(inFnd) + ' and ' +
         ' CHfyr = ' + %char(inFyr) + ' and ' +
         ' CHcase = ' + %char(inCase) +
         ' order by CHefDt desc';
        Exec sql close cursorWMCHP;
        Exec sql prepare sqlWMCHP from :strSql;
        Exec sql declare cursorWMCHP cursor for sqlWMCHP;
        Exec sql open cursorWMCHP;
        Select;
        When CMstat = 'O';
           strStatus = 'Open';
        When CMstat = 'C';
           strStatus = 'Closed';
        Other;
           strStatus = 'Undefined';
        Endsl;
        Reset dsWMCHP;
        Exec sql
         select * into :dsWMCHP
          from WMCHP
          where CHco# = :inCo# and CHfnd = :inFnd and CHfyr = :inFyr
           and CHcase = :inCase and CHoCSt = :CMstat
          order by CHefDt desc
          limit 1;
        json += '"status":{' +
         BuildJsonNode('code':CMstat) + ',' +
         BuildJsonNode('description':strStatus) + ',' +
         BuildJsonNode('effective':%char(dsWMCHP.CHefDt):'date') + ',';
        json += '"history":[';
        Dou not IsValidFetch(sqlStt);
           Exec sql fetch next from cursorWMCHP into :dsWMCHP;
           Select;
           When not IsValidFetch(sqlstt);
              Iter;
           Endsl;
           Select;
           When dsWMCHP.CHoCSt = 'O';
              strStatus = 'Open';
           When dsWMCHP.CHoCSt = 'C';
              strStatus = 'Closed';
           Other;
              strStatus = 'Undefined';
           Endsl;
           json += '{' +
            BuildJsonNode('effective':%char(dsWMCHP.CHefDt):'date') + ',' +
            BuildJsonNode('code':dsWMCHP.CHoCSt) + ',' +
            BuildJsonNode('description':strStatus) + ',' +
            BuildJsonNode('explanation':dsWMCHP.CHexpl) + '},';
        Enddo;
        Exec sql close cursorWMCHP;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ']},';
       Endsr;

      /End-Free
     pClaimJSONData    e

       //*** Beg Add ***** 11/03/21 **********************************
       //========================================================================
       // Create 'with' conditions to build Claim information including data from:
       //  SqlWithPolicyInfo(WDELP, WMEMP, WMAHP, WMAAP) and WMCMP
       //             - Returns a partial SQL string to be included in 'with'
       //========================================================================
     pSqlWithClaimInfo...
     p                 b                   export
     dSqlWithClaimInfo...
     d                 pi          5000    varying
      *** beg add ***** 12/15/21 ***************************
     d inAsOfDate                      d   options(*nopass)
      *** end add ***** 12/15/21 ***************************
     dstrWith          s           5000    varying
      *** beg chg ***** 12/15/21 ***************************
     d dtAsOfDate      s               d
       //strWith = SqlWithPolicyInfo + ', ' +
       // 'claim_info as (select * ' +
       // ' from WMCMP ' +
       // ' left join policy_info ' +
       // ' on CMco# = ELco# and CMfnd = ELfnd and CMfyr = ELfyr ' +
       // '  and CMemp# = ELemp# and CMdiv = ELdiv)';
       If %Parms > 1;
          dtAsOfDate = inAsOfDate;
       Else;
          dtAsOfDate = %Date();
       Endif;
       strWith = SqlWithPolicyInfo(dtAsOfDate) + ', ' +
        'claim_info as (select * ' +
        ' from WMCMP ' +
        ' left join policy_info ' +
        ' on CMco# = ELco# and CMfnd = ELfnd and CMfyr = ELfyr ' +
        '  and CMemp# = ELemp# and CMdiv = ELdiv)';
      *** end chg ***** 12/15/21 ***************************

       Return strWith;
     pSqlWithClaimInfo...
     p                 e
       //*** End Add *****  11/03/21  *************************

