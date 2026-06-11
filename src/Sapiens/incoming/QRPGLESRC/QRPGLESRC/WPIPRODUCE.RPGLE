     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H nomain
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 10/07/20  C:33  Project: WC2073                                                    *
     H*                 GetProdNm - Show name as First Last                                *
     H*                                                                                    *
     H* 06/02/20  C:33  Project: SY406                                                     *
     H*                 Add ValidStateForProducer                                          *
     H*                                                                                    *
     H* 05/21/03  C:132 Support: 37011                                                     *
     H*                 Recompiled to include WPRUSRPRMS change  (no chg to this module)   *
     H*                                                                                    *
     H**************************************************************************************
    F*
     Fwmapl     if   e           k disk    rename(pap01:pap00) infds(apfds)
     Fwmapp     if   e             disk    rename(pap01:pap0p)
     F***  Begin Add  ***  06/02/20  ******************
     Fwmaal1    if   e           k disk    rename(paa01:paa01)
     Fwmpsl     if   e           k disk    rename(pps01:pps00)
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     FwmexL     if   e           k disk    rename(pex01:pex00)
     F***  End   Add  ***  06/02/20  ******************
    F*
     D apfds           ds
     D aprrn                 397    400b 0

     D***  Begin Add  ***  06/02/20  ******************
     D/copy sPrString
     D***  End   Add  ***  06/02/20  ******************
     D/copy wprusrprms
     D/copy wprproduce

     D psds          esds                  extname(szq1p)





    P*====================================================================
    P* GetProdNm - Get producer name
    P*
    P*  GetProdNm - returns producer name
    P*   fein      - accepts the agent fein to search over (required)
    P*   mod       - accepts the agent mod to search over  (required)
    P*   prod#     - passed in for validation              (required)
    P*   ap_rrn    - returns the RRN of WMAPP
    P*   co#       - accepts the company # to search over
    P*====================================================================
    P*
     PGetProdNm        b                   export
    P*
     DGetProdNm        PI            50
     D fein                           9  0 const
     D mod                            3  0 const
     D prod#                          5  0 const
     D ap_rrn                         9  0              options(*nopass)
     D co#                            3  0 const        options(*nopass)

     D company         s                   like(APCO#)
     D name            s             50
     D***  Begin Add  ***  10/07/20  *****************************
     D isFirstLast     s              1n   inz('1')
     D***  End   Add  ***  10/07/20  *****************************

     C     apky          klist
     C                   kfld                    company
     C                   kfld                    fein
     C                   kfld                    mod
     C                   kfld                    prod#

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif
     C*
     C                   clear                   APLSN
     C                   clear                   APFSN
     C*
     C     apky          chain     pap00                              60
     C*
     C                   eval      name = %trim(APLSN)
     C*
     C                   if        APFSN <> *blanks
     C***  Begin Add  ***  10/07/20  *************************************
     C                   Select
     C                   When      isFirstLast
     C                   eval      name = %trim(APFSN) + ' ' + %trim(name)
     C                   Other
     C***  End   Add  ***  10/07/20  *************************************
     C                   eval      name = %trim(name) + ', ' + %trim(APFSN)
     C***  Begin Add  ***  10/07/20  *************************************
     C                   Endsl
     C***  End   Add  ***  10/07/20  *************************************
     C                   endif
     C*
     C                   if        %parms >= 4
     C                   eval      ap_rrn = aprrn
     C                   endif
     C*
     C                   return    name

     PGetProdNm        e





    P*====================================================================
    P* GetProdRRN - Get producer RRN
    P*
    P*  GetProdRRN - returns the RRN of WMAPP
    P*   fein     - passed in for validation  (required)
    P*   mod      - passed in for validation  (required)
    P*   prod#    - passed in for validation  (required)
    P*   co#      - passed in for validation
    P*====================================================================
    P*
     PGetProdRRN       b                   export
    P*
     DGetProdRRN       PI             9  0
     D fein                           9  0                  const
     D mod                            3  0                  const
     D prod#                          5  0                  const
     D co#                            3  0 options(*nopass) const

     D company         s                   like(APCO#)

     C     apky          klist
     C                   kfld                    company
     C                   kfld                    fein
     C                   kfld                    mod
     C                   kfld                    prod#

     C                   eval      company = UserCo#(q1user)
     C*
     C                   if        %parms >= 4
     C                   eval      company = co#
     C                   endif
     C*
     C     apky          chain     pap00                              60
     C*
     C                   return    aprrn
     C*
     PGetProdRRN       e




      //***  Begin Add  ***  06/02/20  ************************************
    P*====================================================================
    P* Validate the producer is associated with a state
    P*
    P*  ValidStateForProducer - returns *on if the state is valid
    P*   co#      - passed in for validation  (required)
    P*   fnd      - passed in for validation  (required)
    P*   agt#     - passed in for validation  (required)
    P*   prod     - passed in for validation  (required)
    P*   code     - passed in for validation  (required) this can be state or rate set code
     P*              returns the state evaluated
     P*   type     - passed in for validation  (optional) {State/RateSet, defaults to state}
    P*====================================================================
     PValidStateForProducer...
     P                 b                   export
     dValidStateForProducer...
     d                 pi             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inAgt#                         5  0 const
     d inProd                         5  0 const
     d ioCode                         3
     d inType                        10    const varying options(*nopass)

     disValid          s              1n   inz(*on)
     dstrState         s              2    inz
     dstrType          s             10    inz('State') varying
      /Free
       If %parms >= 6 and inType <> *blanks;
          strType = inType;
       Endif;

       // These values are used to determine if the state should be evaluated
       Clear EXprSL;
       Clear EXprST;
       Chain (inCo#:inFnd) pex00;

       If EXprSL <> *zero or EXprST <> *blank;
          isValid = *off;
          // Get employer rating state
          Select;
          When Upper(strType) = 'STATE';
             strState = %trim(ioCode);
          When Upper(strType) = 'RATESET';
             Clear RSjrSt;
             Chain (inCo#:ioCode) prs00;
             strState = RSjrSt;
          Endsl;
          Select;
          When strState = *blanks; // Only when rate set code not found
          Other;
             Clear AAfein;
             Clear AAmod;
             Clear APlFlg;
             Chain (inCo#:inFnd:inAgt#) paa01;
             Chain (inCo#:AAfein:AAmod:inProd) pap00;
             // Find if producer licensed for rating state
             Select;
             When APlFlg = 'Y' and APrSt = strState and APstat = 'A';
                isValid = *on;
             Other;
                Setll (inCo#:AAfein:AAmod:inProd) pps00;
                Dou %eof or isValid;
                   Reade (inCo#:AAfein:AAmod:inProd) pps00;
                   Select;
                   When %eof;
                      Iter;
                   When PSstat <> 'A';
                      Iter;
                   When PSlFlg = 'Y' and PSst = strState;
                      isValid = *on;
                      Iter;
                   Endsl;
                Enddo;
             Endsl;
          Endsl;
       Endif;
       ioCode = strState;
       Return isValid;
      /End-free
     PValidStateForProducer...
     P                 e
      //***  End   Add  ***  06/02/20  ************************************





