     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
     H**************************************************************************************
     H* Created on 11/09/2012 by c:62 for Project WC2115
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H**************************************************************************************
    F*
     Ffmcml1    if   e           k disk    infds(cmfds)
     Ffaeal1    if   e           k disk    infds(eafds)
     Ffaeol4    if   e           k disk    rename(peo01:peo04) infds(eofds)

     d* Get Subscriber/Group
     d GetSubGroup     pr                  ExtPgm('FA700R')
     d   returnCode                   1
     d   strCompany#                  3
     d   strWcmsGroup                 3
     d   strSubscrber                 5
     d   strWcmcGroup                 3

     D cmfds           ds
     D rrnFMCMP              397    400b 0

     D eafds           ds
     D rrnFAEAP              397    400b 0

     D eofds           ds
     D rrnFAEOP              397    400b 0

     d returnCode      s              1    Inz(*Blanks)
     d strCompany#     s              3
     d strWcmsGroup    s              3
     d strSubscrber    s              5
     d strWcmcGroup    s              3
     d intSubscrber    s                   Like(CMsub#)
     d intWcmcGroup    s                   Like(CMfnd)

     D/copy fprcase

     D psds          esds                  extname(szq1p)
    P*====================================================================
    P* GetAdjState - Get Adjudication State
    P*
    P*  GetAdjState - returns Adjudication State
    P*   co#        - accepts the company #                       (required)
    P*   group      - accepts the group                           (required)
    P*   yr         - accepts the policy year                     (required)
    P*   claim#     - accepts the claim#                          (required)
    P*====================================================================
     PGetAdjState      b                   export
     DGetAdjState      PI             2
     D intCompany                     3  0 const
     D intWcmsGroup                   3  0 const
     D intYear                        3  0 const
     D intCase                        7  0 const

     C     keyFMCML1     klist
     C                   kfld                    intCompany
     C                   kfld                    intSubscrber
     C                   kfld                    intWcmcGroup
     C                   kfld                    intYear
     C                   kfld                    intCase

      /free

       returnCode=' ';
       strCompany#=%EditC(intCompany:'X');
       strWcmsGroup=%EditC(intWcmsGroup:'X');
       strSubscrber='00000';
       strWcmcGroup='000';

       GetSubGroup(returnCode:strCompany#:strWcmsGroup:strSubscrber:
                   strWcmcGroup);

       intSubscrber=%Dec(strSubscrber:5:0);
       intWcmcGroup=%Dec(strWcmcGroup:3:0);

       Chain keyFMCML1 pcm01;
       If %Found(FMCML1);
          Return CMaSt;
       Else;
          Return '  ';
       EndIf;

      /end-free

     PGetAdjState      e
    P*====================================================================
    P* GetFmcmpRrn - Get FMCMP relative record number
    P*
    P*  GetFmcmpRrn - returns FMCMP rrn
    P*   co#        - accepts the company #                       (required)
    P*   group      - accepts the group                           (required)
    P*   yr         - accepts the policy year                     (required)
    P*   claim#     - accepts the claim#                          (required)
    P*====================================================================
     PGetFmcmpRrn      b                   export
     DGetFmcmpRrn      PI             9  0
     D intCompany                     3  0 const
     D intWcmsGroup                   3  0 const
     D intYear                        3  0 const
     D intCase                        7  0 const

     C     keyFMCML1     klist
     C                   kfld                    intCompany
     C                   kfld                    intSubscrber
     C                   kfld                    intWcmcGroup
     C                   kfld                    intYear
     C                   kfld                    intCase

      /free

       returnCode=' ';
       strCompany#=%EditC(intCompany:'X');
       strWcmsGroup=%EditC(intWcmsGroup:'X');
       strSubscrber='00000';
       strWcmcGroup='000';

       GetSubGroup(returnCode:strCompany#:strWcmsGroup:strSubscrber:
                   strWcmcGroup);

       intSubscrber=%Dec(strSubscrber:5:0);
       intWcmcGroup=%Dec(strWcmcGroup:3:0);

       Chain keyFMCML1 pcm01;
       If %Found(FMCML1);
          Return rrnFMCMP;
       Else;
          Return 0;
       EndIf;

      /end-free

     PGetFmcmpRrn      e
    P*====================================================================
    P* GetFaeapRrn - Get FAEAP relative record number
    P*
    P*  GetFaeapRrn - returns FAEOP rrn
    P*   co#        - accepts the company #                       (required)
    P*   group      - accepts the group                           (required)
    P*   yr         - accepts the policy year                     (required)
    P*   claim#     - accepts the claim#                          (required)
    P*   eob#       - accepts the eob#                            (required)
    P*   recon      - accepts the recon#                          (required)
    P*====================================================================
     PGetFaeapRrn      b                   export
     DGetFaeapRrn      PI             9  0
     D intCompany                     3  0 const
     D intWcmsGroup                   3  0 const
     D intYear                        3  0 const
     D intCase                        7  0 const
     D intEob#                        9  0 const
     D intRecon                       3  0 const

     C     keyFAEAL1     klist
     C                   kfld                    intCompany
     C                   kfld                    intSubscrber
     C                   kfld                    intWcmcGroup
     C                   kfld                    intYear
     C                   kfld                    intCase
     C                   kfld                    intEob#
     C                   kfld                    intRecon

      /free

       returnCode=' ';
       strCompany#=%EditC(intCompany:'X');
       strWcmsGroup=%EditC(intWcmsGroup:'X');
       strSubscrber='00000';
       strWcmcGroup='000';

       GetSubGroup(returnCode:strCompany#:strWcmsGroup:strSubscrber:
                   strWcmcGroup);

       intSubscrber=%Dec(strSubscrber:5:0);
       intWcmcGroup=%Dec(strWcmcGroup:3:0);

       Chain keyFAEAL1 pea01;
       If %Found(FAEAL1);
          Return rrnFAEAP;
       Else;
          Return 0;
       EndIf;

      /end-free

     PGetFaeapRrn      e
    P*====================================================================
    P* GetFaeopRrn - Get FAEOP relative record number
    P*
    P*  GetFaeopRrn - returns FAEOP rrn
    P*   co#        - accepts the company #                       (required)
    P*   group      - accepts the group                           (required)
    P*   yr         - accepts the policy year                     (required)
    P*   claim#     - accepts the claim#                          (required)
    P*   eob#       - accepts the eob#                            (required)
    P*====================================================================
     PGetFaeopRrn      b                   export
     DGetFaeopRrn      PI             9  0
     D intCompany                     3  0 const
     D intWcmsGroup                   3  0 const
     D intYear                        3  0 const
     D intCase                        7  0 const
     D intEob#                        9  0 const

     C     keyFAEOL4     klist
     C                   kfld                    intCompany
     C                   kfld                    intSubscrber
     C                   kfld                    intWcmcGroup
     C                   kfld                    intYear
     C                   kfld                    intCase
     C                   kfld                    intEob#

      /free

       returnCode=' ';
       strCompany#=%EditC(intCompany:'X');
       strWcmsGroup=%EditC(intWcmsGroup:'X');
       strSubscrber='00000';
       strWcmcGroup='000';

       GetSubGroup(returnCode:strCompany#:strWcmsGroup:strSubscrber:
                   strWcmcGroup);

       intSubscrber=%Dec(strSubscrber:5:0);
       intWcmcGroup=%Dec(strWcmcGroup:3:0);

       Chain keyFAEOL4 peo04;
       If %Found(FAEOL4);
          Return rrnFAEOP;
       Else;
          Return 0;
       EndIf;

      /end-free

     PGetFaeopRrn      e
