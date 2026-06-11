     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H/TITLE CVTSTETP - Program for creating Email Token File Records
     H dftname(CVTSTETP)
     H option(*nodebugio:*srcstmt)
     H ALWNULL(*UsrCtl)
     H bnddir('STBNDDIR')
     H bnddir('WTGRPCFG')
     H**********************************************************************
     H*===============================================================*
     H*  Description:
     H*
     H*  This program creates the Email Token records.
     H*
     H*  Notes: When new email tokens are added, a new record needs to
     H*         be added to the Email Tokens Map file 'STKMP'.
     H*
     H*  Created:  01/03/23  C:344 Project: WC2875
     H*===============================================================*
     H*  Revisions:                                                   *
    H*                                                               *
    H*  08/26/24 C:053 Support: 116977                               *
    H*                 Additional tokens added.                      *
    H*                                                               *
   H* ------------------------------------------------------------- *
   H*  NOTE: Please add comments to the top of Revisions.           *
   H* ------------------------------------------------------------- *
   H*===============================================================*
     FSTETL     UF A E           K DISK

     D/copy sPrDate
     D/copy sPrUsrPrms

     d intCo#          s              3s 0 inz
     d strToken        s                   like (ETToken) inz
     d psd           esds                  extname(szq1p)
     d CreateToken     pr
     d                               50    varying const
     d                                1    varying const
     d                                1    varying const
     d                                1    varying const
     d                                1    varying const
     d                                1    varying const
     d                                1    varying const
     d                                1    varying const
     d                                1    varying const

      *                                                               *
       //===============================================================
       // Main
       //===============================================================

       // Populate Tokens
       CreateToken('#agency#':
        'Y':'Y':'Y':'Y':' ':'Y':'Y':'Y');
       CreateToken('#company name#':
        'Y':'Y':'Y':'Y':'Y':'Y':'Y':'Y');
       CreateToken('#current billed#':
        ' ':' ':' ':' ':' ':' ':'Y':' ');
       CreateToken('#current date#':
        'Y':'Y':'Y':'Y':'Y':'Y':'Y':'Y');
       CreateToken('#division number#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#due date#':
        ' ':' ':' ':' ':' ':' ':'Y':' ');
       CreateToken('#insured fein#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#insured name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#ip#':
        ' ':' ':' ':' ':' ':' ':'Y':'Y');
       CreateToken('#logo#':
        'Y':'Y':'Y':'Y':'Y':'Y':'Y':'Y');
       CreateToken('#policy effective date#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#policy expiration date#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#policy number#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#posting date#':
        ' ':' ':' ':' ':' ':' ':'Y':' ');
       CreateToken('#previous balance#':
        ' ':' ':' ':' ':' ':' ':'Y':' ');
       CreateToken('#producer#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#report period#':
        ' ':' ':' ':' ':' ':' ':'Y':' ');
       CreateToken('#submitter email#':
        ' ':' ':' ':' ':' ':' ':' ':'Y');
       CreateToken('#submitter name#':
        ' ':' ':' ':' ':' ':' ':' ':'Y');
       CreateToken('#submitter phone#':
        ' ':' ':' ':' ':' ':' ':' ':'Y');
       CreateToken('#total period payment#':
        ' ':' ':' ':' ':' ':' ':'Y':' ');
       // policy services
       CreateToken('#policy services staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#policy services email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#policy services name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#policy services phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       // underwriter
       CreateToken('#underwriter staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#underwriter email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#underwriter name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#underwriter phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       // premium accountant
       CreateToken('#premium accountant staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#premium accountant email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#premium accountant name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#premium accountant phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       // audit reviewer
       CreateToken('#audit reviewer staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#audit reviewer email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#audit reviewer name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#audit reviewer phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       // audit manager
       CreateToken('#audit manager staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#audit manager email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#audit manager name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#audit manager phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       // loss control rep
       CreateToken('#loss control rep staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#loss control rep email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#loss control rep name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#loss control rep phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       // claim contact
       CreateToken('#claim contact staff code#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#claim contact email#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#claim contact name#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       CreateToken('#claim contact phone#':
        'Y':'Y':'Y':' ':' ':'Y':'Y':'Y');
       //*** beg add *** 08/26/24 ***//
       // claims
       CreateToken('#claimant name#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       CreateToken('#accident date#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       CreateToken('#claim number#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       CreateToken('#claim type#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       CreateToken('#adjuster name#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       CreateToken('#adjuster phone#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       CreateToken('#adjuster email#':
        'Y':'Y':'Y':' ':'Y':'Y':'Y':'Y');
       //*** end add *** 08/26/24 ***//
       // End Program
       *inlr = *on;
       return;

       //===============================================================
       // Map Flags and Create Tokens Procedure
       //===============================================================
     pCreateToken      b                   export

     dCreateToken      pi
     d inToken                       50    varying const
     d inPolicyFlg                    1    varying const
     d inClaimFlg                     1    varying const
     d inMangcFlg                     1    varying const
     d inAgentFlg                     1    varying const
     d inReportFlg                    1    varying const
     d inPrecertFlg                   1    varying const
     d inPaygoFlg                     1    varying const
     d inQuote                        1    varying const

       // Execute the following token creation procedures
       intCo# = GetUserInfo(q1user);
       Chain (intCo#:inToken) pet01;
       If %found(STETL);
          ETpolicy   = inPolicyFlg;
          ETclaim    = inClaimFlg;
          ETmangc    = inMangcFlg;
          ETagent    = inAgentFlg;
          ETreport   = inReportFlg;
          ETprecert  = inPrecertFlg;
          ETpaygo    = inPaygoFlg;
          ETquote    = inQuote;
          ETpgm      = Q1PGM;
          ETuser     = Q1USER;
          ETdt       = Dateto7(SystemDate());
          ETtime     = Timeto6(SystemTime());
          ETstat     = 'C';
          Update pet01;
       Else;
          ETco#      = intCo#;
          ETtoken    = inToken;
          ETpolicy   = inPolicyFlg;
          ETclaim    = inClaimFlg;
          ETmangc    = inMangcFlg;
          ETagent    = inAgentFlg;
          ETreport   = inReportFlg;
          ETprecert  = inPrecertFlg;
          ETpaygo    = inPaygoFlg;
          ETquote    = inQuote;
          ETpgm      = Q1PGM;
          ETuser     = Q1USER;
          ETdt       = Dateto7(SystemDate());
          ETtime     = Timeto6(SystemTime());
          ETipgm     = Q1PGM;
          ETiusr     = Q1USER;
          ETidt      = Dateto7(SystemDate());
          ETitim     = Timeto6(SystemTime());
          ETstat     = 'A';
          Write pet01;
       Endif;

     pCreateToken      e

