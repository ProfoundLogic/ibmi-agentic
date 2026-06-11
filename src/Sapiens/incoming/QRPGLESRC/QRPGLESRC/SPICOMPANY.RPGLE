     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
    H**********************************************************************************************
     H* REVISIONS:                                                                                 *
     H*                                                                                            *
     H* 03/02/18  C:180 Project: 114MJC                                                            *
     H*                 Add procedure for retrieving area code and phone #.                        *
     H*                                                                                            *
     H* 02/21/18  C:180 Project: 114MJC                                                            *
     H*                 Add procedures for retrieving address, FEIN                                *
     H*                                                                                            *
     H* 02/15/18  C:33  Support: 51241                                                             *
     H*                 Add Procedure for Retrieving only CO#                                      *
     H*                                                                                            *
     H* 01/23/18  C:62  Project: 129WUV                                                            *
     H*                 Add Procedure for Inquiry Authority Override Flag                          *
     H*                                                                                            *
     H* 11/10/03  C:33  Support: WA129/39885                                                       *
     H*                 Add procedure ValidCo#.                                                    *
    H**********************************************************************************************
     Fsmcol     if   e           k disk    infds(coFDS)

     D coFDS           ds
     D co_RRN                397    400b 0

     D/copy sprcompany

     P*=====================================================================
     P* getCompNam - get company name
     P*  Parameter 1 - 3 numeric
     P*  Returns - 40 character (blanks if invalid)
     P*=====================================================================
     PgetCompNam       B                   Export
     DgetCompNam       PI            40
     D compNum                        3  0 Const
     D
     D name            s                   like(coname)
     C*
     C     compNum       chain     smcol                              99
     C                   if        *in99 = *off
     C                   movel(p)  coname        name
     C                   endif
     C
     C                   return    name
     PgetCompNam       E
     P


     P***  Begin Add  ***  02/15/18  **************************************
     P*=====================================================================
     P* GetOnlyCo# - Returns co# if only one exists, otherwise *zero
     P*=====================================================================
     PGetOnlyCo#       b                   export
    P*
     DGetOnlyCo#       pi             3  0
     D*
     DintCo#           s                   like(COco#)
      /Free
       Setll *loval smcol;
       Read smcol;
       intCo# = COco#;
       Read smcol;
       If COco# <> intCo#;
          Clear intCo#;
       Endif;
       Return intCo#;
      /End-free
     PGetOnlyCo#       e


     P*=====================================================================
    P*  ValidCo# - return *on if company is valid
    P*   group   - passed in for validation  (required)
    P*   coRRN   - returns the RRN of SMCOP
     P*=====================================================================
    P*
     PValidCo#         b                   export
    P*
     DValidCo#         PI             1n
     D co#                                 like(COCO#)                  const
     D coRRN                          9  0             options(*nopass)

     C*
     C     co#           chain     smcol
     C*
     C                   if        %found and %parms >= 2
     C                   eval      coRRN = co_RRN
     C                   endif
     C*
     C                   return    %found

     PValidCo#         e
     P*** Begin Add *** 01/23/18 *****************************************
     P*=====================================================================
     P* getInqAuthOvr - get Inquiry Authority Override
     P*  Parameter 1 - 3 numeric
     P*  Returns - 1 character
     P*=====================================================================
     PGetInqAuthOvr    B                   Export
     DGetInqAuthOvr    PI             1
     D compNum                        3  0 Const
     D
     D inqAuthFlag     s                   like(COinqA) Inz(' ')
     C*
     C     compNum       chain     smcol                              99
     C                   if        *in99 = *off
     C                   movel(p)  COinqA        inqAuthFlag
     C                   endif
     C
     C                   return    inqAuthFlag
     PGetInqAuthOvr    E
     P*** End   Add *** 01/23/18 *****************************************
     P***  Begin Add  ***  02/15/18  **************************************

     P*=====================================================================
     P* GetCompAdr - get company address
     P*  Parameters - Company number (3,0), Address type (1)
     P*    Returns:   Address line 1  (40), Address Line 2 (40), City (25),
     P*               State (2), Zip code (9,0).
     P*=====================================================================
     PGetCompAdr       B                   Export
     DGetCompAdr       PI
     D compNum                        3  0 Const
     D AdrType                        1    Const
     D Adr1                                like(COadr1)
     D Adr2                                like(COadr2)
     D City                                like(COcty)
     D State                               like(COst)
     D Zip                                 like(COzip)
      /Free
       Adr1  = *blanks;
       Adr2  = *blanks;
       City  = *blanks;
       State = *blanks;
       Zip   = *zeros;
       Chain compNum smcol;
       Select;
       When AdrType = 'D' or AdrType = *blanks;   //Default address
          If %found(smcol);
             Adr1  = COadr1;
             Adr2  = COadr2;
             City  = COcty;
             State = COst;
             Zip   =  COzip;
          Endif;
       When AdrType = 'P';                         //Physical addres
           //Code when needed.
       When AdrType = 'M';                         //Mailing  addres
           //Code when needed.
       Endsl;
      /End-Free
     PGetCompAdr       E
     P*=====================================================================
     P* GetCompFein - Returns FEIN if it exists, otherwise *zero
     P*=====================================================================
     PGetCompFein      B                   export
     DGetCompFein      PI             9P 0
     D intCo#                              like(COco#) CONST
      *
     D intFein         s                   like(COfein) inz
      /Free
       intFein = *zeros;
       Chain intCo# smcol;
       If %found(smcol);
          IntFein = COfein;
       Endif;
       Return intFein;
      /End-free
     PGetCompFein      e
     P***  Begin Add  ***  03/02/18  **************************************
     P*=====================================================================
     P* GetCompPhon - Returns Area code and Phone number.
     P*=====================================================================
     PGetCompPhon      B                   export
     DGetCompPhon      PI            10A
     D intCo#                              like(COco#) CONST
     D PhoneType                      1A   CONST
      *
     D AreaPhone       s             10A
      /Free
       AreaPhone = *zeros;
       Chain intCo# smcol;
       If %found(smcol);
          Select;
          When PhoneType = '1';     //First phone #
            AreaPhone =  %Editc(COarea:'X') + %Editc(COphn#:'X');
          When PhoneType = '2';     //Next  phone #
          Other;
          Endsl;
       Endif;
          Return AreaPhone;
      /End-free
     PGetCompPhon      e
     P***  End   Add  ***  03/02/18  **************************************
