     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')

     *===============================================================================
     *
     * SPIMASK - Procedures for masking
     *
     * Created   : 06/06/20
     * Programmer: C:33
     * Project   : SY420
     *
     *===============================================================================
      * Revisions:                                                                                 *
      *                                                                                            *
     *===============================================================================

     d/copy sPrMask
     d/copy sPrString
     d/copy sPrUser
     d/copy sPrUsrPrms

     di1               s             10i 0
     dmaskCo#          s              3  0
     dmaskDone         s            300a   dtaara('SPIMASK')
     disMaskDoneSSN    s              1n   inz('1')
     disMaskDoneBank   s              1n   inz

     d psds          esds                  extname(szq1p)

     ddsMasking        ds                  dim(100) qualified
     d name                          25a   varying
     d value                        100a   varying
     d mask                         100a   varying

     ddsSMCOP          ds                  qualified
     d mask01                        35a
     d mask02                        35a
     d mask03                        35a
     d mask04                        35a
     d mask05                        35a
     d mask06                        35a
     d mask07                        35a
     d mask08                        35a
     d mask09                        35a
     d mask10                        35a

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Get client ID
     d CheckObj        pr                  extpgm('SRCHKOBJ')
     d   outObjName                  10
     d   outObjType                   7
     d   outObjErr                    1

      *Entry
     dMasking          pr                  extpgm('SPIMASK')
     d inMode                        10a   const
     d inMaskType                    25a   const
     d inFieldName                   25a   const
     d ioValue                      100a
     dMasking          pi
     d inMode                        10a   const
     d inMaskType                    25a   const
     d inFieldName                   25a   const
     d ioValue                      100a

      /Free
       Exec SQL set option commit = *NONE;

       Select;
       When Upper(inMode) = '*MASK';
          ioValue = MaskData(inMaskType:inFieldName:%trim(ioValue));
       When Upper(inMode) = '*UNMASK';
          ioValue = UnMaskData(inMaskType:inFieldName:%trim(ioValue));
       When Upper(inMode) = '*END';
          *inlr = *on;
       Endsl;

       Return;
      /End-free

      *=====================================================================
      * Retrieve the SMCOP values
      *=====================================================================
     pGetSMCOP         b
     dGetSMCOP         pi
      /Free
       maskCo# = GetUserInfo(q1user);
       Exec SQL
        select COmk01, COmk02, COmk03, COmk04, COmk05,
               COmk06, COmk07, COmk08, COmk09, COmk10
         into :dsSMCOP
         from SMCOP
         where COco# = :maskCo#;
      /End-free
     pGetSMCOP         e

      *=====================================================================
      * Determine if masking is active
      *=====================================================================
     pIsMaskingActive  b                   export
     dIsMaskingActive  pi             1n
     d inMaskType                    25a   varying const

     disMaskActive     s              1n   inz
     dstrMaskDone      s            300a   inz
      /free
       Exsr SetDone;
       Select;
       When Upper(inMaskType) = 'SSN';
          Return isMaskDoneSSN;
       When Upper(inMaskType) = 'BANK';
          Return isMaskDoneBank;
       When Upper(inMaskType) = '*ANY';
          Return isMaskDoneSSN or isMaskDoneBank;
       Endsl;
       //==================
       Begsr SetDone;
        Monitor;
           In maskDone;
        On-error;
        Endmon;
        Dou strMaskDone = *blanks;
           strMaskDone = ParseString(maskDone:',':1:*on);
           Select;
           When strMaskDone = *blanks;
              Iter;
           When Upper(strMaskDone) = 'SSN';
              isMaskDoneSSN = *on;
           When Upper(strMaskDone) = 'BANK';
              isMaskDoneBank = *on;
           Endsl;
        Enddo;
       Endsr;
      /end-free
     pIsMaskingActive  e

      *=====================================================================
      * Mask data - show only last 4
      *=====================================================================
     pMaskData         b                   export
     dMaskData         pi           100a   varying
     d inMaskType                    25a   varying const
     d inFieldName                   25a   varying const
     d inValue                      100a   varying const

     dconShowCharBank  c                   4
     dconShowCharSSN   c                   4

     daryAssociatedProfiles...
     d                 s             10    dim(20)
     ddecShowChar      s              3  0 inz(4)
     ddecValueLen      s              5  0 inz
     disMask           s              1n   inz(*off)
     dstrMaskAlpha     s            100a   inz(*ALL'*')

     ddsMask           ds                  likeds(dsMaskingGlobal) inz
      /free
       i1 = %lookup(inFieldName:dsMasking(*).name);
       If i1 <= 0;
          i1 = %lookup(*blanks:dsMasking(*).name);
       Endif;
       Exsr SetMask;
       dsMasking(i1).name = inFieldName;
       dsMasking(i1).value = inValue;
       dsMasking(i1).mask = inValue;
       decValueLen = %len(%trim(inValue));
       If isMask and inValue <> *blank and decValueLen > decShowChar;
          %subst(dsMasking(i1).mask:1:decValueLen-decShowChar) = strMaskAlpha;
       Endif;
       Return dsMasking(i1).mask;
       //==================
       Begsr SetMask;
        If not IsMaskingActive(inMaskType);
           Leavesr;
        Endif;
        GetSMCOP();
        Select;
        When Upper(inMaskType) = 'SSN';
           dsMask = dsSMCOP.mask01;
           decShowChar = conShowCharSSN;
        When Upper(inMaskType) = 'BANK';
           dsMask = dsSMCOP.mask02;
           decShowChar = conShowCharBank;
        Endsl;
        isMask = dsMask.flag1 = 'Y';
        // need to check against user, group profile and supplimental group profiles
        aryAssociatedProfiles = GetAssociatedProfiles();
        If dsMask.user1 <> *blanks
         and %lookup(dsMask.user1:aryAssociatedProfiles) > 0;
           isMask = *off;
        Endif;
        If dsMask.user2 <> *blanks
         and %lookup(dsMask.user2:aryAssociatedProfiles) > 0;
           isMask = *off;
        Endif;
        If dsMask.user3 <> *blanks
         and %lookup(dsMask.user3:aryAssociatedProfiles) > 0;
           isMask = *off;
        Endif;
       Endsr;
      /end-free
     pMaskData         e

      *=====================================================================
      * Unmask data - return value prior to masking
      *=====================================================================
     pUnMaskData       b                   export
     dUnMaskData       pi           100a   varying
     d inMaskType                    25a   varying const
     d inFieldName                   25a   varying const
     d inValue                      100a   varying const
      /free
       i1 = %lookup(inFieldName:dsMasking(*).name);
       Select;
       When i1 <= 0;
          Return inValue;
       When dsMasking(i1).value = *blanks;
          Return inValue;
       When dsMasking(i1).mask = inValue;
          Return dsMasking(i1).value;
       Endsl;
       Return inValue;
      /end-free
     pUnMaskData       e
