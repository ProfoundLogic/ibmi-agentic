     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
    H/TITLE SRWXFMP - Populate fields in WXFMP & WXFMP3 for DTM
    H*********************************************************************
    H*  Created: 04/18/06    C:33   Seq#: WA132/52353                    *
    H*********************************************************************
    H* Revisions:                                                        *
    H*                                                                   *
    H* 09/20/13 C:136 Support: WA114 83544                               *
    H*                Fix GetFolderPath subprocedure to not return       *
    H*                garbage in varying parm                            *
    H*                                                                   *
    H* 09/03/13  C:33  Project: SY195                                    *
    H*                 Allow merge document to access IFS.               *
    H*                                                                   *
    H* 03/01/10  C:61  support wa114 71857                               *
    H*                 Enable processing for clt114.                     *
    H*                                                                   *
    H* 11/20/09  C:09  Seq #: WA136/70786                                *
    H*                 Enable processing for clt136.                     *
    H*                                                                   *
    H* 10/13/09  C:61  PROJ: 139WLR                                      *
    H*                 Enable processing for clt139.                     *
    H*                                                                   *
    H* 04/02/07  C:33  Seq#: WA109/54469                                 *
    H*                 Do not write records to WXFMP if there is no      *
    H*                  document ID passed in.                          *
    H*********************************************************************
     Fwxfmp     uf a e             disk
     Fwxfmp3    if a e           k disk
     Fwtenl     if   e           k disk
     D                 ds
     D FMFORM                        15
     D  xxFrm1                        8    overlay(FMFORM)
     D  xxFrm2                        3    overlay(FMFORM:9)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##Fldr          s             80
     D ##Fold          s             12
     D @@DcID          s             12
     D @@Fold          s             40
     D @@Mode          s              1
     D cltID           s              3
     D document        s                   like(@@DcID)
     D folder          s                   like(@@Fold)
     D load3           s              1n
     D pds             s              1n
     D xxForm          s                   like(FMFORM)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  09/03/13  **************************************
     DRtvDocEnv        pr                  ExtPgm('RTVDOCENV')
     D outEnv                         1
     DRtvDocRoot       pr                  ExtPgm('RTVDOCROOT')
     D outRoot                       10
     DGetFolderPath    pr            80    opdesc
     D inPath                        80    options(*varsize)
     D***  End   Add  ***  09/03/13  **************************************
      *** beg add ***** 09/20/13 ******************************************
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0
      *** end add ***** 09/20/13 ******************************************
    C*
    C*
     C     keyWTENL      klist
     C                   kfld                    FMCO#
     C                   kfld                    FMFND
     C                   kfld                    FMFORM
    C*
     C     *entry        plist
     C                   parm                    @@Mode
     C                   parm                    @@DcID
     C                   parm                    @@Fold
     C*
     C                   exsr      Initialize
    C*
     C                   select
     C                   when      @@Mode = '3'
     C                              and load3 = *on
     C                   exsr      Process3
     C                   when      @@Mode = 'E'
     C                   exsr      ProcessE
     C                   other
     C                   exsr      Process
     C                   endsl
    C*
     C                   eval      *inlr = *on
    C*
    C*====================================================================
    C* Initialize fields
    C*====================================================================
     C     Initialize    begsr
     C*
     C                   clear                   folder
     C                   clear                   document
     C*
     C                   if        %parms >= 2
     C                   eval      document = @@DcID
     C                   endif
     C*
     C                   if        %parms >= 3
     C                   eval      folder = @@Fold
     C                   endif
    C*
     C                   call      'SACHKPDS'
     C                   parm                    pds
    C*
     C                   call      'RTVCLTID'
     C                   parm                    cltID
    C*
     C                   select
      ******** beg chg ****** 11/20/09 ***********************************
      ******** beg chg ****** 10/13/09 ***********************************
     C********           when      pds = *on
     C********                      or cltID = '132'
     C*************      when      pds = *on
     C*************                 or cltID = '132'
     C*************                 or cltID = '139'
     C                   when      pds = *on
     C                              or cltID = '132'
     C                              or cltID = '139'
     C                              or cltID = '136'
      ******** end chg ****** 10/13/09 ***********************************
      ******** end chg ****** 11/20/09 ***********************************
     C                   move      *on           load3

      ******** beg add ****** 03/01/10 ***********************************
     C                   when      cltid = '114'
     C                   move      *on           load3
      ******** end add ****** 03/01/10 ***********************************

     C                   other
     C                   move      *off          load3
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Load WXFMP3 if needed
    C*====================================================================
     C     LoadP3        begsr
    C*
     C                   eval      xxForm = FMFORM
    C*
     C     xxForm        chain     pfm03
    C*
     C                   if        not %found
     C                   clear                   pfm03
     C                   eval      FMFORM = xxForm
     C                   write     pfm03
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Process any records
    C*====================================================================
     C     Process       begsr
     C*
     C***  Begin Add  ***  04/02/07  **************************************
     C                   if        document <> *blanks
     C***  End   Add  ***  04/02/07  **************************************
    C*
     C                   clear                   pfm01
    C*
     C                   eval      FMDCID = document
     C                   eval      ##Fold = folder
    C*
     C                   call      'DTCHKFLR'
     C                   parm                    ##Fold
    C*
     C                   if        %parms >= 3
     C                   eval      @@Fold = ##Fold
     C                   endif
    C*
     C                   call      'SA039R'
     C                   parm      folder        ##Fldr
    C*
     C***  Begin Chg  ***  09/03/13  **************************************
     C**********         eval      FMFOLD = ##Fldr
      /Free
       FMFOLD = GetFolderPath(##Fldr);
      /End-free
     C***  End   Chg  ***  09/03/13  **************************************
    C*
     C                   write     pfm01
    C*
     C                   if        load3 = *on
     C                   exsr      LoadP3
     C                   endif
    C*
     C***  Begin Add  ***  04/02/07  **************************************
     C                   endif
     C***  End   Add  ***  04/02/07  **************************************
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Process endorsement records
    C*====================================================================
     C     ProcessE      begsr
    C*
     C                   read      pfm01
    C*
     C                   dow       not %eof
    C*
     C     keyWTENL      chain     pen01                              61
    C*
     C                   if        %found
    C*
     C                   movel(p)  ENFLDR        ##Fold
    C*
     C                   call      'DTCHKFLR'
     C                   parm                    ##Fold
    C*
     C                   movel(p)  ##Fold        ENFLDR
    C*
     C                   call      'SA039R'
     C                   parm      ENFLDR        ##Fldr
    C*
     C                   movel     ##Fldr        ENFLDR
     C***  Begin Chg  ***  09/03/13  **************************************
     C**********         movel     ENFLDR        FMFOLD
      /Free
       FMFOLD = GetFolderPath(ENFLDR);
      /End-free
     C***  End   Chg  ***  09/03/13  **************************************
    C*
     C                   if        xxFrm2 <> *blanks
     C                   eval      FMDCID = xxFrm1 + '.' + xxFrm2
     C                   else
     C                   eval      FMDCID = FMFORM
     C                   endif
    C*
     C                   update    pfm01
    C*
     C                   if        load3 = *on
     C                   exsr      LoadP3
     C                   endif
    C*
     C                   endif
    C*
     C                   read      pfm01
    C*
     C                   enddo
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Process all records to load WXFMP3
    C*====================================================================
     C     Process3      begsr
    C*
     C                   read(n)   pfm01
    C*
     C                   dow       not %eof
    C*
     C                   exsr      LoadP3
    C*
     C                   read(n)   pfm01
    C*
     C                   enddo
    C*
     C                   endsr
     P***  Begin Add  ***  09/03/13  **************************************
     P*====================================================================
     P* Get the path for the document (IFS vs. QDLS)
     P*====================================================================
     PGetFolderPath    b
     DGetFolderPath    pi            80    opdesc
     D inPath                        80    options(*varsize)

     DdocEnv           s              1a
     DdocRoot          s             10a
      *** beg add ***** 09/20/13 *************************************
     dparmSize         s              5  0
      *** end add ***** 09/20/13 *************************************

      /Free
       //*** beg add ***** 09/20/13 *************************************
       Callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
       parmSize = inLen;
       //*** end add ***** 09/20/13 *************************************
       RtvDocEnv(docEnv);
       Select;
       When docEnv = 'I';
          RtvDocRoot(docRoot);
       //*** beg chg ***** 09/20/13 *************************************
       //*********** Return '/' + %trim(docRoot) + '/' + %trim(inPath);
          Return '/' + %trim(docRoot) + '/' + %subst(inPath:1:parmSize);
       //*** end chg ***** 09/20/13 *************************************
       Endsl;

       //*** beg chg ***** 09/20/13 *************************************
       //****Return inPath;
       Return %subst(inPath:1:parmSize);
       //*** end chg ***** 09/20/13 *************************************
      /End-free
     PGetFolderPath    e
     P***  End   Add  ***  09/03/13  **************************************
