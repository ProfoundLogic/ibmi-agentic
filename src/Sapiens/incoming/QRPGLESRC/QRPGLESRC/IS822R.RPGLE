     *=====================================================================
     *  (c) Copyright Progressive Data Solutions, Inc., 2000
     *=====================================================================
    H/TITLE IS822R - GetDocTyp
     *=====================================================================
     *
     * Description:
     *  Get the Document Type record and return the info to the calling
     *  program.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     * Revisions:
     *
     *=====================================================================
     * Document Type File  - DOC TYPE KEY
     FIMDTL     IF   E           K Disk

     * Document Type File  - DARTS RMCD KEY
     FIMDTL2    IF   E           K Disk    Rename(pdt01:pdt02)

     *=====================================================================
     * Data Structure & Array Definitions
     *=====================================================================
     D psds          esds                  extname(szq1p)

     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     D e#Docs          s              5a
     D eBchSts         s              2a
     D eDesc           s             40a
     D eDelim          s              3a
     D eDocType        s              8a
     d eFileSys        s             10a
     D eQue            s             10a
     D eIdxPgm         s             10a
     D eRmkCode        s              5a
     D eRtnCode        s              1a
     D eSecLvl         s              3a
     d eImportPgm      s             10a
     d eImpDir         s             12a

     *=====================================================================
     * KList Definitions
     *=====================================================================
     * DOCUMENT TYPE FILE
     C     dtKey         klist
     C                   kfld                    eDocType

     * DOCUMENT TYPE FILE
     C     dtKey1        klist
     C                   kfld                    eRmkCode

     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** ENTRY PARAMETERS
     C     *entry        plist
     C                   parm                    eDocType                       Document type
     C                   parm                    eRmkCode                       Remark code
     c                   parm                    eFileSys                       File system
     C                   parm                    eDelim                         Delimiter
     C                   parm                    eDesc                          Document desc
     C                   parm                    eQue                           Document que
     C                   parm                    e#Docs                         # Docs in batch
     C                   parm                    eSecLvl                        Security level
     C                   parm                    eIdxPgm                        Index program
     C                   parm                    eBchSts                        Batch status
     C                   parm                    eImportPgm
     C                   parm                    eImpDir
     C                   parm                    eRtnCode                       Return code

     *=====================================================================
     * Main
     *=====================================================================
     C                   clear                   eRtnCode

     * Use either the Doc type or the DARTS remark code to get the
     * Document Type record.
     C                   if        eDocType <> *blanks
     C     dtKey         chain     pdt01                              60
     C                   else
     C     dtKey1        chain     pdt02                              60
     C                   endif

     C                   if        *in60 = *off
     C                   movel     dtdoct        eDocType
     C                   movel     dtrmcd        eRmkCode
     c                   movel     dtfsid        eFileSys
     C                   move      dtdlim        eDelim
     C                   movel     dtdesc        eDesc
     C                   movel     dtdque        eQue
     C                   move      dt#dib        e#Docs
     C                   movel     dtsecv        eSecLvl
     C                   movel     dtidxp        eIdxPgm
     C                   movel     dtbsts        eBchSts
     C                   movel     dtimpg        eImportPgm
     C                   movel     dtdid         eImpDir

     C                   else
     * ERROR - Document Type record not found.
     C                   clear                   eDocType
     C                   clear                   eRmkCode
     c                   clear                   eFileSys
     C                   clear                   eDelim
     C                   clear                   eDesc
     C                   clear                   eQue
     C                   clear                   e#Docs
     C                   clear                   eSecLvl
     C                   clear                   eIdxPgm
     C                   clear                   eBchSts
     C                   clear                   eImportPgm
     C                   clear                   eImpDir
     C                   eval      eRtnCode = '1'

     C                   endif

     C                   return
