    h*  (C) Copyright Progressive Data Solutions, Inc., 2006
    h*********************************************************************
    h/TITLE FIXWC1179A - Load Default Cash Dissolve Hierarchy
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    h*********************************************************************
    h* Called from:                                                      *
    h*   Command line                                                    *
    h*                                                                   *
    h* Calls to:                                                         *
    h*   None                                                            *
    h*                                                                   *
    h* Created:  01/20/06     C:136    PROJECT: WC1179                   *
    h*********************************************************************
    h* Revisions:                                                        *
    h*                                                                   *
     h* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    h*********************************************************************

     fwdf1l     if   e           k disk
     fwdcdp     uf a e             disk
     fwtctl     if   e           k disk
     fwtc3p     uf a e             disk

     d psds          esds                  extname(szq1p)
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
     d FIXWC1179A      pr
     d  @@co#                         3  0
     d  @@fnd                         3  0
     d FIXWC1179A      pi
     d  @@co#                         3  0
     d  @@fnd                         3  0

     d/copy sprdate

     d catseq          s              1    dim(99) ctdata perrcd(12)

     d                 ds                  inz
     d  ARCode                 1     24
     d                                     dim(12)
     d    F1CDET               1      2
     d    F1TXTX               3      4
     d    F1MFPT               5      6
     d    F1SFTX               7      8
     d    F11STT               9     10
     d    F12NDT              11     12
     d    F13RDT              13     14
     d    F14THT              15     16
     d    F15THT              17     18
     d    F1TXPY              19     20
     d    F1NBDT              21     22
     d    F1PRTR              23     24

     d  kkfyr          s                   like(f1fyr)
     d  xxhier         s                   like(cdhier)
     d  x1hier         s                   like(cdhier)
     d  x              s              2  0

     c     ctky          klist
     c                   kfld                    @@co#

     c     f1ky1         klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    kkfyr

     c     f1ky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c
     c                   eval      CDCO#  = @@CO#
     c                   eval      CDFND  = @@FND
     c                   eval      CDIUSR = Q1USER
     c                   eval      CDUSER = Q1USER
     c                   eval      CDIPGM = Q1PGM
     c                   eval      CDPGM  = Q1PGM
     c                   eval      CDIDT  = DateTo7(SystemDate)
     c                   eval      CDDT   = CDIDT
     c                   eval      CDTIME = TimeTo6(SystemTime)
     c                   eval      C3CO#  = @@CO#
     c                   eval      C3FND  = @@FND
     c                   eval      C3IUSR = Q1USER
     c                   eval      C3USER = Q1USER
     c                   eval      C3IPGM = Q1PGM
     c                   eval      C3PGM  = Q1PGM
     c                   eval      C3IDT  = CDDT
     c                   eval      C3DT   = CDDT
     c                   eval      C3TIME = CDTIME
     c
     c                   move      *hival        kkfyr
     c                   eval      xxhier = 1
     c                   eval      x1hier = 10000
     c     f1ky1         setgt     pf101
     c     f1ky          readpe    pf101
     c                   if        not(%eof)

     c                   z-add     1             x
     c                   dow       catseq(x) <> ' '
     c                   eval      CDCTYP = catseq(x)

     c                   if        catseq(x) <> 'P'
     c                   eval      CDNBCD = ARCode(x)

     c                   if        CDNBCD = *blank
     c                   eval      CDHIER = x1hier
     c                   add       1000          x1hier
     c                   else
     c                   eval      CDHIER = xxhier
     c                   add       1             xxhier
     c                   endif

     c                   else
     c                   eval      CDNBCD = *blanks
     c                   eval      C3CTYP = catseq(x)
     c                   eval      C3CNCD = ARCode(x)
     c                   write     pc301

     c                   if        C3CNCD = *blank
     c                   eval      CDHIER = x1hier
     c                   add       1000          x1hier
     c                   else
     c                   eval      CDHIER = xxhier
     c                   add       1             xxhier
     c                   endif

     c                   endif

     c                   write     pcd01
     c                   add       1             x
     c                   enddo                                                       catseq <> ' '

     c     ctky          setll     pct01
     c     ctky          reade     pct01
     c                   dow       not(%eof)
     c                   z-add     1             x
     c     ctctyp        lookup    catseq(x)                              60
     c                   if        *in60 = *off
     c                   eval      CDCTYP = CTCTYP
     c                   eval      CDNBCD = *blank
     c                   eval      CDCPCD = *blank
     c                   eval      CDHIER = x1hier
     c                   add       1000          x1hier
     c                   write     pcd01
      *
      * Write another General Fee record to be used seperately from installment fees.
      *
     c                   if        CTCTYP = 'F'
     c                   write     pcd01
     c                   endif

     c                   endif
     c     ctky          reade     pct01
     c                   enddo
     c                   endif                                                       pf101 not(%eof)

     c                   eval      *inlr = *on
** CATSEQ  (Default order of disbursement found in W8010R)
ETMS12345DDP
