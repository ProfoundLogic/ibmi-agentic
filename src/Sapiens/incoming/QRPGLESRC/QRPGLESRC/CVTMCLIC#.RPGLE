     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2007
     H*                                                               *
     H/TITLE CVTMCLIC#: Move data from 9 char field to 13 character   *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: Move data from 9 char license field      *
     H*                      to 13 char newly expanded field          *
     H*                                                               *
     H* CREATED: 12/10/07  C:198 PROJECT FL064                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     Ffmpdp     uf   e             disk
     C                   read      ppd01
     C                   dow       not %eof(fmpdp)
     C                   movel     pdflic        pdlic#
     C                   update    ppd01
     C                   read      ppd01
     C                   enddo
     C                   seton                                        lr
