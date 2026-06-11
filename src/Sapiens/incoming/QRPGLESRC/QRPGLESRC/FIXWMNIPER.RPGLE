     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE REMOVE DECIMAL DATA ERROR FROM FILE WMNIP
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*  CREATED: 10/12/10      C:257     SUPPORT: SEQ74482               *
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H* XX/XX/XX C: XXX                                                   *
    H*********************************************************************
     Fwmnip     uf   e             disk
     C*
     C                   read      pni01
     C                   dow       not(%eof)
     C                   update    pni01
     C                   read      pni01
     C                   enddo
     C                   seton                                        lr
