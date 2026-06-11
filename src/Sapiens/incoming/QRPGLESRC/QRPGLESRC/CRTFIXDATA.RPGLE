    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2007
      *========================================================================*
      *
      *  Purpose:
      *    To fix decimal data errors in any file by running FIXDATA xxxxx from
      *    the command line. This command will compile a new program into QTEMP
      *    over the file with decimal data errors and automatically be called
      *    to run over the file to fix the decimal data errors. After running,
      *    the program will be deleted from QTEMP. If a compiler isn't installed
      *    on the client machine, you can create the program on PDSDEV over the
      *    appropriate file structure, install at the client site, and call the
      *    program from the command line to remove the decimal data errors.
      *
      *  How to use:
      *    1) On client system, set up libl and run FIXDATA xxxxx, where xxxxx is
      *       the file name.
      *    2) If a library name is included in the command, that library will be
      *       moved to the top of the library list and the FIXDATA program will be
      *       run on the file in that library.
      *    3) If you wish to keep the program object that fixes the decimal data
      *       error, create a one char data area in qtemp named FIXDATA.  The
      *       program will be named FIXxxxxx where xxxxx is the file name. The
      *       source that created the program will be QTEMPSRC/SHELLFIXDD in
      *       QTEMP.
      *
      *  Installation instructions:
      *    Find out where WDAILYA is installed on the client machine. It should be in
      *    PDSOBJ or PDSTST. Install the following to that library:
      *
      *      FIXDATA *CMD
      *      FIXDECDATA *PGM
      *      CRTFIXDATA *PGM
      *      QRPGLESRC/SHELLFIXDD *SRC
      *
      *  Created by C:136 for support item 58558
      *========================================================================*
      *  Revisions:
      *========================================================================*
     Fqtempsrc  uf   e           k disk    rename(qtempsrc:rcdfmt)

     D                 ds
     D srcdta                       100
     D  spec                          1    overlay(srcdta:6)
     D  filenm                       10    overlay(srcdta:7)
     D  fieldnm                      10    overlay(srcdta:8)
     D  cors                          1    overlay(srcdta:24)
     D  opcode                        6    overlay(srcdta:26)
     D  fact2                        10    overlay(srcdta:36)
     D  const                        37    overlay(srcdta:44)
     D  result                        8    overlay(srcdta:50)
     D  rcdfnm                       23    overlay(srcdta:51)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D upper           c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lower           c                   'abcdefghijklmnopqrstuvwxyz'
     D @@pfnm          s             10
     D ##rfnm          s             10
     D kkseq           s                   like(srcseq)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     C     *entry        plist
     C                   parm                    @@pfnm

     C     key1          klist
     C                   kfld                    kkseq

     C                   eval      kkseq = 2
     C     key1          chain     qtempsrc                           99
     C                   if        *in99 = *off
     C     upper:lower   xlate     @@pfnm        @@pfnm
     C                   movel     @@pfnm        filenm
     C                   call      'RTVRCDF'
     C                   parm                    @@pfnm
     C                   parm                    ##rfnm
     C     upper:lower   xlate     ##rfnm        ##rfnm
     C     ##rfnm        cat(p)    ':pfrcdf)':0  rcdfnm
     C                   update    rcdfmt
     C                   endif

     C                   seton                                        lr
