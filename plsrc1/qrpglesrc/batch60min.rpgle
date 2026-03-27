     H dftactgrp(*no) actgrp(*new)
     H option(*srcstmt:*nodebugio)
     H bnddir('QC2LE')

     FBATCHP    uf a e           k disk

     D getenv          pr              *   extproc('getenv')
     D  name                           *   value options(*string)

     D sleep           pr            10u 0 extproc('sleep')
     D  Interval                     10u 0 value

     D pIpAddr         S             20a   varying
     D qualJobName     S             28a   varying
     D startTime       S               t
     D timeDiff        S              5  0

     D PSDS           sds
     D  pPgmName               1     10
     D  JobName              244    253
     D  JobUser              254    263
     D  JobNumber            264    269

      /free

       qualJobName = JobNumber + '/' + %Trim(JobUser) + '/' + %Trim(JobName);

       monitor;
         pIpAddr = %str(getenv('REMOTE_ADDR'));
       on-error;
         pIpAddr = 'Unknown';
       endmon;

       startTime = %time();

       dow 1 = 1;

         sleep (10);
         timeDiff = %diff(%time() : startTime : *SECONDS);
         if timeDiff >= 3600;
           leave;
         endif;

         chain (qualJobName:pPgmName) BATCHP;
         if %found();
           ENDTIME = %time();
           update BATCHR;
         else;
           QUALJOB = qualJobName;
           PGMNAME = pPgmName;
           IPADDR  = pIpAddr;
           STRTIME = startTime;
           ENDTIME = %time();
           write BATCHR;
         endif;

       enddo;

       *inlr = *on;
       return;

      /end-free
