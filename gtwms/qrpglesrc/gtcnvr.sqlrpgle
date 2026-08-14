**free

//////////////////////////////////////////////////////////////////////////
// GTCNVR -- Cycle Count, variance review and post.
//
// THIS is where the expected quantity finally appears. The counting screen
// never received it (see GTCNER); this one needs it, so it has its own display
// file that carries it. Two programs rather than one is what makes the count
// genuinely blind rather than blind-looking.
//
// THE PHOTO RULE IS DECLARATIVE. GTREASON carries photo_required per reason
// code, so "a variance needs evidence" is data rather than logic: pick a reason
// whose flag is Y and the line will not post until it has a photograph. Change
// the policy by updating a row, not by editing this program.
//
// POSTING IS ONE UNIT OF WORK: the count header, every line, one adjustment
// movement per varying line, and the inventory balance for each. Any failure
// rolls all of it back -- a half-applied count is inventory nobody can trust.
//////////////////////////////////////////////////////////////////////////

ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTIMG');

dcl-f gtcnvd workstn sfile(vasfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql set option commit = *chg, closqlcsr = *endmod;

/copy gtimg_pr.rpgle

dcl-pi *n;
  inCnt  char(10) const;
  inLang char(2);
end-pi;

dcl-c MAX_ROWS 60;
dcl-c MAX_RSN  12;

dcl-s rrn     int(10) inz(0);
dcl-s i       int(10);
dcl-s done    ind inz(*off);

dcl-ds lines qualified dim(MAX_ROWS);
  lineNo int(10);
  sku    char(15);
  exp    packed(11 : 2);
  cnt    packed(11 : 2);
  reason char(4);
  photos int(10);
  need   char(1);
end-ds;
dcl-s lineCount int(10) inz(0);

//  Work fields.
dcl-s wLine   int(10);
dcl-s wSku    char(15);
dcl-s wDesc   varchar(60);
dcl-s wDescEn varchar(60);
dcl-s wDescFr varchar(60);
dcl-s wUom   char(3);
dcl-s wImg   int(10);
dcl-s wExp   packed(11 : 2);
dcl-s wCnt   packed(11 : 2);
dcl-s wVar   packed(11 : 2);
dcl-s wRsn   char(4);
dcl-s wPho   int(10);
dcl-s wFlag  char(1);
dcl-s wNeed  char(1);
dcl-s wCode  char(4);
dcl-s wRDesc varchar(40);
dcl-s wRDsEn varchar(40);
dcl-s wRDsFr varchar(40);
dcl-s wRPho  char(1);
dcl-s wOper  varchar(18);
dcl-s wRest  varchar(1024);
dcl-s wTok   varchar(32);
dcl-s wIdx   int(10);
dcl-s wPos   int(10);
dcl-s wCount int(10);
dcl-s wKey   char(20);
dcl-s wImgId int(10);
dcl-s wCap   varchar(60);

//------------------------------------------------------------------------
langpref = inLang;
hcnt     = inCnt;
posted   = 'N';

exsr loadOperator;
exsr loadHeader;

dow not done;
  exsr loadLineSubfile;
  exsr loadReasonList;
  action  = *blanks;
  vedits  = *blanks;
  imgdata = *blanks;
  imgline = 0;
  exfmt cntvar;
  msg     = *blanks;
  msgkind = *blanks;

  select;
    when action = 'EXIT';
      done = *on;

    when action = 'LANGEN';
      langpref = 'EN';
      inLang   = 'EN';
      exsr saveLanguage;

    when action = 'LANGFR';
      langpref = 'FR';
      inLang   = 'FR';
      exsr saveLanguage;

    when action = 'APPLY';
      //  Reason codes chosen on the cards.
      exsr applyReasons;

    when action = 'ADDIMG';
      exsr storePhoto;

    when action = 'POST';
      exsr postCount;
      if posted = 'Y';
        done = *on;
      endif;

    when action = *blanks;
      //  Enter with nothing pressed. Redisplay.

    other;
  endsl;
enddo;

*inlr = *on;
return;

//------------------------------------------------------------------------
// Reason codes, carried in one field as "seq:code;" -- the same channel the
// quantities use, and for the same reason: READC cannot see a template-rendered
// subfile's inputs.
//------------------------------------------------------------------------
begsr applyReasons;
  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  wCount = 0;
  wRest  = %trim(vedits);

  dow wRest <> *blanks;
    wPos = %scan(';' : wRest);
    if wPos > 0;
      if wPos > 1;
        wTok = %subst(wRest : 1 : wPos - 1);
      else;
        wTok = *blanks;
      endif;
      if wPos >= %len(wRest);
        wRest = *blanks;
      else;
        wRest = %subst(wRest : wPos + 1);
      endif;
    else;
      wTok  = wRest;
      wRest = *blanks;
    endif;

    if %trim(wTok) <> *blanks;
      wPos = %scan(':' : wTok);
      if wPos > 1 and wPos < %len(%trim(wTok));
        monitor;
          wIdx = %int(%subst(wTok : 1 : wPos - 1));
          wRsn = %subst(%trim(wTok) : wPos + 1);

          if wIdx >= 1 and wIdx <= lineCount and wRsn <> lines(wIdx).reason;
            wLine = lines(wIdx).lineNo;
            exec sql
              update gtcountdtl
                 set reason_code = :wRsn
               where count_id = :hcnt and line_no = :wLine;
            if sqlcode >= 0;
              exec sql commit;
              lines(wIdx).reason = wRsn;
              wCount += 1;
            else;
              exec sql rollback;
            endif;
          endif;
        on-error;
        endmon;
      endif;
    endif;
  enddo;

  if wCount = 0;
    exsr msgNothingChanged;
  else;
    exsr msgReasonsSaved;
  endif;
endsr;

//------------------------------------------------------------------------
// A photograph for one variance line. The image travels base64 on a display
// file field for the same reason the item photos do -- no upload endpoint can
// reach TIGERPOC (design doc §29).
//------------------------------------------------------------------------
begsr storePhoto;
  if imgdata = *blanks or imgline <= 0;
    exsr msgNoPhoto;
    leavesr;
  endif;

  wIdx = 0;
  for i = 1 to lineCount;
    if lines(i).lineNo = imgline;
      wIdx = i;
      leave;
    endif;
  endfor;

  if wIdx = 0;
    exsr msgNoPhoto;
    leavesr;
  endif;

  //  ref_key is the count and the line, so the evidence is attached to the
  //  finding rather than to the item in general.
  wKey = %trim(hcnt) + %char(lines(wIdx).lineNo);
  if langpref = 'FR';
    wCap = 'Écart de dénombrement ' + %trim(lines(wIdx).sku);
  else;
    wCap = 'Count variance ' + %trim(lines(wIdx).sku);
  endif;

  wImgId = gtimg_add('CNT' : wKey : %trim(imgdata) : wCap : wCap);

  if wImgId <= 0;
    exec sql rollback;
    exsr msgPhotoFailed;
    leavesr;
  endif;

  wLine = lines(wIdx).lineNo;
  exec sql
    update gtcountdtl
       set photo_count = photo_count + 1
     where count_id = :hcnt and line_no = :wLine;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPhotoFailed;
    leavesr;
  endif;

  exec sql commit;
  exsr msgPhotoSaved;
endsr;

//------------------------------------------------------------------------
// POST. One unit of work: header, lines, an adjustment movement per varying
// line, and the balance for each.
//
// Validation first, OUTSIDE the commit boundary, so a refused post never
// starts a transaction it has to unwind.
//------------------------------------------------------------------------
begsr postCount;
  posted = 'N';

  if hstat = 'POST';
    exsr msgAlreadyPosted;
    leavesr;
  endif;

  if ndone < nlines;
    exsr msgNotAllCounted;
    leavesr;
  endif;

  //  THE PHOTO RULE. A line whose reason demands evidence and has none blocks
  //  the post, and the screen says how many are blocking.
  if nblock > 0;
    exsr msgPhotoRequired;
    leavesr;
  endif;

  //  ---- 1. one adjustment movement per varying line ---------------
  //  ADJU up, ADJD down. from/to carry the location on the side the stock
  //  moved, which is what makes the audit row readable on its own.
  exec sql
    insert into gtmovement (movement_type, sku, from_location, to_location,
                            qty, reason_code, source_doc, moved_by)
    select case when d.qty_counted > d.qty_expected then 'ADJC' else 'ADJC' end,
           d.sku,
           case when d.qty_counted < d.qty_expected then :hloc else '' end,
           case when d.qty_counted > d.qty_expected then :hloc else '' end,
           abs(d.qty_counted - d.qty_expected),
           case when d.reason_code = '' then 'CVAR' else d.reason_code end,
           d.count_id, :wOper
      from gtcountdtl d
     where d.count_id = :hcnt
       and d.counted_flag = 'Y'
       and d.qty_counted <> d.qty_expected;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  //  ---- 2. the counted quantity BECOMES the balance ---------------
  //  A cycle count is authoritative: what is on the shelf is the truth, so
  //  the balance is set to the count rather than adjusted by the delta.
  exec sql
    update gtinvbal b
       set qty_on_hand = (select d.qty_counted
                            from gtcountdtl d
                           where d.count_id = :hcnt
                             and d.sku = b.sku
                             and d.counted_flag = 'Y'),
           last_count_ts = current timestamp,
           updated_by    = :wOper,
           updated_ts    = current timestamp
     where b.location_id = :hloc
       and exists (select 1 from gtcountdtl d
                    where d.count_id = :hcnt
                      and d.sku = b.sku
                      and d.counted_flag = 'Y');

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  //  ---- 3. extra stock found needs a balance row of its own -------
  exec sql
    insert into gtinvbal (sku, location_id, qty_on_hand, qty_allocated,
                          last_count_ts, updated_by)
    select d.sku, :hloc, d.qty_counted, 0, current timestamp, :wOper
      from gtcountdtl d
     where d.count_id = :hcnt
       and d.counted_flag = 'Y'
       and d.qty_counted > 0
       and not exists (select 1 from gtinvbal b
                        where b.sku = d.sku
                          and b.location_id = :hloc);

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  //  ---- 4. the header --------------------------------------------
  exec sql
    update gtcounthdr
       set count_status = 'POST',
           approved_by  = :wOper,
           approved_ts  = current timestamp
     where count_id = :hcnt;

  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  exec sql commit;

  //  A COMMIT can fail too.
  if sqlcode < 0;
    exec sql rollback;
    exsr msgPostFailed;
    leavesr;
  endif;

  posted = 'Y';
  hstat  = 'POST';
  exsr msgPosted;
endsr;

//------------------------------------------------------------------------
begsr loadHeader;
  exec sql
    select h.location_id, l.zone, l.location_type, h.count_status
      into :hloc, :hzone, :htype, :hstat
      from gtcounthdr h
      join gtlocation l on l.location_id = h.location_id
     where h.count_id = :hcnt
     fetch first row only;

  if sqlcode <> 0;
    done = *on;
  endif;
endsr;

//------------------------------------------------------------------------
// Every counted line, expected and counted side by side. nblock counts the
// lines whose reason demands a photograph and have none -- the number that
// stands between the operator and the post.
//------------------------------------------------------------------------
begsr loadLineSubfile;
  rrn = 0;
  sflclear = '1';
  write cntvar;
  sflclear = '0';
  lineCount = 0;
  ndone  = 0;
  nvar   = 0;
  nphoto = 0;
  nblock = 0;

  exec sql
    declare vaCsr cursor for
      select v.line_no, v.sku, v.description_en, v.description_fr, v.uom,
             v.thumb_id, v.qty_expected, v.qty_counted, v.qty_variance,
             v.reason_code, v.photo_count, v.counted_flag,
             coalesce(rn.photo_required, 'N')
        from gtvcntlin v
        left join gtreason rn on rn.reason_code = v.reason_code
                             and rn.reason_type = 'CNT'
       where v.count_id = :hcnt
       order by v.has_variance desc, v.qty_variance_abs desc, v.line_no
       fetch first 60 rows only;

  exec sql open vaCsr;

  dow lineCount < MAX_ROWS;
    exec sql
      fetch vaCsr into :wLine, :wSku, :wDescEn, :wDescFr, :wUom,
                       :wImg, :wExp, :wCnt, :wVar, :wRsn, :wPho, :wFlag,
                       :wNeed;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wDescFr <> *blanks;
      wDesc = wDescFr;
    else;
      wDesc = wDescEn;
    endif;

    lineCount += 1;
    lines(lineCount).lineNo = wLine;
    lines(lineCount).sku    = wSku;
    lines(lineCount).exp    = wExp;
    lines(lineCount).cnt    = wCnt;
    lines(lineCount).reason = wRsn;
    lines(lineCount).photos = wPho;
    lines(lineCount).need   = wNeed;

    if wFlag = 'Y';
      ndone += 1;
    endif;
    if wFlag = 'Y' and wVar <> 0;
      nvar += 1;
    endif;
    nphoto += wPho;
    if wNeed = 'Y' and wPho = 0;
      nblock += 1;
    endif;

    rrn     += 1;
    vseq     = lineCount;
    vline    = wLine;
    vsku     = wSku;
    vdesc    = wDesc;
    vuom     = wUom;
    vimg     = wImg;
    vexp     = wExp;
    vcnt     = wCnt;
    vvar     = wVar;
    vreason  = wRsn;
    vphoto   = wPho;
    vneed    = wNeed;
    vflag    = wFlag;
    write vasfl;
  enddo;

  exec sql close vaCsr;
  nlines = lineCount;
endsr;

//------------------------------------------------------------------------
// The reason codes, with their photo_required flag, so the screen can warn
// BEFORE the operator presses post rather than after.
//
// A JSON STRING, NOT A SECOND SUBFILE. Two subfiles in one EJS format compile
// to malformed DDS here -- CPD7836 "subfile control record not found" and
// CPD7835 "subfile record not found" -- a known limitation of the RDF-to-DDS
// conversion. A short list that the template parses costs nothing and avoids
// a second display file for seven rows.
//------------------------------------------------------------------------
begsr loadReasonList;
  reasons = '[';

  exec sql
    declare rsnCsr cursor for
      select reason_code, reason_desc_en, reason_desc_fr, photo_required
        from gtreason
       where reason_type = 'CNT'
       order by sort_order, reason_code
       fetch first 12 rows only;

  exec sql open rsnCsr;

  i = 0;
  dow i < MAX_RSN;
    exec sql fetch rsnCsr into :wCode, :wRDsEn, :wRDsFr, :wRPho;
    if sqlcode <> 0;
      leave;
    endif;

    if langpref = 'FR' and wRDsFr <> *blanks;
      wRDesc = wRDsFr;
    else;
      wRDesc = wRDsEn;
    endif;

    i += 1;
    if i > 1;
      reasons = %trim(reasons) + ',';
    endif;
    //  The descriptions are ours and contain no quotes or backslashes, so
    //  plain concatenation is safe. Anything user-entered would need escaping.
    reasons = %trim(reasons) + '{"c":"' + %trim(wCode) +
              '","d":"' + %trim(wRDesc) +
              '","p":"' + %trim(wRPho) + '"}';
  enddo;

  exec sql close rsnCsr;
  reasons = %trim(reasons) + ']';
endsr;

//------------------------------------------------------------------------
begsr loadOperator;
  exec sql
    select full_name, operator_id into :opername, :wOper
      from gtoperator
     where operator_id = 'GT001' and active_flag = 'Y'
     fetch first row only;

  if sqlcode <> 0;
    opername = 'Demo Operator';
    wOper    = 'GT001';
  endif;
endsr;

//------------------------------------------------------------------------
begsr saveLanguage;
  exec sql
    update gtoperator set lang_pref = :langpref
     where operator_id = 'GT001';
  if sqlcode < 0;
    exec sql rollback;
  else;
    exec sql commit;
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgReasonsSaved;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Motifs enregistrés.';
  else;
    msg = 'Reasons saved.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNothingChanged;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Aucun motif modifié.';
  else;
    msg = 'No reasons were changed.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPhotoSaved;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Photo enregistrée (image ' + %trim(%char(wImgId)) + ').';
  else;
    msg = 'Photo saved (image ' + %trim(%char(wImgId)) + ').';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNoPhoto;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Aucune photo reçue.';
  else;
    msg = 'No photo was received.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPhotoFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Échec de l''enregistrement de la photo.';
  else;
    msg = 'The photo could not be stored.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPhotoRequired;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = %trim(%char(nblock)) + ' ligne(s) exigent une photo avant validation.';
  else;
    msg = %trim(%char(nblock)) + ' line(s) need a photo before posting.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgNotAllCounted;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Dénombrez toutes les lignes avant de valider.';
  else;
    msg = 'Count every line before posting.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPosted;
  msgkind = 'GOOD';
  if langpref = 'FR';
    msg = 'Dénombrement validé : les soldes reflètent le comptage.';
  else;
    msg = 'Count posted -- balances now match what was counted.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgAlreadyPosted;
  msgkind = 'WARN';
  if langpref = 'FR';
    msg = 'Dénombrement déjà validé : lecture seule.';
  else;
    msg = 'This count is already posted -- read only.';
  endif;
endsr;

//------------------------------------------------------------------------
begsr msgPostFailed;
  msgkind = 'BAD';
  if langpref = 'FR';
    msg = 'Validation annulée, rien changé (SQL ' + %trim(%char(sqlcode)) + ').';
  else;
    msg = 'Post rolled back, nothing changed (SQL ' +
          %trim(%char(sqlcode)) + ').';
  endif;
endsr;
