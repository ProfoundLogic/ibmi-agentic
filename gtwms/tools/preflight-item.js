const ejs=require('ejs'), fs=require('fs'), path=require('path');
const {chromium}=require('playwright-core');
const UI='/workspace/workspace/ibmi-agentic/docs/htdocs/profoundui/userdata/ui';
const CH='/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome';

const SCREENS=[
 { name:'itmscan', tpl:['gtitmd','itmscan.ejs'], css:['gtcommon/gt-theme.css','gtitmd/itmscan.css'],
   declared:['action','scanval','search','langpref','opername','msg','nfound','srchsfl'],
   data:(lang)=>({action:'',scanval:'',search:lang==='FR'?'riz':'rice',langpref:lang,
     opername:lang==='FR'?'Marie Tremblay':'James Okonkwo',msg:'',nfound:3,
     srchsfl:[
      {_rrn:1,sseq:1,ssku:'GROC-000001',sdesc:'Long Grain White Rice - 250 g',sdept:'GROC',simg:1,soh:412,suom:'EA'},
      {_rrn:2,sseq:2,ssku:'GROC-000004',sdesc:'Long Grain White Rice - 1 kg',sdept:'GROC',simg:1,soh:87.5,suom:'EA'},
      {_rrn:3,sseq:3,ssku:'PETS-000330',sdesc:"Nourriture sèche pour chien - 4 kg",sdept:'PETS',simg:0,soh:0,suom:'EA'}]}) },
 { name:'itmdetl', tpl:['gtitmd','itmdetl.ejs'], css:['gtcommon/gt-theme.css','gtitmd/itmdetl.css'],
   declared:['action','langpref','opername','msg','dsku','ddesc','ddept','duom','dcasepk','dweight',
     'dlen','dwid','dhgt','dretail','dupc','dcase','dtotoh','dtotav','dbulk','dnlocs',
     'dimg1','dimg2','dimg3','dimg4','dcap1','dcap2','dcap3','dcap4','locsfl'],
   data:(lang)=>({action:'',langpref:lang,opername:lang==='FR'?'Marie Tremblay':'James Okonkwo',msg:'',
     dsku:'GROC-000004',
     ddesc:lang==='FR'?'Riz blanc à grain long - 1 kg':'Long Grain White Rice - 1 kg',
     ddept:'GROC',duom:'EA',dcasepk:12,dweight:1.024,dlen:22.5,dwid:14.2,dhgt:8.6,dretail:6.49,
     dupc:'061414100045',dcase:'10614141000040',
     dtotoh:487,dtotav:461,dbulk:340,dnlocs:4,
     dimg1:1,dimg2:2,dimg3:0,dimg4:0,
     dcap1:lang==='FR'?'Riz blanc à grain long - Avant':'Long Grain White Rice - Front',
     dcap2:lang==='FR'?'Riz blanc à grain long - Détail':'Long Grain White Rice - Detail',
     dcap3:'',dcap4:'',
     locsfl:[
      {_rrn:1,lseq:1,lloc:'A01011',ltype:'PICK',lzone:'A',lqty:48,lalloc:6},
      {_rrn:2,lseq:2,lloc:'D01011',ltype:'BULK',lzone:'D',lqty:220,lalloc:0},
      {_rrn:3,lseq:3,lloc:'D01024',ltype:'BULK',lzone:'D',lqty:120,lalloc:0},
      {_rrn:4,lseq:4,lloc:'STAGE01',ltype:'STAG',lzone:'S',lqty:99,lalloc:20}]}) },
];
const CASES=[{lang:'EN',w:412,h:915},{lang:'FR',w:412,h:915},{lang:'FR',w:360,h:800},{lang:'EN',w:1280,h:900}];

(async()=>{
  const browser=await chromium.launch({executablePath:CH,args:['--no-sandbox']});
  let fail=0;
  for(const sc of SCREENS){
    const src=fs.readFileSync(path.join(UI,sc.tpl[0],sc.tpl[1]),'utf8');
    const css=sc.css.map(f=>fs.readFileSync(path.join(UI,f),'utf8')).join('\n');
    const locals=new Set();
    [...src.matchAll(/\b(?:var|function)\s+([A-Za-z_$][\w$]*)/g)].forEach(m=>locals.add(m[1]));
    [...src.matchAll(/function\s*\(([^)]*)\)/g)].forEach(m=>
      m[1].split(',').map(x=>x.trim()).filter(Boolean).forEach(x=>locals.add(x)));
    const refs=new Set([...src.matchAll(/<%[=-]\s*([A-Za-z_$][\w$]*)/g)].map(m=>m[1]));
    const unknown=[...refs].filter(r=>!sc.declared.includes(r)&&!locals.has(r)&&!['typeof','Number','String','Math','JSON'].includes(r));
    console.log('\n'+sc.name+'  identifier audit:',unknown.length?'UNKNOWN -> '+unknown.join(', '):'clean');
    if(unknown.length) fail++;
    for(const c of CASES){
      let html;
      try{ html=ejs.render(src,sc.data(c.lang)); }
      catch(e){ console.log('  RENDER FAILED',c.lang,c.w,e.message); fail++; continue; }
      const page=await browser.newPage({viewport:{width:c.w,height:c.h}});
      await page.setContent('<!doctype html><meta charset="utf-8"><style>html,body{margin:0;padding:0}'+
        'div{white-space:nowrap;z-index:10;padding:1px}</style><style>'+css+'</style>'+html,{waitUntil:'load'});
      const o=await page.evaluate(()=>{const de=document.documentElement;const wide=[];
        document.querySelectorAll('.gt-app *').forEach(el=>{const r=el.getBoundingClientRect();
          if(r.right>de.clientWidth+1) wide.push((typeof el.className==='string'?el.className:el.tagName)+' r='+Math.round(r.right));});
        return {sw:de.scrollWidth,cw:de.clientWidth,wide:wide.slice(0,3)};});
      const bad=o.sw>o.cw+1; if(bad) fail++;
      console.log('  '+(c.lang+' '+c.w+'px').padEnd(11)+' sw='+o.sw+' cw='+o.cw+' '+(bad?'OVERFLOW -> '+o.wide.join(' | '):'no overflow'));
      await page.screenshot({path:'/tmp/ejspre/item-'+sc.name+'-'+c.lang.toLowerCase()+'-'+c.w+'.png',fullPage:true});
      await page.close();
    }
  }
  await browser.close();
  console.log(fail?'\n'+fail+' case(s) failed':'\nAll cases clean');
})();
