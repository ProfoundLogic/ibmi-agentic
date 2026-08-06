const https=require('https');const {chromium}=require('playwright-core');
const get=u=>new Promise((r,j)=>https.get(u,{rejectUnauthorized:false},s=>{let d='';s.on('data',c=>d+=c);s.on('end',()=>r(d))}).on('error',j));
(async()=>{
  const H='https://idev.profoundlogic.com:8103/profoundui/userdata/genie%20skins/';
  const body=await get(H+'Classic/start.html');
  const b=body.indexOf('<!-- GTWMS EJS shim BEGIN'), e=body.indexOf('<!-- GTWMS EJS shim END -->');
  const blk=body.slice(b,e);
  const js=blk.slice(blk.indexOf('<script type="text/javascript">')+31, blk.lastIndexOf('</script>'));
  const br=await chromium.launch({executablePath:'/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome',args:['--no-sandbox']});

  // Case 1: skin with NO viewport (Classic) -> shim must add one.
  let p=await br.newPage();
  await p.setContent('<!doctype html><head></head><body></body>');
  await p.addScriptTag({content:js});
  console.log('no-viewport skin  -> added:', await p.evaluate(()=>{
    const m=document.querySelector('meta[name="viewport"]'); return m?m.content:'NONE'; }));
  await p.close();

  // Case 2: skin that already declares one (pls) -> must be left untouched.
  p=await br.newPage();
  await p.setContent('<!doctype html><head><meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0,user-scalable=0"></head><body></body>');
  await p.addScriptTag({content:js});
  const after=await p.evaluate(()=>document.querySelectorAll('meta[name="viewport"]').length+'|'+document.querySelector('meta[name="viewport"]').content);
  console.log('existing viewport -> count|content:', after);
  await p.close();
  await br.close();
})();
