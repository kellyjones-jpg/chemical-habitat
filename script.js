const canvas = document.getElementById('habitat-canvas');
const renderer = new THREE.WebGLRenderer({ canvas, antialias: true });
renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.outputEncoding = THREE.sRGBEncoding;

const scene = new THREE.Scene();
scene.background = new THREE.Color(0x07131a);
scene.fog = new THREE.FogExp2(0x07131a, 0.035);

const camera = new THREE.PerspectiveCamera(70, window.innerWidth / window.innerHeight, 0.05, 300);
camera.position.set(0, 3, 8);

const ambient = new THREE.AmbientLight(0x2a5158, 0.6);
scene.add(ambient);
const sun = new THREE.DirectionalLight(0xcceeff, 0.8);
sun.position.set(12, 30, 8);
sun.castShadow = true;
scene.add(sun);

const TERRAIN_SIZE = 120;
const TERRAIN_SEG = 192;
const terrainGeo = new THREE.PlaneGeometry(TERRAIN_SIZE, TERRAIN_SIZE, TERRAIN_SEG, TERRAIN_SEG);
terrainGeo.rotateX(-Math.PI / 2);

function hash(n){return Math.fract(Math.sin(n)*43758.5453123);} 

for (let i=0;i<terrainGeo.attributes.position.count;i++){
  const vx = terrainGeo.attributes.position.getX(i);
  const vz = terrainGeo.attributes.position.getZ ? terrainGeo.attributes.position.getZ(i) : terrainGeo.attributes.position.getY(i);
  const base = Math.sin(vx*0.04) * Math.cos(vz*0.04) * 2.0;
  const detail = Math.sin(vx*0.15 + 3.14) * Math.cos(vz*0.12 + 1.7) * 0.6;
  const micro = Math.sin(vx*0.6) * Math.cos(vz*0.5) * 0.25;
  const riverBias = Math.exp(-((vx*0.008)*(vx*0.008) + (vz*0.008)*(vz*0.008)));
  const h = base + detail + micro - riverBias*5.0 - 2.2;
  terrainGeo.attributes.position.setY(i, h);
}
terrainGeo.computeVertexNormals();

const terrainMat = new THREE.MeshStandardMaterial({ color: 0x3a2820, roughness: 0.92, metalness: 0.02 });
const terrain = new THREE.Mesh(terrainGeo, terrainMat);
terrain.receiveShadow = true;
scene.add(terrain);

const ROCK_COUNT = 700;
const rockGeom = new THREE.IcosahedronGeometry(1, 0);
const rockMat = new THREE.MeshStandardMaterial({ color: 0x4b3b33, roughness: 0.95 });
const rockInst = new THREE.InstancedMesh(rockGeom, rockMat, ROCK_COUNT);
rockInst.castShadow = true;
scene.add(rockInst);
const tmp = new THREE.Object3D();
for(let i=0;i<ROCK_COUNT;i++){
  const angle = Math.random()*Math.PI*2;
  const r = Math.random() * (TERRAIN_SIZE*0.46);
  const x = Math.cos(angle)*r;
  const z = Math.sin(angle)*r;
  const h = terrainHeightApprox(x,z);
  tmp.position.set(x,h + (Math.random()-0.4)*0.3,z);
  const s = 0.2 + Math.random()*1.2;
  tmp.scale.setScalar(s*(0.5+Math.random()*0.9));
  tmp.rotation.set(Math.random()*Math.PI, Math.random()*Math.PI, Math.random()*Math.PI);
  tmp.updateMatrix();
  rockInst.setMatrixAt(i,tmp.matrix);
}
rockInst.instanceMatrix.needsUpdate = true;

function terrainHeightApprox(x,z){
  const half = TERRAIN_SIZE*0.5;
  const u = (x + half) / TERRAIN_SIZE;
  const v = (z + half) / TERRAIN_SIZE;
  const ix = Math.floor(u * TERRAIN_SEG);
  const iz = Math.floor(v * TERRAIN_SEG);
  const clamp = (n, a, b) => Math.max(a, Math.min(b, n));
  const cx = clamp(ix, 0, TERRAIN_SEG);
  const cz = clamp(iz, 0, TERRAIN_SEG);
  const idx = cz * (TERRAIN_SEG+1) + cx;
  if(idx < terrainGeo.attributes.position.count){
    return terrainGeo.attributes.position.getY(idx);
  }
  return -2;
}

const PLANT_COUNT = 1400;
const plantGeom = new THREE.PlaneGeometry(0.35, 1.8, 4, 6);
plantGeom.translate(0,1.0,0);

const offsets = new Float32Array(PLANT_COUNT*3);
const scales = new Float32Array(PLANT_COUNT);
const rots = new Float32Array(PLANT_COUNT);
for (let i=0;i<PLANT_COUNT;i++){
  const angle = Math.random()*Math.PI*2;
  const r = Math.random() * (TERRAIN_SIZE*0.46);
  const x = Math.cos(angle)*r;
  const z = Math.sin(angle)*r;
  const h = terrainHeightApprox(x,z);
  offsets[i*3] = x; offsets[i*3+1] = h + 0.02; offsets[i*3+2] = z;
  scales[i] = 0.6 + Math.random()*1.1;
  rots[i] = Math.random()*Math.PI*2;
}
plantGeom.setAttribute('instanceOffset', new THREE.InstancedBufferAttribute(offsets,3));
plantGeom.setAttribute('instanceScale', new THREE.InstancedBufferAttribute(scales,1));
plantGeom.setAttribute('instanceRot', new THREE.InstancedBufferAttribute(rots,1));

const MAX_FISH_UNIFORMS = 64; 

const plantMat = new THREE.ShaderMaterial({
  uniforms: {
    time: { value: 0 },
    windAmp: { value: 1.0 },
    fishCount: { value: 0 },
    fishPositions: { value: new Array(MAX_FISH_UNIFORMS).fill(new THREE.Vector3()) },
    fishInfluenceRadius: { value: 1.9 },
    fishPushStrength: { value: 0.55 }
  },
  vertexShader: `
    attribute vec3 instanceOffset;
    attribute float instanceScale;
    attribute float instanceRot;
    uniform float time;
    uniform float windAmp;
    uniform int fishCount;
    uniform vec3 fishPositions[${MAX_FISH_UNIFORMS}];
    uniform float fishInfluenceRadius;
    uniform float fishPushStrength;
    varying float vShade;
    mat3 rotateY(float a){ float c=cos(a), s=sin(a); return mat3(c,0.,s,0.,1.,0.,-s,0.,c); }
    void main(){
      vec3 p = position;
      float h = p.y;
      float wind = sin(time*1.8 + instanceOffset.x*0.22) * 0.42 + sin(time*0.65 + instanceOffset.z*0.28) * 0.56;
      p.x += wind * h * windAmp;
      p.z += wind * h * 0.28 * windAmp;
      // fish interaction
      for(int i=0;i<${MAX_FISH_UNIFORMS};i++){
        if(i >= fishCount) break;
        vec3 f = fishPositions[i];
        vec3 d = instanceOffset - f;
        float dist = length(d);
        if(dist < fishInfluenceRadius){
          float force = (1.0 - dist / fishInfluenceRadius);
          p += normalize(d) * force * fishPushStrength * h;
        }
      }
      // instance transform
      p *= instanceScale;
      p = rotateY(instanceRot) * p;
      p += instanceOffset;
      vShade = h;
      gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(p,1.0);
    }
  `,
  fragmentShader: `
    varying float vShade;
    void main(){
      vec3 base = vec3(0.12,0.45,0.18);
      float shade = 0.6 + vShade * 0.4;
      gl_FragColor = vec4(base * shade, 1.0);
    }
  `,
  side: THREE.DoubleSide
});
const plantInst = new THREE.InstancedMesh(plantGeom, plantMat, PLANT_COUNT);
plantInst.frustumCulled = false;
scene.add(plantInst);

const PARTICLE_COUNT = 1800;
const particleGeo = new THREE.BufferGeometry();
const pPos = new Float32Array(PARTICLE_COUNT*3);
const pVel = new Float32Array(PARTICLE_COUNT*3);
for(let i=0;i<PARTICLE_COUNT;i++){
  pPos[i*3] = (Math.random()-0.5) * TERRAIN_SIZE * 0.9;
  pPos[i*3+1] = Math.random()*12 - 2.0;
  pPos[i*3+2] = (Math.random()-0.5) * TERRAIN_SIZE * 0.9;
  pVel[i*3] = (Math.random()-0.5)*0.01;
  pVel[i*3+1] = Math.random()*0.01 + 0.0005;
  pVel[i*3+2] = (Math.random()-0.5)*0.01;
}
particleGeo.setAttribute('position', new THREE.BufferAttribute(pPos,3));
const particleMat = new THREE.PointsMaterial({ size: 0.06, transparent:true, opacity:0.6, depthWrite:false });
const particles = new THREE.Points(particleGeo, particleMat);
scene.add(particles);

function flowField(x,z,t){
  const s = 0.025;
  const a = Math.sin((x*s) + (t*0.6)) + Math.cos((z*s*0.9) + (t*0.3));
  const b = Math.cos((x*s*1.2) - (t*0.4)) + Math.sin((z*s*1.1) - (t*0.2));
  const ang = (a+b) * 1.2;
  return [Math.cos(ang)*0.6, Math.sin(ang)*0.6];
}

const FISH_COUNT = 96;
const fishGeom = new THREE.ConeGeometry(0.18, 0.7, 8);
fishGeom.translate(0,0,-0.35);
const fishMat = new THREE.MeshStandardMaterial({ color: 0xffa36a, roughness:0.5 });
const fishInst = new THREE.InstancedMesh(fishGeom, fishMat, FISH_COUNT);
fishInst.instanceMatrix.setUsage(THREE.DynamicDrawUsage);
scene.add(fishInst);

const fishPos = new Array(FISH_COUNT).fill().map(()=> new THREE.Vector3((Math.random()-0.5)*12, Math.random()*5+1, (Math.random()-0.5)*12));
const fishVel = new Array(FISH_COUNT).fill().map(()=> new THREE.Vector3((Math.random()-0.5)*0.8, (Math.random()-0.5)*0.1, (Math.random()-0.5)*0.8));

const ALIGN_R = 2.5, COH_R = 3.3, SEP_R = 0.9;
const MAX_SPEED = 2.0, MAX_FORCE = 0.06;

const causticMat = new THREE.ShaderMaterial({
  uniforms: { time:{value:0}, projectorPos:{value:new THREE.Vector3(0,18,0)}, intensity:{value:0.8} },
  transparent: true, depthWrite:false,
  vertexShader:`
    varying vec3 vWorld;
    void main(){ vWorld = (modelMatrix*vec4(position,1.0)).xyz; gl_Position = projectionMatrix*viewMatrix*modelMatrix*vec4(position,1.0); }
  `,
  fragmentShader:`
    uniform float time; uniform vec3 projectorPos; uniform float intensity; varying vec3 vWorld;
    float hash(vec2 p){ return fract(sin(dot(p,vec2(127.1,311.7)))*43758.5453123); }
    float noise(vec2 p){ vec2 i=floor(p); vec2 f=fract(p); float a=hash(i); float b=hash(i+vec2(1.0,0.0)); float c=hash(i+vec2(0.0,1.0)); float d=hash(i+vec2(1.0,1.0)); vec2 u=f*f*(3.0-2.0*f); return mix(a,b,u.x) + (c-a)*u.y*(1.0-u.x) + (d-b)*u.x*u.y; }
    void main(){ vec2 uv = vWorld.xz * 0.08 + time*vec2(0.05,-0.03); float c=0.0; c += pow(abs(sin((uv.x+uv.y*0.5)*6.0 + sin(time*0.6 + uv.x*2.0)*1.5)),1.8)*0.6; c += pow(abs(sin((uv.x*2.0 - uv.y*1.3)*12.0 + time*0.9)),2.6)*0.25; c *= 1.0 + 0.6*noise(uv*3.0 + time*0.2); float d = length(projectorPos.xz - vWorld.xz) / 40.0; float fade = smoothstep(1.0, 0.0, d); float final = c * intensity * fade; gl_FragColor = vec4(vec3(final), final*0.95); }
  `
});
const causticsMesh = new THREE.Mesh(terrainGeo.clone(), causticMat);
causticsMesh.rotation.copy(terrain.rotation);
causticsMesh.position.copy(terrain.position);
scene.add(causticsMesh);

let yaw = 0, pitch = 0;
let isLocked = false;
const player = new THREE.Object3D(); player.position.set(0,3,8); scene.add(player);

renderer.domElement.addEventListener('click', ()=>{ renderer.domElement.requestPointerLock?.(); });

document.addEventListener('pointerlockchange', ()=>{ isLocked = document.pointerLockElement === renderer.domElement; });

document.addEventListener('mousemove', (e)=>{
  if(!isLocked) return;
  const mx = e.movementX || e.mozMovementX || e.webkitMovementX || 0;
  const my = e.movementY || e.mozMovementY || e.webkitMovementY || 0;
  yaw -= mx * 0.0025; pitch -= my * 0.0025;
  pitch = Math.max(-Math.PI/2 + 0.1, Math.min(Math.PI/2 - 0.1, pitch));
});

const keys = {w:0,s:0,a:0,d:0};
window.addEventListener('keydown', e=>{ if(e.code==='KeyW') keys.w=1; if(e.code==='KeyS') keys.s=1; if(e.code==='KeyA') keys.a=1; if(e.code==='KeyD') keys.d=1; });
window.addEventListener('keyup', e=>{ if(e.code==='KeyW') keys.w=0; if(e.code==='KeyS') keys.s=0; if(e.code==='KeyA') keys.a=0; if(e.code==='KeyD') keys.d=0; });

let touchState = { look:false, move:false, lx:0, ly:0, mx:0, my:0 };
renderer.domElement.addEventListener('touchstart', (ev)=>{
  if(ev.touches.length===1){ touchState.look=true; touchState.lx=ev.touches[0].clientX; touchState.ly=ev.touches[0].clientY; }
  if(ev.touches.length===2){ touchState.move=true; touchState.mx=ev.touches[0].clientX; touchState.my=ev.touches[0].clientY; }
});
renderer.domElement.addEventListener('touchmove', (ev)=>{ ev.preventDefault(); if(touchState.look && ev.touches[0]){ const dx = ev.touches[0].clientX - touchState.lx; const dy = ev.touches[0].clientY - touchState.ly; yaw -= dx * 0.003; pitch -= dy * 0.003; touchState.lx = ev.touches[0].clientX; touchState.ly = ev.touches[0].clientY; } if(touchState.move && ev.touches[1]){ const dx = ev.touches[1].clientX - touchState.mx; const dy = ev.touches[1].clientY - touchState.my; keys.w = dy < -10 ? 1 : 0; keys.s = dy > 10 ? 1 : 0; keys.a = dx < -10 ? 1 : 0; keys.d = dx > 10 ? 1 : 0; touchState.mx = ev.touches[1].clientX; touchState.my = ev.touches[1].clientY; } }, {passive:false});
renderer.domElement.addEventListener('touchend', ()=>{ touchState.look=false; touchState.move=false; keys.w=keys.a=keys.s=keys.d=0; });

const rt1 = new THREE.WebGLRenderTarget(window.innerWidth, window.innerHeight, { samples: Math.min(4, renderer.getContext().getParameter(renderer.getContext().MAX_SAMPLES) || 0) });
const postScene = new THREE.Scene();
const postCam = new THREE.OrthographicCamera(-1,1,1,-1,0,1);
const quadGeo = new THREE.PlaneGeometry(2,2);

const postMat = new THREE.ShaderMaterial({
  uniforms: {
    tDiffuse: { value: null },
    time: { value: 0 },
    bloomAmt: { value: 0.6 },
    chroma: { value: 0.003 },
    vignette: { value: 0.35 }
  },
  vertexShader: `varying vec2 vUv; void main(){ vUv = uv; gl_Position = vec4(position,1.0); }`,
  fragmentShader: `
    varying vec2 vUv; uniform sampler2D tDiffuse; uniform float time; uniform float bloomAmt; uniform float chroma; uniform float vignette;
    vec3 bloom(vec2 uv){ // cheap additive blur by sampling around
      vec3 c = vec3(0.0);
      float off = 0.004 + sin(time*0.5)*0.0005;
      c += texture2D(tDiffuse, uv + vec2(off,0.0)).rgb * 0.6;
      c += texture2D(tDiffuse, uv + vec2(-off,0.0)).rgb * 0.6;
      c += texture2D(tDiffuse, uv + vec2(0.0,off)).rgb * 0.6;
      c += texture2D(tDiffuse, uv + vec2(0.0,-off)).rgb * 0.6;
      return c * bloomAmt;
    }
    void main(){
      vec2 uv = vUv;
      vec3 col = texture2D(tDiffuse, uv).rgb;
      // chromatic slight separation
      float c = chroma;
      vec3 colR = texture2D(tDiffuse, uv + vec2(c,0)).r * vec3(1.0,0.0,0.0);
      vec3 colG = texture2D(tDiffuse, uv).g * vec3(0.0,1.0,0.0);
      vec3 colB = texture2D(tDiffuse, uv - vec2(c,0)).b * vec3(0.0,0.0,1.0);
      vec3 chrom = colR + colG + colB;
      vec3 blo = bloom(uv);
      // vignette
      float dist = distance(uv, vec2(0.5));
      float vig = smoothstep(0.8, vignette+0.2, dist);
      vec3 final = mix(col + blo, chrom + blo*0.6, 0.04);
      final *= 1.0 - vig*0.65;
      gl_FragColor = vec4(final,1.0);
    }
  `
});
const quad = new THREE.Mesh(quadGeo, postMat);
postScene.add(quad);

let prev = performance.now();
const playerSpeed = 4.2;

// fish uniform helper
function updatePlantFishUniforms(){
  const arr = plantMat.uniforms.fishPositions.value;
  for(let i=0;i<MAX_FISH_UNIFORMS;i++){
    arr[i] = arr[i] || new THREE.Vector3();
    if(i < FISH_COUNT) arr[i].copy(fishPos[i]);
    else arr[i].set(9999,9999,9999);
  }
  plantMat.uniforms.fishCount.value = Math.min(FISH_COUNT, MAX_FISH_UNIFORMS);
}

// animation
function animate(now){
  const dt = Math.min(0.05, (now - prev) * 0.001);
  prev = now;
  const t = now * 0.001;

  // update flow field
  const positions = particleGeo.attributes.position.array;
  for(let i=0;i<PARTICLE_COUNT;i++){
    const ix = i*3;
    let x = positions[ix], y = positions[ix+1], z = positions[ix+2];
    const ff = flowField(x,z,t);
    const vx = ff[0]* (0.02 + Math.sin(t*0.7 + x*0.005)*0.01);
    const vz = ff[1]* (0.02 + Math.cos(t*0.5 + z*0.006)*0.01);
    y += (Math.sin(t*0.4 + x*0.01)*0.002 + 0.0005) * 20 * dt;
    x += vx * 40 * dt;
    z += vz * 40 * dt;
    if(y > 12) y = -2 + Math.random()*0.6;
    if(y < -6) y = -1 + Math.random()*0.6;
    if(Math.abs(x) > TERRAIN_SIZE*0.48) x = -x*0.85;
    if(Math.abs(z) > TERRAIN_SIZE*0.48) z = -z*0.85;
    positions[ix]=x; positions[ix+1]=y; positions[ix+2]=z;
  }
  particleGeo.attributes.position.needsUpdate = true;

  // fish boids
  for(let i=0;i<FISH_COUNT;i++){
    const pos = fishPos[i]; const vel = fishVel[i];
    const align = new THREE.Vector3(), coh = new THREE.Vector3(), sep = new THREE.Vector3();
    let ca=0, cc=0;
    for(let j=0;j<FISH_COUNT;j++){ if(i===j) continue; const pj = fishPos[j]; const dist = pos.distanceTo(pj); if(dist < ALIGN_R){ align.add(fishVel[j]); ca++; } if(dist < COH_R){ coh.add(pj); cc++; } if(dist < SEP_R){ const diff = pos.clone().sub(pj).normalize().divideScalar(dist); sep.add(diff); } }
    if(ca>0) align.divideScalar(ca).normalize().multiplyScalar(MAX_SPEED).sub(vel).clampScalar(-MAX_FORCE,MAX_FORCE);
    if(cc>0) coh.divideScalar(cc).sub(pos).normalize().multiplyScalar(MAX_SPEED).sub(vel).clampScalar(-MAX_FORCE,MAX_FORCE);
    sep.clampScalar(-MAX_FORCE,MAX_FORCE);
    const f = flowField(pos.x,pos.z,t);
    const flowVec = new THREE.Vector3(f[0]*0.6, Math.sin(t*0.6 + pos.x*0.01)*0.05, f[1]*0.6);
    vel.add(align.multiplyScalar(1.0)); vel.add(coh.multiplyScalar(0.7)); vel.add(sep.multiplyScalar(1.3)); vel.add(flowVec.multiplyScalar(0.4));
    if(vel.length() > MAX_SPEED) vel.setLength(MAX_SPEED);
    pos.addScaledVector(vel, dt);
    if(pos.x > TERRAIN_SIZE*0.46) pos.x = -TERRAIN_SIZE*0.46;
    if(pos.x < -TERRAIN_SIZE*0.46) pos.x = TERRAIN_SIZE*0.46;
    if(pos.z > TERRAIN_SIZE*0.46) pos.z = -TERRAIN_SIZE*0.46;
    if(pos.z < -TERRAIN_SIZE*0.46) pos.z = TERRAIN_SIZE*0.46;
    const bed = terrainHeightApprox(pos.x,pos.z);
    if(pos.y < bed + 0.6) pos.y = bed + 0.6 + Math.random()*0.4;
    if(pos.y > 10) pos.y = 8 + Math.random()*1.5;
    // update instanced matrix
    const m = new THREE.Matrix4();
    const look = pos.clone().add(vel);
    m.lookAt(pos, look, new THREE.Vector3(0,1,0));
    m.setPosition(pos);
    fishInst.setMatrixAt(i, m);
  }
  fishInst.instanceMatrix.needsUpdate = true;

  // update plant uniforms fish array
  updatePlantFishUniforms();
  plantMat.uniforms.time.value = t;
  causticMat.uniforms.time.value = t;
  causticMat.uniforms.projectorPos.value.set(camera.position.x, 18, camera.position.z);

  // player movement
  if(isLocked){
    const dir = new THREE.Vector3();
    if(keys.w) dir.z -= 1; if(keys.s) dir.z += 1; if(keys.a) dir.x -= 1; if(keys.d) dir.x += 1;
    if(dir.lengthSq()>0){ dir.normalize(); const q = new THREE.Quaternion().setFromEuler(new THREE.Euler(pitch,yaw,0,'YXZ')); dir.applyQuaternion(q); player.position.addScaledVector(dir, playerSpeed * dt); }
    player.rotation.set(pitch, yaw, 0);
    camera.position.copy(player.position);
    camera.position.y += 0; // offset if needed
    camera.quaternion.copy(player.quaternion);
    camera.rotation.set(pitch,yaw,0,'YXZ');
    // keep above bed
    const bed = terrainHeightApprox(player.position.x, player.position.z);
    if(camera.position.y < bed + 0.6) camera.position.y = bed + 0.6;
  } else {
    camera.position.x = Math.sin(t*0.12) * 6;
    camera.position.z = Math.cos(t*0.1) * 8;
    camera.position.y = 3 + Math.sin(t*0.18)*0.5;
    camera.lookAt(0,-1,0);
  }

  // render to RT then post-process
  renderer.setRenderTarget(rt1);
  renderer.render(scene, camera);
  renderer.setRenderTarget(null);
  postMat.uniforms.tDiffuse.value = rt1.texture;
  postMat.uniforms.time.value = t;
  renderer.render(postScene, postCam);

  requestAnimationFrame(animate);
}
requestAnimationFrame(animate);

// resize handling
window.addEventListener('resize', ()=>{
  renderer.setSize(window.innerWidth, window.innerHeight);
  camera.aspect = window.innerWidth / window.innerHeight; camera.updateProjectionMatrix();
  rt1.setSize(window.innerWidth, window.innerHeight);
});

// initial fish
updatePlantFishUniforms();

// Toggles
window.__river = { scene, camera, renderer, plantMat, particleGeo, fishPos };
