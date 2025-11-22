const controls = {
  scriptTime: document.getElementById('scriptTime'),
  renderCommands: document.getElementById('renderCommands'),
  genFactor: document.getElementById('genFactor'),
  procFactor: document.getElementById('procFactor'),
  bufferSize: document.getElementById('bufferSize'),
  maxQueued: document.getElementById('maxQueued'),
};

const valueSpans = document.querySelectorAll('.value');
const cpuLane = document.getElementById('cpuLane');
const gpuLane = document.getElementById('gpuLane');
const timeAxis = document.getElementById('timeAxis');
const explanationText = document.getElementById('explanationText');

const metrics = {
  frameTime: document.getElementById('frameTime'),
  fps: document.getElementById('fps'),
  bottleneck: document.getElementById('bottleneck'),
  bufferUse: document.getElementById('bufferUse'),
  queuedFrames: document.getElementById('queuedFrames'),
  inputLatency: document.getElementById('inputLatency'),
};

function formatMs(value) {
  return `${value.toFixed(2)} ms`;
}

function formatNumber(value) {
  return new Intl.NumberFormat('en-US').format(value);
}

function updateValueLabels() {
  valueSpans.forEach((el) => {
    const id = el.dataset.for;
    const input = controls[id];
    el.textContent = input.id === 'maxQueued' || input.id === 'renderCommands' || input.id === 'bufferSize'
      ? input.value
      : Number(input.value).toFixed(3);
  });
}

function simulate(params) {
  const framesToSim = 6;
  const frames = [];
  let prevRenderEnd = 0;
  let prevGpuEnd = 0;
  let queuedCommands = 0;
  const releases = [];

  const settleReleases = (time) => {
    // Free buffer space for any frame whose GPU work has started.
    for (let i = releases.length - 1; i >= 0; i--) {
      if (releases[i].time <= time) {
        queuedCommands = Math.max(0, queuedCommands - releases[i].amount);
        releases.splice(i, 1);
      }
    }
  };

  for (let i = 0; i < framesToSim; i++) {
    const script = params.scriptTime;
    const render = params.renderCommands * params.genFactor;
    const gpuDuration = params.renderCommands * params.procFactor;
    const frame = { index: i };

    let start = prevRenderEnd;
    let waitMax = 0;
    let waitBuffer = 0;

    settleReleases(start);

    if (i - params.maxQueued >= 0 && frames[i - params.maxQueued]) {
      const gateTime = frames[i - params.maxQueued].gpuEnd;
      if (gateTime > start) {
        waitMax = gateTime - start;
        start = gateTime;
        settleReleases(start);
      }
    }

    while (queuedCommands + params.renderCommands > params.bufferSize) {
      const nextRelease = releases.length ? Math.min(...releases.map((r) => r.time)) : Infinity;
      if (!Number.isFinite(nextRelease)) break;
      const waitUntil = Math.max(start, nextRelease);
      waitBuffer += waitUntil - start;
      start = waitUntil;
      settleReleases(start);
    }

    frame.waitMax = waitMax;
    frame.waitBuffer = waitBuffer;
    frame.scriptStart = start;
    frame.scriptEnd = frame.scriptStart + script;
    frame.renderStart = frame.scriptEnd;
    frame.renderEnd = frame.renderStart + render;

    // Commands land in the buffer at renderEnd until the GPU begins this frame.
    queuedCommands += params.renderCommands;
    const bufferSnapshot = queuedCommands;

    frame.gpuStart = Math.max(frame.renderEnd, prevGpuEnd);
    frame.gpuIdle = Math.max(0, frame.renderEnd - prevGpuEnd);
    frame.gpuWait = Math.max(0, prevGpuEnd - frame.renderEnd);
    frame.gpuEnd = frame.gpuStart + gpuDuration;
    frame.gpuDuration = gpuDuration;
    frame.bufferUse = bufferSnapshot;

    releases.push({ time: frame.gpuStart, amount: params.renderCommands });
    frames.push(frame);
    prevRenderEnd = frame.renderEnd;
    prevGpuEnd = frame.gpuEnd;
  }

  return frames;
}

function createBar({ lane, start, end, label, type, row, frameIndex }) {
  const bar = document.createElement('div');
  bar.className = `bar ${type}`;
  bar.textContent = label;
  bar.style.left = `${start}%`;
  bar.style.width = `${Math.max(0.5, end - start)}%`;
  bar.style.top = `${frameIndex * 24 + (type === 'gpu' ? 40 : 8)}px`;
  lane.appendChild(bar);
  return bar;
}

function createMarker(track, timePercent, text) {
  const marker = document.createElement('div');
  marker.className = 'frame-marker';
  marker.style.left = `${timePercent}%`;
  const label = document.createElement('div');
  label.className = 'label';
  label.textContent = text;
  marker.appendChild(label);
  track.appendChild(marker);
}

function renderTimeline(frames, params) {
  cpuLane.innerHTML = '';
  gpuLane.innerHTML = '';
  timeAxis.innerHTML = '';

  const lastGpuEnd = Math.max(...frames.map((f) => f.gpuEnd));
  const scale = (value) => (value / lastGpuEnd) * 100;

  const ticks = 8;
  for (let i = 0; i <= ticks; i++) {
    const pos = (i / ticks) * 100;
    const tick = document.createElement('div');
    tick.className = 'tick';
    tick.style.left = `${pos}%`;
    const label = document.createElement('span');
    label.textContent = `${(lastGpuEnd * (i / ticks)).toFixed(1)} ms`;
    tick.appendChild(label);
    timeAxis.appendChild(tick);
  }

  frames.forEach((frame) => {
    const yIndex = frame.index % 5;
    if (frame.waitMax > 0) {
      createBar({
        lane: cpuLane,
        start: scale(frame.scriptStart - frame.waitMax),
        end: scale(frame.scriptStart),
        label: 'CPU Wait (Max Frames)',
        type: 'wait',
        frameIndex: yIndex,
      });
    }
    if (frame.waitBuffer > 0) {
      createBar({
        lane: cpuLane,
        start: scale(frame.scriptStart - frame.waitBuffer),
        end: scale(frame.scriptStart),
        label: 'CPU Wait (Buffer Full)',
        type: 'wait',
        frameIndex: yIndex,
      });
    }

    createBar({
      lane: cpuLane,
      start: scale(frame.scriptStart),
      end: scale(frame.scriptEnd),
      label: 'CPU Scripts',
      type: 'script',
      frameIndex: yIndex,
    });

    createBar({
      lane: cpuLane,
      start: scale(frame.renderStart),
      end: scale(frame.renderEnd),
      label: 'Render Thread',
      type: 'render',
      frameIndex: yIndex,
    });

    if (frame.gpuIdle > 0) {
      createBar({
        lane: gpuLane,
        start: scale(frame.gpuStart - frame.gpuIdle),
        end: scale(frame.gpuStart),
        label: 'GPU Idle',
        type: 'wait',
        frameIndex: yIndex,
      });
    }

    if (frame.gpuWait > 0) {
      createBar({
        lane: gpuLane,
        start: scale(frame.gpuStart - frame.gpuWait),
        end: scale(frame.gpuStart),
        label: 'GPU Wait',
        type: 'wait',
        frameIndex: yIndex,
      });
    }

    createBar({
      lane: gpuLane,
      start: scale(frame.gpuStart),
      end: scale(frame.gpuEnd),
      label: 'GPU Execution',
      type: 'gpu',
      frameIndex: yIndex,
    });

    createMarker(cpuLane, scale(frame.scriptStart), `Frame ${frame.index}`);
    createMarker(gpuLane, scale(frame.gpuEnd), `Present ${frame.index}`);
  });
}


function computeMetrics(frames, params) {
  const last = frames[frames.length - 1];
  const prev = frames[frames.length - 2];
  const frameTime = last.gpuEnd - prev.gpuEnd;
  const fps = 1000 / frameTime;
  const bufferOccupancy = Math.max(...frames.map((f) => f.bufferUse));
  const queued = frames.filter((f) => f.gpuEnd > last.renderEnd).length;

  const bottleneck = (() => {
    if (last.waitBuffer > 0) return 'Buffer Full';
    if (last.waitMax > 0) return 'Max Frames Queued';
    const cpuCost = params.scriptTime + params.renderCommands * params.genFactor;
    const gpuCost = params.renderCommands * params.procFactor;
    if (cpuCost > gpuCost) {
      return params.scriptTime >= params.renderCommands * params.genFactor ? 'CPU Scripts' : 'CPU Render';
    }
    return 'GPU';
  })();

  metrics.frameTime.textContent = formatMs(frameTime);
  metrics.fps.textContent = fps.toFixed(1);
  metrics.bottleneck.textContent = bottleneck;
  metrics.bufferUse.textContent = `${formatNumber(bufferOccupancy)} / ${formatNumber(params.bufferSize)}`;
  metrics.queuedFrames.textContent = `${queued} / ${params.maxQueued}`;
  metrics.inputLatency.textContent = formatMs(last.gpuEnd - last.scriptStart);
}


function updateExplanation(params) {
  const text = [
    `Scripts take ${params.scriptTime} ms, generating ${params.renderCommands} commands in ${(params.renderCommands * params.genFactor).toFixed(2)} ms on the CPU.`,
    `The GPU processes commands in ${(params.renderCommands * params.procFactor).toFixed(2)} ms while the buffer can hold ${params.bufferSize} commands.`,
    `The CPU is limited to ${params.maxQueued} queued frames; additional frames wait for GPU completion or buffer space.`,
  ];
  explanationText.textContent = text.join(' ');
}

function readParams() {
  return {
    scriptTime: Number(controls.scriptTime.value),
    renderCommands: Number(controls.renderCommands.value),
    genFactor: Number(controls.genFactor.value),
    procFactor: Number(controls.procFactor.value),
    bufferSize: Number(controls.bufferSize.value),
    maxQueued: Number(controls.maxQueued.value),
  };
}

function update() {
  updateValueLabels();
  const params = readParams();
  const frames = simulate(params);
  renderTimeline(frames, params);
  computeMetrics(frames, params);
  updateExplanation(params);
}

Object.values(controls).forEach((input) => {
  input.addEventListener('input', update);
});

update();
