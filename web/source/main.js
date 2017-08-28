import vis from 'vis';
import { List, Map } from 'immutable';
// types / internals

const valid_pitches = [
    'Cff', 'Cf', 'C',
    'Dff', 'Cs', 'Df',
    'Css', 'D', 'Eff',
    'Ds', 'Ef', 'Fff',
    'Dss', 'E', 'Ff',
    'Es', 'F', 'Gff',
    'Ess', 'Fs', 'Gf',
    'Fss', 'G', 'Aff',
    'Gs', 'Af', 'Gss',
    'A', 'Bff', 'As',
    'Bf', 'Ass', 'B',
    'Bs', 'Bss', 'Rest'
];

const display_pitches = [
    'C♯♯', 'C♯', 'C',
    'D♯♯', 'C♭', 'D♯',
    'C♭♭', 'D', 'E♯♯',
    'D♭', 'E♯', 'F♯♯',
    'D♭♭', 'E', 'F♯',
    'E♭', 'F', 'Gff',
    'E♭♭', 'F♭', 'G♯',
    'F♭♭', 'G', 'A♯♯',
    'G♭', 'A♯', 'G♭♭',
    'A', 'B♯♯', 'A♭',
    'B♯', 'A♭♭', 'B',
    'B♭', 'B♭♭', 'Rest'
];

function displayPitch(pitch) {
    var i = valid_pitches.indexOf(pitch);
    if(i === -1) {
        throw "Invalid pitch";
    } else {
        return display_pitches[i];
    }
}

class Music {
    constructor(dur, pitch_class, octave) {
        this.dur = dur;
        if(valid_pitches.indexOf(pitch_class) !== -1) {
            this.pitch = pitch_class;
        } else {
            throw "Invalid pitch class";
        }
        this.octave = octave;
    }

    toString() {
        return `${displayPitch(this.pitch)}${this.octave} (${this.dur.toString()})`
    }
}

class Rational {
    constructor(a, b) {
        this.num = a;
        this.den = b;
        this.reduce();
    }

    reduce() {
        let gcd = function(a, b) {
            if ( ! b) {
                return a;
            }
            return gcd(b, a % b);
        }

        let div = function(a, b) {
            if(b === 0) {
                throw "Divide by zero";
            } else {
                return Math.floor(a / b);
            }
        }

        var d = gcd(this.num, this.den);
        this.num = div(this.num, d);
        this.den = div(this.den, d);
    }
    
    toString() {
        return `${this.num}/${this.den}`
    }
}

// graph code

var nodeData = new Map();
var edgeData = new Map();
var network = null;

function showOverlay(id) {
    document.getElementById(id).classList.remove('hidden');
}

function genericEditNode(data, callback) {
    function clearOverlay() {
        document.getElementById('node-save').onclick = null;
        document.getElementById('node-cancel').onclick = null;
        hideOverlay('node-overlay');
    }

    function saveNode(data, callback) {
        var duration = new Rational(document.getElementById('numerator').value,
            document.getElementById('denominator').value);
        var music = new Music(duration, document.getElementById('pitch').value,
            document.getElementById('octave').value);
        nodeData = nodeData.set(data.id, music);
        data.label = music.toString();
        clearOverlay();
        callback(data);
    }

    function discardNode(callback) {
        clearOverlay();
        callback(null);
    }

    showOverlay('node-overlay');
    document.getElementById('node-save').onclick = saveNode.bind(this, data, callback);
    document.getElementById('node-cancel').onclick = discardNode.bind(this, callback);
}

function genericEditEdge(data, callback) {
    function clearOverlay() {
        document.getElementById('edge-save').onclick = saveEdge.bind(this, data, callback);
        document.getElementById('edge-cancel').onclick = discardEdge.bind(this, callback);
        hideOverlay('edge-overlay');
    }

    function saveEdge(data, callback) {
        var prob = document.getElementById('prob').value / 100;
        data.label = `${prob * 100}%`;
        edgeData = edgeData.set(data.id, prob);
        clearOverlay();
        callback(data);
    }

    function discardEdge(callback) {
        clearOverlay();
        callback(null);
    }

    showOverlay('edge-overlay');
    document.getElementById('edge-save').onclick = saveEdge.bind(this, data, callback);
    document.getElementById('edge-cancel').onclick = discardEdge.bind(this, callback);
}


function hideOverlay(id) {
    document.getElementById(id).classList.add('hidden');
}

function main() {
    var container = document.getElementById('network');

    var options = {
        manipulation: {
            addNode: function(nodeData, callback) {
                document.getElementById('node-operation').innerHTML = 'Add';
                genericEditNode(nodeData, callback);
            },
            addEdge: function(edgeData, callback) {
                document.getElementById('edge-operation').innerHTML = 'Add';
                genericEditEdge(edgeData, callback);
            },
            editNode: function(nodeData, callback) {
                document.getElementById('node-operation').innerHTML = 'Edit';
                genericEditNode(nodeData, callback);
            },
            editEdge: function(edgeData, callback) {
                document.getElementById('edge-operation').innerHTML = 'Edit';
                genericEditEdge(edgeData, callback);
            },
            deleteNode: true,
            deleteEdge: true
        },
        edges: {
            arrows: {
                to: { enable: true }
            }
        }
    };

    network = new vis.Network(container, {}, options);

    network.on("click", function(params) {
        var infop = document.getElementById('graphinfo');
        var text = "";
        // TODO
        infop.innerHTML = text;
    });

    const pitch_selector = valid_pitches.map((p, i) =>
            `<option value="${p}">${display_pitches[i]}</option>`)
        .reduce((acc, v) =>
            acc + v, "");
    document.getElementById('pitch').innerHTML = pitch_selector;
}

window.onload = main;
