//  Copyright 2017 Lukas Epple
//
//  This file is part of likely music.
//
//  likely music is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Affero General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  likely music is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Affero General Public License for more details.
//
//  You should have received a copy of the GNU Affero General Public License
//  along with likely music. If not, see <http://www.gnu.org/licenses/>.

import vis from 'vis';
import { Map } from 'immutable';
// types / internals

const valid_pitches = [
    'Rest',
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
    'Bs', 'Bss'
];

const display_pitches = [
    'Rest',
    'C♯♯', 'C♯', 'C',
    'D♯♯', 'C♭', 'D♯',
    'C𝄫', 'D', 'E♯♯',
    'D♭', 'E♯', 'F♯♯',
    'D𝄫', 'E', 'F♯',
    'E♭', 'F', 'Gff',
    'E𝄫', 'F♭', 'G♯',
    'F𝄫', 'G', 'A♯♯',
    'G♭', 'A♯', 'G𝄫',
    'A', 'B♯♯', 'A♭',
    'B♯', 'A𝄫', 'B',
    'B♭', 'B𝄫'
];

function displayPitch(pitch) {
    var i = valid_pitches.indexOf(pitch);
    if(i === -1) {
        throw 'Invalid pitch';
    } else {
        return display_pitches[i];
    }
}

function standard_rests(dur) {
    if(dur.numerator === 1) {
        switch(dur.denominator) {
            case 1:
                return '𝄻 ';
                break;
            case 2:
                return '𝄼 ';
                break;
            case 4:
                return '𝄽 ';
                break;
            case 8:
                return '𝄾 ';
                break;
            case 16:
                return '𝄿 ';
                break;
            case 32:
                return '𝅀 ';
                break;
            case 64:
                return '𝅁 '
                break;
            case 128:
                return '𝅂 '
                break;
            default:
                return null;
                break;
        }
    } else {
        return null;
    }
}

function standard_notes(dur) {
    if(dur.numerator === 1) {
        switch(dur.denominator) {
            case 1:
                return '𝅝 ';
                break;
            case 2:
                return '𝅗𝅥 ';
                break;
            case 4:
                return '𝅘𝅥 ';
                break;
            case 8:
                return '𝅘𝅥𝅮 ';
                break;
            case 16:
                return '𝅘𝅥𝅯 ';
                break;
            case 32:
                return '𝅘𝅥𝅰 ';
                break;
            case 64:
                return '𝅘𝅥𝅱 '
                break;
            case 128:
                return '𝅘𝅥𝅲 '
                break;
            default:
                return null;
                break;
        }
    } else if(dur.numerator === 2 && dur.denominator === 1) {
        return '𝅜 '
    } else {
        return null;
    }
}

function compute_dot_times(dur, den) {
    let term = den * ( (2 / den) - (dur.numerator / dur.denominator));
    return [ den, -Math.log2(term) ];
}

function musical_symbol(lookup, dur) {
    // unicode characters sometimes hide from you!
    const dot = '𝅭 ';
    let isNat = n => {
        if (typeof n !== 'number')
            return false;
        return (n >= 0.0) && (Math.floor(n) === n) && n !== Infinity;
    };
    var standard_symbol = lookup(dur);
    var bla = [0, 1, 2, 3, 4, 5, 6, 7 ].map(compute_dot_times.bind(this, dur));
    console.log(bla);
    var dots = bla.filter(([den, dots]) => isNat(dots));
    console.log(dots);

    if(standard_symbol !== null) {
        return standard_symbol;
    } else if (dots.length !== 0) {
        var symbol = lookup(new Rational(1, dots[0][0])) + ' ';
        for(var i = dots[0][1]; i > 0; i--) {
            symbol = symbol + dot;
        }
        return symbol;
    } else {
        return dur.toString();
    }
}

class Music {
    constructor(dur, pitch_class, octave) {
        this.dur = dur;
        if(valid_pitches.indexOf(pitch_class) !== -1) {
            this.pitch = pitch_class;
        } else {
            throw `Invalid pitch class '${pitch_class}'`;
        }
        this.octave = octave;
    }

    toString() {
        if(this.pitch === 'Rest') {
            return `${displayPitch(this.pitch)} for ${this.dur.toString()}`;
        } else {
            return `${displayPitch(this.pitch)}${this.octave} for ${this.dur.toString()}`;
        }
    }

    nodeText() {
        if(this.pitch === 'Rest') {
            return `${musical_symbol(standard_rests, this.dur)} Rest`;
        } else {
            return `${musical_symbol(standard_notes, this.dur)}   ${displayPitch(this.pitch)}${this.octave}`
        }
    }


    static fromObject(obj) {
        return new Music(Rational.fromObject(obj.dur), obj.pitch, Number(obj.octave));
    }
}

class Rational {
    constructor(a, b) {
        this.numerator = a;
        this.denominator = b;
        this.reduce();
    }

    reduce() {
        let gcd = (a, b) => !b ? a : gcd(b, a % b);
        let div = function(a, b) {
            if(b === 0) {
                throw 'Divide by zero';
            } else {
                return Math.floor(a / b);
            }
        };

        var d = gcd(this.numerator, this.denominator);
        this.numerator = div(this.numerator, d);
        this.denominator = div(this.denominator, d);
    }

    toString() {
        return `${this.numerator}/${this.denominator}`;
    }

    static fromObject(obj) {
        return new Rational(obj.numerator, obj.denominator);
    }
}

function collectGraphData(nodeDate, edgeData) {
    return {
        nodes: [... nodeData.values()].map(x => ({
            id: x.nodeData.id,
            music: x.music
        })),
        edges: [... edgeData.values()].map(x => ({
            id: x.edgeData.id,
            from: x.edgeData.from,
            to: x.edgeData.to,
            prob: x.prob
        }))
    };
}

function importGraphData(g) {
    nodeData = new Map();
    edgeData = new Map();
    var nodeSet = new vis.DataSet({});
    var edgeSet = new vis.DataSet({});
    for(let node of g.nodes) {
        var music = Music.fromObject(node.music);
        var data = { id: node.id, label: music.nodeText() };
        nodeData = nodeData.set(node.id, { nodeData: data, music: node.music });
        nodeSet.add(data);
    }

    for(let edge of g.edges) {
        var data = {
            id: edge.id,
            from: edge.from,
            to: edge.to,
            label: `${edge.prob * 100}%`
        };
        edgeData = edgeData.set(edge.id, { edgeData: data, prob: edge.prob });
        edgeSet.add(data);
    }

    network.setData({ nodes: nodeSet, edges: edgeSet });
}

// helper

function download(url, filename) {
    var link = document.createElement('a');
    link.setAttribute('href', url);
    link.setAttribute('download', filename);
    link.style.display = 'none';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}

function downloadFile(content_type, filename, content) {
    var data = `data:${content_type},${encodeURIComponent(content)}`;
    download(data, filename);
}


// graph code

var nodeData = Map();
var edgeData = Map();
var network = null;
var starting_node_id = null;


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
            Number(document.getElementById('octave').value));
        data.label = music.nodeText();
        clearOverlay();
        callback(data);
        nodeData = nodeData.set(data.id, { music: music, nodeData: data });
    }

    function discardNode(callback) {
        clearOverlay();
        callback(null);
    }

    showOverlay('node-overlay');
    var node = nodeData.get(data.id);
    if(node !== undefined) {
        var music = node.music;
        document.getElementById('pitch').value = music.pitch;
        document.getElementById('octave').value = music.octave;
        document.getElementById('numerator').value = music.dur.numerator;
        document.getElementById('denominator').value = music.dur.denominator;
    }
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
        // for some reason, editWithoutDrag
        // sets from & to to the node respective
        // node objects, which results in the edge
        // disappearing.
        if (typeof data.to === 'object')
            data.to = data.to.id
        if (typeof data.from === 'object')
            data.from = data.from.id

        var prob = document.getElementById('prob').value / 100;
        data.label = `${prob * 100}%`;
        clearOverlay();
        callback(data);
        edgeData = edgeData.set(data.id, { prob: prob, edgeData: data } );
    }

    function discardEdge(callback) {
        clearOverlay();
        callback(null);
    }

    showOverlay('edge-overlay');
    var edge = edgeData.get(data.id);
    if(edge !== undefined) {
        document.getElementById('prob').value = edge.prob * 100;
    }
    document.getElementById('edge-save').onclick = saveEdge.bind(this, data, callback);
    document.getElementById('edge-cancel').onclick = discardEdge.bind(this, callback);
}

function deleteFromMap(data, callback) {
    for(let node of data.nodes) {
        nodeData = nodeData.delete(node);
    }

    for(let edge of data.edges) {
        edgeData = edgeData.delete(edge);
    }

    callback(data);
}


function hideOverlay(id) {
    document.getElementById(id).classList.add('hidden');
}

function handleImport() {
    var files = document.getElementById('upload-score').files;
    if(files.length === 0) {
        alert('Select a file first!');
    } else {
        var file = files[0];
        var reader = new FileReader();
        reader.addEventListener('loadend', function() {
            var parsed = JSON.parse(this.result);
            if(parsed === undefined) {
                alert('Could not parse likely score');
            } else {
                var confirmation = window.confirm('Proceeding will overwrite the current graph. Are you sure?');
                if(confirmation) {
                    try {
                        importGraphData(parsed);
                    } catch(e) {
                        alert(`Could not import likely score, probably the file was malformed. Error: ${e}`);
                    }
                }
            }
        });
        reader.readAsText(file);
    }
}

function saveDataToLocalStorage() {
    const json = JSON.stringify(collectGraphData(nodeData, edgeData));
    const params = JSON.stringify(gatherParams());
    localStorage.setItem("score", json)
    localStorage.setItem("params", params)
}

function showStartingNode() {
    if(typeof starting_node_id === 'string') {
        network.selectNodes([starting_node_id], false);
    } else {
        alert('No starting node selected yet!');
    }
}

function setStartingNode() {
    var selected = network.getSelectedNodes();
    if(selected.length > 1) {
        alert('Only select one node!');
    } else if(selected.length === 0) {
        alert('Select a node first!');
    } else {
        starting_node_id = selected[0];
    }
}

function fetchInterpretation(params, format) {
    var jsonRequest = JSON.stringify({
        graph: collectGraphData(nodeData, edgeData),
        params: params
    });

    var myHeaders = new Headers();
    myHeaders.set('Content-Type', 'application/json');

    var myInit = {
        method: 'POST',
        headers: myHeaders,
        mode: 'cors',
        body: jsonRequest
    };

    var myRequest = new Request(`/interpretation/${format}`, myInit);

    return fetch(myRequest).then(res => res.blob());
}

function gatherParams() {
    var starting_node_entry = nodeData.get(starting_node_id);
    if(starting_node_entry !== undefined && starting_node_entry !== null) {
        var starting_node = {
            id: starting_node_entry.nodeData.id,
            music: starting_node_entry.music
        };
    } else {
        var starting_node = null
    }

    var maxhops = document.getElementById('hop-count').value;
    if(maxhops === "" || Number(maxhops) === NaN) {
        maxhops = null;
    } else {
        maxhops = Number(maxhops);
    }

    var seed = document.getElementById('seed').value;
    if(seed === "" || Number(seed) === NaN) {
        seed = null;
    } else {
        seed = Number(seed);
    }

    return {
        maxhops:  maxhops,
        starting_node: starting_node,
        seed: seed
    };
}

function completeGatherParams() {
    var p = gatherParams();
    if(p.starting_node === null) {
        alert('Set a starting node first!');
        return null;
    }

    if(p.maxhops === null) {
        alert('Set the maximum amount of hops to a valid number');
        return null;
    }

    if(p.seed === null) {
        // TODO auto generate a random one, let the user confirm before
        alert('Set the seed to a valid number!');
        return null;
    }

    return p;
}

function importParams(p) {
    if(p.starting_node !== null) {
        starting_node_id = p.starting_node.id;
    }
    if(p.seed !== null) {
        document.getElementById('seed').value = p.seed;
    }
    if(p.maxhops !== null) {
        document.getElementById('hop-count').value = p.maxhops;
    }
}

function randomSeed() {
    if(window.crypto) {
        var array = new Int32Array(1);
        window.crypto.getRandomValues(array);
        document.getElementById('seed').value = array[0];
    }
}

function downloadInterpretation(format) {
    var params = completeGatherParams();
    if(params != null) {
        try {
            fetchInterpretation(params, format).then(file => {
                var url = URL.createObjectURL(file);
                download(url, `export.${format}`);
                URL.revokeObjectURL(url);
            });
        } catch(e) {
            alert('An error occured while contacting the API: ' + e);
        }
    }
}

function reloadPlayer() {
    var params = completeGatherParams();
    if(params !== null) {
        if(document.getElementById('player').src) {
            URL.revokeObjectURL(document.getElementById('player').src);
        }

        document.getElementById('player').src = null;

        try {
            fetchInterpretation(params, 'wav').then(file => {
                var url = URL.createObjectURL(file);
                document.getElementById('player').src = url;
            });
        } catch(e) {
            alert('An error occured while contacting the API: ' + e);
        }
    }
}

function init() {
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
            editEdge: {
                editWithoutDrag: function(edgeData, callback) {
                    document.getElementById('edge-operation').innerHTML = 'Edit';
                    genericEditEdge(edgeData, callback);
                }
            },
            deleteNode: deleteFromMap,
            deleteEdge: deleteFromMap,
            controlNodeStyle: {
            }
        },
        nodes: {
            borderWidth: 0,
            color: {
                background: '#563d7c',
                hover: {
                    background: '#8f14ff'
                },
                highlight: {
                    background: '#8f14ff'
                }
            },
            chosen: true,
            font: {
                color: 'white',
                size: 20,
                align: 'center'
            },
            shape: 'circle',
        },
        edges: {
            arrows: {
                to: { enabled: true }
            },
            color: {
                color: '#563d7c',
                hover: '#563d7c',
                highlight: '#563d7c',
            },
            font: {
                color: '#ffffff',
                strokeWidth: 0
            }
        }
    };

    network = new vis.Network(container, {}, options);

    try {
        const score = localStorage.getItem('score');
        if(score !== null) {
            importGraphData(JSON.parse(score));
        }
    } catch(e) {
        localStorage.removeItem('score');
    }

    try {
        const params = localStorage.getItem('params')
        if(params !== null) {
            importParams(JSON.parse(params));
        }
    } catch(e) {
        localStorage.removeItem('params');
    }

    const pitch_selector = valid_pitches.map((p, i) =>
        `<option value="${p}">${display_pitches[i]}</option>`)
        .reduce((acc, v) =>
            acc + v, '');
    document.getElementById('pitch').innerHTML = pitch_selector;

    /* event handling, order as in sidebar */
    document.getElementById('set-starting-node').onclick = setStartingNode;
    document.getElementById('show-starting-node').onclick = showStartingNode;

    document.getElementById('random-seed').onclick = randomSeed;

    document.getElementById('reload-player').onclick = reloadPlayer;
    document.getElementById('download-audio').onclick = () => {
        var format = document.getElementById('format').value;
        downloadInterpretation(format);
    };

    document.getElementById('gen-score').onclick = () =>
        downloadFile('application/json', 'score.likely.json',
            JSON.stringify(collectGraphData(nodeData, edgeData)));
    document.getElementById('upload-score').addEventListener('change',handleImport);
    document.getElementById('clear-score').onclick = () =>
        importGraphData({ nodes: [], edges: []});

    window.setInterval(saveDataToLocalStorage, 5000);
}

document.addEventListener('DOMContentLoaded', () => init());
