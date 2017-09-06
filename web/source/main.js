import vis from 'vis';
import { Map } from 'immutable';
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
        throw 'Invalid pitch';
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
            throw `Invalid pitch class '${pitch_class}'`;
        }
        this.octave = octave;
    }

    toString() {
        if(this.pitch === 'Rest') {
            return `${displayPitch(this.pitch)} for
${this.dur.toString()}`;
        } else {
            return `${this.octave}${displayPitch(this.pitch)} for
${this.dur.toString()}`;
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
    var nodeSet = new vis.DataSet({});
    var edgeSet = new vis.DataSet({});
    for(let node of g.nodes) {
        var music = Music.fromObject(node.music);
        var data = { id: node.id, label: music };
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
var starting_node_id = undefined;


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
        data.label = music.toString();
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

function saveGraphToLocalStorage() {
    const json = JSON.stringify(collectGraphData(nodeData, edgeData));
    localStorage.setItem("score", json)
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

function genInterpretation(format) {
    try {
        var starting_node_entry = nodeData.get(starting_node_id);
        var starting_node = {
            id: starting_node_entry.nodeData.id,
            music: starting_node_entry.music
        };
    } catch(e) {
        alert('Set a starting node first!');
        return;
    }

    try {
        var maxhops = document.getElementById('hop-count').value;

        var jsonRequest = JSON.stringify({
            graph: collectGraphData(nodeData, edgeData),
            params: { maxhops: 100, starting_node: starting_node }
        });


        var myHeaders = new Headers();
        myHeaders.set('Content-Type', 'application/json');

        var myInit = {
            method: 'POST',
            headers: myHeaders,
            mode: 'cors',
            body: jsonRequest
        };

        var myRequest = new Request(`http://localhost:8081/interpretation/${format}`, myInit);

        fetch(myRequest).then(res => res.blob()).then(file => {
            var url = URL.createObjectURL(file);
            download(url, 'export.midi');
        });
    } catch(e) {
        alert('An error occured while contacting the API: ' + e);
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
            deleteNode: true,
            deleteEdge: true,
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
                color: 'white'
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


    const pitch_selector = valid_pitches.map((p, i) =>
        `<option value="${p}">${display_pitches[i]}</option>`)
        .reduce((acc, v) =>
            acc + v, '');
    document.getElementById('pitch').innerHTML = pitch_selector;

    document.getElementById('gen-midi').onclick =
        genInterpretation.bind(this, ('midi'));

    document.getElementById('gen-score').onclick = () =>
        downloadFile('application/json', 'score.likely.json',
            JSON.stringify(collectGraphData(nodeData, edgeData)));

    document.getElementById('upload-score').addEventListener('change',handleImport);

    document.getElementById('show-starting-node').onclick = showStartingNode;
    document.getElementById('set-starting-node').onclick = setStartingNode;

    window.setInterval(saveGraphToLocalStorage, 10000);
}

document.addEventListener('DOMContentLoaded', () => init());
