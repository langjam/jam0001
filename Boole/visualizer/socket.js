
function getParameterByName(name, url = window.location.href) {
    name = name.replace(/[\[\]]/g, '\\$&');
    var regex = new RegExp('[?&]' + name + '(=([^&#]*)|&|#|$)'),
        results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, ' '));
}

const WS_URL = getParameterByName("wsurl") || "ws://localhost:12345/ws"

function startSocket() {
    console.info(WS_URL)
    let s = new WebSocket(WS_URL);

    s.addEventListener("message", (m) => {
        console.log(m)
    })

    s.addEventListener("open", () => {
        console.log("open")
    })

    return {
        nextTimeStep: function () {
            s.send(JSON.stringify({
                "AdvanceSimulation": null
            }))
        }
    }
}
