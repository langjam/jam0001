
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

        let message = JSON.parse(m.data)
        console.log(message);
        if (typeof message["type"] !== "undefined") {
            switch (message["type"]) {
                case "VisualizerData": {
                    const path = message["path"];
                    loadData(path.substr(1, path.length - 2));

                    break;
                }
                case "CreateDataError": {
                    window.localStorage.setItem("error", "failed to create visualizer data. Retrying")
                    location.reload()
                    break;
                }
                case "Error": {
                    error(message["message"])
                    break;
                }
                case "MoveTrain": {
                    const from_station_name = message["from_station"]["name"];
                    const from_track = message["start_track"];
                    const to_station_name = message["to_station"]["name"];
                    const to_track = message["end_track"];
                    const train_identifier = message["train"]["identifier"];

                    console.log(from_station_name, from_track, to_station_name, to_track, train_identifier);

                    const line = lineLookup.get(from_station_name).get(from_track)

                    const line_start_location = createVector(line[0][0], line[0][1])
                    let direction = DIRECTION.North;
                    if (line.length > 1) {
                        const line_second_location = createVector(line[1][0], line[1][1])


                        if (line_second_location.x > line_start_location.x) {
                            direction = DIRECTION.East;
                        } else if (line_second_location.x < line_start_location.x) {
                            direction = DIRECTION.West;
                        }else if (line_second_location.y > line_start_location.y) {
                            direction = DIRECTION.South;
                        } else {
                            direction = DIRECTION.North;
                        }
                    }


                    if (!grid.hasTrain(train_identifier)) {
                        grid.addTrain(new Train(
                            line_start_location,
                            COLOR.DarkGreen,
                            COLOR.Brown,
                            direction,
                            train_identifier,
                        ))
                    }

                    const train = grid.trains.get(train_identifier);

                    train.location = line_start_location
                    train.travelAlongPath(line);

                    break;
                }
            }
        }
    })

    s.addEventListener("open", () => {
        console.log("open")
    })

    return {
        nextTimeStep: function () {
            s.send(JSON.stringify({
                "type": "AdvanceSimulation",
            }))
        }
    }
}
