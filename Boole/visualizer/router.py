import json
from math import sqrt
from pprint import pprint
from random import random

from matplotlib import pyplot as plt

ATTRACTION = 1


class Station:
    def __init__(self, id, inputs, to):
        self.id = id
        self.inputs = inputs
        self.to = to
        self.x = random()*2
        self.y = random()*2
        self.force_x = 0
        self.force_y = 0

    def sim(self,dt):
        self.x += self.force_x*dt
        self.y += self.force_y*dt

    def __eq__(self, other):
        return self.id == other.id

class World:
    def __init__(self, stations):
        self.stations = stations
        self.roads = []
        self.blocked = dict()

    @staticmethod
    def build(stations):
        for itt in range(10000):
            err = 0
            for movable in stations:
                movable.force_x += random()*10000/(itt+50)
                movable.force_y += random()*10000/(itt+50)
                for other in stations:
                    if movable == other:
                        continue
                    delta = (1/(movable.x-other.x),1/(movable.y-other.y))
                    movable.force_x += delta[0]
                    movable.force_y += delta[1]
                for neighbour in movable.to:
                    other = stations[neighbour]
                    if movable == other:
                        continue
                    delta = ((movable.x - other.x), (movable.y - other.y))
                    normalization = sqrt(delta[0]**2+delta[1]**2)/((len(movable.to)+other.inputs)**0.2)
                    err+=sqrt(delta[0]**2+delta[1]**2)
                    # print(normalization)
                    delta = (delta[0]/normalization,delta[1]/normalization)
                    movable.force_x -= delta[0]*ATTRACTION
                    movable.force_y -= delta[1]*ATTRACTION

                    other.force_x += delta[0] * ATTRACTION
                    other.force_y += delta[1] * ATTRACTION

            print(err)
            for movable in stations:
                movable.sim(0.01)
                movable.force_x = 0
                movable.force_y = 0

        # ax.scatter(z, y)
        xs = []
        ys = []
        ids = []
        for station in stations:
            print(station.id,station.x,station.y)
            xs.append(station.x)
            ys.append(station.y)
            ids.append(station.id)

        plt.scatter(xs, ys)

        for i, txt in enumerate(ids):
            plt.annotate(txt, (xs[i], ys[i]))
        plt.show()

        return World(stations)

if __name__ == '__main__':
    with open("test_places.json") as f:
        data = json.load(f)
    pprint(data)

    stations = [Station(id, d["inputs"], d["to"]) for id,d in enumerate(data)]
    world = World.build(stations)