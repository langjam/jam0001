import json
from collections import defaultdict
from math import sqrt, inf
from pprint import pprint
from random import random, shuffle
from queue import PriorityQueue

from matplotlib import pyplot as plt, patches
import matplotlib

matplotlib.use("TkAgg")

ATTRACTION = 0.5


class Station:
    def __init__(self, id, inputs, to):
        self.id = id
        self.inputs = inputs
        self.to = to
        self.x = random() * 2
        self.y = random() * 2
        self.force_x = 0
        self.force_y = 0
        self.port_locs = dict()

    def sim(self, dt):
        self.x += self.force_x * dt
        self.y += self.force_y * dt

    def dist(self, other):
        if type(other) == Station:
            return sqrt((self.x - other.x) ** 2 + (self.y - other.y) ** 2)
        else:
            return sqrt((self.x - other[0]) ** 2 + (self.y - other[1]) ** 2)

    def __eq__(self, other):
        return self.id == other.id


class StationGroup:
    def __init__(self, stations):
        self.stations = stations

    @staticmethod
    def build(stations):
        for itt in range(40000):
            err = 0
            for movable in stations:
                movable.force_x += random() * 10000 / (itt + 50)
                movable.force_y += random() * 10000 / (itt + 50)
                for other in stations:
                    if movable == other:
                        continue
                    delta = (1 / (movable.x - other.x), 1 / (movable.y - other.y))
                    movable.force_x += delta[0]
                    movable.force_y += delta[1]
                for neighbour in movable.to:
                    other = stations[neighbour[0]]
                    if movable == other:
                        continue
                    delta = ((movable.x - other.x), (movable.y - other.y))
                    normalization = sqrt(delta[0] ** 2 + delta[1] ** 2) / ((len(movable.to) + other.inputs) ** 0.2)
                    err += sqrt(delta[0] ** 2 + delta[1] ** 2)
                    # print(normalization)
                    delta = (delta[0] / normalization, delta[1] / normalization)
                    movable.force_x -= delta[0] * ATTRACTION
                    movable.force_y -= delta[1] * ATTRACTION

                    other.force_x += delta[0] * ATTRACTION
                    other.force_y += delta[1] * ATTRACTION

            # print(err)
            for movable in stations:
                movable.sim(0.01)
                movable.force_x = 0
                movable.force_y = 0

        return StationGroup(stations)

    def scale(self, min_dist):
        dx = inf
        dy = inf
        avg_x = 0
        avg_y = 0
        for i in range(len(self.stations)):
            avg_x += self.stations[i].x
            avg_y += self.stations[i].y
            for j in range(i):
                dx = min(dx, abs(self.stations[i].x - self.stations[j].x) + abs(
                    self.stations[i].y - self.stations[j].y) / 3)
                dy = min(dy, abs(self.stations[i].y - self.stations[j].y) + abs(
                    self.stations[i].x - self.stations[j].x) / 3)

        avg_x, avg_y = avg_x / len(self.stations), avg_y / len(self.stations)

        for station in self.stations:
            station.x = round((station.x - avg_x) * min_dist / dx)
            station.y = round((station.y - avg_y) * min_dist / dy)

    def plot(self):
        fig, ax = plt.subplots()

        xs = []
        ys = []
        ids = []
        for station in self.stations:
            print(station.id, station.x, station.y)
            xs.append(station.x)
            ys.append(station.y)
            ids.append(station.id)

        plt.xlim(min(xs) - 3, max(xs) + 5)
        plt.ylim(min(ys) - 3, max(ys) + 5)
        ax.set_aspect('equal')

        for n, id in enumerate(ids):
            ax.add_patch(patches.Rectangle((xs[n], ys[n]), 2, 2))

        for i, txt in enumerate(ids):
            ax.annotate(txt, (xs[i], ys[i]))
        plt.show()


class World:
    def __init__(self, stations):
        self.stations = stations
        self.roads = []
        self.filled = dict()

        for station in stations:
            self.filled[(station.x, station.y)] = (0, station.id, 0)
            self.filled[(station.x + 1, station.y)] = (0, station.id, 0)
            self.filled[(station.x, station.y + 1)] = (0, station.id, 0)
            self.filled[(station.x + 1, station.y + 1)] = (0, station.id, 0)

    def build(self):
        # Direction 1 -> horizontal
        # Direction 2 -> vertical
        # Direction 3 -> corner

        for station in self.stations:
            for goal in station.to:
                self.roads.append(self.a_star(station, goal))

    def a_star(self, station, goal):

        open_set = set()

        open_set.add((station.x, station.y, 0))
        open_set.add((station.x + 1, station.y, 0))
        open_set.add((station.x, station.y + 1, 0))
        open_set.add((station.x + 1, station.y + 1, 0))

        goal, port = self.stations[goal[0]], goal[1]

        if port not in goal.port_locs:

            goal_set = set()

            goal_set.add((goal.x, goal.y))
            goal_set.add((goal.x + 1, goal.y))
            goal_set.add((goal.x, goal.y + 1))
            goal_set.add((goal.x + 1, goal.y + 1))

        else:
            goal_set = set()
            goal_set.add(goal.port_locs[port][:2])

        came_from = dict()
        g_score = defaultdict(lambda: inf)
        f_score = defaultdict(lambda: inf)
        for loc in open_set:
            g_score[loc] = 0
            f_score[loc] = goal.dist(loc[:2])

        while open_set:
            current = min(open_set, key=lambda x: f_score[x])
            if current[:2] in goal_set or (current[:2] in self.filled and self.filled[current[:2]][1] == goal.id and self.filled[current[:2]][2] == port and self.filled[current[:2]][0]!=0):
                if port not in goal.port_locs:
                    goal.port_locs[port] = came_from[current]
                else:
                    print("YUS")
                return self.reconstruct(came_from, current, goal.id, port)

            open_set.remove(current)

            if (current[0] + 1, current[1]) not in self.filled or self.filled[(current[0] + 1, current[1])][
                1:] == [goal.id,port] or (current[0] + 1, current[1]) in goal_set:
                neighbour = (current[0] + 1, current[1], 1)
                cost = [0, 1, 2, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.add(neighbour)
            else:
                if self.filled[(current[0] + 1, current[1])][0] == 2 and (
                        current[0] + 2, current[1]) not in self.filled:
                    neighbour = (current[0] + 2, current[1], 1)
                    cost = [0, 2, 4, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = current
                        g_score[neighbour] = tentative_score
                        f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.add(neighbour)

            if (current[0] - 1, current[1]) not in self.filled or self.filled[(current[0] - 1, current[1])][
                1:] == [goal.id,port] or (current[0] - 1, current[1]) in goal_set:
                neighbour = (current[0] - 1, current[1], 1)
                cost = [0, 1, 2, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.add(neighbour)
            else:
                if self.filled[(current[0] - 1, current[1])][0] == 2 and (
                        current[0] - 2, current[1]) not in self.filled:
                    neighbour = (current[0] - 2, current[1], 1)
                    cost = [0, 2, 4, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = current
                        g_score[neighbour] = tentative_score
                        f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.add(neighbour)

            if (current[0], current[1] + 1) not in self.filled or self.filled[(current[0], current[1] + 1)][
                1:] == [goal.id,port] or (current[0], current[1]+1) in goal_set:
                neighbour = (current[0], current[1] + 1, 2)
                cost = [0, 2, 1, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.add(neighbour)
            else:
                if self.filled[(current[0], current[1] + 1)][0] == 2 and (
                        current[0], current[1] + 2) not in self.filled:
                    neighbour = (current[0], current[1] + 2, 2)
                    cost = [0, 4, 2, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = current
                        g_score[neighbour] = tentative_score
                        f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.add(neighbour)

            if (current[0], current[1] - 1) not in self.filled or self.filled[(current[0], current[1] - 1)][
                1:] == [goal.id,port] or (current[0], current[1]-1) in goal_set:
                neighbour = (current[0], current[1] - 1, 2)
                cost = [0, 2, 1, 3][current[2]]
                tentative_score = g_score[current] + cost
                if tentative_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_score
                    f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                    open_set.add(neighbour)
            else:
                if self.filled[(current[0], current[1] - 1)][0] == 2 and (
                        current[0], current[1] - 2) not in self.filled:
                    neighbour = (current[0], current[1] - 2, 2)
                    cost = [0, 4, 2, 6][current[2]]
                    tentative_score = g_score[current] + cost
                    if tentative_score < g_score[neighbour]:
                        came_from[neighbour] = current
                        g_score[neighbour] = tentative_score
                        f_score[neighbour] = tentative_score + goal.dist(neighbour[:2])
                        open_set.add(neighbour)

    def reconstruct(self, came_from, current, goal, port):
        total_path = [current[:2]]
        self.filled[current[:2]] = (current[2], goal,port)
        while current in came_from:
            current = came_from[current]
            self.filled[current[:2]] = (current[2], goal, port)
            total_path.append(current[:2])
        return total_path[::-1]

    def plot(self):
        fig, ax = plt.subplots()

        xs = []
        ys = []
        ids = []
        for station in self.stations:
            print(station.id, station.x, station.y)
            xs.append(station.x)
            ys.append(station.y)
            ids.append(station.id)

        plt.xlim(min(xs) - 3, max(xs) + 5)
        plt.ylim(min(ys) - 3, max(ys) + 5)
        ax.set_aspect('equal')

        for n, id in enumerate(ids):
            ax.add_patch(patches.Rectangle((xs[n], ys[n]), 2, 2))

        for i, txt in enumerate(ids):
            ax.annotate(txt, (xs[i], ys[i]))

        for road in self.roads:
            xs, ys = zip(*road)
            xs = [x + 0.5 for x in xs]
            ys = [y + 0.5 for y in ys]
            ax.plot(xs, ys)

        plt.show()

    def to_json(self):
        pass

if __name__ == '__main__':
    with open("test_places.json") as f:
        data = json.load(f)
    pprint(data)

    stations = [Station(id, d["inputs"], d["to"]) for id, d in enumerate(data)]
    stations = StationGroup.build(stations)
    # stations.plot()
    stations.scale(3)
    stations.plot()

    world = World(stations.stations)
    world.build()
    print(world.roads)
    world.plot()