
import {VM} from "./VM";
import {Model} from "./Model";
import * as readline from "readline";

let model = new Model();
model.addPost({
    title: "main",
    id: "22",
    upvotes: 4,
    date: 100,
    children: [
        {
            id: "5",
            content: "[]",
            upvotes: -5,
            date: 100,
            children: []
        },
        {
            id: "10",
            content: "0",
            upvotes: -5,
            date: 100,
            children: []
        },
        {
            id: "32",
            content: "while ${get 1 comment 1 up} < 10:",
            upvotes: 1,
            date: 200,
            children: [
                {
                    id: "36",
                    content: "if ${get 1 comment 1 left 1 up}<=5:",
                    upvotes: -5,
                    date: 300,
                    children: [
                        {
                            id: "33",
                            content: "log(${get 1 comment 2 left 1 up}, ${get 1 comment 2 left 2 up})",
                            upvotes: -5,
                            date: 400,
                            children: []
                        }
                    ]
                },
                {
                    id: "35",
                    content: "set {get 1 comment 1 left 1 up} to ${get 1 comment 1 left 1 up} + 1",
                    upvotes: -5,
                    date: 400,
                    children: []
                },
                {
                    id: "100",
                    content: "set {get 1 comment 1 left 2 up} to (${get 1 comment 1 left 2 up} + [${get 1 comment 1 left 1 up}])",
                    upvotes: -5,
                    date: 400,
                    children: []
                }
            ]
        }
    ]
});
let vm = new VM(model.makeCommentProvider("22"));
vm.run();
