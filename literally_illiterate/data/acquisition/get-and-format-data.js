'use strict';
const snoowrap = require('snoowrap');
const fs = require('fs');
const {readFile} = require('fs/promises');
const assert = require('assert');
require('dotenv').config()

const labelMap = { compliment: 0, question: 1, joke: 2, hacker: 3 }

const { userAgent, clientId, clientSecret, refreshToken, accessToken } = process.env
const r = new snoowrap({ userAgent, clientId, clientSecret, refreshToken, accessToken })

r.config({ debug: true })

const removeTabNewlines = str => str.replace(/\n/g, ' ').replace(/\t/, ' ')

const fromFile = async (filename) => {
	const data = await readFile(filename, 'utf8')
	return data.split('\n')
}

const getBody = post => post.selftext
const getTitle = post => post.title
const getTitlesFromPosts = posts => posts.map(getTitle)
const postsFromSub = async (sub, limit) => {
	// limit seems to max at 1000, so its largely ignored since i'm requesting way more than 1000
	const promises = await Promise.all([
		r.getSubreddit(sub).getTop({ limit, time: 'all' }),
		r.getSubreddit(sub).getTop({ limit, time: 'year' }),
		r.getSubreddit(sub).getTop({ limit, time: 'month' }),
		r.getSubreddit(sub).getTop({ limit, time: 'day' }),
		r.getSubreddit(sub).getHot({ limit, time: 'all' }),
		r.getSubreddit(sub).getHot({ limit, time: 'year' }),
		r.getSubreddit(sub).getHot({ limit, time: 'month' }),
		r.getSubreddit(sub).getHot({ limit, time: 'day' }),
	])

	const results = promises.flatMap(x => x)
	const ids = results.map(p => p.id)

	// deduplicate
	return results.filter(({ id }, index) => !ids.includes(id, index + 1))
}

async function getQuestions(total) {
	const numReddits = 1
	const limit = Math.floor(total / numReddits)

	const fromAskReddit = async () => {
		const posts = await postsFromSub('AskReddit', limit)
		return getTitlesFromPosts(posts)
	}

	return (
		await Promise.all([ 
			fromAskReddit(),  
			fromFile('./data/questions'),
		])
	).flatMap(x => x)
}

async function getHacker(total) {
	const posts = await postsFromSub('hackernews', total)
	return getTitlesFromPosts(posts)
}

async function getJokes(total) {
	const posts = await postsFromSub('jokes', total * 2)
	const jokeFile = await fromFile('./data/jokes')
	
	return [...jokeFile, ...posts]
		.map(post => getTitle(post) + ' ' + getBody(post))
		// FIXME this causes the number of jokes to be cut in half..roughly. 
		// Dirty fix is the above * 2
		.filter(joke => joke.length < 300)		
}

async function getCompliments() {
	return fromFile('./data/compliments')
}

// TODO compliments are very underrepresented in dataset
async function main() {
	const categorySize = 4_000
	const trainSize = .80	
	
	// TODO history fact	
	const [compliment, question, hacker, joke] = await Promise.all([
		getCompliments(),
		getQuestions(categorySize),
		getHacker(categorySize),
		// TODO use https://pun.me/funny/
		getJokes(categorySize),
	])

	const results = { compliment, question, joke, hacker }		
	
	assert(Object.keys(results).join() == Object.keys(labelMap).join())

	/** From https://huggingface.co/docs/datasets/v1.1.3/loading_datasets.html#json-files 
		 The most efficient format is to have JSON files consisting of multiple JSON objects, 
		one per line, representing individual data rows:
		```
		{"a": 1, "b": 2.0, "c": "foo", "d": false}
		{"a": 4, "b": -5.5, "c": null, "d": true}
		```
	 */
	const formattedForHuggingFace = Object.entries(results)
		.flatMap(([labelKey, sentences]) => {
			const label = labelMap[labelKey]

			if (label == null) throw new Error(`Unknown label id ${label}`)

			return sentences
				.map(removeTabNewlines)
				// syntax uses " to delineate comment (<name>, "<comment>"...)
				// so it must be replaced
				.map(sentence => sentence.replace('"', '\''))
				.map(sentence => JSON.stringify({ label, sentence }))
		})
		
	shuffle(formattedForHuggingFace)

	const partition = (start, end) => formattedForHuggingFace.slice(start, end)
	
	const trainEnd = Math.floor(formattedForHuggingFace.length * trainSize)
	const train = partition(0, trainEnd)
	
	// split test and validate set evenly
	const mid = Math.floor((formattedForHuggingFace.length - trainEnd) / 2) + trainEnd
	const test = partition(trainEnd, mid)
	const validation = partition(mid)	

	const stats = Object.entries(results)
		.map(([name, res]) => name + ' : ' + res.length)
		.join('\n') 
		+ `\ntotal: ${formattedForHuggingFace.length}`
		+ `\ntrain: ${train.length}`
		+ `\ntest: ${test.length}`
		+ `\nvalidation: ${validation.length}`

	console.log(results)
	console.log(stats)

	fs.writeFileSync('./data/train.json', train.join('\n'))	
	fs.writeFileSync('./data/test.json', test.join('\n'))	
	fs.writeFileSync('./data/validation.json', validation.join('\n'))	
	fs.writeFileSync('./data/data.stats', stats)	
}

main()

// https://stackoverflow.com/a/12646864
function shuffle(array) {
    for (let i = array.length - 1; i > 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        [array[i], array[j]] = [array[j], array[i]];
    }
}
