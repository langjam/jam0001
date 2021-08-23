import { oakCors } from "https://deno.land/x/cors@v1.2.2/mod.ts";
import type { RouterContext } from 'https://deno.land/x/oak@v8.0.0/mod.ts';
import { Application, Router } from 'https://deno.land/x/oak@v8.0.0/mod.ts';
import { getEnv } from './env.ts';

const log = (...items: any) => {
    console.log(`[${new Date().toLocaleString()}]`, ...items)
}

// Copy pasted from data/acquisition/get-and-format-data.js
const labelMap = { compliment: 0, question: 1, joke: 2, hacker: 3 }

// Invert label map
const LABEL_MAP: Record<number, string> = Object.entries(labelMap)
    .reduce((acc, curr) => ({ ...acc, [curr[1]]: curr[0] }), {})

const PORT = 8999
const router = new Router()
const { HuggingFaceKey, HuggingFaceAPI }: Record<string, string> = await getEnv()

if (!HuggingFaceKey) throw new Error('Set HuggingFaceKey in .env file')

const errorOut = (ctx: RouterContext, message: string, status=400) => {
    ctx.response.status = status
    ctx.response.body = message
}

router
    .get('/', (ctx) => { ctx.response.body = 'healthy' })
    .post('/inference', async (ctx) => { 
        const body = await ctx.request.body().value
        const { comment } = JSON.parse(body)

        log(comment)

        if (!comment) 
            return errorOut(ctx, 'add comment to body')
        
        if (comment.length > 100) 
            return errorOut(ctx, 'comment length cannot exceed length 100')

        log('... Inferring')

        const start = Date.now()

        const response = await fetch(HuggingFaceAPI, {
            method: 'POST',
            body: JSON.stringify({ inputs: comment }),
            headers: { Authorization: HuggingFaceKey }
        })

        if (response.status !== 200) {
            const error = await response.json()

            if (error?.error?.includes('is currently loading')) {
                const time = error['estimated_time']
                log('HuggingFace.loading', time)
                return errorOut(ctx, `HuggingFace.loading.${time}`)
            }

            log(`"${comment}" unknown error ${error}`)
            return errorOut(ctx, `HuggingFace.unknown`)
        }            

        log(`"${comment}" took ${((Date.now() - start) / 1000).toFixed(2)} seconds`)

        type HFResult = {score: number, label: string}        

        const json = await response.json()
        const predictions: HFResult[] = json[0] ?? json[0][0]

        if (!predictions) 
            return errorOut(ctx, `No predictions available: ${json}`)

        const best: HFResult = predictions.reduce((best, pred) => 
            best.score > pred.score ? best : pred
        , { label: 'LABEL__', score: -Infinity })        

        const label = best.label.replace('LABEL_', '')

        try {
            const response = LABEL_MAP[parseInt(label)]
            ctx.response.body = response
        } catch {
            return errorOut(ctx, `Hugging face returned an invalid label: ${label}`)
        }
    })
		
const app = new Application()

app.use(
    oakCors({ 
        // TODO
        // origin: dev ? '*' : 'https://first-class-comment.netlify.app/'
        origin: '*'
    })
)
app.use(router.routes())
app.use(router.allowedMethods())

console.log(`Running on port ${PORT}`)
await app.listen({ port: PORT })