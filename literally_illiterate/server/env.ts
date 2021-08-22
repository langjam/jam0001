export const dev = Deno.env.get('MODE') === 'dev'

export const getEnv = async () => {
    const defaultEnv = {
        HuggingFaceAPI: 'https://api-inference.huggingface.co/models/Kieran/distilbert-base-uncased-finetuned-cola'
    }

    if (dev) {
        console.log('Loading .env file...')
        const dotenv = await import('https://deno.land/x/dotenv@v1.0.1/mod.ts')
        return { ...defaultEnv, ...dotenv.config() }
    }

    return {
        ...defaultEnv,
        HuggingFaceKey: Deno.env.get('HuggingFaceKey') ?? '',
    }
}