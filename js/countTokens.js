const { countTokens } = require('@anthropic-ai/tokenizer')
const firstArg = process.argv[2]
console.log(countTokens(firstArg))
