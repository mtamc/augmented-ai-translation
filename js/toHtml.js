const { countTokens } = require('@anthropic-ai/tokenizer')
const firstArg = process.argv[2]

function convertStr(text) {
return text
  .replace(/\n{2,}/g, '<div style=\'line-height:33%;\'><br></div><div style=\'line-height:33%;\'><br></div>')
  .replace(/\n/g, '<div style=\'line-height:33%;\'><br></div>')
  .replace(/"(.*?)"/g, '<span style=\'color: white\'>“$1”</span>')
}

console.log(convertStr(firstArg))
