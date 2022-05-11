const languageToExtention = {
    'python': 'py',
    'java': 'java',
    'viz': 'viz'
}

const languageToCompiler = {
    '.py': 'python',
    '.java': 'javac',
    '.viz': './viz'
}

module.exports = {
    languageToExtention,
    languageToCompiler
}