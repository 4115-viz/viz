const { v4: uuid } = require("uuid");
const path = require('path');
const fs = require('fs');
const { languageToExtention } = require('../languageMaps');

const dirCodes = path.join(__dirname, "viz/online_programs");

if (!fs.existsSync(dirCodes)) {
    fs.mkdirSync(dirCodes, { recursive: true });
}

const generateFile = async (content, language) => {
    const jobId = uuid();
    const format = languageToExtention[language];
    const filename = `${jobId}.${format}`;
    const filepath = path.join(dirCodes, filename);
    await fs.writeFileSync(filepath, content);
    // return filepath;
    return filename;
}

module.exports = {
    generateFile
};