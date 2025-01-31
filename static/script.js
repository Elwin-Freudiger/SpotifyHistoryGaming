const resultsDiv = document.getElementById("results");
const form = document.getElementById("uploadForm");

form.addEventListener("submit", async (event) => {
    event.preventDefault(); // Prevent form reload

    const fileInput = document.getElementById("fileInput");
    const playerName = document.getElementById("playerName").value;

    const files = fileInput.files; // Get all files
    console.log("Selected files:", files); // Debugging: Log selected files
    console.log("Player name:", playerName); // Debugging: Log player name

    if (files.length === 0) {
        alert("No files selected. Please select a folder.");
        return;
    }

    const formData = new FormData();
    formData.append("player_name", playerName);

    // Append all files to the FormData object
    for (let i = 0; i < files.length; i++) {
        console.log(`Appending file: ${files[i].name}`); // Debugging: Log each file
        formData.append("files[]", files[i]);
    }

    try {
        const response = await fetch("/upload", {
            method: "POST",
            body: formData,
        });

        if (!response.ok) {
            const error = await response.json();
            console.error("Server error:", error); // Log server error
            alert(`Error: ${error.error}`);
            return;
        }

        const data = await response.json();
        console.log("Server response:", data); // Log success response
    } catch (error) {
        console.error("Error uploading files:", error); // Log fetch error
    }
});