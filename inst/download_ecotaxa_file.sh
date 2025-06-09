#!/bin/bash

# === DEFAULTS ===
UNZIP=false

# === USAGE FUNCTION ===
function usage() {
    cat <<EOF
Usage: $0 [options] <job_id> <token_file_path> <output_file_path>

Download a job file from the EcoTaxa API and optionally extract it.

Arguments:
  job_id             The numeric job ID from EcoTaxa.
  token_file_path    Path to a file containing your API bearer token.
  output_file_path   Path (with filename) where the downloaded file will be saved.

Options:
  --unzip            Extract the ZIP file after download (default: off)
  --help             Display this help message and exit.

Examples:
  # Download only (no extraction)
  $0 149243 ~/.ecotaxa_token ./job_149243_output.zip

  # Download and extract the zip
  $0 --unzip 149243 ~/.ecotaxa_token ./output.zip

EOF
    exit 0
}

# === ERROR HANDLING ===
function error_exit() {
    echo "❌ Error: $1"
    exit 1
}

# === ARGUMENT PARSING ===
POSITIONAL_ARGS=()

while [[ $# -gt 0 ]]; do
    case $1 in
        --unzip)
            UNZIP=true
            shift
            ;;
        --help)
            usage
            ;;
        -*|--*)
            error_exit "Unknown option: $1"
            ;;
        *)
            POSITIONAL_ARGS+=("$1")
            shift
            ;;
    esac
done

# Restore positional arguments
set -- "${POSITIONAL_ARGS[@]}"

# === VALIDATE REQUIRED ARGS ===
JOB_ID="$1"
TOKEN_FILE="$2"
OUTPUT_FILE="$3"

[ -z "$JOB_ID" ] && usage
[ -z "$TOKEN_FILE" ] && usage
[ -z "$OUTPUT_FILE" ] && usage
[ ! -f "$TOKEN_FILE" ] && error_exit "Token file not found: $TOKEN_FILE"
[ ! -r "$TOKEN_FILE" ] && error_exit "Token file is not readable: $TOKEN_FILE"

# === PREPARE ===
API_URL="https://ecotaxa.obs-vlfr.fr/api/jobs/${JOB_ID}/file"
TOKEN=$(cat "$TOKEN_FILE")

# === DOWNLOAD ===
echo "📥 Downloading job file ID ${JOB_ID}..."
HTTP_STATUS=$(curl -s -w "%{http_code}" -o "$OUTPUT_FILE" \
  -H "Authorization: Bearer ${TOKEN}" \
  -L "$API_URL")

# === CHECK STATUS CODE ===
if [ "$HTTP_STATUS" -eq 200 ]; then
  echo "✅ Download successful: $OUTPUT_FILE"
else
  rm -f "$OUTPUT_FILE"  # Remove partial file if failed
  error_exit "Download failed. HTTP status: $HTTP_STATUS"
fi

# === UNZIP IF REQUESTED ===
if [ "$UNZIP" = true ]; then
  if file "$OUTPUT_FILE" | grep -q "Zip archive data"; then
    echo "📦 Extracting ZIP file..."
    unzip -o "$OUTPUT_FILE" -d "$(dirname "$OUTPUT_FILE")" || error_exit "Failed to unzip file."
    echo "✅ Extraction complete."
  else
    echo "⚠️ File is not a ZIP archive. Skipping extraction."
  fi
fi
