
import os
import requests
import zipfile
from time import sleep

def download_with_retries(url, file_path, max_retries=5, backoff_factor=1):
    retries = 0
    while retries < max_retries:
        try:
            # Baixa o conteúdo em chunks para lidar com grandes arquivos
            with requests.get(url, stream=True) as response:
                response.raise_for_status()
                mode = 'ab' if os.path.exists(file_path) else 'wb'
                downloaded = os.path.getsize(file_path) if os.path.exists(file_path) else 0
                
                headers = {'Range': f'bytes={downloaded}-'}
                with requests.get(url, headers=headers, stream=True) as response:
                    response.raise_for_status()
                    with open(file_path, mode) as file:
                        for chunk in response.iter_content(chunk_size=8192):
                            if chunk:
                                file.write(chunk)
            
            print("Download concluído!")
            return
        except (requests.exceptions.RequestException, requests.exceptions.ChunkedEncodingError) as e:
            retries += 1
            print(f"Erro durante o download: {e}. Tentativa {retries} de {max_retries}...")
            sleep(backoff_factor * retries)  # Espera um tempo crescente antes de tentar novamente
            if retries == max_retries:
                raise Exception("Número máximo de tentativas de download alcançado. Abortando.")

def rename_zip_if_needed(zip_path, desired_name):
    current_name = os.path.basename(zip_path)
    if desired_name not in current_name:
        new_zip_path = os.path.join(os.path.dirname(zip_path), desired_name + ".zip")
        os.rename(zip_path, new_zip_path)
        return new_zip_path
    return zip_path

def download_and_extract_zip(url, extract_path):
    zip_filename = url.split('/')[-1]
    zip_path = os.path.join(extract_path, zip_filename)
    
    print(f"Fazendo o download do arquivo: {zip_filename} ...")
    download_with_retries(url, zip_path)
    
    ano = zip_filename.split('-')[-1].split('.')[0]  # Extrai o ano do nome original do arquivo
    desired_name = f"producao-pocos-{ano}"
    zip_path = rename_zip_if_needed(zip_path, desired_name)
    
    print(f"Arquivo ZIP final: {os.path.basename(zip_path)}")

    print("Verificando o conteúdo do arquivo ZIP...")
    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_content = zip_ref.namelist()
        print("Conteúdo do ZIP:", zip_content)
        
        directories = [item for item in zip_content if item.endswith('/')]
        if directories:
            print("Diretórios encontrados no ZIP:", directories)
            extraction_dir = os.path.join(extract_path, directories[0])
        else:
            print("Nenhum diretório encontrado no ZIP.")
            extraction_dir = os.path.join(extract_path, desired_name)

    if not os.path.exists(extraction_dir):
        os.makedirs(extraction_dir)
    
    print("Extraindo o conteúdo do arquivo ZIP...")
    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extractall(extraction_dir)
    
    os.remove(zip_path)
    
    print("Extração concluída!")
    print("")

def repeticao_anos(ano_inicial, ano_final):
    ano_final = ano_final + 1
    
    for i in range(ano_inicial, ano_final):
        url_ano = f"https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/ppg/pp/producao-pocos-{i}.zip"
        print("Ano:", i)
        download_and_extract_zip(url_ano, extract_path)


x = int(input("ano_inicial:"))
y = int(input("ano_final:"))
print("")

extract_path = r"C:\Users\kaior\git\projeto_r_aula\0. data-raw"
repeticao_anos(x, y)

#ano = 2021
#url = f"https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/de/ppg/pp/producao-pocos-{ano}.zip"
#download_and_extract_zip(url, extract_path)
