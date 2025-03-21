#!/bin/bash

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

show_processes=false
show_proc_mem=false
show_sys_mem=false
show_disk=false

while getopts "psmd" opt; do
  case $opt in
    p) show_processes=true ;;
    s) show_sys_mem=true ;;
    m) show_proc_mem=true ;;
    d) show_disk=true ;;
    \?) echo "Invalid option: -$OPTARG" >&2; exit 1 ;;
  esac
done

if $show_processes; then
  echo -e "${GREEN}=== Process Status ===${NC}"
  ps aux | grep -E 'nginx|mysqld|postgres|node|java|python' | grep -v grep
fi

if $show_proc_mem; then
  echo -e "\n${YELLOW}=== Memory Usage Per Process ===${NC}"

  mysql_raw=$(ps aux | grep mysqld | grep -v grep | awk '{sum+=$6} END {print sum/1024}')
  postgres_raw=$(ps aux | grep postgres | grep -v grep | awk '{sum+=$6} END {print sum/1024}')
  nginx_raw=$(ps aux | grep nginx | grep -v grep | awk '{sum+=$6} END {print sum/1024}')
  node_raw=$(ps aux | grep node | grep -v grep | awk '{sum+=$6} END {print sum/1024}')
  java_raw=$(ps aux | grep java | grep -v grep | awk '{sum+=$6} END {print sum/1024}')
  python_raw=$(ps aux | grep python | grep -v grep | awk '{sum+=$6} END {print sum/1024}')

  mysql_mem=$(printf "%.2f" "$mysql_raw")
  postgres_mem=$(printf "%.2f" "$postgres_raw")
  nginx_mem=$(printf "%.2f" "$nginx_raw")
  node_mem=$(printf "%.2f" "$node_raw")
  java_mem=$(printf "%.2f" "$java_raw")
  python_mem=$(printf "%.2f" "$python_raw")

  if [[ $(echo "$mysql_mem > 0" | bc -l) -eq 1 ]]; then
    echo "MySQL: ${mysql_mem}MB"
  fi
  if [[ $(echo "$postgres_mem > 0" | bc -l) -eq 1 ]]; then
    echo "PostgreSQL: ${postgres_mem}MB"
  fi
  if [[ $(echo "$nginx_mem > 0" | bc -l) -eq 1 ]]; then
    echo "Nginx: ${nginx_mem}MB"
  fi
  if [[ $(echo "$node_mem > 0" | bc -l) -eq 1 ]]; then
    echo "Node: ${node_mem}MB"
  fi
  if [[ $(echo "$java_mem > 0" | bc -l) -eq 1 ]]; then
    echo "Java: ${java_mem}MB"
  fi
  if [[ $(echo "$python_mem > 0" | bc -l) -eq 1 ]]; then
    echo "Python: ${python_mem}MB"
  fi
fi

if $show_sys_mem; then
  echo -e "\n${BLUE}=== System Memory ===${NC}"
  free -h
fi

if $show_disk; then
  echo -e "\n${MAGENTA}=== Disk Space ===${NC}"
  df -h
fi
